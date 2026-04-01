#!/usr/bin/env python3
"""Convert an HTML report card to a single-page PDF using headless Chrome.

Usage: python3 html_to_pdf.py <input.html> [output.pdf]

Creates a temp copy with a very tall single @page, renders via Chrome CLI,
then trims trailing blank space using page content boundaries.
No external dependencies — uses only Python stdlib + Chrome.
"""

import os
import re
import shutil
import subprocess
import sys
import tempfile


def find_chrome():
    candidates = [
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
        "/Applications/Chromium.app/Contents/MacOS/Chromium",
        "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
        "google-chrome", "google-chrome-stable", "chromium", "chromium-browser",
    ]
    for c in candidates:
        if os.path.isfile(c):
            return c
        found = shutil.which(c)
        if found:
            return found
    return None


def html_to_pdf(html_path, pdf_path=None):
    html_path = os.path.abspath(html_path)
    if not os.path.exists(html_path):
        print(f"Error: {html_path} not found", file=sys.stderr)
        sys.exit(1)

    if pdf_path is None:
        pdf_path = os.path.splitext(html_path)[0] + ".pdf"
    pdf_path = os.path.abspath(pdf_path)

    chrome = find_chrome()
    if not chrome:
        print("Error: Chrome/Chromium not found.", file=sys.stderr)
        sys.exit(1)

    # Create a temp HTML that forces a single very tall page
    with open(html_path, "r") as f:
        html = f.read()

    # Inject/replace @page rule to use a single tall page
    tall_page_css = "@page { size: 11in 500in; margin: 0.4in; }"
    if "@page" in html:
        html = re.sub(r"@page\s*\{[^}]*\}", tall_page_css, html)
    elif "</style>" in html:
        html = html.replace("</style>", f"  {tall_page_css}\n</style>", 1)
    else:
        html = html.replace("</head>", f"<style>{tall_page_css}</style>\n</head>", 1)

    fd, tmp_html = tempfile.mkstemp(suffix=".html")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(html)

        cmd = [
            chrome,
            "--headless",
            "--disable-gpu",
            "--no-sandbox",
            "--run-all-compositor-stages-before-draw",
            "--virtual-time-budget=5000",
            f"--print-to-pdf={pdf_path}",
            "--print-to-pdf-no-header",
            "--no-pdf-header-footer",
            f"file://{tmp_html}",
        ]

        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
        if not (os.path.exists(pdf_path) and os.path.getsize(pdf_path) > 0):
            print(f"Error: PDF generation failed", file=sys.stderr)
            if result.stderr:
                print(result.stderr, file=sys.stderr)
            sys.exit(1)

        # Trim the PDF: rewrite the MediaBox to crop trailing blank space.
        # Read the PDF, find the page MediaBox, and set height to match content.
        # We detect content boundary by finding the last content stream position.
        try:
            trim_pdf_blank_space(pdf_path)
        except Exception as e:
            # Trimming is best-effort; the PDF is still valid
            print(f"Note: Could not trim blank space: {e}", file=sys.stderr)

        print(f"PDF saved to: {pdf_path}")

    finally:
        if os.path.exists(tmp_html):
            os.unlink(tmp_html)


def trim_pdf_blank_space(pdf_path):
    """Trim trailing blank space from a single-page PDF by adjusting MediaBox height.

    Uses Chrome's content stream to find actual content bounds.
    Falls back to a heuristic based on text/graphics positions.
    """
    with open(pdf_path, "rb") as f:
        data = f.read()

    # Find MediaBox - format: /MediaBox [x0 y0 x1 y1]
    mb_match = re.search(rb"/MediaBox\s*\[\s*([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s*\]", data)
    if not mb_match:
        return

    page_width = float(mb_match.group(3))
    page_height = float(mb_match.group(4))

    # Find all Y-coordinate references in content streams to determine lowest content
    # Look for text positioning (Td, TD, Tm) and graphics (m, l, re, c) commands
    # In PDF coordinate system, Y=0 is at the bottom of the page
    y_positions = []

    # Find "number Td" or "number TD" patterns (text positioning)
    for m in re.finditer(rb"([\d.]+)\s+([\d.]+)\s+(?:Td|TD|Tm|m|l|re|cm)", data):
        try:
            y = float(m.group(2))
            if 0 < y < page_height:
                y_positions.append(y)
        except ValueError:
            pass

    if not y_positions:
        return

    # The minimum Y position is the lowest content on the page
    # (PDF Y goes bottom-up, so min Y = closest to bottom)
    min_y = min(y_positions)

    # Add padding below the lowest content (72 points = 1 inch)
    new_height = page_height - min_y + 72

    if new_height >= page_height * 0.9:
        return  # Not much to trim

    # Replace MediaBox with cropped height
    old_mb = mb_match.group(0)
    new_mb = f"/MediaBox [0 {page_height - new_height:.1f} {page_width:.1f} {page_height:.1f}]".encode()
    data = data.replace(old_mb, new_mb, 1)

    with open(pdf_path, "wb") as f:
        f.write(data)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input.html> [output.pdf]", file=sys.stderr)
        sys.exit(1)

    html_to_pdf(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else None)
