/**
 * Claude Code Auto-Memory Bridge
 *
 * Claude Code accumulates project learnings on its own ("auto memory") under
 * ~/.claude/projects/<project>/memory/ — MEMORY.md as an index plus topic
 * files with more detail. This extension surfaces that same MEMORY.md index
 * inside pi's system prompt, so learnings Claude Code captured about this
 * repo (build commands, debugging insights, preferences, gotchas) are
 * visible to pi too, without duplicating the work.
 *
 * Read-only: this extension never writes to Claude Code's memory directory.
 * pi's own memory/instruction story (AGENTS.md, skills) is untouched.
 *
 * Project key derivation mirrors Claude Code: the git repo root (falling
 * back to cwd outside a repo) with `/` replaced by `-`.
 *
 * Usage:
 * 1. Lives in ~/.pi/agent/extensions/ (global, auto-discovered).
 * 2. Work as usual — if a matching Claude Code memory directory exists,
 *    its MEMORY.md gets appended to pi's system prompt automatically,
 *    capped at 200 lines / 25KB (same limit Claude Code itself applies).
 * 3. Run /claude-memory to check what was found and where.
 */

import * as fs from "node:fs";
import * as path from "node:path";
import * as os from "node:os";
import { execSync } from "node:child_process";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const MAX_LINES = 200;
const MAX_BYTES = 25 * 1024; // 25KB — matches Claude Code's own MEMORY.md load cap

function findGitRoot(cwd: string): string {
  try {
    const root = execSync("git rev-parse --show-toplevel", {
      cwd,
      stdio: ["ignore", "pipe", "ignore"],
    })
      .toString()
      .trim();
    return root || cwd;
  } catch {
    return cwd;
  }
}

function projectKey(root: string): string {
  return root.replace(/\//g, "-");
}

function truncateMemory(content: string): { text: string; truncated: boolean } {
  const lines = content.split("\n");
  let text = lines.slice(0, MAX_LINES).join("\n");
  let truncated = lines.length > MAX_LINES;
  if (Buffer.byteLength(text, "utf8") > MAX_BYTES) {
    text = Buffer.from(text, "utf8").subarray(0, MAX_BYTES).toString("utf8");
    truncated = true;
  }
  return { text, truncated };
}

export default function claudeAutoMemoryExtension(pi: ExtensionAPI) {
  let memoryDir: string | undefined;
  let memoryMd: string | undefined;
  let truncated = false;
  let topicFiles: string[] = [];

  function load(cwd: string) {
    memoryDir = undefined;
    memoryMd = undefined;
    truncated = false;
    topicFiles = [];

    const root = findGitRoot(cwd);
    const dir = path.join(os.homedir(), ".claude", "projects", projectKey(root), "memory");
    const memoryPath = path.join(dir, "MEMORY.md");

    if (!fs.existsSync(memoryPath)) return;

    const raw = fs.readFileSync(memoryPath, "utf8").trim();
    if (!raw) return;

    memoryDir = dir;
    const result = truncateMemory(raw);
    memoryMd = result.text;
    truncated = result.truncated;

    topicFiles = fs
      .readdirSync(dir, { withFileTypes: true })
      .filter((e) => e.isFile() && e.name.endsWith(".md") && e.name !== "MEMORY.md")
      .map((e) => e.name)
      .sort();
  }

  // (Re)scan on every session lifecycle start, matching CLAUDE.md-style loading.
  pi.on("session_start", async (_event, ctx) => {
    load(ctx.cwd);
    if (memoryMd) {
      ctx.ui.notify(
        `Loaded Claude Code auto-memory (${topicFiles.length} topic file(s)) from ${memoryDir}`,
        "info",
      );
    }
  });

  pi.on("before_agent_start", async (event) => {
    if (!memoryMd) return;

    const topicsBlock =
      topicFiles.length > 0
        ? `\nAdditional topic files with more detail (read on demand with the read tool):\n${topicFiles
            .map((f) => `- ${path.join(memoryDir as string, f)}`)
            .join("\n")}\n`
        : "";

    return {
      systemPrompt:
        event.systemPrompt +
        `

## Claude Code Auto-Memory

Claude Code has separately accumulated the following learnings about this
project (read-only reference from ${memoryDir}/MEMORY.md; pi does not write
to this directory):

${memoryMd}
${truncated ? "\n(index truncated — read the file directly for the full contents)\n" : ""}${topicsBlock}`,
    };
  });

  pi.registerCommand("claude-memory", {
    description: "Show Claude Code auto-memory status for this project",
    handler: async (_args, ctx) => {
      if (!memoryMd) {
        ctx.ui.notify("No Claude Code auto-memory found for this project.", "warning");
        return;
      }
      ctx.ui.notify(
        `Claude Code auto-memory: ${memoryDir} (${topicFiles.length} topic file(s), ${
          truncated ? "index truncated at load" : "full index loaded"
        })`,
        "info",
      );
    },
  });
}
