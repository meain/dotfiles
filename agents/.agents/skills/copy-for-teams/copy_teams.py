import subprocess
import sys

html = sys.stdin.read()
proc = subprocess.Popen(
    ["osascript", "-e", """
use framework "AppKit"
on run argv
    set htmlString to item 1 of argv
    set htmlData to (current application's NSString's stringWithString:htmlString)'s dataUsingEncoding:(current application's NSUTF8StringEncoding)
    set pb to current application's NSPasteboard's generalPasteboard()
    pb's clearContents()
    pb's setData:htmlData forType:"public.html"
end run
""", html], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
out, err = proc.communicate()
if proc.returncode != 0:
    print(err.decode())
else:
    print("OK")
