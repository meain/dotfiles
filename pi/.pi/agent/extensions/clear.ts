/**
 * /clear — clear the terminal screen (and scrollback) without touching
 * the session or conversation history. Just a screen wipe, like running
 * `clear` in a shell, so a cluttered transcript doesn't force /new.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("clear", {
    description: "Clear the terminal screen",
    handler: async () => {
      // Clear scrollback (3J), visible screen (2J), and move cursor home (H)
      process.stdout.write("\x1b[3J\x1b[2J\x1b[H");
    },
  });
}
