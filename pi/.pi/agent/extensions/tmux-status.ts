/**
 * Tmux Status Extension
 *
 * Updates ~/.local/share/aistatus/window-<window_id>.json on agent lifecycle
 * events so the tmux window status line can show pi's working/idle state.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { writeFile, mkdir } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

export default function (pi: ExtensionAPI) {
  const STATUS_DIR = join(homedir(), ".local/share/aistatus");
  let windowId: string | undefined;

  async function resolveWindowId(): Promise<string | undefined> {
    const tmuxPane = process.env.TMUX_PANE;
    if (!tmuxPane) return undefined;
    try {
      const result = await pi.exec("tmux", [
        "display-message",
        "-p",
        "-t",
        tmuxPane,
        "#{window_id}",
      ]);
      return result.stdout.trim() || undefined;
    } catch {
      return undefined;
    }
  }

  async function writeStatus(
    status: "working" | "idle",
    sessionId: string,
    project: string,
    event: string
  ): Promise<void> {
    if (!windowId) return;
    await mkdir(STATUS_DIR, { recursive: true });
    const payload = JSON.stringify({
      session_id: sessionId,
      window_id: windowId,
      status,
      hook_event: event,
      project,
      agent: "pi",
      timestamp: Math.floor(Date.now() / 1000),
    });
    const tmp = join(STATUS_DIR, `.tmp.${windowId}.${process.pid}`);
    await writeFile(tmp, payload);
    await pi.exec("mv", [tmp, join(STATUS_DIR, `window-${windowId}.json`)]);
  }

  pi.on("session_start", async (_event, _ctx) => {
    windowId = await resolveWindowId();
  });

  pi.on("agent_start", async (_event, ctx) => {
    const sessionId = ctx.sessionManager.getSessionId?.() ?? "";
    const project = ctx.cwd.split("/").pop() ?? "";
    await writeStatus("working", sessionId, project, "agent_start");
  });

  pi.on("agent_end", async (_event, ctx) => {
    const sessionId = ctx.sessionManager.getSessionId?.() ?? "";
    const project = ctx.cwd.split("/").pop() ?? "";
    await writeStatus("idle", sessionId, project, "agent_end");
  });
}
