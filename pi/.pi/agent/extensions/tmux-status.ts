/**
 * Tmux Status Extension
 *
 * Updates ~/.local/share/aistatus/pid-<pid>.json on agent lifecycle events.
 * Readers discover live pi/claude processes from the tmux pane tree and use
 * this file only as a status/project lookup keyed by pid, so it stays
 * accurate across pane splits, moves, and restarts.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { writeFile, mkdir } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

export default function (pi: ExtensionAPI) {
  const STATUS_DIR = join(homedir(), ".local/share/aistatus");

  async function writeStatus(
    status: "working" | "idle",
    sessionId: string,
    project: string,
    event: string
  ): Promise<void> {
    if (!process.env.TMUX_PANE) return;
    await mkdir(STATUS_DIR, { recursive: true });
    const pid = process.pid;
    const payload = JSON.stringify({
      session_id: sessionId,
      pid,
      status,
      hook_event: event,
      project,
      timestamp: Math.floor(Date.now() / 1000),
    });
    const tmp = join(STATUS_DIR, `.tmp.${pid}`);
    await writeFile(tmp, payload);
    await pi.exec("mv", [tmp, join(STATUS_DIR, `pid-${pid}.json`)]);
  }

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
