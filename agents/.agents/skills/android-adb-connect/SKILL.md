---
name: android-adb-connect
description: >
  Connect to an Android phone over ADB wireless (Galaxy S25 at meain's setup),
  install/launch APKs, take screenshots, and drive the UI via uiautomator.
  Triggers: "connect to phone", "adb connect", "test on phone", "screenshot the phone",
  "install on phone", "phone is at 192.168.1.3", any task that needs to drive an Android
  device from the host.
argument-hint: "[host:port or pairing code]"
---

# Connect to Android phone over ADB wireless

This skill captures the actual quirks of meain's setup. The phone is a **Samsung Galaxy S25** on the same WiFi as the host. The ADB SDK lives at `~/Android/sdk-edo` (not the system path) — always `export ANDROID_HOME=$HOME/Android/sdk-edo` before running `adb`.

## When to use

- The user says "connect to the phone", "test on phone", "adb connect …"
- They paste a `host:port` and optionally a 6-digit pairing code
- You need to install an APK on the device, drive the UI, or screenshot it
- A previous connection dropped (port rotates every time wireless debugging is toggled)

## Step 1 — set up the environment

```bash
export ANDROID_HOME=$HOME/Android/sdk-edo
# Java is only needed for builds, not for adb itself
export PATH=$ANDROID_HOME/platform-tools:$PATH

adb devices   # check what's already attached
```

If `adb` isn't found, the SDK is at `~/Android/sdk-edo/platform-tools/adb`.

If `adb devices` is empty, don't ask the user for a `host:port` yet. The phone usually reconnects by itself over mDNS (it shows up as `adb-<serial>-xxxx._adb-tls-connect._tcp`) once the adb server restarts, so try `adb kill-server && adb start-server; sleep 2; adb devices -l` (with `dangerouslyDisableSandbox: true`) first. Only fall back to Step 2 if the phone is still missing.

If any entry shows status `device`, check if it's the Galaxy S25 before proceeding to connect:

```bash
adb -s <device-id> shell getprop ro.product.model
# SM-S938B → it's the Galaxy S25, use this device ID and skip Step 2 entirely
```

Use that ID with `-s` for all subsequent commands.

## Step 2 — connect

### Already paired (the common case)

The phone remembers the pairing across reboots, but the **wireless port changes** every time Wireless Debugging is toggled. The user will typically paste a new `host:port` — use it directly:

```bash
adb connect 192.168.1.3:38181
```

If that fails with `failed to authenticate` or hangs, the pairing has expired — go to Step 2b.

### First-time or expired pairing

The user needs to open **Settings → Developer options → Wireless debugging → Pair device with pairing code** on the phone. That screen shows a `host:pairing_port` and a 6-digit code. They are *separate* from the regular `host:port` shown on the main Wireless debugging screen.

```bash
adb pair 192.168.1.3:40319        # pairing port from the phone (different from connect port)
# adb prompts for the code; or pass it inline:
adb pair 192.168.1.3:40319 657538
```

Then connect using the regular (non-pairing) port shown on the main Wireless debugging screen:

```bash
adb connect 192.168.1.3:38181
```

### QR pairing (no code or port to type)

Prefer this for first-time or expired pairing. Run `bash ~/.claude/skills/android-adb-connect/qr-pair.sh` with `dangerouslyDisableSandbox: true` and `run_in_background: true`. It makes a QR (`WIFI:T:ADB;S:<name>;P:<password>;;`, via `nix run nixpkgs#qrencode`), writes `/tmp/adb-qr.png` (and opens it in Preview), waits for the phone's pairing service, then runs `adb pair` and lists devices. Once paired, the phone shows in `adb devices` as `adb-<serial>-xxxx._adb-tls-connect._tcp`; no `adb connect` needed.

**Show the QR in the chat**, don't just say it was opened: as soon as the script prints "QR for … opened", reply with a line that is only `![adb pairing QR](/tmp/adb-qr.png)` (Dashy renders it inline), plus one line telling the user to scan it. Do this every run, since each run makes a new QR.

Gotchas, all hit in practice:

- **Timing:** the phone advertises `_adb-tls-pairing._tcp` only while **Pair device with QR code** (the camera screen) is open. The script waits about 120 s, and the user can't reply while it blocks. So first ask the user to open the camera screen and say "ready", then start the script and tell them to scan straight away. Each run makes a new QR with a new password, so a scan of an old image never works (the phone just spins).
- **`adb mdns services` is empty on this Mac**, so discovery goes through `dns-sd` (`-B` to browse, `-L` for host and port, `-G v4` for the IP). When parsing `-G`, match rows where `$2=="Add"`; the header line also contains "Add".
- **`error: protocol fault (couldn't read status message)`** from `adb pair` means the local adb server is stale (one had been up for days), not that the phone is wrong. `adb kill-server && adb start-server`, then re-run with a fresh scan. It's safe when no devices are attached.
- **Stray `dns-sd` processes** can be left behind by interrupted runs; `pkill dns-sd`.
- The script writes `ip:port password` to `/tmp/adb-qr.last` so a failed `adb pair` can be retried without scanning again, while the phone is still on the camera screen.
- Don't use `tr ... | head -c` for random strings under `set -o pipefail` (SIGPIPE, exit 141); the script uses `openssl rand -hex`.

### Discovering the port

If the user can't read the port off the phone, try mDNS:

```bash
adb mdns services
```

It lists `adb-tls-pairing` and `adb-tls-connect` entries with their ports. The pairing entry rotates each time the pairing code screen is opened.

### TLS failures

`SSLV3_ALERT_CERTIFICATE_UNKNOWN` or "TLS handshake failed" means the host and phone disagree on the pairing — re-pair from scratch via Step 2b. Don't retry the same `adb connect` in a loop.

## Step 3 — verify and identify

```bash
adb devices
# Should show e.g. 192.168.1.3:38181  device
```

Always pass `-s <host:port>` to subsequent commands when more than one device is listed (the `adb-RZGY...` mDNS entry can show up alongside the IP one).

## Step 4 — common operations

### Install / launch an app

```bash
adb -s 192.168.1.3:38181 install -r app/build/outputs/apk/debug/app-debug.apk
adb -s 192.168.1.3:38181 shell am force-stop com.example.app
adb -s 192.168.1.3:38181 shell am start -n com.example.app/.MainActivity
```

### Screenshot

```bash
adb -s 192.168.1.3:38181 exec-out screencap -p > /tmp/phone.png
# Read it with the Read tool. If the API rejects large images, resize:
# sips -z 1200 555 /tmp/phone.png --out /tmp/phone_small.png
```

### Drive the UI (taps, text)

Coordinates are in the **device's native resolution** (1080×2340 on the S25). Don't scale to display size. Get exact bounds via:

```bash
adb -s 192.168.1.3:38181 shell uiautomator dump /sdcard/window.xml >/dev/null
adb -s 192.168.1.3:38181 shell cat /sdcard/window.xml \
  | tr '>' '\n' \
  | grep -oE '(text|content-desc|bounds)="[^"]+'
```

Then tap the center of `bounds="[x1,y1][x2,y2]"`:

```bash
adb -s 192.168.1.3:38181 shell input tap 540 1200
adb -s 192.168.1.3:38181 shell input text "hello%sworld"   # %s for spaces
adb -s 192.168.1.3:38181 shell input keyevent KEYCODE_BACK
adb -s 192.168.1.3:38181 shell input keyevent KEYCODE_WAKEUP
```

Quirks of `input text`:
- Spaces must be `%s`, not literal spaces.
- Avoid `!` — zsh history expansion can trigger before the command reaches the device.
- Newlines aren't supported; use `KEYCODE_ENTER` separately.

### Keep screen awake during testing

```bash
adb -s 192.168.1.3:38181 shell svc power stayon true    # USB or AC
# Always turn it back off when done:
adb -s 192.168.1.3:38181 shell svc power stayon false
```

### Logs

```bash
adb -s 192.168.1.3:38181 logcat -d -t 300 | grep com.example.app
```

### Pulling the app database (Room WAL gotcha)

`adb pull` of just the `.db` file is incomplete because Room uses WAL mode. Force-stop the app first to flush, then pull all three files:

```bash
adb -s 192.168.1.3:38181 shell am force-stop com.example.app
for f in app.db app.db-wal app.db-shm; do
  adb -s 192.168.1.3:38181 shell "run-as com.example.app cat databases/$f 2>/dev/null" > /tmp/$f
done
sqlite3 /tmp/app.db "SELECT ..."
```

The device's `sqlite3` binary is missing on stock Samsung — always pull and query locally.

## Project context

For the Edo project at `/Users/meain/dev/src/edo`, see its `CLAUDE.md` for the install/launch/screenshot pattern with the actual package name (`com.edo.app`).
