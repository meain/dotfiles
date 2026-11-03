#!/usr/bin/env bash
# QR pairing for Android wireless debugging (macOS: discovery through dns-sd, since `adb mdns services` is empty here).
# Phone: Settings > Developer options > Wireless debugging > Pair device with QR code, then scan the QR shown here.
export ANDROID_HOME=$HOME/Android/sdk-edo
export PATH=$ANDROID_HOME/platform-tools:$PATH

name="dashy-$(openssl rand -hex 3)"
pw="$(openssl rand -hex 5)"
png=/tmp/adb-qr.png
type=_adb-tls-pairing._tcp

nix run nixpkgs#qrencode -- -s 12 -m 2 -o "$png" "WIFI:T:ADB;S:$name;P:$pw;;" || exit 1
open "$png"
echo "QR for '$name' opened ($png). Scan it from Wireless debugging > Pair device with QR code. Waiting up to 120s..."

# Run a dns-sd command for a few seconds and print what it said.
sd() { local secs=$1; shift; dns-sd "$@" >/tmp/adb-sd.out 2>&1 & local p=$!; sleep "$secs"; kill "$p" 2>/dev/null; wait "$p" 2>/dev/null; cat /tmp/adb-sd.out; }

for _ in $(seq 1 40); do
  if sd 3 -B "$type" local. | grep -qF "$name"; then
    echo "Phone advertised '$name', resolving..."
    reach=$(sd 3 -L "$name" "$type" local. | grep -o 'can be reached at .*' | head -1)
    host=$(sed -E 's/can be reached at (.*):([0-9]+).*/\1/' <<<"$reach")
    port=$(sed -E 's/can be reached at (.*):([0-9]+).*/\2/' <<<"$reach")
    ip=$(sd 3 -G v4 "$host" | awk '$2=="Add" {print $6; exit}')
    echo "host=$host ip=$ip port=$port"
    [ -n "$ip" ] && [ -n "$port" ] || { echo "Could not resolve the phone." >&2; exit 1; }
    echo "$ip:$port $pw" >/tmp/adb-qr.last
    ADB_TRACE=adb,auth adb pair "$ip:$port" "$pw" 2>&1 | tail -25
    sleep 3
    adb devices
    exit 0
  fi
done
echo "Timed out: the phone never advertised '$name'." >&2
exit 1
