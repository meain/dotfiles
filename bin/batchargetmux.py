#!/usr/bin/env python
# coding=UTF-8

import math, subprocess

color_green = '%{[32m%}'
color_yellow = '%{[1;33m%}'
color_red = '%{[31m%}'
color_reset = '%{[00m%}'

try:
    p = subprocess.Popen(["ioreg", "-rc", "AppleSmartBattery"], stdout=subprocess.PIPE)
    output = p.communicate()[0]

    o_max = [l for l in output.splitlines() if 'MaxCapacity' in l][0]
    o_cur = [l for l in output.splitlines() if 'CurrentCapacity' in l][0]

    b_max = float(o_max.rpartition('=')[-1].strip())
    b_cur = float(o_cur.rpartition('=')[-1].strip())

    charge = b_cur / b_max
    charge_threshold = int(math.ceil(10 * charge))

    # Output
    total_slots, slots = 10, []
    filled = int(math.ceil(charge_threshold * (total_slots / 10.0))) * u'▸'
    filled_string = int((charge_threshold * (total_slots)/10))
    empty = (total_slots - len(filled)) * u'▹'

    # out = (filled + empty).encode('utf-8')
    out = str(filled_string)

    color_out = (
            color_green if len(filled) > 6
            else color_yellow if len(filled) > 4
            else color_red
            )
except:
    cur = open('/sys/class/power_supply/BAT0/charge_now', 'r').read()
    full = open('/sys/class/power_supply/BAT0/charge_full', 'r').read()
    out = str(int(float(cur)/float(full)*100))

    color_out = (
            color_green if int(out) > 60
            else color_yellow if int(out) > 40
            else color_red
            )

import sys

out = '±' + out
sys.stdout.write(out)
