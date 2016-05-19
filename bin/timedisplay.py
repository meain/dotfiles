#!/usr/bin/env python
# coding=UTF-8

import datetime

current_time = datetime.datetime.now()
hour = current_time.hour
minute = current_time.minute

out = str(hour)
import sys

color_green = '%{[32m%}'
color_yellow = '%{[1;33m%}'
color_red = '%{[31m%}'
color_blue = '%{[34m%}'
color_reset = '%{[00m%}'
color_out = (
        color_red if minute > 39
        else color_green if minute > 19
        else color_blue
        )

out = color_out + out + color_reset
sys.stdout.write(out)
