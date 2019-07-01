VALUE=$(pmset -g batt | grep InternalBattery | awk '{print $3}' | sed 's:..$::g')
SOURCE=$(pmset -g batt | grep 'Now drawing from' | awk '{print $4}' | sed 's:^.::g')

# BATTERY_ICON is only available with patched fonts
#   i      
BATTERY_ICON=" "
if [ "$VALUE" -lt 10 ];then
  VALUE="#[bg=red,fg=white] LOW BATTERY | $VALUE% #[bg=default]"
elif [ "$VALUE" -lt 20 ];then
  BATTERY_ICON=" "
  VALUE="$VALUE%"
elif [ "$VALUE" -lt 40 ];then
  BATTERY_ICON=" "
  VALUE="$VALUE%"
elif [ "$VALUE" -lt 70 ];then
  BATTERY_ICON=" "
  VALUE="$VALUE%"
elif [ "$VALUE" -lt 90 ];then
  BATTERY_ICON=" "
  VALUE="$VALUE%"
else
  VALUE="$VALUE%"
fi
if [ "$SOURCE" = "AC" ]; then
  echo "#[fg=yellow]$VALUE"
else
  echo "$VALUE"
fi
