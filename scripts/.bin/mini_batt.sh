VALUE=$(pmset -g batt | grep InternalBattery | awk '{print $3}' | sed 's:..$::g')
SOURCE=$(pmset -g batt | grep 'Now drawing from' | awk '{print $4}' | sed 's:^.::g')
#   i      
BATTERY_ICON=" "
if [ $VALUE -lt 20 ];then
  BATTERY_ICON=" "
elif [ $VALUE -lt 40 ];then
  BATTERY_ICON=" "
elif [ $VALUE -lt 70 ];then
  BATTERY_ICON=" "
elif [ $VALUE -lt 90 ];then
  BATTERY_ICON=" "
fi
if [ "$SOURCE" = "AC" ]; then
  echo "$VALUE"
else
  echo "$VALUE"
fi
