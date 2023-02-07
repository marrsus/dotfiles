#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch polybar
exec polybar -c /home/marrinus/.config/i3/polyconf-1 &
exec polybar -c /home/marrinus/.config/i3/polyconf-2 &

