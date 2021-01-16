~~Superseded by my Rust `evdev-share` tool.~~ Not entirely - see #2.

Examples:
```
net-evdev --port 56701 --ip 192.168.68.113 --switch-key KeyRightalt --idle-cmd 'xrandr --output eDP-1 --brightness 1' --active-cmd 'xrandr --output eDP-1 --brightness 0.4'
```
```
net-evdev --port 56701 --ip 192.168.68.113 --switch-key KeyRightalt --active-cmd 'gxmessage -name net-evdev-popup -borderless Sending keypresses to Pi. Press AltGr again to return control.' --idle-cmd 'wmctrl -c net-evdev-popup'
```
