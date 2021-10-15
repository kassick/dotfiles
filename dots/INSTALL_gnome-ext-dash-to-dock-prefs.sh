#!/bin/sh

# autohide behaviour
gsettings set org.gnome.shell.extensions.dash-to-dock autohide true
gsettings set org.gnome.shell.extensions.dash-to-dock pressure-threshold 50
gsettings set org.gnome.shell.extensions.dash-to-dock intellihide-mode 'ALL_WINDOWS'
gsettings set org.gnome.shell.extensions.dash-to-dock animation-time '0.1'

# appearence
gsettings set org.gnome.shell.extensions.dash-to-dock dock-fixed false
gsettings set org.gnome.shell.extensions.dash-to-dock dock-position BOTTOM

# disable super-NUM launchers
gsettings set org.gnome.shell.extensions.dash-to-dock hot-keys false
gsettings set org.gnome.shell.keybindings switch-to-application-1 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-2 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-3 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-4 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-5 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-6 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-7 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-8 '[]'
gsettings set org.gnome.shell.keybindings switch-to-application-9 '[]'
