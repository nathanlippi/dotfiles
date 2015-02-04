#!/usr/bin/python3

import i3Common

# Read input piped from dmenu
new_workspace = sys.stdin.readlines()[0]

i3Common.switch_workspace_to_active_display(new_workspace)
