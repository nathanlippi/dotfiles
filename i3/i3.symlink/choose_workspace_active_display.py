#!/usr/bin/python3

import i3Common

new_workspace = i3Common.choose_workspace()

i3Common.switch_workspace_to_active_display(new_workspace.strip())
