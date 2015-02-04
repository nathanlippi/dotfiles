#!/usr/bin/python3

import i3Common

new_workspace = i3Common.choose_workspace()

command = "move container to workspace "
command += new_workspace

i3Common.ipc_query(command, outputCommand=False)
