#!/usr/bin/python3

import subprocess
import json
from json import loads
from os import popen

def ipc_query(req="command", outputCommand=True):
    command = "i3-msg "
    if outputCommand:
      command += "-t "
    command += req

    ans = popen(command)
    if outputCommand:
        return loads(ans.readlines()[0])

def get_active_display():
    active_display = None
    for w in ipc_query(req="get_workspaces"):
        if w['focused']:
            active_display = w['output']
    return active_display

def i3Command(command):
    return popen("i3-msg -t " + command).readlines()[0]

def i3CommandPipe(command):
    return subprocess.Popen(["i3-msg", command], stdout=subprocess.PIPE)

def switch_workspace_to_active_display(new_workspace):
    active_display = get_active_display()

    command = "workspace " + new_workspace + "; "
    command += "move workspace to output " + active_display + ";"
    command += " workspace " + new_workspace + ";"
    # This double command focuses the new workspace
    command += " workspace " + new_workspace + ";"

    i3CommandPipe(command)

def get_workspaces():
  output = i3Command("get_workspaces");
  data = json.loads(output)
  data = sorted(data, key=lambda k: k['name'])
  return data

def get_workspace_names():
    workspace_names = []
    for workspace in get_workspaces():
      workspace_names.append(workspace['name'])
    return workspace_names

def choose_workspace():
    command = "echo '"
    command += '\n'.join(get_workspace_names())
    command += "' | dmenu -b"

    main_process = subprocess.Popen([command], stdout=subprocess.PIPE, shell=True)
    res = main_process.communicate() # Wait
    std_out = res[0]

    return std_out.decode("utf-8").strip()
