#!/usr/bin/python3

import subprocess
import sys
from json import loads
from os import popen

# Read input piped from dmenu
newWorkspace = sys.stdin.readlines()[0]

def ipc_query(req="command", msg=""):
    ans = popen("i3-msg -t " + req + " " +  msg).readlines()[0]
    return loads(ans)

active_display = None
for w in ipc_query(req="get_workspaces"):
    if w['focused']:
        active_display = w['output']

command = "workspace " + newWorkspace + "; "
command += "move workspace to output " + active_display + ";"
command += " workspace " + newWorkspace + ";"
command += " workspace " + newWorkspace + ";" # Double command focuses workspace

test = subprocess.Popen(["i3-msg", command], stdout=subprocess.PIPE)
