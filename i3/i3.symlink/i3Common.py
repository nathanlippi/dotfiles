import subprocess
from json import loads
from os import popen

def ipc_query(req="command", msg=""):
    ans = popen("i3-msg -t " + req + " " +  msg).readlines()[0]
    return loads(ans)

def get_active_display():
    active_display = None
    for w in ipc_query(req="get_workspaces"):
        if w['focused']:
            active_display = w['output']
    return active_display

def i3Command(command):
    subprocess.Popen(["i3-msg", command], stdout=subprocess.PIPE)

def switch_workspace_to_active_display(new_workspace):
    active_display = get_active_display()

    command = "workspace " + new_workspace + "; "
    command += "move workspace to output " + active_display + ";"
    command += " workspace " + new_workspace + ";"
    # This double command focuses the new workspace
    command += " workspace " + new_workspace + ";"

    i3Command(command)
