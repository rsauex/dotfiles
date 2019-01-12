#!/usr/bin/env python3

from socket import socket, AF_UNIX, SOCK_STREAM
from signal import SIGRTMIN
from sys    import exit, argv
from subprocess import check_output, Popen
from logging import getLogger
from systemd.journal import JournalHandler

#### OPEN SYSTEMD JOURNAL LOGGER

log = getLogger('i3blocks acpi helper')
log.addHandler(JournalHandler())

log.info("Start i3blocks acpi helper")

#### RUN I3BLOCKS

i3blocks = Popen(["i3blocks"] + argv[1:])

#### ACPI PART

# Create socket
acpid_s = socket(AF_UNIX, SOCK_STREAM)

# Connect
try:
    acpid_s.connect("/var/run/acpid.socket")
except socket.error as msg:
    log.error("Cannot connect to ACPID socket")
    exit(1)

def acpid():
    while True:
        yield acpid_s.recv(4096)

#### MAIN LOOP

# Receive data
try:
    for event in acpid():
        if event[:24] == b"jack/headphone HEADPHONE":
            i3blocks.send_signal(SIGRTMIN+10)
        elif event[:10] == b"ac_adapter":
            i3blocks.send_signal(SIGRTMIN+12)
finally:
    log.info("Stop i3blocks acpi helper")
    acpid_s.close()
    i3blocks.terminate()
