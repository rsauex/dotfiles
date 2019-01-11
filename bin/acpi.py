#!/usr/bin/env python3
from socket import socket, AF_UNIX, SOCK_STREAM
from os import kill
import signal
import logging
import sys
from systemd.journal import JournalHandler
from subprocess import check_output

# Open systemd logger
log = logging.getLogger('i3blocks acpi helper')
log.addHandler(JournalHandler())
log.info("Start i3blocks acpi helper")

def get_pid(name):
    return int(check_output(["pidof","-s",name]))

# Get i3blocks PID
try:
    i3blocks = get_pid("i3blocks")
except:
    log.error("Cannot find i3blocks PID")
    sys.exit(1)
    
# Create socket
s = socket(AF_UNIX, SOCK_STREAM)

# Path to acpi socket
acpi_addr = "/var/run/acpid.socket"

# Connect
try:
    s.connect(acpi_addr)
except socket.error as msg:
    log.error("Cannot connect to ACPI socket")
    sys.exit(1)

# Receive data
try:
    while True:
        data = s.recv(4096)
        #print(data)
        if data[:24] == b"jack/headphone HEADPHONE":
            kill(i3blocks, 44)
        elif data[:10] == b"ac_adapter":
            kill(i3blocks, 46)

finally:
    log.info("Stop i3blocks acpi helper")
    s.close()
