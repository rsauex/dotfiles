#!/usr/bin/env python3

from Xlib.display import Display
from Xlib.ext import xinput

import yaml
import re
import sys

import subprocess

###

def display_enter(self):
    return self

def display_exit(self, type, value, traceback):
    self.close()
    #return isinstance(value, TypeError)
    return False

Display.__enter__ = display_enter
Display.__exit__  = display_exit

###

config_path = sys.argv[1]#"~/.keyboard.yaml"

with open(config_path, 'r') as config_stream:
    config = yaml.load(config_stream)

print(config)
    
def find_keyboard_config(name):
    for keyboard in config:
        if re.match(keyboard['name'], name) != None:
            return keyboard

def run_setxkbmap(deviceid, config):
    args = ['setxkbmap', '-device', str(deviceid), '-option']
    if 'layouts' in config:
        args.append('-layout')
        args.append(','.join(config['layouts']))
    if 'variants' in config:
        args.append('-variant')
        args.append(','.join(config['variants']))
    if 'options' in config:
        for option in config['options']:
            args.append('-option')
            args.append(option)
    print(args)
    subprocess.run(args)


###

def print_hierarchy_changed_event(display, event):
    subprocess.run(['xinput'])
    for info in event.data.info:
        print_info(display, info)

def print_info(display, info):
    if (info.flags & xinput.DeviceEnabled) != 0:
        name = display.xinput_query_device(info.deviceid).devices[0].name
        print('  <deviceid=%s attachment=%s type=%s enabled=%s flags=%s name=%s>' % (
            info.deviceid,
            info.attachment,
            info.type,
            info.enabled,
            info.flags,
            name,
        ))
        keyboard = find_keyboard_config(name)
        print(keyboard)
        run_setxkbmap(info.deviceid, keyboard)

def main():
    with Display() as display:
        extension_info = display.query_extension('XInputExtension')
        xinput_major = extension_info.major_opcode

        version_info = display.xinput_query_version()
        print('Found XInput version %u.%u' % (
          version_info.major_version,
          version_info.minor_version,
        ))

        screen = display.screen()
        screen.root.xinput_select_events([
          (xinput.AllDevices, xinput.HierarchyChangedMask),
        ])

        while True:
            event = display.next_event()
            if (event.type == display.extension_event.GenericEvent
                  and event.extension == xinput_major
                  and event.evtype == 11):
                print_hierarchy_changed_event(display, event)


if __name__ == '__main__':
    main()


