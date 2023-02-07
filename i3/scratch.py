#!/usr/bin/env python3

import i3ipc



i3 = i3ipc.Connection()

last_matched = False

s_class = 'scratchpad'



def move_to_scratchpad(w_id):

    i3.command('[instance=%s] kill' % (str(w_id)))



# Decide to switch window

def on_window_focus(i3, e):

    focused = i3.get_tree().find_focused()

    w_id = focused.window_class

    global last_matched

    global s_class



    if w_id == s_class:

        last_matched = True



    if last_matched and w_id != s_class:

        move_to_scratchpad(s_class)



# Subscribe to events

i3.on("window::focus", on_window_focus)



# Start the main loop and wait for events to come in.

i3.main()