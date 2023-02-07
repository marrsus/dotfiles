import os
import subprocess
from datetime import datetime

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile import qtile
from libqtile import hook



mod = "mod4"
terminal = "alacritty"
browser = "vivaldi-stable"
file_manager = "alacritty -e ranger"
app_runner = "rofi -show run"
ide="code"

keys = [
    # Switch between windows
    Key([mod], "n", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "o", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "i", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "e", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    Key([mod, "shift"], "n", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "o", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "i", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "e", lazy.layout.shuffle_up(), desc="Move window up"),
    Key([mod, "control"], "n", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "o", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "i", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "e", lazy.layout.grow_up(), desc="Grow window up"),
    #Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "c", lazy.spawn(ide), desc="Launch vscode"),
    Key([mod], "x", lazy.spawn(file_manager), desc="Launch dolphin"),
    Key([mod], "s", lazy.spawn(browser), desc="Launch falkon"),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "a", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    #Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r",lazy.spawn(app_runner), desc="Spawn a command using a prompt widget"),
]
group_names = [
    ({"name": "","spawn": ide}),
    ({"name": "","spawn": terminal}),
    ({"name": "","spawn": browser}),
    ({"name": ""}),
    ({"name": ""}),
    ({"name": ""}),
    ({"name": ""}),
    ({"name": ""}),
    ({"name": "","spawn": file_manager}),
]
names=["1","2","3","4","5","6","7","8","9"]
groups = [Group(**kwargs) for kwargs in group_names]

for i,v in enumerate(groups):
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key([mod], names[i], lazy.group[v.name].toscreen()),
            # mod1+shift+group letter = switch to & move focused window to group
            Key([mod, "control"], names[i], lazy.window.togroup(v.name)),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )


layout_theme = {"margin":0,"border width":3,"border normal":"#000000","border_focus":"#6666ff"}

layouts = [
    layout.Columns(**layout_theme),
    # layout.Max(),
    # layout.Stack(num_stacks=2,**layout_theme),
    # layout.Bsp(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.MonadTall(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Tile(**layout_theme),
    # layout.TreeTab(**layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Zoomy(**layout_theme),
]

widget_defaults = dict(
    font="Fira Sans Medium",
    fontsize=13,
    padding=3,
)
extension_defaults = widget_defaults.copy()

sep_theme = {"padding":10,"foreground":"ff00ff","linewidth":2}

widgets_list: list = [
    ### Groups ###
    widget.Sep(linewidth=0, padding=6, background='#000000'),
    widget.GroupBox(
        font='nerdfont',
        fontsize=24,
        active='#995555',
        inactive='#333333',
        rounded=False,
        highlight_method="line",
    ),
    widget.WindowName(padding=5),
    widget.Sep(**sep_theme),
    widget.TextBox(text="",fontsize=30),
    widget.Net(format='{down} \u2193\u2191 {up}',mouse_callbacks={'Button1':  lazy.spawn('nm-connection-editor')}),
    widget.NetGraph(mouse_callbacks={'Button1':  lazy.spawn('nm-applet')}),
    widget.Sep(**sep_theme),
    widget.CPU(mouse_callbacks={'Button1':  lazy.spawn('alacritty -e htop')}),
    widget.CPUGraph(),
    widget.Sep(**sep_theme),
    widget.Memory(measure_mem='G'),
    widget.Sep(**sep_theme),
    widget.Volume(fmt='{}',mouse_callbacks={'Button1':  lazy.spawn('alacritty -e alsamixer')}),
    widget.Sep(**sep_theme),
    widget.Systray(),
    widget.Sep(**sep_theme),
    widget.Clock(format="%a %d-%b-%Y %-H:%M"),
    widget.Notify(),
    widget.CheckUpdates(),
    widget.Sep(**sep_theme),
    widget.WidgetBox(fontsize=20,text_closed='',close_button_location='right',widgets=[
        widget.Sep(linewidth=0,padding=4,),
        widget.TextBox(text="Shutdown",
            mouse_callbacks={"Button1": lambda: qtile.cmd_spawn("shutdown now")},),
        widget.Sep(linewidth=1),
        widget.TextBox(text="Reboot",
            mouse_callbacks={"Button1": lambda: qtile.cmd_spawn("reboot")},),
        ]),
    widget.Sep(padding=7,linewidth=0),
]


screen = Screen(
    top=bar.Bar(
       widgets_list,
            15,
            background='#111115',
   ),
)
screens = [screen]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    **layout_theme,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_type="utility"),
        Match(wm_type="notification"),
        Match(wm_type="toolbar"),
        Match(wm_type="splash"),
        Match(wm_type="dialog"),
        Match(wm_class="confirm"),
        Match(wm_class="dialog"),
        Match(wm_class="download"),
        Match(wm_class="error"),
        Match(wm_class="file_progress"),
        Match(wm_class="notification"),
        Match(wm_class="splash"),
        Match(wm_class="toolbar"),
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="pomotroid"),
        Match(wm_class="cmatrixterm"),
        Match(title="Farge"),
        Match(title="nnn"),
        Match(wm_class="org.gnome.Nautilus"),
        Match(wm_class="feh"),
        Match(wm_class="plank"),
        Match(wm_class="gnome-calculator"),
        Match(wm_class="blueberry"),
        Match(wm_class="protonvpn"),
    ]
)


floating_types = ["notification", "toolbar", "splash", "dialog", "dock"]

@lazy.function
def float_to_front(qtile):
    """
    Bring all floating windows of the group to front
    """
    global floating_windows
    floating_windows = []
    for window in qtile.currentGroup.windows:
        if window.floating:
            window.cmd_bring_to_front()
            floating_windows.append(window)
    floating_windows[-1].cmd_focus()

@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, "parent"):
        window.parent.minimized = False


wmname = "wahlqvist wm"
