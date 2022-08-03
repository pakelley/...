#!/usr/bin/env python3
import logging
import pathlib
import re
from shutil import which
import subprocess
import sys
from simple_term_menu import TerminalMenu

DOTFILE_PATH = pathlib.Path(__file__).parent.parent

# logging setup
RESET_SEQ = "\033[0m"
COLOR_SEQ = "\033[1;%dm"
BOLD_SEQ = "\033[1m"
BLUE = COLOR_SEQ % 34

LOG = logging.getLogger(__name__)
LOG.setLevel(logging.DEBUG)
_HANDLER = logging.StreamHandler()
_HANDLER.setLevel(logging.DEBUG)
# TODO base color on logging level
_FORMATTER = logging.Formatter(f"%(asctime)s - {BLUE}%(levelname)s{RESET_SEQ} - %(message)s")
_HANDLER.setFormatter(_FORMATTER)
LOG.addHandler(_HANDLER)

# link config
link_menu = TerminalMenu(
    ["Yes", "No"],
    title="Ok, now I need to link your config files to the right places. Cool with that?",
)
may_i_please_link_configs = link_menu.show()
if may_i_please_link_configs == 0:
    LOG.info("Thank you, linking configs")
    subprocess.run(
        [
            "ln",
            "-svf",
            str(DOTFILE_PATH / ".mackup.cfg"),
            str(pathlib.Path.home() / ".mackup.cfg"),
        ]
    )
    if (DOTFILE_PATH / ".mackup").exists():
        subprocess.run(
            [
                "ln",
                "-svf",
                str(DOTFILE_PATH / ".mackup"),
                str(pathlib.Path.home() / ".mackup"),
            ]
        )
    subprocess.run(["mackup", "restore"])
else:
    LOG.info("Sorry to hear that, exiting as I cannot run without mackup :'(")
    sys.exit()

# Install programs with brew
brewpath = pathlib.Path.home() / ".config" / "homebrew"
brew_specs = [
    (brew_group.group("module"), path.name)
    for path in brewpath.iterdir()
    if (brew_group := re.match(r"(?P<module>[a-zA-Z]+)\.Brewfile$", path.name))
]
brew_groups, brewfiles = zip(*brew_specs)

terminal_menu = TerminalMenu(
    brew_groups,
    title="Which collections should brew install?",
    multi_select=True,
    show_multi_select_hint=True,
)
selected_indices = terminal_menu.show()

selected_brewfiles = {brew_groups[i]: brewfiles[i] for i in selected_indices}

LOG.info("This could take a while... plug me in and go touch some grass.")
for groupname, brewfile in selected_brewfiles.items():
    brewfile_path = brewpath / brewfile
    LOG.info("Installing %s dependencies...", groupname)
    subprocess.run(["brew", "bundle", "install", "--file", brewfile_path])
    LOG.info("Finished installing %s dependencies!", groupname)

LOG.info("Linking emacs")
# TODO this part might not be necessary, need to test
subprocess.run(
    ["brew", "link", "emacs-mac"]
)
# see `brew info emacs-mac`
subprocess.run(
    ["osascript", "-e",
     """'tell application "Finder" to make alias file to POSIX file "/opt/homebrew/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'"""]
)

LOG.info("Installing chemacs")
subprocess.run(
    ["git", "clone", "https://github.com/plexus/chemacs2.git", str(pathlib.Path.home() / ".emacs.d")]
)

LOG.info("Installing doom")
subprocess.run(
    ["git", "clone", "https://github.com/hlissner/doom-emacs", str(pathlib.Path.home() / ".config/doom-emacs")]
)
subprocess.run(
    ["doom", "install", "-!"]
)

LOG.info("Adding `dots` alias for `...` dir so nushell can find it (without 'smart'ly resolving the ...)")
subprocess.run(
    [
        "ln",
        "-svf",
        str(DOTFILE_PATH),
        str(pathlib.Path.home() / ".config" / "dots"),
    ]
)

# set up from other scripts
# TODO maybe move these eventually
# anki-connect (so emacs can talk to anki)
LOG.info("Setting up anki connect.")
subprocess.run(
    ["sh", str(pathlib.Path(__file__).parent / "anki-connect-mac-setup.sh")]
)
# osx settings
LOG.info("Setting MacOS settings.")
subprocess.run(
    ["sh", str(pathlib.Path(__file__).parent / "osxdefaults.sh")]
)

LOG.info("Starting brew services")
BREW_SERVICES = [
    "yabai",
    "skhd",
]
for service in BREW_SERVICES:
    subprocess.run(
        ["brew", "services", "start", service]
    )
