#!/usr/bin/env python

import subprocess
import tomllib
from pathlib import Path

import tomli_w
from textual.app import App, ComposeResult
from textual.binding import Binding
from textual.widgets import OptionList, SelectionList, Static

CHEZMOI_CONFIG_PATH = Path.home() / ".config" / "chezmoi" / "chezmoi.toml"


class SelectSingle(App[str]):
    """Full-screen prompt that lets the user select a single item

    Parameters
    ----------
    options : list[str]
        The full list of available options
    """

    CSS_PATH = None

    def __init__(self, options: list[str], prompt: str = "Pick one:"):
        super().__init__()
        self._options = options
        self._prompt = prompt

    def compose(self) -> ComposeResult:
        yield Static(self._prompt, classes="prompt")
        yield OptionList(*self._options)

    def on_option_list_option_selected(self, event: OptionList.OptionSelected) -> None:
        self.exit(event.option.prompt)


class SelectMultiple(App[list[str]]):
    """Full-screen prompt that lets the user select any number of items

    Parameters
    ----------
    options : list[str]
        The full list of available options
    selected_by_default : list[str] | None, default None
        Options that are selected by default
    """

    CSS_PATH = None
    BINDINGS = [
        Binding("escape", "submit", "Done"),
    ]

    def __init__(
        self,
        options: list[str],
        selected_by_default: list[str] | None = None,
        prompt: str = "Pick any (space/enter to toggle, esc to finish)",
    ) -> None:
        super().__init__()
        self._options = options
        self._defaults = set(selected_by_default or [])
        self._prompt = prompt

    def compose(self) -> ComposeResult:
        yield Static(self._prompt, classes="prompt")
        # A SelectionList accepts 3-tuples: (prompt, value, initially_selected_bool)
        yield SelectionList(
            *((item, item, item in self._defaults) for item in self._options)
        )

    def action_submit(self):
        self.exit(self.query_one(SelectionList).selected)


def main():
    # if the user has a valid config use it
    if (
        CHEZMOI_CONFIG_PATH.is_file()
        and "data" in (cfg := tomllib.loads(CHEZMOI_CONFIG_PATH.read_text())).keys()
        and cfg["data"].keys() >= {"environment", "optionalGroups", "brewPrefix"}
    ):
        print(f"[virus] Found existing config file at {CHEZMOI_CONFIG_PATH}, using it")
        return

    # otherwise, prompt user for their config selections and write the config out
    print(f"[virus] No valid config file at {CHEZMOI_CONFIG_PATH}, creating new one")
    environment = SelectSingle(
        prompt="Select an environment", options=["home", "work"]
    ).run()
    optional_groups = SelectMultiple(
        prompt="Select optional groupd of deps to install (space/enter to toggle, esc to finish)",
        options=[
            "aws",
            "azure",
            "cli-extras",
            "clojure",
            "dev-misc",
            "docker",
            "erlang",
            "gcp",
            "go",
            "haskell",
            "julia",
            "k8s",
            "latex",
            "mysql",
            "ocaml",
            "ollama",
            "postgres",
            "r",
            "rust",
            "scala",
            "terraform",
            "zsh",
        ],
        selected_by_default=[
            "cli-extras",
            "dev-misc",
            "docker",
            "latex",
        ],
    ).run()
    brew_prefix = subprocess.check_output(["brew", "--prefix"], text=True).strip()
    print(f"[virus] Environment: {environment}")
    print(f"[virus] Optional groups: {optional_groups}")
    print(f"[virus] Brew prefix: {brew_prefix}")

    # write out config
    with open(CHEZMOI_CONFIG_PATH, "wb") as f:
        tomli_w.dump(
            {
                "data": {
                    "environment": environment,
                    "optionalGroups": optional_groups,
                    "brewPrefix": brew_prefix,
                }
            },
            f,
        )

    print("\n[virus] Configuration complete!")


if __name__ == "__main__":
    main()
