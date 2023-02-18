source ~/.config/dots/.config/nu/starship/init.nu

let-env config = {
  menus: [
      {
        name: history_menu
        only_buffer_difference: true # Search is done on the text written after activating the menu
        marker: "? "                 # Indicator that appears with the menu is active
        type: {
            layout: list             # Type of menu
            page_size: 10            # Number of entries that will presented when activating the menu
        }
        style: {
            selected_text: {fg: "#B4D7AC" bg: "#232323" attr: b}
        }
      }
    ]
}

# pyenv
# Typically handled py `pyenv init`, but nu isn't supported so we have to do this manually
pyenv rehash

# zoxide
source ~/.local/share/.zoxide.nu

# custom commands

# Rename the current tab
def rename-tab [
  name: string  # The new name for the current tab
] {
  zsh -c $"echo -ne \"\\x1b]0;($name)\\x1b\\\\\""
}
