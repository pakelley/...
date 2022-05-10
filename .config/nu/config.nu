source ~/.config/dots/.config/nu/zoxide/.zoxide.nu
source ~/.config/dots/.config/nu/starship/init.nu

let $config = {
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
