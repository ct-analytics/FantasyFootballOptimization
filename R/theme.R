library(bslib)
library(thematic)
theme_ct <- bs_theme(
  bg = "#1d2127",
  fg = "#ffffff",
  primary = "#0088cc",
  base_font = font_google("B612 Mono"),
  code_font = font_google("Source Code Pro"),
  heading_font = font_google("Orbitron")
)

thematic_shiny(font = "auto")

# Preview custom theme
# bs_theme_preview(theme_ct)

##### 
### Colors from Mr Robot theme in Wowchemy
# https://github.com/donaldRwilliams/hugo-academic/blob/master/data/themes/mr_robot.toml
# Primary
# primary = "rgb(0, 136, 204)"

# Menu
# menu_primary = "rgb(33, 37, 41)"
# menu_text = "rgb(0, 136, 204)"
# menu_text_active = "rgba(255,255,255,1)"
# menu_title = "rgb(153, 153, 153)"

# Home sections
# home_section_odd = "rgb(29, 33, 39)"
# home_section_even = "rgb(29, 33, 39)"