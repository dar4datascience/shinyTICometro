#Fresh theme 4 Directivos TICometro
#FRESH THEME
# CODE TO CHANGE COLORS OF THE APP

get_my_fresh_theme <- function(){

myTheme <- fresh::create_theme( #FIND MORE CUSTOMIZATION AT fresh::search_vars_bs4dash("navbar")
  #fresh::bs4dash_vars(
   # main_header_light_form_control_bg = "gray_x_light",
    #navbar_light_color = "white"
    #navbar_light_active_color = "#FFF",
    #navbar_light_hover_color = "#FFF"
  #),
  #bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30")
  #,
  fresh::bs4dash_layout(
    main_bg = "#f5f6fa"
  ),
  #fresh::bs4dash_sidebar_light(
   # bg = NULL,
    #hover_bg = NULL,
    #color = NULL,
    #hover_color = NULL,
    #active_color = NULL,
    #submenu_bg = NULL,
    #submenu_color = NULL,
    #submenu_hover_color = NULL,
    #submenu_hover_bg = NULL,
    #submenu_active_color = NULL,
    #submenu_active_bg = NULL,
    #header_color = NULL
  #),
  #fresh::bs4dash_sidebar_dark(    bg = NULL,    hover_bg = NULL,    color = NULL,    hover_color = NULL,    active_color = NULL,    submenu_bg = NULL,    submenu_color = NULL,    submenu_hover_color = NULL,    submenu_hover_bg = NULL,    submenu_active_color = NULL,    submenu_active_bg = NULL,    header_color = NULL  ),
  fresh::bs4dash_status(
    #6 statutes available
    primary = "#007bff",
    danger = "#BF616A",
    secondary = "#6c757d",
    info = "#17a2b8",
    success = "#28a745",
    warning = "#ffc107",
    #Background color of the navbar is defined by the light or dark status
    light = "#272c30",
    dark = "#343a40"
  ),
  #bs4dash_color( #main colors of bs4dash. as used by the app
   # blue = NULL,
    #lightblue = NULL,
    #navy = NULL,
    #cyan = NULL,
    #teal = NULL,
    #olive = NULL,
    #green = NULL,
    #lime = NULL,
    #orange = NULL,
    #yellow = NULL,
    #fuchsia = NULL,
    #purple = NULL,
    #maroon = NULL,
    #red = NULL,
    #black = NULL,
    #gray_x_light = "353a3e", #color for header
    #gray_600 = NULL,
    #gray_800 = NULL,
    #gray_900 = NULL,
    #white = NULL #"#272c30"
#  ),
fresh::bs4dash_font(
    size_base = "1.2rem",
    weight_bold = 900,
    family_base = "MyriadProBold"
  )
)

return(myTheme)
}
