library(htmltools)
library(fresh)

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(bs4Dash)))
suppressMessages(suppressWarnings(library(shinyalert)))
suppressMessages(suppressWarnings(library(shiny)))


#FRESH THEME
path_file <- file.path(getwd(), "custom-theme-ticometro.css")
# CODE TO CHANGE COLORS OF THE APP
myTheme <- create_theme( #FIND MORE CUSTOMIZATION AT fresh::search_vars_bs4dash("navbar")
  bs4dash_vars(
    navbar_light_color = "white",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  #bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30")
  #,
  bs4dash_layout(
    main_bg = "#f5f6fa"
  ),
  bs4dash_status(
    #6 statutes available
    primary = "#007bff",
    danger = "#BF616A",
    secondary = "#6c757d",
    info = "#17a2b8",
    success = "#28a745",
    warning = "#ffc107",
    #Background color of the navbar is defined by the light or dark status
    light = "#353a3e",
    dark = "#343a40"
  ),
  bs4dash_color( #main colors of bs4dash. as used by the app
    blue = NULL,
    lightblue = NULL,
    navy = NULL,
    cyan = NULL,
    teal = NULL,
    olive = NULL,
    green = NULL,
    lime = NULL,
    orange = NULL,
    yellow = NULL,
    fuchsia = NULL,
    purple = NULL,
    maroon = NULL,
    red = NULL,
    black = NULL,
    gray_x_light = "#353a3e", #color for header
    gray_600 = NULL,
    gray_800 = NULL,
    gray_900 = NULL,
    white = NULL #"#272c30"
    
  ),
  bs4dash_font(
    size_base = "1.5rem",
    weight_bold = 900,
    family_base = "MyriadProBold"
  ),
  output_file =  path_file
)
