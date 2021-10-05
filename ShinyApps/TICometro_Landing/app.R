######################################
###### TICometro Landing ##########
######################################

library(shiny)
library(waiter)
library(bs4Dash)
library(shiny)
library()
library(fresh)
library(htmltools)

#FRESH THEME
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
  )
)

# Define UI for application that draws a histogram
ui <- bs4Dash::dashboardPage(
  dark = FALSE,
  #HEAD tags 4 various reasons
  tags$head(
    #include css
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "shortcut icon", href = "favicon.png")#add favicon
  ),#head ends
  # CODE TO CHANGE COLORS OF THE APP
  freshTheme = myTheme,
  #preloader = list(html = tagList(waiter::spin_gauge(), "Cargando el sitio de consulta del TICómetro..."),
   #                color = "#333e48"),
  fullscreen = TRUE,
  scrollToTop = TRUE,
  #HEADER STARTS HERE
  header = bs4Dash::dashboardHeader(disable =TRUE),
  #SIDEBAR STARTS HERE
  sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
  #UNABLE TO DISABLE. USE IT FOR CREDITS
  controlbar = NULL, #END OF CONTROL BAR
  #Footer STARTS HERE
  footer = bs4Dash::dashboardFooter(
    fixed = FALSE,
    left = tags$a(
      href = "https://educatic.unam.mx/publicaciones/informes-ticometro.html",
      target = "_blank", "H@bitat Puma, DGTIC, UNAM. Development Site",
      role = "contentinfo"
    ),
    right = "2021"
  ), #footer ends
  #BODY STARTS HERE
  body = bs4Dash::dashboardBody(
    #here image del ticometro
    fluidRow(
      tags$img(src = "logo_dgtic.png", alt = "un logo compuesto en la izquierda del escudo de la UNAM y las letras DGTIC abajo y a la derecha el texto: Universidad Nacional Autónoma de México, Dirección General de Cómputo y de Tecnologías de Información y Comunicación.",
               width="40%")
    ),
    fluidRow(tags$img(src = "logo_ticometro.jpg", alt = "El logo del TICómetro es  un rectangulo con una regla azul cruzando masomenos por en medio y fondo verde a la izquierda, amarillo arriba, marón a la derecha y rojo abajo. La palabra TICómetro se encuentre en el centro con las letras TIC en naranja y más grande ómetro que está en negro",
                      width="90%", align = "center")),
    tags$div(
      class = "row",
      tags$h1(
        "Resultados"
      ),
      style = "background-color: #F5CB30;"
    ),
    tags$div( #entrap all elements in a white background div
      br(),
      fluidRow(
        tags$p("El TICómetro surge en el año 2012 a partir de la línea rectora 1 del Plan de Desarrollo Institucional 2011-2015. Actualmente, el TICómetro representa un instrumento de evaluación de habilidades digitales que aporta datos valiosos para pensar la estrategia de integración de TIC en las actividades educativas, la formación de profesores y las prioridades en relación con la dotación de infraestructura en los planteles universitarios. Responde, entre otros, al Programa Estratégico 7 del Plan de Desarrollo Institucional 2015-2019: Tecnologías de la Información y Comunicación (TIC) y Tecnologías del Aprendizaje y el Conocimiento (TAC)."
        )
      ),
      fluidRow(
        tags$h3("Consulta los Resultados del 2020!",
        )
      ),
      fluidRow(splitLayout(
        bs4Dash::box(
          width = 12,
          headerBorder = FALSE,
          collapsible = FALSE,
          tags$a(href  = "http://132.248.10.243:3838/El-Duque/Directivos_TICometro/",
                 tags$h4("Sitio para directivos",
                         align = "center"),
                 style = "color: white",
                 role = "button"
          ),
          background = "navy"
        ),
        bs4Dash::box(
          width = 12,
          title = NULL,
          headerBorder = FALSE,
          collapsible = FALSE,
          tags$a(href  = "http://132.248.10.243:3838/El-Duque/Profes_TICometro/",
                 tags$h4("Sitio para profesores",
                         align = "center"),
                 style = "color: white",
                 role = "button"
          ),
          background = "primary"
        )
      )),
      style = "background-color: #FFFFFF;")#div container ends
  ) #BODY ENDS
) # page ends


server <- function(input, output) {
  print("im running")

}


shinyApp(ui = ui, server = server)
