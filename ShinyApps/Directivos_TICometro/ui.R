######################################
###### TICometro 4 Directivos##########
######################################

#List of variable choices
datos_de_contexto <- list("Género" = "genero",
                          "Escuela de Procedencia" = "escuela_de_procedencia"
)

habilidades_digitales <- list("Color de cinta obtenida" = "cinta",
                              "Calificación TICometro" = "calif_checker",
                              "Calif. Procesamiento" = "calif_proces_admin_infor",
                              "Calif. Acceso" = "calif_acceso_informacion",
                              "Calif. Seguridad" = "calif_seguridad",
                              "Calif. Colaboración" = "calif_colabor_comunic")

nivel_de_acceso <- list(
  "Edad de primer uso de TIC" = "edad_uso_dispositivo",
  "Acceso a Dispositivos" = "dispositivos_electronicos",
  "# de Dispositivos TIC" = "total_de_dispositivos_por_estudiante",
  "Uso compartido de laptop o computadora" = "compartes_tic",
  "Estabilidad de la red en casa" = "estabilidad_internet_4_clases",
  "Conexión a Internet fuera de casa" = "internet_fuera_d_casa",
  "Conocimiento sobre plataformas educativas" = "plataformas_edu_known",
  "# de Plataformas Educativas que conoce el estudiante" = "total_de_plataformas_por_estudiante"
)

#FRESH THEME
# CODE TO CHANGE COLORS OF THE APP
myTheme <- create_theme( #FIND MORE CUSTOMIZATION AT fresh::search_vars_bs4dash("navbar")
  bs4dash_vars(
    main_header_light_form_control_bg = "gray_x_light",
    navbar_light_color = "white"
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
    warning = "#ffc107"
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
    gray_x_light = "353a3e",
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



####################################


#Lista Escuelas ENP
ENP_escuelas <- dplyr::tibble(escuela_name = 1:9) %>%
  purrr::map_df(.,
                \(x) {
                  paste0("ENP ", x)
                })

#ESCUELAS CCH

#ESCUELAS CCH
CCH_escuelas <- dplyr::tibble(escuela_name = c("CCH Azcapotzalco",
                                               "CCH Naucalpan",
                                               "CCH Oriente",
                                               "CCH Sur",
                                               "CCH Vallejo")
)



# Define UI for application that draws a histogram
shinyUI(
  bs4Dash::dashboardPage(
    title = "Sitio de Consulta de los Resultados del TICómetro para directivos.",
    dark = FALSE,
    #HEAD tags 4 various reasons
    tags$head(# Note the wrapping of the string in HTML()
      tags$link(rel="shortcut icon", href="favicon.png"),#add favicon
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ), #END OF HEAD
    preloader = list(
      html = tagList(waiter::spin_gauge(), "Cargando sitio de consulta del TICómetro ..."),
                     color = "#343a40"
    ),
    fullscreen = TRUE,
    #HEADER STARTS HERE
    header = bs4Dash::dashboardHeader(
      fixed = FALSE,
      title = tags$a(href = 'http://132.248.10.243:3838/El-Duque/TICometro_Landing',
                     tags$img(src = 'logo_ticometro_pequenio.jpg',
                              width="100%", 
                              alt = "El logo del TICómetro es  un rectangulo con una regla azul cruzando masomenos por en medio y fondo verde a la izquierda, amarillo arriba, marón a la derecha y rojo abajo. La palabra TICómetro se encuentre en el centro con las letras TIC en naranja y más grande ómetro que está en negro"
                              )
                     ),
      border = FALSE,
      tags$h5(
        as.character("Consulta los datos del TICómetro  |  Directivos"),
        id = "title-navbar",
        style = "padding-top: 7px;"
      )
    ),#HEADER ENS
    #SIDEBAR STARS HERE
    sidebar = bs4Dash::dashboardSidebar(
      fixed = FALSE,
      expandOnHover = FALSE,
      status = "primary",
      id = "sidebar",
      bs4Dash::sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        #here is a styler
        sidebarHeader(tags$b("Resultados del TICómetro")),
        menuItem(
          text = "TICómetro 2020",
          icon = shiny::icon("search"),
          tabName = "consulta2020",
          badgeLabel = "nuevo",
          badgeColor = "danger"
          # menuSubItem(
          #    text = "Compara",
          #   icon = shiny::icon("chart-bar"),
          #  tabName = "compara"
          #)
        ),
        menuItem(
          text = "Descarga Masiva",
          icon = icon("download"),
          startExpanded = FALSE,
          tabName = "descarga"
        )
      )
    ),# end of side bar
    #Control BAR STARTS HERE
    #UNABLE TO DISABLE. USE IT FOR CREDITS
    controlbar = NULL, #END OF CONTROL BAR,
    #FOOTER STARTS
    footer = bs4Dash::dashboardFooter(
      fixed = FALSE,
      left = tags$a(
        href = "https://educatic.unam.mx/publicaciones/informes-ticometro.html",
        target = "_blank", "H@bitat Puma, DGTIC, UNAM, Sitio en Desarrollo...."
      ),
      right = "2021"
    ),
    body = bs4Dash::dashboardBody(
      #BODY STARTS
      bs4Dash::tabItems(
        #ENP TAB starts here!
        bs4Dash::tabItem(
          tabName = "consulta2020",
          fluidRow(
            tags$h6("Seleccione una o varias opciones:")
          ),
          fluidRow(
            column(#STARTS SCHOOL INPUT
              width = 4,
              #USE PICKER FOR EASY MULTIPLE SELECTION
              shinyWidgets::pickerInput(
                inputId = "escuelas_directivos_picked",
                label = "Escuela y/o plantel:",
                choices = list(
                  "ENP" = c(ENP_escuelas$escuela_name),
                  "CCH" = c(CCH_escuelas$escuela_name)
                ),
                multiple = TRUE,
                selected = c("ENP 1", "CCH Azcapotzalco"),
                options = list(
                  style = "btn-primary"
                )
              )
            ),#END OF SCHOOL INPUT
            column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = 'plot_directivo_var',
                label = 'Variables del TIcometro',
                choices = list(
                  "Datos de Contexto" = datos_de_contexto,
                  "Nivel de Acceso" = nivel_de_acceso,
                  "Habilidades Digitales" = habilidades_digitales
                ),
                selected = "calif_checker",
                options = list(style = "btn-danger")
              )
            ),#END SELECT VAR input
            column( #STARTS CONSULTA BUTTON
              width = 3,
              br(), #try to align widgets
              div(id = "button-consulta-styler",
              shinyWidgets::actionBttn(
                inputId = "activa_consulta",
                label = "Consulta",
                style = "gradient",
                icon = shiny::icon("question"),
                color = "success"
              ),
              style = "margin-top: 7px;"
              )
            )# ENDS column ACTION BUTTON
          ),
          fluidRow(
            tags$h6("La gráfica muestra el resultado de la selección de planteles y grupos seleccionados")
          ),
          #Second Fluid Row
          fluidRow(
            # Output: Tabset w/ plot, summary, and table ----
            tabBox(
              title = NULL,
              elevation = 2,
              id = "display-resultados-box",
              width = 12,
              background = "info",
              collapsible = FALSE,
              maximizable = FALSE,
              gradient = TRUE,
              closable = FALSE,
              type = "tabs",
              #HERE A STYLER
              status = "primary",
              solidHeader = TRUE,
              selected = "Grafica",
              tabPanel(
                "Grafica",
                icon = shiny::icon(name = "signal", lib = "glyphicon"),
                #Plot inputs
                plotly::plotlyOutput("Directivos_plot") %>% shinycssloaders::withSpinner(type = 1,
                                                                                  size = 3,
                                                                                  color =  "#FFFFFF")
              ),
              tabPanel( #tab for data table
                "Tabulado de Datos",
                icon = icon(name = "calculator"),
                DT::DTOutput("TabulatedVars_Directivos")
              ), #tab panel ends HOJA CON DATOS TABULADOS
              tabPanel(
                "Hoja de Datos",
                icon = icon(name = "database"),
                DT::DTOutput("MainVars_Directivos")
              ), #tab panel HOJA DE DATOS RAW
              dropdownMenu = boxDropdown(
                icon = shiny::icon("file-download", class = "fa-2x"),
                #when passing an id to boxDropdownItem it will behave like a button!
                boxDropdownItem("Descarga tu gráfica", id = "descarga_grafica", icon = shiny::icon(name = "signal", lib = "glyphicon")),
                boxDropdownItem("Descarga tu selección de datos", id = "descarga_tabulado", icon = icon(name = "calculator")),
                boxDropdownItem("Descarga la hoja de datos", id = "descarga_hoja_datos", icon = icon(name = "database"))
              )
            )#end of tab box
          ), #end of fluid row
          #Third Fluid row
          fluidRow(# divides in 12
            bs4Dash::valueBoxOutput("value_box_Directivos", width = 3), # width 2
            bs4Dash::valueBoxOutput("average_box_Directivos", width = 3), #width 3
            crea_tabla_clas_cintas()
          )
        ),
        #   tabItem(tabName = "compara",
        #          "Hola2"),
        tabItem(tabName = "descarga",
                fluidRow(
                  tags$h2("Descarga de base de datos completa")
                ),
                div(
                  fluidRow(
                    column(1,
                           ""),
                    column(3,
                           br(),
                           br(),
                           fluidRow(tags$h4(tags$b("ENP"))),
                           fluidRow(tags$h4(tags$b("CCH"))),
                           fluidRow(tags$h4(tags$b("FES Acatlán"))),
                           fluidRow(tags$h4(tags$b("FES Aragón"))),
                           fluidRow(tags$h4(tags$b("ENTS"))),
                           id = "colum-descarga-pad"),
                    column(1,
                           fluidRow(tags$h4(tags$b("2020"))),
                           fluidRow(
                             awesomeCheckbox(
                               inputId = "enp_2020",
                               label = "",
                               value = TRUE
                             )
                           ),
                           fluidRow(
                             awesomeCheckbox(
                               inputId = "cch_2020",
                               label = "",
                               value = FALSE
                             )
                           )
                    ),
                    column(1,
                           fluidRow(tags$h4(tags$b("2021"))),
                           fluidRow(
                             awesomeCheckbox(
                               inputId = "enp_2021",
                               label = "",
                               value = FALSE
                             )
                           ),
                           fluidRow(
                             awesomeCheckbox(
                               inputId = "cch_2021",
                               label = "",
                               value = FALSE
                             )
                           )
                    ),
                    column(1,
                           br(),
                           br(),
                           fluidRow(icon("file-excel"))
                    )#last column ends
                  ),#fluid row ends
                  style = "background-color: #696969;")#div ends
        ) #TAB DESCARGA ENDS
      ), #tabItems ENDS
      useWaiter(),
      useSever(),
      autoWaiter(id = c("value_box_Directivos",
                        "average_box_Directivos",
                        "TabulatedVars_Directivos",
                        "MainVars_Directivos"),
                 html = spin_solar(),
                 fadeout = FALSE,
                 color = "#FFFFF",
                 image = ""),
      useShinyalert() # include 
    )#BODY ENDS
  )#PAGE ENDS
)#SHINYUI ENDS

