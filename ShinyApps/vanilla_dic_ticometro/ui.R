######################################
###### TICometro 4 Directivos##########
######################################

library(htmltools)
library(shinyFeedback)
library(fresh)
library(sever)
library(waiter)
library(shinycssloaders)
suppressMessages(suppressWarnings(library(bs4Dash)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(plotly)))



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
        dark = FALSE,
        #HEAD tags 4 various reasons
        tags$head(# Note the wrapping of the string in HTML()
            tags$link(rel="shortcut icon", href="favicon.png"),#add favicon
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
            ), #END OF HEAD
        useWaiter(),
        preloader = list(html = tagList(waiter::spin_pong(), "Cargando sitio de consulta del TICómetro ...", image = "https://i.pinimg.com/280x280_RS/45/d4/01/45d40177e1ce8015e9e4dd2a2115ed36.jpg"),
                         color = "#343a40"
                         ),
        fullscreen = TRUE,
        #HEADER STARTS HERE
        header = bs4Dash::dashboardHeader(
            fixed = FALSE,
            skin = "dark",
            title = tags$a(href = 'http://132.248.10.243:3838/El-Duque/TICometro_Landing',
                           tags$img(src = 'logo_ticometro_pequenio.jpg')),
            border = FALSE,
            tags$h5(
              "Consulta los datos del TICómetro", id = "title-navbar"
                    ),
            controlbarIcon = shiny::icon("laptop-code")
        ),
        #SIDEBAR STARS HERE
        sidebar = bs4Dash::dashboardSidebar(
            fixed = FALSE,
            expandOnHover = FALSE,
            status = "primary",
            id = "sidebar",
            bs4Dash::sidebarUserPanel(
                image = 'favicon.png',
                name = "Bienvenido!"
            ),
            bs4Dash::sidebarMenu(
                id = "current_tab",
                flat = FALSE,
                compact = FALSE,
                childIndent = TRUE,
            #here is a styler
            sidebarHeader(tags$b("Resultados del TICómetro")),
            menuItem(
                text = "TICómetro 2020",
                startExpanded = FALSE,
                selected = "consulta",
                menuSubItem(
                    text = "Consulta",
                    icon = shiny::icon("eye"),
                    tabName = "consulta"
                )
               # menuSubItem(
                #    text = "Compara",
                 #   icon = shiny::icon("chart-bar"),
                  #  tabName = "compara"
                #)
                ),
            menuItem(
                    text = "Descarga",
                    icon = icon("download"),
                    startExpanded = FALSE,
                    tabName = "descarga"
                )
            )
            ),
        #Control BAR STARTS HERE
        #UNABLE TO DISABLE. USE IT FOR CREDITS
        controlbar = NULL, #END OF CONTROL BAR,
    #FOOTER STARTS
        footer = bs4Dash::dashboardFooter(
            fixed = FALSE,
            left = tags$a(
                href = "https://educatic.unam.mx/publicaciones/informes-ticometro.html",
                target = "_blank", "Sitio desarollador x Daniel Amieva Rodrigeuz para H@bitat Puma, DGTIC, UNAM."
            ),
            right = "2021"
        ),
        body = bs4Dash::dashboardBody(
            #BODY STARTS
            bs4Dash::tabItems(
                #ENP TAB starts here!
                bs4Dash::tabItem(
                    tabName = "consulta",
                    fluidRow(
                      tags$h6("Seleccione una o varias opciones:")
                    ),
                    fluidRow(
                        column(#STARTS SCHOOL INPUT
                            width = 4,
                            #USE PICKER FOR EASY MULTIPLE SELECTION
                            shinyWidgets::pickerInput(
                                inputId = "escuelas_cch_picked",
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
                                    inputId = 'plot_cch_var',
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
                            br(),
                            shinyWidgets::actionBttn(
                                inputId = "activa_consulta",
                                label = "Consulta",
                                style = "fill",
                                icon = shiny::icon("question"),
                                color = "success"
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
                            id = "data_tabs",
                            width = 12,
                            background = "info",
                            collapsible = FALSE,
                            maximizable = TRUE,
                            gradient = TRUE,
                            closable = FALSE,
                            type = "tabs",
                            #HERE A STYLER
                            status = "primary",
                            solidHeader = TRUE,
                            selected = "Grafica",
                            tabPanel(
                                "Grafica",
                                icon = ionicon(name = "stats"),
                                #Plot inputs
                                plotly::plotlyOutput("CCH_plot") %>% shinycssloaders::withSpinner(type = 1,
                                                                                                  size = 3,
                                                                                                  color =  "#FFFFFF")
                            ),
                            tabPanel( #tab for data table
                              "Tabulado de Datos",
                              icon = icon(name = "calculator"),
                              DT::DTOutput("TabulatedVars_CCH")
                            ), #tab panel ends HOJA CON DATOS TABULADOS
                            tabPanel(
                              "Hoja de Datos",
                              icon = icon(name = "database"),
                              DT::DTOutput("MainVars_CCH")
                            ) #tab panel HOJA DE DATOS RAW
                        )
                    ),
                    #Third Fluid row
                    fluidRow(
                        valueBox(
                            value = "12,567",
                            width = 4,
                            subtitle = "Alumnos seleccionados",
                            color = "success",
                            icon = icon("user-friends")
                            # href = "#" #Referencia directo a la pagina principal de la aplicacion
                        ),
                        valueBox(
                            value = "Naranja",
                            width = 4,
                            subtitle = "Cinta más común de los alumnos",
                            color = "success",
                            icon = icon("user-graduate")
                        ),
                        infoBox(
                            width = 4,
                            #HERE IS A STYLER
                            title = tags$b("¿Que son las cintas?"),
                            color = "info",
                            value = HTML("
             - Cinta blanca: 0 a 30 puntos.  <br>

              - Cinta amarilla: 31 a 60 puntos.  <br>

              - Cinta azul: 61 a 84 puntos.  <br>

              - Cinta negra: 85 a 100 puntos."),
             icon = icon("info")
                        )
                    )
                ),
             #   tabItem(tabName = "compara",
              #          "Hola2"),
             tabItem(tabName = "descarga",
                     fluidRow(
                       bs4Dash::bs4Jumbotron(
                         title = "Descarga todas las variables del TICómetro!",
                         #HERE WAS A STYLER
                         lead = tags$h2(
                           "Aquí podrás descargar en formato abierto los datos del TICómetro.",
                                       ),
                         HTML("Actualmente, la base de datos está compuesta por 40 columnas y 31,762 registros únicos.
              <br> Los planteles participantes son: ENP, CCH.
              <br> <b> ¿Cómo citar los datos? </b>
              <br> FORMATO DE CITADO PENDIENTE.........."
                         ),
              btnName = "Descarga",
              status = "primary",
              #Link a google drive DANIEL AMIEVA con archivo csv
              href = "https://drive.google.com/file/d/1lIljkK9h7GTgYeXW7RT2XILlvAJBd7--/view?usp=sharing"
                       )
                     )
             ) #TAB DESCARGA ENDS
            ), #tabItems ENDS
            useSever(),
            h1("sever"),
            actionButton("stop", "Stop App"),
            autoWaiter(id = c("MainVars_CCH",
                              "TabulatedVars_CCH"),
                       html = spin_solar(),
                       fadeout = FALSE,
                       color = "#FFFFF",
                       image = ""),
            useShinyFeedback(), # include shinyFeedback
        )#BODY ENDS
    )#PAGE ENDS
    )#SHINYUI ENDS

