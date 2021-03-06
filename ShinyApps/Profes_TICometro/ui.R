######################################
###### TICometro 4 Profesores##########
######################################


#ENP UI FUNCTIONS

#ESCUELAS ENP
ENP_escuelas <- dplyr::tibble(escuela_name = 1:9) %>%
  purrr::map_df(.,
                \(x) {
                  paste0("ENP ", x)
                })

#Grupos ENP
ENP_grupos <- c("401", "402",  "403",  "404",  "405",  "406",  "407",  "408",  "409",  "410",  "411",
                "412",  "413",  "414",  "415",  "416",  "417",  "418",  "419",  "420",  "421",  "422",
                "423",  "424",  "425",  "426",  "427",  "428",  "429",  "430",  "431",  "432",  "433",
                "434", "435",  "436",  "437",  "438",  "439",  "440",  "451",  "452",  "453",  "454",
                "455",  "456",  "457",  "458",  "459",  "460",  "461",  "462",  "462A", "462B", "463",
                "464",  "465",  "466",  "467",  "468",  "469",  "470",  "471",  "472",  "473",  "474",
                "475",  "476",  "477",  "478" )

#Variables ENP

#ESCUELAS CCH
CCH_escuelas <- dplyr::tibble(escuela_name = c("CCH Azcapotzalco",
                                               "CCH Naucalpan",
                                               "CCH Oriente",
                                               "CCH Sur",
                                               "CCH Vallejo")
)

#variables TICometro

datos_de_contexto <- list("Género" = "genero",
                          "Escuela de Procedencia" = "escuela_de_procedencia"
)

habilidades_digitales <- list("Color de cinta obtenida" = "cinta",
                              "Calificación TICometro" = "calif_checker",
                              "Calif. Procesamiento" = "calif_proces_admin_infor",
                              "Calif. Acceso" = "calif_acceso_informacion",
                              "Calif. Seguridad" = "calif_seguridad",
                              "Calif. Colaboración" = "calif_colabor_comunic")

nivel_de_acceso_ENP <- list("Edad de primer uso de TIC" = "edad_uso_dispositivo",
                            "Acceso a Dispositivos" = "dispositivos_electronicos",
                            "Uso compartido de laptop o computadora" = "compartes_tic",
                            "Estabilidad de la red en casa" = "estabilidad_internet_4_clases",
                            "Conexión a Internet fuera de casa" = "internet_fuera_d_casa",
                            "Conocimiento sobre plataformas educativas" = "plataformas_edu_known",
                            "# de Plataformas Educativas que conoce el estudiante" = "total_de_plataformas_por_estudiante")

nivel_de_acceso_CCH <- list(
  "Edad de primer uso de TIC" = "edad_uso_dispositivo",
  "Acceso a Dispositivos" = "dispositivos_electronicos",
  "# de Dispositivos TIC por estudiante" = "total_de_dispositivos_por_estudiante",
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


#DATA FOR DEVELOPMENT

# Define UI for application that draws a histogram
shinyUI(
  bs4Dash::dashboardPage(
    title = "Sitio de Consulta de los Resultados del TICómetro",
    dark = TRUE,
    #HEAD tags 4 various reasons
    tags$head(# Note the wrapping of the string in HTML()
      tags$link(rel="shortcut icon", href="favicon.png"),#add favicon
      #include css
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),# END OF TAGS HEAD
    # CODE TO CHANGE COLORS OF THE APP
    freshTheme = myTheme,
    preloader = list(html = tagList(waiter::spin_gauge(), "Cargando el sitio de consulta del TICómetro..."),
                     color = "#333e48"
    ), #image =  "https://i.pinimg.com/280x280_RS/45/d4/01/45d40177e1ce8015e9e4dd2a2115ed36.jpg"
    fullscreen = TRUE,
    #HEADER STARTS HERE
    header = bs4Dash::dashboardHeader(
      fixed = FALSE,
      title = tags$a(href = 'https://educatic.unam.mx/publicaciones/informes-ticometro.html',
                     tags$img(src = 'logo_ticometro_pequenio.jpg',
                              width = "100%")
                     ),
      border = FALSE,
      tags$h5("Consulta los datos del TICómetro  |  Profesores", id = "title-navbar")
      
    ),#HEADER ENDS
    #SIDEBAR STARS HERE BUT ITS DISABLED
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
        sidebarHeader(tags$b("Resultados del TICómetro")),
        menuItem(
          text = "TICómetro 2020",
          startExpanded = FALSE,
          selected = "cch",
          menuSubItem(
            text = HTML(
              paste(
                "CCH",
                dashboardBadge(
                  "dev",
                  position = "right",
                  color = "warning"
                )
              )
            ),
            icon = shiny::icon(name = "angle-double-right"),
            tabName = "cch"
          ),
          menuSubItem(
            text = HTML(
              paste(
                "ENP",
                dashboardBadge(
                  "dev",
                  position = "right",
                  color = "warning"
                )
              )
            ),
            icon = shiny::icon("angle-double-right"),
            tabName = "enp"
          ),
          menuItem(
            text =  HTML(
              paste(
                "Descarga Masiva",
                dashboardBadge(
                  "dev",
                  position = "right",
                  color = "warning"
                )
              )
            ),
            icon = shiny::icon("download"),
            startExpanded = FALSE,
            tabName = "descarga"
          )
        )
      )
    ),
    controlbar = NULL, #END OF CONTROL BAR
    #FOOTER STARTS
    footer = bs4Dash::dashboardFooter(
      fixed = FALSE,
      left = tags$a(
        href = "https://educatic.unam.mx/publicaciones/informes-ticometro.html",
        target = "_blank",
        "H@bitat Puma, DGTIC, UNAM, Sitio en Desarrollo...."
      ),
      right = "2021"
    ),
    #BODY STARTS
    body = bs4Dash::dashboardBody(
      #Body is divided by ENP, CCH, DESCARGA main tabs
      tabItems(
        #ENP TAB starts here!
        tabItem(
          tabName = "enp",
          fluidRow(
            tags$h6("Seleccione una o varias opciones:")
          ),
          fluidRow(
            column(#STARTS SCHOOL INPUT
              width = 3,
              #USE PICKER FOR EASY MULTIPLE SELECTION
              shinyWidgets::pickerInput(
                inputId = "escuelas_enp_picked",
                label = "Escuela y/o plantel:",
                choices = list(
                  "Escuela Nacional Preparatoria" = c(ENP_escuelas$escuela_name)
                ),
                multiple = TRUE,
                selected = ENP_escuelas$escuela_name[1],
                options = list(
                  style = "btn-secondary"
                )
              )
            ),#END OF SCHOOL INPUT
            column( #sTARTS GRUPO INPUT
              width = 2,
              #use picker with searchable
              shinyWidgets::pickerInput(
                inputId = "grupo_enp_select",
                label = "Grupo:",
                choices = ENP_grupos,
                multiple = TRUE,
                selected = ENP_grupos[1],
                options = list(
                  title = "Seleccione una opcion",
                  style = "btn-secondary",
                  `live-search` = TRUE
                )
              )
            ), #ENDS GRUPO INPUT
            column(
              width = 4,
              #use picker with searchable
              shinyWidgets::pickerInput(
                inputId = "plot_enp_var",
                label = "Variables del TICómetro",
                choices =  list(
                  "Datos de Contexto" = datos_de_contexto,
                  "Nivel de Acceso" = nivel_de_acceso_ENP,
                  "Habilidades Digitales" = habilidades_digitales
                ),
                multiple = FALSE,
                selected = "cinta",
                options = list(
                  title = "Seleccione una opcion",
                  style = "btn-secondary",
                  `live-search` = TRUE
                )
              ) #END OF PICKER
            ), #END OF VARS INPUT
            column( #STARTS CONSULTA BUTTON
              width = 3,
              br(), #try to align widgets
              div(id = "buton-consulta",
                  role = "button",
                  shinyWidgets::actionBttn(
                    inputId = "activa_consulta_enp",
                    label = "Consulta",
                    style = "gradient",
                    color = "success",
                    size = "lg"
                  ),
                  style = "margin-top: 7px;"
              )
            )# ENDS ACTION BUTTON
          ), #ENDS 1 FLUID ROW
          fluidRow(
            tags$h6("La gráfica muestra el resultado de la selección de planteles y grupos seleccionados")
          ),
          #Second Fluid Row: DATA SHOW
          fluidRow(
            # Output: Tabset w/ plot, summary, and table ----
            tabBox(
              title = NULL,
              elevation = 2,
              id = "ENP_data_and_plots_tabs",
              width = 12,
              background = "info",
              collapsible = FALSE,
              maximizable = TRUE,
              gradient = TRUE,
              closable = FALSE,
              type = "tabs",
              status = "primary",
              solidHeader = TRUE,
              selected = "Gráfica",
              htmltools::tagAppendAttributes(
              tabPanel( #tab panel for plots
                "Gráfica",
                #Plot output
                plotly::plotlyOutput("ENP_plot") %>% shinycssloaders::withSpinner(type = 1,
                                                                                  size = 3,
                                                                                  color =  "#FFFFFF")
              ),
              style = "margin:-18px;"
              ), #tab panel ends
              tabPanel( #tab for data table
                "Tabulado de Datos",
                DT::DTOutput("TabulatedVars_ENP")
              ), #tab panel ends HOJA CON DATOS TABULADOS
              tabPanel(
                "Hoja de Datos",
                DT::DTOutput("MainVars_ENP")
              ) #tab panel HOJA DE DATOS RAW
            ) #tab box ends
          ), #second fluid row ends
          #Third Fluid row: BOXES DATA
          fluidRow(
            bs4Dash::bs4ValueBoxOutput("value_box_ENP",
                                       width = 3),
            bs4Dash::bs4ValueBoxOutput("average_box_ENP",
                                       width = 3),
            infoBox(
              width = 6,
              title = tags$b("¿Que son las cintas?"),
              color = "info",
              value = HTML("
             - Cinta blanca: 0 a 30 puntos.  <br>

              - Cinta amarilla: 31 a 60 puntos.  <br>

              - Cinta azul: 61 a 84 puntos.  <br>

              - Cinta negra: 85 a 100 puntos."),
             icon = icon("info")
            )
          )#FLUID ROW ENDS
        ), #TAB ENP ENDS
        ###########################################
        tabItem(tabName = "cch",
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
                        "Escuela Nacional Colegio de Ciencias y Humanidades" = c(CCH_escuelas$escuela_name)
                      ),
                      multiple = TRUE,
                      selected = CCH_escuelas$escuela_name[1],
                      options = list(
                        style = "btn-secondary"
                      )
                    )
                  ),#END OF COLUMN SCHOOL INPUT
                  column(
                    width = 4,
                    #use picker with searchable
                    shinyWidgets::pickerInput(
                      inputId = "plot_cch_var",
                      label = "Variables del TICómetro",
                      #TAYLORED VARS CCH
                      choices = list(
                        "Datos de Contexto" = datos_de_contexto,
                        "Nivel de Acceso" = nivel_de_acceso_CCH,
                        "Habilidades Digitales" = habilidades_digitales
                      ),
                      multiple = FALSE,
                      selected = "cinta",
                      options = list(
                        style = "btn-secondary",
                        `live-search` = TRUE
                      )
                    )#END OF PICKER INPUT
                  ),
                  column( #STARTS CONSULTA BUTTON
                    width = 3,
                    br(), #try to align widgets
                    div(id = "button-consulta-styler",
                        shinyWidgets::actionBttn(
                          inputId = "activa_consulta_cch",
                          label = "Consulta",
                          style = "gradient",
                          color = "success",
                          size = "lg"
                        ),
                        style = "margin-top: 7px;"
                    )
                  )# ENDS ACTION BUTTON
                ), #ENDS 1 FLUID ROW
                fluidRow(
                  tags$h6("La gráfica muestra el resultado de la selección de planteles y grupos seleccionados")
                ),
                #Second Fluid Row: DATA SHOW
                fluidRow(
                  # Output: Tabset w/ plot, summary, and table ----
                  tabBox(
                    title = NULL,
                    elevation = 2,
                    id = "CCH_tables_plots",
                    width = 12,
                    background = "orange",
                    collapsible = FALSE,
                    maximizable = TRUE,
                    gradient = TRUE,
                    closable = FALSE,
                    type = "tabs",
                    status = "primary",
                    solidHeader = TRUE,
                    selected = "Gráfica",
                    htmltools::tagAppendAttributes(
                    tabPanel( #tab panel for plots
                      "Gráfica",
                      #Plot output
                      plotly::plotlyOutput("CCH_plot") %>% shinycssloaders::withSpinner(type = 1,
                                                                                        size = 3,
                                                                                        color =  "#FFFFFF")
                    ),#tab panel ends
                    style = "margin:-18px;"
                  ), 
                    tabPanel( #tab for data table
                      "Tabulado de Datos",
                      DT::DTOutput("TabulatedVars_CCH")
                    ), #tab panel ends HOJA CON DATOS TABULADOS
                    tabPanel(
                      "Hoja de Datos",
                      DT::DTOutput("MainVars_CCH")
                    ) #tab panel HOJA DE DATOS RAW
                  ) #tab box ends
                ), #second fluid row ends
                #Third Fluid row: BOXES DATA
                fluidRow(
                  bs4Dash::bs4ValueBoxOutput("value_box_CCH",
                                             width = 3),
                  bs4Dash::bs4ValueBoxOutput("average_box_CCH",
                                             width = 3),
                  infoBox(
                    width = 6,
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
        ), #TAB CCH ENDS
        ############################################################
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
      ), #tab items end
      useSever(),
      useWaiter(),
      autoWaiter(id = c("MainVars_CCH",
                        "TabulatedVars_CCH",
                        "TabulatedVars_ENP",
                        "MainVars_ENP",
                        "value_box_ENP",
                        "average_box_ENP",
                        "value_box_CCH",
                        "average_box_CCH"),
                 html = spin_solar(),
                 fadeout = FALSE,
                 color = "#FFFFF",
                 image = ""),
      useShinyalert()
    )#BODY ENDS
    
  )# PAGE ENDS
)# SHINY UI ENDS
