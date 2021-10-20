######################################
###### TICometro 4 Directivos##########
######################################

##### List of variable choices

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

# FRESH THEME
# CODE TO CHANGE COLORS OF THE APP
myTheme <- create_theme( #FIND MORE CUSTOMIZATION AT fresh::search_vars_bs4dash("navbar")
  bs4dash_vars(
    main_header_light_form_control_bg = "gray_x_light",
    navbar_light_color = "#343A40 !important"
  ),
  bs4dash_sidebar_light(
    bg = "#EBEBEB",
  ),
  bs4dash_layout(
    main_bg = "#f0f5f8"
  ),
  bs4dash_status(
    #6 statutes available
    primary = "#386FB5",
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
    gray_x_light = "#353a3e",
    gray_600 = NULL,
    gray_800 = NULL,
    gray_900 = NULL
    #white = "#EBEBEB" 
  ),
  bs4dash_font(
    #size_base = "1rem",
    #weight_bold = 900,
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
    
    freshTheme = myTheme,
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
      tags$h3(
        as.character("Consulta los datos del TICómetro  |  Directivos"),
        id = "title-navbar",
        style = "padding-top: 7px;"
      )
    ),#HEADER ENS
    #SIDEBAR STARS HERE
    sidebar = bs4Dash::dashboardSidebar(
      fixed = FALSE,
      skin = "light",
      expandOnHover = FALSE,
      status = "primary",
      id = "sidebar",
      htmltools::tagAppendAttributes(
      bs4Dash::sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        #here is a styler
        sidebarHeader(
          tags$h6(
          "Resultados del TICómetro",
          style = "text-align: center;"
          )
          ),
        menuItem(
          text = "TICómetro 2020",
          icon = shiny::icon("search"),
          tabName = "consulta2020",
          badgeLabel = "dev",
          badgeColor = "warning"
          # menuSubItem(
          #    text = "Compara",
          #   icon = shiny::icon("chart-bar"),
          #  tabName = "compara"
          #)
        ),
        menuItem(
          text = "Descarga Masiva",
          icon = icon("download"),
          tabName = "descarga"
        )
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
      htmltools::tagAppendAttributes(
      #BODY STARTS
      bs4Dash::tabItems(
        #ENP TAB starts here!
        bs4Dash::tabItem(
          role = "tab",
          tabName = "consulta2020",
          fluidRow( id = "texto encima de selectores",
            tags$h5(
              tags$b("Seleccione una o varias opciones:")
            )
          ),
          fluidRow(
            role = "main",
            column(#STARTS SCHOOL INPUT
              width = 4,
              htmltools::tagAppendAttributes(
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
                  style = "btn-secondary"
                )
              ),
              role = "button",
              id = "selector de escuelas")
            ),#END OF SCHOOL INPUT
            column(
              width = 4,
              htmltools::tagAppendAttributes(
              shinyWidgets::pickerInput(
                inputId = 'plot_directivo_var',
                label = 'Variables del TICómetro',
                choices = list(
                  "Datos de Contexto" = datos_de_contexto,
                  "Nivel de Acceso" = nivel_de_acceso,
                  "Habilidades Digitales" = habilidades_digitales
                ),
                selected = "calif_checker",
                options = list(style = "btn-secondary")
              ),
              role = "button",
              id = "selector de variables"
              )
            ),#END SELECT VAR input
            column( #STARTS CONSULTA BUTTON
              width = 3,
              br(), #try to align widgets
              div(id = "button que se activa con ENTER",
                  htmltools::tagAppendAttributes(
              shinyWidgets::actionBttn(
                inputId = "activa_consulta",
                label = "Consulta",
                style = "gradient",
                color = "success"
              ),
              role = "button"
              ),
              style = "margin-top: 7px;"
              )
            )# ENDS column ACTION BUTTON
          ), #end of fluid row
          fluidRow(
            id = "texto encima de tab box",
            tags$p(
              tags$b(
                "La gráfica muestra el resultado de la selección de planteles y grupos seleccionados"
                )
            )
          ),
          #Second Fluid Row
          fluidRow(
            role = "main",
            id = "results container",
            htmltools::tagAppendAttributes(
            # Output: Tabset w/ plot, summary, and table ----
            tabBox(
              title = NULL,
              elevation = 2,
              id = "display-resultados-box",
              width = 12,
              background = "primary",
              collapsible = FALSE,
              maximizable = FALSE,
              gradient = TRUE,
              closable = FALSE,
              type = "tabs",
              #HERE A STYLER
              status = "primary",
              solidHeader = TRUE,
              headerBorder = FALSE,
              selected = "Gráfica",
              # strech the plot with negative margins
              htmltools::tagAppendAttributes(
                tabPanel(
                  role = "tabpanel",
                  "Gráfica",
                  #Plot inputs
                  plotly::plotlyOutput("Directivos_plot") %>% shinycssloaders::withSpinner(type = 1,
                                                                                           size = 3,
                                                                                           color =  "#FFFFFF")
                ),
                style = "margin:-18px;"
              ),
              tabPanel( #tab for data table
                role = "tabpanel",
                "Tabulado de Datos",
                reactable::reactableOutput("TabulatedVars_Directivos")
              ), #tab panel ends HOJA CON DATOS TABULADOS
              tabPanel(
                role = "tabpanel",
                "Hoja de Datos",
                DT::DTOutput("MainVars_Directivos")
              ), #tab panel HOJA DE DATOS RAW
              dropdownMenu = boxDropdown(
                id = "seccion mi descarga",
                role = "region",
                icon = shiny::icon("download", 
                                   class = "fa-2x"),
                #when passing an id to boxDropdownItem it will behave like a button!
                boxDropdownItem("Descargue su gráfica",
                                id = "descarga_grafica",
                                role = "button",
                                icon = shiny::icon(name = "signal", lib = "glyphicon")
                                ),
                  shiny::downloadButton(outputId = "downloadtabulado",
                               "Descargue su selección de datos",
                               icon = icon(name = "calculator")
                               ),
                shiny::downloadButton(outputId = "downloadData",
                                "Descargue su hoja de datos",
                                icon = icon(name = "database")
                  )
              )# end of drop down
            )#end of tab box
          ),#end of fluid row
          role = "tab"
          ), 
          #Third Fluid row
          fluidRow(# divides in 12
            role = "region",
            id = "value boxes y explicacion de cintas",
            bs4Dash::valueBoxOutput("value_box_Directivos", width = 3), # width 2
            bs4Dash::valueBoxOutput("average_box_Directivos", width = 3), #width 3
            crea_tabla_clas_cintas()
          )
        ),#end of tab items 1
        #   tabItem(tabName = "compara",
        #          "Hola2"),
        tabItem(
          role = "tab",
          tabName = "descarga",
                fluidRow(
                  id = "titulo antes de div de descarga",
                  tags$h2("Descargue completa la base de datos")
                ),
                fluidRow(
                  id = "texto antes de div de descarga",
                  tags$h4("Seleccione una o varias opciones: "),
                  div(id = "button que se activa con ENTER",
                      htmltools::tagAppendAttributes(
                        shinyWidgets::actionBttn(
                          inputId = "activa_descarga",
                          label = "Consulta",
                          style = "gradient",
                          color = "success"
                        ),
                        role = "button"
                      ),
                      style = "padding-left: 200px;"
                  )
                ),#end of fluid row
                tags$div(
                  id = "descarga masiva",
                  role = "main",
                  fluidRow(
                    column(1,
                           ""),
                    column(3,
                           br(),
                           checkboxGroupButtons(
                             inputId = "massiveDownload",
                             label = "2020",
                             choices = c("ENP", 
                                         "CCH"),
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square", 
                                            style = "color: steelblue"),
                               no = tags$i(class = "fa fa-square-o", 
                                           style = "color: #A1A1A1;")
                               ),
                             direction = "vertical",
                             size = "lg"
                           )
                           ),#end of column
                  ),#fluid row ends
                  style = "background-color: white;")#div ends
        ) #TAB DESCARGA ENDS
      ), #tabItems ENDS
      #appeding attributes to main tabset
      role = "tablist",
      id = "tab para consultar los datos del ticometro"
      ),
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

