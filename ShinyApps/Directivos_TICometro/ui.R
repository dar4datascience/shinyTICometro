#### TICometro 4 Directivos UI 2021##########

# Declaring useful variables ----------------------------------------------

grupos_y_escuelas <- grupos_y_escuelas_2021()

grupos_enp <- grupos_y_escuelas %>% filter(grepl("ENP", institucion))

grupos_cch <- grupos_y_escuelas %>% filter(grepl("CCH", institucion))

##### List of variable choices



datos_de_contexto <- get_variables_del_ticometro("contexto")

habilidades_digitales <- get_variables_del_ticometro("habilidades")

nivel_de_acceso <- get_variables_del_ticometro("acceso")


# Lista Escuelas ENP
ENP_escuelas <- get_escuelas_ticometro(instituto = "ENP")

# ESCUELAS CCH

# ESCUELAS CCH
CCH_escuelas <- get_escuelas_ticometro(instituto = "CCH")


# Fresh theme -------------------------------------------------------------

# CODE TO CHANGE COLORS OF THE APP
myTheme <- fresh::create_theme( # FIND MORE CUSTOMIZATION AT fresh::search_vars_bs4dash("navbar")
  bs4dash_vars(
    main_header_light_form_control_bg = "gray_x_light",
    navbar_light_color = "#343A40 !important"
  ),
  bs4dash_sidebar_light(
    bg = "#EBEBEB"
  ),
  bs4dash_layout(
    main_bg = "#f0f5f8"
  ),
  bs4dash_status(
    # 6 statutes available
    primary = "#386FB5",
    danger = "#BF616A",
    secondary = "#6c757d",
    info = "#17a2b8",
    success = "#28a745",
    warning = "#ffc107"
  ),
  bs4dash_color( # main colors of bs4dash. as used by the app
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
    # white = "#EBEBEB"
  ),
  bs4dash_font(
    # size_base = "1rem",
    # weight_bold = 900,
    family_base = "Arial"
  )
)



# UI Begins ---------------------------------------------------------------


shinyUI(
  bs4Dash::dashboardPage(

    #* Page elements -----------------------------------------------------------
    freshTheme = myTheme,
    # HEAD tags 4 various reasons
    tags$head( # Note the wrapping of the string in HTML()
      tags$link(rel = "shortcut icon", href = "favicon.png"), # add favicon
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ), # END OF HEAD
    preloader = list(
      html = tagList(waiter::spin_gauge(), "Cargando sitio de consulta del TICómetro ..."),
      color = "#343a40"
    ),
    fullscreen = TRUE,
    title = "Sitio de Consulta TICometro",
    dark = NULL,

    # Control BAR STARTS HERE
    # UNABLE TO DISABLE. USE IT FOR CREDITS
    controlbar = NULL, # END OF CONTROL BAR,


    #** Header ------------------------------------------------------------------


    header = bs4Dash::dashboardHeader(
      fixed = FALSE,
      title = tags$a(
        href = "http://132.248.10.243:3838/El-Duque/TICometro_Landing",
        tags$img(
          src = "logo_ticometro_pequenio.jpg",
          width = "100%",
          alt = "El logo del TICómetro es  un rectangulo con una regla azul cruzando masomenos por en medio y fondo verde a la izquierda, amarillo arriba, marón a la derecha y rojo abajo. La palabra TICómetro se encuentre en el centro con las letras TIC en naranja y más grande ómetro que está en negro"
        )
      ),
      border = FALSE,
      tags$h3(
        as.character("Consulta los datos del TICómetro"),
        id = "title-navbar",
        style = "padding-top: 7px;"
      )
    ), # HEADER ENS

    # **Sidebar ---------------------------------------------------------------


    # SIDEBAR STARS HERE
    sidebar = bs4Dash::dashboardSidebar(
      fixed = FALSE,
      collapsed = FALSE,
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
          # here is a styler
          sidebarHeader(
            tags$h6(
              "Resultados del TICómetro",
              style = "text-align: center;"
            )
          ),
          menuItem(
            text = "TICómetro 2021",
            icon = shiny::icon("search"),
            tabName = "consulta2021",
            badgeLabel = "nuevo",
            badgeColor = "success"
            # menuSubItem(
            #    text = "Compara",
            #   icon = shiny::icon("chart-bar"),
            #  tabName = "compara"
            # )
          ),
          menuItem(
            text = "Descarga Masiva",
            icon = icon("download"),
            tabName = "descarga"
          )
        )
      )
    ), # end of side bar

    #** Footer ------------------------------------------------------------------


    footer = dashboardFooter(
      left = tags$p(
        "®Hecho en México, Universidad Nacional Autónoma de México (UNAM), todos los derechos reservados 2012 - 2021. Esta página puede ser reproducida con fines no lucrativos, siempre y cuando se cite
la fuente completa y su dirección electrónica, y no se mutile. De otra forma requiere permiso previo por escrito de la institución. Sitio web diseñado y administrado en la Coordinación de Tecnologías
para la Educación de la Dirección de Innovación y Desarrollo Tecnológico de la DGTIC.",
        style = "font-size: 0.7rem;
text-align: center;
margin-top: 0;
margin-bottom: 0;
padding-left: 150px;
padding-right: 150px;
color: black;"
      ),
      right = "2021"
    ),
    # footer ends
    # **Body ------------------------------------------------------------------


    body = bs4Dash::dashboardBody(
      htmltools::tagAppendAttributes(
        # BODY STARTS
        bs4Dash::tabItems(

          #*** tab content ---------------------------------------------------------


          # ENP TAB starts here!
          bs4Dash::tabItem(
            role = "tab",
            tabName = "consulta2021",
            fluidRow(
              id = "texto encima de selectores",
              tags$h5(
                tags$b("Seleccione una o varias opciones:")
              )
            ),

            #*** selectors ----------------------------------------------------------
            fluidRow(
              role = "main",
              column( # STARTS SCHOOL INPUT
                width = 3,
                htmltools::tagAppendAttributes(
                  # USE PICKER FOR EASY MULTIPLE SELECTION
                  shiny::selectizeInput(
                    inputId = "escuelas_directivos_picked",
                    label = "Escuela y/o plantel:",
                    choices = list(
                      "ENP" = c(ENP_escuelas$escuela_name),
                      "CCH" = c(CCH_escuelas$escuela_name)
                    ),
                    multiple = TRUE,
                    selected = c("CCH NAUCALPAN",
                                 "ENP 1"),
                    options = list(placeholder = 'Tienes que escoger algún instituto')
                  ),
                  role = "button",
                  id = "selector de escuelas"
                )
              ), # END OF SCHOOL INPUT
              column( # sTARTS GRUPO INPUT
                width = 2,
                # use picker with searchable
                shiny::selectizeInput(
                  inputId = "grupo_select",
                  label = "Grupo:",
                  choices = "Ninguno",
                  multiple = TRUE,
                  selected = "Ninguno"
                )
              ), # ENDS GRUPO INPUT
              column(
                width = 4,
                htmltools::tagAppendAttributes(
                  shiny::selectizeInput(
                    inputId = "plot_directivo_var",
                    label = "Variables del TICómetro",
                    choices = list(
                      "Datos de Contexto" = datos_de_contexto,
                      "Nivel de Acceso" = nivel_de_acceso,
                      "Habilidades Digitales" = habilidades_digitales
                    ),
                    selected = "calif_checker"
                  ),
                  role = "button",
                  id = "selector de variables"
                )
              ), # END SELECT VAR input
              column( # STARTS CONSULTA BUTTON
                width = 3,
                br(), # try to align widgets
                div(
                  id = "button que se activa con ENTER",
                  htmltools::tagAppendAttributes(
                    shinyWidgets::actionBttn(
                      inputId = "activa_consulta",
                      label = "Consulta",
                      style = "simple",
                      color = "success"
                    ),
                    role = "button",
                    style = "background: #026928 !important;"
                  ),
                  style = "margin-top: 7px;"
                )
              ) # ENDS column ACTION BUTTON
            ), # end of fluid row

            # ***TabBox -----------------------------------------------------------


            fluidRow(
              id = "texto encima de tab box",
              tags$p(
                tags$b(
                  "La gráfica muestra el resultado de la selección de planteles y grupos seleccionados"
                )
              )
            ),
            # Second Fluid Row
            fluidRow(
              role = "main",
              id = "results container",
              htmltools::tagAppendAttributes(
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
                  # HERE A STYLER
                  status = "primary",
                  solidHeader = TRUE,
                  headerBorder = FALSE,
                  selected = "Gráfica",
                  # strech the plot with negative margins
                htmltools::tagAppendAttributes(
                    tabPanel(
                      role = "tabpanel",
                      "Gráfica",
                      # Plot inputs
                      plotlyOutput("directivos_plot") %>% shinycssloaders::withSpinner(
                        type = 1,
                        size = 3,
                        color = "#FFFFFF"
                      )
                    ),
                    style = "margin:-18px;"
                  ), # end of tag append attributes
                  tabPanel( # tab for data table
                    role = "tabpanel",
                    "Tabulado de Datos",
                    reactable::reactableOutput("tabulated_vars_directivos")
                  ), # tab panel ends HOJA CON DATOS TABULADOS
                  tabPanel(
                    role = "tabpanel",
                    "Hoja de Datos",
                    reactable::reactableOutput("main_vars_directivos")
                  ), # tab panel HOJA DE DATOS RAW


                  # **** Dropdown 4 download ------------------------------------------------


                  dropdownMenu = boxDropdown(
                    id = "seccion mi descarga",
                    role = "region",
                    icon = shiny::icon("download",
                      class = "fa-2x"
                    ),
                    # when passing an id to boxDropdownItem it will behave like a button!
                    # boxDropdownItem("Descargue su gráfica",
                    #               id = "descarga_grafica",
                    #              role = "button",
                    #             icon = shiny::icon(name = "signal", lib = "glyphicon")
                    #            ),
                    htmltools::tagAppendAttributes(
                      style = "background: black;
                  width: 100%;",
                      shiny::downloadButton(
                        outputId = "download_tabulado",
                        "Tabulado de datos",
                        icon = icon("file-csv")
                      )
                    ), # end of append attributes,
                    htmltools::tagAppendAttributes(
                      style = "background: black;
                  width: 100%;",
                      shiny::downloadButton(
                        outputId = "download_data",
                        "Hoja de datos",
                        icon = icon("database")
                      )
                    ) # end of append attributes
                  ) # end of drop down
                ) # end of tab box
              ), # end of fluid row
              role = "tab"
            ),

            #*** Value and info boxes ----------------------------------------------------


            # Third Fluid row
            fluidRow( # divides in 12
              role = "region",
              id = "value boxes y explicacion de cintas",
              bs4Dash::valueBoxOutput("value_box_directivos",
                                      width = 4), # width 2
              bs4Dash::valueBoxOutput("mode_box_directivos",
                                      width = 5), # width 3
              crea_tabla_de_cintas()
            )
          ), # end of tab items 1
          #   tabItem(tabName = "compara",
          #          "Hola2"),

          #*** Tab Descarga ------------------------------------------------------------


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
              div(
                id = "button que se activa con ENTER",
                htmltools::tagAppendAttributes(
                  shiny::downloadButton(
                    outputId = "MassivedownloadData",
                    "Consula",
                    class = "bttn bttn-gradient bttn-md bttn-success bttn-no-outline",
                    icon = icon(name = "database"),
                    role = "button",
                    style = "margin-left: 100px;"
                  )
                  # ,style = "padding-left: 200px;"
                ) # end of append attributes
              ) # end of div
            ), # end of fluid row
            tags$div(
              id = "descarga masiva",
              role = "main",
              fluidRow(
                column(
                  1,
                  ""
                ),
                column(
                  3,
                  br(),
                  checkboxGroupButtons(
                    inputId = "massiveDownload",
                    label = "2021",
                    choices = c(
                      "ENP",
                      "CCH"
                    ),
                    checkIcon = list(
                      yes = tags$i(
                        class = "fa fa-check-square",
                        style = "color: steelblue"
                      ),
                      no = tags$i(
                        class = "fa fa-square-o",
                        style = "color: #A1A1A1;"
                      )
                    ),
                    direction = "vertical",
                    size = "lg"
                  )
                ) # end of column
              ), # fluid row ends
              style = "background-color: white;"
            ) # div ends
          ) # TAB DESCARGA ENDS
        ), # tabItems ENDS
        # appeding attributes to main tabset
        role = "tablist",
        id = "tab para consultar los datos del ticometro"
      ),

      #* Final page elements -----------------------------------------------------
      useWaiter(),
      useSever(),
      autoWaiter(
        id = c(
          "value_box_directivos",
          "mode_box_directivos",
          "tabulated_vars_directivos",
          "main_vars_directivos"
        ),
        html = spin_plus(),
        fadeout = FALSE,
        color = "#FFFFF",
        image = ""
      ),
      useShinyalert() # include
    ) # BODY ENDS
  ) # PAGE ENDS
) # SHINYUI ENDS
