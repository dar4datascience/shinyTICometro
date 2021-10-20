######################################
###### TICometro Landing ##########
######################################
library(htmltools)
library(fresh)
library(reactable)
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(bs4Dash)))
suppressMessages(suppressWarnings(library(shinyalert)))
suppressMessages(suppressWarnings(library(shiny)))
#targeting the header to dealete it. see: 
header <- dashboardHeader(compact = TRUE) 
header[[1]] <- tagAppendAttributes(header[[1]], id = "header2HIDE")

datos_de_contexto <- list("Género",
                          "Escuela de Procedencia" 
                          )

habilidades_digitales <- list("Color de cinta obtenida",
                              "Calificación TICometro",
                              "Calif. Procesamiento",
                              "Calif. Acceso",
                              "Calif. Seguridad",
                              "Calif. Colaboración")

nivel_de_acceso <- list(
  "Edad de primer uso de TIC",
  "Acceso a Dispositivos" ,
  "# de Dispositivos TIC" ,
  "Uso compartido de laptop o computadora",
  "Estabilidad de la red en casa" ,
  "Conexión a Internet fuera de casa" ,
  "Conocimiento sobre plataformas educativas" ,
  "# de Plataformas Educativas que conoce el estudiante" 
  )

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

variables_del_ticometro_df <- dplyr::tibble(
  'Variable'= c(datos_de_contexto,nivel_de_acceso, habilidades_digitales)
)

participacion_en_el_ticometro_df <- dplyr::tibble(
  "Escuela" =   c(CCH_escuelas$escuela_name, ENP_escuelas$escuela_name),
  `# alumnos que participaron` = c(3161, 2884, 2891, 3461, 3282, 1090, 1597, 1257, 1224, 2100, 1553, 1542, 1429, 1467
                                   )
)

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
    olive = "#026928", #change to fit buton profesores
    green = NULL,
    lime = NULL,
    orange = NULL,
    yellow = NULL,
    fuchsia = NULL,
    purple = NULL,
    maroon = "#972415", #color for directivos buton
    red = NULL,
    black = NULL,
    gray_x_light = NULL, 
    gray_600 = NULL,
    gray_800 = NULL,
    gray_900 = NULL,
    white = NULL #"#272c30"
    
  ),
  bs4dash_font(
    #size_base = "1.5rem",
    #weight_bold = 900,
    family_base = "MyriadPro-Regular"
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  #HEAD tags 4 various reasons
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.png"),#add favicon
    #include css
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    
  ),#head ends

  # CODE TO CHANGE COLORS OF THE APP
  freshTheme = myTheme,
  #HEADER STARTS HERE
  header = header,
  #SIDEBAR STARTS HERE
  sidebar = dashboardSidebar(disable=TRUE),
  #UNABLE TO DISABLE. USE IT FOR CREDITS
  controlbar = NULL, #END OF CONTROL BAR
  #Footer STARTS HERE
  footer = dashboardFooter(
    left = tags$a(
      href = "https://educatic.unam.mx/publicaciones/informes-ticometro.html",
      target = "_blank",
      tags$p("®Hecho en México, Universidad Nacional Autónoma de México (UNAM), todos los derechos reservados 2012 - 2021. Esta página puede ser reproducida con fines no lucrativos, siempre y cuando se cite
la fuente completa y su dirección electrónica, y no se mutile. De otra forma requiere permiso previo por escrito de la institución. Sitio web diseñado y administrado en la Coordinación de Tecnologías

para la Educación de la Dirección de Innovación y Desarrollo Tecnológico de la DGTIC.",
style = "font-size: 0.7rem;
text-align: center;
margin-top: 0;
margin-bottom: 0;
padding-left: 150px;
padding-right: 150px;")
    ),
    right = "2021"
  ), #footer ends
  #BODY STARTS HERE
  body = dashboardBody(
    #here image del ticometro
    fluidRow(
      id = "logo dgtic",
      tags$img(src = "logo_dgtic.png", alt = "un logo compuesto en la izquierda del escudo de la UNAM y las letras DGTIC abajo y a la derecha el texto: Universidad Nacional Autónoma de México, Dirección General de Cómputo y de Tecnologías de Información y Comunicación.",
               width="40%")
    ),
    fluidRow(
      id = "logo ticometro",
      tags$img(src = "logo_ticometro.jpg", alt = "El logo del TICómetro es  un rectangulo con una regla azul cruzando masomenos por en medio y fondo verde a la izquierda, amarillo arriba, marón a la derecha y rojo abajo. La palabra TICómetro se encuentre en el centro con las letras TIC en naranja y más grande ómetro que está en negro",
                      width = "100%",
               align = "center",
               style = "padding-right: 275px;
               padding-left: 275px;")
      ),
    fluidRow(
      tags$h1(
        "RESULTADOS",
        style = "font-size: 4.5em;
        font-family: 'MyriadProBold';"
      ),
      id = "banner resultados",
      style = "background-color: #fcd753;
      margin-left: 268px;
      margin-right: 268px;
      margin-bottom: -13px;"
    ),
    tags$div( #entrap all elements in a white background div
      br(),
      fluidRow(
        id = "descripcion del ticometro",
        tags$p(
          "El TICómetro surge en el año 2012 a partir de la línea rectora 1 del Plan de Desarrollo Institucional 2011-2015. Actualmente, el TICómetro representa un instrumento de evaluación de habilidades digitales que aporta datos valiosos para pensar la estrategia de integración de TIC en las actividades educativas, la formación de profesores y las prioridades en relación con la dotación de infraestructura en los planteles universitarios. Responde, entre otros, al Programa Estratégico 7 del Plan de Desarrollo Institucional 2015-2019: Tecnologías de la Información y Comunicación (TIC) y Tecnologías del Aprendizaje y el Conocimiento (TAC).",
          style = "color: black;
          font-size: 1.2rem;
	padding-left: 300px;
  padding-right: 300px;
  text-align: justify;"
        )
      ),
      fluidRow(
        id = "titulo encima de botones",
        tags$h2(
          tags$b("Consulta los Resultados del 2020!")
        )
      ),
      fluidRow(
        id = "renglon con botones para los sitios de consulta",
        splitLayout(
          htmltools::tagAppendAttributes(
            style = "left: 60%;",
        box(
          id = "buton_directivos",
          width = 4,
          headerBorder = FALSE,
          collapsible = FALSE,
          tags$a(href  = "http://132.248.10.243:3838/El-Duque/Directivos_TICometro/",
                 tags$h3(tags$b("Directivos"),
                         align = "center",
                         style = "margin-bottom: 0px;
                         margin-top: -25px;"
                         ),
                 style = "color: white",
                 role = "button"
          ),
          background = "maroon"
        )
  ),          
  htmltools::tagAppendAttributes(
    style = "",
        box(
          id = "buton_profesores",
          width = 4,
          title = NULL,
          headerBorder = FALSE,
          collapsible = FALSE,
          tags$a(href  = "http://132.248.10.243:3838/El-Duque/Profes_TICometro/",
                 tags$h3(tags$b("Profesores",),
                         align = "center",
                         style = "margin-bottom: 0px;
                         margin-top: -25px;"
                         ),
                 style = "color: white",
                 role = "button"
          ),
          background = "olive"
        )
      )#end of append attributes
        )#end of split layout
    ),#end of fluid row
    fluidRow(
      bs4Dash::box(
        title = tags$b("Variables de análisis del TICómetro",
                       style = "font-size: 1.5rem;"),
        closable = FALSE,
        width = 4,
        solidHeader = FALSE,
        headerBorder = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        reactable::reactableOutput("ticometro_variables")
      )
    ),#end fluid row
    fluidRow(
      bs4Dash::box(
        title = tags$b("Porcentaje de participación",
                       style = "font-size: 1.5rem;"),
        closable = FALSE,
        width = 4,
        headerBorder = FALSE,
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        reactable::reactableOutput("ticometro_participacion")
      )
    ),#end fluid row
      style = "background-color: #FFFFFF;")#div container ends
  ) #BODY ENDS
) # page ends


server <- function(input, output) {

  output$ticometro_variables <- renderReactable({
    reactable(variables_del_ticometro_df,
              borderless = TRUE,
              highlight = TRUE,
              defaultPageSize = 16)
  })
  output$ticometro_participacion <- renderReactable({
    reactable(participacion_en_el_ticometro_df,
              outlined = TRUE,
              highlight = TRUE,
              defaultPageSize  = 14,
              columns = list(
                "Escuela" = colDef(footer = "Total General"),
                `# alumnos que participaron` = colDef(format = colFormat(separators = TRUE),
                                                      footer = prettyNum(sum(participacion_en_el_ticometro_df$`# alumnos que participaron`),  big.mark = ",")
                )
                                                      
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              )
  })
  
  

}


shinyApp(ui = ui, server = server)
