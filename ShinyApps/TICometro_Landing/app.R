######################################
###### TICometro Landing ##########
######################################

#targeting the header to dealete it. see: 
header <- dashboardHeader(compact = TRUE) 
header[[1]] <- tagAppendAttributes(header[[1]], id = "header2HIDE")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  #HEAD tags 4 various reasons
  tags$head(
    #include css
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "shortcut icon", href = "favicon.png")#add favicon
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
      target = "_blank", "H@bitat Puma, DGTIC, UNAM. Development Site",
      role = "contentinfo"
    ),
    right = "2021"
  ), #footer ends
  #BODY STARTS HERE
  body = dashboardBody(
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
        box(
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
        box(
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
