######################################
###### TICometro Landing ##########
######################################

library(shiny)
library(bs4Dash)
library(fresh)
library(htmltools)



# Define UI for application that draws a histogram
ui <- bs4Dash::dashboardPage(
    dark = NULL,
    #HEAD tags 4 various reasons
    tags$head(
    #include css
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "shortcut icon", href = "favicon.png")#add favicon
    ),#head ends
# CODE TO CHANGE COLORS OF THE APP
freshTheme = myTheme,
preloader = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#343a40"),
    fullscreen = TRUE,
    scrollToTop = TRUE,
    #HEADER STARTS HERE
    header = bs4Dash::dashboardHeader(
        fixed = FALSE,
        title = tags$a(href = 'https://educatic.unam.mx/publicaciones/informes-ticometro.html',
                       tags$img(src = 'logo_ticometro_pequenio.jpg')),
        border = FALSE,
        tags$h5("Consulta los datos del TICómetro", id = "title-navbar"),
        controlbarIcon = shiny::icon("laptop-code"),
        leftUi = tagList(
            dropdownMenu(
                badgeStatus = "info",
                type = "tasks",
                taskItem(
                    inputId = "triggerAction3",
                    text = "Mi progreso de la App",
                    color = "warning",
                    value = 70
                )
            )
        )
    ),
    #SIDEBAR STARTS HERE
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
#UNABLE TO DISABLE. USE IT FOR CREDITS
controlbar = NULL, #END OF CONTROL BAR
    #Footer STARTS HERE
    footer = bs4Dash::dashboardFooter(
        fixed = FALSE,
        left = tags$a(
            href = "https://educatic.unam.mx/publicaciones/informes-ticometro.html",
            target = "_blank", "H@bitat Puma, DGTIC, UNAM CHANGE ME"
        ),
        right = "2021"
    )    ,
    #BODY STARTS HERE
    body = bs4Dash::dashboardBody(
    #here image del ticometro
    fluidRow(
        tags$img(src = "logo_dgtic.png",  width="50%",
                 )
  ),
    fluidRow(tags$img(src = "logo_ticometro.jpg",width="100%", align = "center")),
      tags$div(
        class = "row",
      tags$h1(
        "Resultados"
      )
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
                           align = "center")
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
                    align = "center",
                    )
            ),
            background = "primary"
        )
    )),
    br(),
    fluidRow(
        h2("¿Con que variables cuenta el TICometro?")

    ),
    fluidRow(
        accordion(
            id = "accordion1",
            accordionItem(
                title = "ENP",
                status = "info",
                tags$p(HTML("institucion <br>
            grupo	<br>
            genero	<br>
            escuela_de_procedencia <br>
            compartes_TIC	<br>
            estabilidadInternet4Clases
            InternetFueraDCasa	<br>
            Dispositivos Electronicos <br>
            total_de_dispositivos_por_estudiante <br>
            Cuales Plataformas educativas conoces* (i.e moodle, edmodo, blackboard, google_classroom, etc.	<br>
            total_de_plataformas_por_estudiante
            edad_uso_dispositivo	<br>
            cinta obtenida
            calificacion General (1-10) <br>
            calif_proces_admin_infor <br>
            calif_acceso_informacion <br>
            calif_seguridad	<br>
            calif_colabor_comunic <br>
            *Corresponden a preguntas de opcion multiple.")
                )
            ),
            accordionItem(
                title = "CCH",
                status = "orange",
                tags$p(
                  HTML(
                    "institucion <br>
            genero	<br>
            escuela_de_procedencia <br>
            compartes_TIC	<br>
            estabilidadInternet4Clases
            InternetFueraDCasa	<br>
            Dispositivos Electronicos* <br>
            total_de_dispositivos_por_estudiante <br>
            Cuales Plataformas educativas conoces* (i.e moodle, edmodo, blackboard, google_classroom, etc.	<br>
            total_de_plataformas_por_estudiante	<br>
            edad_uso_dispositivo	<br>
            cinta obtenida	<br>
            calificacion General (1-10) <br>
            calif_proces_admin_infor <br>
            calif_acceso_informacion <br>
            calif_seguridad	<br>
            calif_colabor_comunic     <br>
            *Corresponden a preguntas de opcion multiple."
                )
                )
            ) #ACCORDION ITEM ENDS
        )
    ), #FLUID ROW OF ACCORDION 1 ENDS

    br(),
    fluidRow(
        h2("¿Cuál es el porcentaje de estudiantes de primer ingreso que participaron?")
    ),
    fluidRow(
        accordion(
            id = "accordion2",
            accordionItem(
                title = "ENP",
                status = "info",
                tags$p(
                  HTML("Información no disponible.")
                )
            ),
            accordionItem(
                title = "CCH",
                status = "orange",
               tags$p(
                 HTML(
                    "<TABLE>
   <TR>
      <TD>CCH AZCAPOTZALCO</TD>
      <TD style='padding-left: 10px;'>89.13%</TD>
   </TR>
   <TR>
      <TD>CCH NAUCALPAN</TD>
      <TD style='padding-left: 10px;'>83.29%</TD>
   </TR>
   <TR>
      <TD>CCH ORIENTE</TD>
      <TD style='padding-left: 10px;'>77.07%</TD>
   </TR>
      <TR>
      <TD>CCH SUR</TD>
      <TD style='padding-left: 10px;'>89.11%</TD>
   </TR>
   <TR>
      <TD>CCH VALLEJO</TD>
      <TD style='padding-left: 10px;'>87.52%</TD>
   </TR>
   <TR>
      <TD>TOTAL</TD>
      <TD style='padding-left: 10px;'>84.06%</TD>
   </TR>
</TABLE>
            "
                )
)
            )
)
)
        )#fluid row ENDS accordion 2
#div container ends
    )#BODY ENDS
)# page ends

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
