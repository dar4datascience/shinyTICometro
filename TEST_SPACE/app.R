if (interactive()) {
       options(device.ask.default = FALSE)
  library(shiny)
  table_descripcion <- dplyr::tibble(
    cintas = c(
      "cinta_blanca.png",
      "cinta_naranja.png",
      "cinta_azul.png",
      "cinta_negra.png"
    ),
    colores = c("Blanca:", "Naranja:", "Azul:", "Negra:"),
    descripcion = c("0 - 30%", "30.1% - 60%", "60.1% - 85%", "85.1 - 100%")
  )
  
  
my_box <- bs4Dash::box(
    id = "explicacion-cintas-ticometro",
    title = tags$h5("Clasificación de las cintas", id = "titulo-box-clasificacion-cinta"),
    collapsible = FALSE,
    width = 6,
    solidHeader = TRUE,
    headerBorder = FALSE,
    status = "gray-dark",
    fluidRow(
      splitLayout(
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[1]),
          src = table_descripcion$cintas[1],
          width = "25%"
        ),
        tags$p(tags$b(table_descripcion$colores[1]),
               table_descripcion$descripcion[1]),
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[3]),
          src = table_descripcion$cintas[3],
          width = "25%"
        ),
        tags$p(tags$b(table_descripcion$colores[3]),
               table_descripcion$descripcion[3])
      )
          ),
    fluidRow(
      splitLayout(
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[2]),
          src = table_descripcion$cintas[2],
          width = "25%"
        ),
        tags$p(tags$b(table_descripcion$colores[2]),
               table_descripcion$descripcion[2]),
        
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[4]),
          src = table_descripcion$cintas[4],
          width = "25%"
        )
        ,
        tags$p(tags$b(table_descripcion$colores[4]),
               table_descripcion$descripcion[4])
      )
    )
  )#end of box
  
  
         # Server code used for all examples
         server <- function(input, output) {
           }
         
           # Equal sizing
           ui <- my_box
           shinyApp(ui, server)
           
           }
