#' conjunto de funciones esenciales para la aplicacion del TICometro
#'
#' @description Las funciones que se crean son las siguientes:
#'
#' @noRd


#' pantala_desconexion
#' @description una funcion para configurar la pantalla de error
#' @return a tag list for shiny to read in the server
#' @importFrom sever sever_default
pantalla_desconexion <- function() {
  disconnected <- sever_default(
    title = "Error: Interrupción del procesamiento",
    subtitle = "Disculpe las molestias. Si esta pantalla continua apareciendo, favor de comunicarse con el administrador del sitio.",
    button = "Actualizar",
    button_class = "info"
  )
  
  return(disconnected)
}

#' Devuelve una lista con las escuelas que participan en el TICometro
#'
#' @param instituto un string: ENP o CCH
#' @return una lista con el nombre de todas las escuelas pertenecientes al string
#' @noRd
#' @importFrom dplyr tibble
#' @importFrom purrr map_df
get_name_escuelas_del_ticometro <- function(instituto) {
  if (instituto == "ENP") {
    #Lista Escuelas ENP
    ENP_escuelas <- dplyr::tibble(escuela_name = 1:9) %>%
      purrr::map_df(.,
                    \(x) {
                      paste0("ENP ", x)
                    })
    return(ENP_escuelas)
  } else if (instituto == "CCH") {
    #ESCUELAS CCH
    CCH_escuelas <-
      dplyr::tibble(
        escuela_name = c(
          "CCH Azcapotzalco",
          "CCH Naucalpan",
          "CCH Oriente",
          "CCH Sur",
          "CCH Vallejo"
        )
      )
    return(CCH_escuelas)
  } else{
    print("Selección no valida.")
  }
  
  
}


#' Devuelve una lista con las escuelas que participan en el TICometro
#'
#' @param tema un string. las opciones son: contexto, acceso y habilidades.
#' @return una lista con el nombre de todas las escuelas pertenecientes al string
#' @noRd
#' @importFrom dplyr tibble
#' @importFrom purrr map_df
get_variables_del_ticometro <- function(tema) {
  #List of variable choices
  if (tema == "contexto") {
    datos_de_contexto <- list("Género" = "genero",
                              "Escuela de Procedencia" = "escuela_de_procedencia")
    return(datos_de_contexto)
  } else if (tema == "habilidades") {
    habilidades_digitales <- list(
      "Color de cinta obtenida" = "cinta",
      "Calificación TICometro" = "calif_checker",
      "Calif. Procesamiento" = "calif_proces_admin_infor",
      "Calif. Acceso" = "calif_acceso_informacion",
      "Calif. Seguridad" = "calif_seguridad",
      "Calif. Colaboración" = "calif_colabor_comunic"
    )
    return(habilidades_digitales)
  } else if (tema == "acceso") {
    nivel_de_acceso <- list(
      "Edad de primer uso de TIC" = "edad_uso_dispositivo",
      "Acceso a Dispositivos" = "dispositivos_electronicos",
      "# de Dispositivos TIC" = "total_de_dispositivos_por_estudiante",
      "Uso compartido de laptop o computadora" = "compartes_tic",
      "Estabilidad de la red en casa" = "internet_fuera_d_casa",
      "Conexión a Internet fuera de casa" = "internet_fuera_d_casa",
      "Conocimiento sobre plataformas educativas" = "plataformas_edu_known",
      "# de Plataformas Educativas que conoce el estudiante" = "total_de_plataformas_por_estudiante"
    )
    return(nivel_de_acceso)
  } else {
    print("Opción no valida.")
  }
  
} #END FUNCTION

grupos_2021 <- function(){
  db_connection <- connect2database()  
  grupos <- dplyr::tbl(db_connection, "ticometro_resultados_2021") %>% 
    distinct(grupo) %>% 
    collect()
  pool::poolClose(db_connection)
  return(grupos)
}

db_connection <- connect2database()

grupo <- grupos_2021()$grupo

#' clean_plot_titles
#'
#' @param variable un string con el nombre de la variable
#' @description una funcion para obtener
#'  nombres adecuados para los titulos de las
#'  graficas
#' @return devuelve un solo valor string

clean_plot_titles <- function(variable) {
  #REVERSE LIST: used for putting neat names to plots
  reverse_TICometro_variables <- list(
    "num_alumno" = "Alumno",
    "institucion" = "Plantel",
    "grupo" = "Grupo",
    "genero" = "Género",
    "escuela_de_procedencia" = "Escuela de Procedencia",
    "compartes_tic" = "Uso compartido de laptop o computadora",
    "estabilidad_internet_4_clases" = "Estabilidad de la red en casa",
    "internet_fuera_d_casa" = "Conexión a Internet fuera de casa",
    "dispositivos_electronicos" = "Principal dispositivo para Clases a Distancia",
    "total_de_dispositivos_por_estudiante" = "# de Dispositivos TIC",
    "edad_uso_dispositivo" = "Edad de primer uso de TIC",
    "plataformas_edu_known" = "Plataformas Educativas que conoce el estudiante",
    "total_de_plataformas_por_estudiante" = "# de Plataformas Educativas que conoce el estudiante",
    "cinta" = "Color de cinta obtenida",
    "calif_checker" = "Calificación TICómetro",
    "calif_proces_admin_infor" = "Calif. Procesamiento",
    "calif_acceso_informacion" = "Calif. Acceso",
    "calif_seguridad" = "Calif. Seguridad",
    "calif_colabor_comunic" = "Calif. Colaboración"
  )
  
  return(as.character(reverse_TICometro_variables[variable]))
  
}

#FUNCION PARA CALCULAR LA MODA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#' crea_tabla_clas_cintas
#'
#' @description una funcion para armar la tabla que va abajo a la derecha del
#' ticometro y explica las cintas
#' @returns a bs4Table
crea_tabla_clas_cintas <- function() {

# Declara df a usar en la tabla de explicacion ----------------------------

  
  table_descripcion <- tibble(
    cintas = c(
      "cinta_blanca.png",
      "cinta_naranja.png",
      "cinta_azul.png",
      "cinta_negra.png"
    ),
    colores = c("Blanca:", "Naranja:", "Azul:", "Negra:"),
    descripcion = c("0 - 30%", "30.1% - 60%", "60.1% - 85%", "85.1 - 100%")
  )
  

# Box ---------------------------------------------------------------------

  
  bs4Dash::box(
    id = "explicacion-cintas-ticometro",
    title = tags$h5("Clasificación de las cintas",
                    id = "titulo-box-clasificacion-cinta",
                    style = "text-align: center;
                    margin-bottom: 0px;"),
    collapsible = FALSE,
    width = 6,
    solidHeader = TRUE,
    headerBorder = FALSE,
    status = "gray-dark",

# Row 1 de la tabla -------------------------------------------------------

    
    fluidRow(
      splitLayout(
        cellWidths = c("20%", "30%", "20%", "30%"),
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[1]),
          src = table_descripcion$cintas[1],
          width = "50%"
        ),
        tags$p(tags$b(table_descripcion$colores[1]),
               table_descripcion$descripcion[1]),
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[3]),
          src = table_descripcion$cintas[3],
          width = "50%"
        ),
        tags$p(tags$b(table_descripcion$colores[3]),
               table_descripcion$descripcion[3])
      )
    ),

# Row 2 de la tabla -------------------------------------------------------


    fluidRow(
      splitLayout(
        cellWidths = c("20%", "30%", "20%", "30%"),
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[2]),
          src = table_descripcion$cintas[2],
          width = "50%"
        ),
        tags$p(tags$b(table_descripcion$colores[2]),
               table_descripcion$descripcion[2]),
        tags$img(
          class = "cinta",
          alt = paste(table_descripcion$cintas[4]),
          src = table_descripcion$cintas[4],
          width = "50%"
        )
        ,
        tags$p(tags$b(table_descripcion$colores[4]),
               table_descripcion$descripcion[4])
      )
    )
  )#end of box
  
}
