#' get_mainVars_4_planteles: A function to get main vars CCH from the ticometro filtering escuela 
#' @description ABOUT POOL connection:First, you should create your pool at the top of server.R (or in global.R), but outside the actual server function. Then, for each query, you should use tbl (or equivalent). In a Shiny app, this means that each reactive or function that queries the database has its own call to tbl
#' @import dplyr
#' @import stringr
get_mainVars_4_planteles <- function(db_connection, select_schools, select_groups = NULL, fecha_aplicacion = 2020){
  
  #declare connection: https://shiny.rstudio.com/articles/pool-dplyr.html
  ticometro_table <- dplyr::tbl(db_connection, "ticometro_resultados_main_table")
  
  #handle CCH and directivos queries
  #check if group is null or if selected schools are mixed to signal CCH or directivo variables
  if (is.null(select_groups) | (any(stringr::str_detect(select_schools, "ENP"))
                                    &
                                    any(stringr::str_detect(select_schools, "CCH"))
                                )
      ){ #end of conditional
    
  #QUERY database 4 main variables to present
  all_main_vars <- ticometro_table %>% 
    dplyr::select(1:20) %>% #the first 20 are the main variables
    dplyr::filter(institucion %in% select_schools,  #filtra por escuela
           fecha_aplicacion == fecha_aplicacion) %>%  #filtra el fecha_aplicacion de aplicacion
    dplyr::collect()%>%  #pull data into the applicacion
    dplyr::select(!c(fecha_aplicacion)) #remove year of aplicacion
  
  #We group is not selected we know its the CCH variables or directivos so we show all
  #Pretty titles
  all_TICometro_variables <- c(
    #"Año del TICómetro",
    "Alumno",
    "Plantel",
    "Grupo",
    "Género",
    "Escuela de Procedencia",
    "Uso compartido de laptop o computadora",
    "Estabilidad de la red en casa",
    "Conexión a Internet fuera de casa",
    "Dispositivo TIC de acceso",
    "# de dispositivos TIC",
    "Edad de primer uso de TIC",
    "Plataformas Educativas que conoce el estudiante",
    "# de Plataformas Educativas que conoce el estudiante",
    "Color de cinta obtenida",
    "Calificación TICómetro",
    "Calif. Procesamiento",
    "Calif. Acceso",
    "Calif. Seguridad",
    "Calif. Colaboración"
  )
  
  #rename columns
  colnames(all_main_vars) <- all_TICometro_variables
  
  return(all_main_vars)
  
  }else{ #ENP logic
    
    #QUERY database 4 main variables to present
    ENP_main_vars <- ticometro_table %>% 
      dplyr::select(1:10, 12:20) %>% #the first 20 are the main variables. skip 11 que es redundante en la enp
      dplyr::filter(institucion %in% select_schools,  #filtra por escuela
             grupo %in% select_groups, #filtra los grupos del enp
             fecha_aplicacion == fecha_aplicacion) %>%  #filtra el fecha_aplicacion de aplicacion
      dplyr::collect() %>%  #pull data into the applicacion
      dplyr::select(!c(fecha_aplicacion)) #remove year of aplicacion
    
    #We group is not selected we know its the CCH variables or directivos so we show all
    #Pretty titles
    ENP_TICometro_variables <- c(
      #"Año del TICómetro",
      "Alumno",
      "Institución",
      "Grupo",
      "Género",
      "Escuela de Procedencia",
      "Uso compartido de laptop o computadora",
      "Estabilidad de la red en casa",
      "Conexión a Internet fuera de casa",
      "Dispositivo TIC de acceso",
      "Edad de primer uso de TIC",
      "Plataformas Educativas que conoce el estudiante",
      "# de Plataformas Educativas que conoce el estudiante",
      "Color de cinta obtenida",
      "Calificación TICómetro",
      "Calif. Procesamiento",
      "Calif. Acceso",
      "Calif. Seguridad",
      "Calif. Colaboración"
    )
    
    #rename columns
    colnames(ENP_main_vars) <- ENP_TICometro_variables
    
    return(ENP_main_vars)
    
  }
  
}
