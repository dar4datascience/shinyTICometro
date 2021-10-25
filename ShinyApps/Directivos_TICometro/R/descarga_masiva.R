descarga_masiva <- function(escuelas, fecha_de_aplicacion){
  
  
  if (fecha_de_aplicacion == "2020") {
    
    #DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html
    
    ticometro_table <- dplyr::tbl(db_connection, "ticometro_resultados_main_table")
    
    print("funcionalidad no lista")
    
  } else if (fecha_de_aplicacion == "2021") {
    
    
    #* connect 2 specific table ------------------------------------------------
    
    #DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html
    
    ticometro_table <- dplyr::tbl(db_connection, "ticometro_resultados_2021")
    
    all_data <- ticometro_table %>% 
      filter(institucion %in% contains(escuelas)) %>% 
      collect()
    
    #** Rename columns ----------------------------------------------------------
    
    
    #Pretty titles
    all_TICometro_variables <- c(
      #"Año del TICómetro",
      "Alumno",
      "Plantel",
      "Grupo",
      "Género",
      "Escuela de Procedencia",
      "Principal dispositivo para Clases a Distancia",
      "uso_compu_lap?",
      "Estabilidad de la red para clases",
      "Conexión a Internet fuera de casa",
      "Dispositivo TIC de acceso",
      "Plataformas Educativas que conoce el estudiante",
      "# de Plataformas Educativas que conoce el estudiante",
      "Edad de primer uso de TIC",
      "Calificación TICómetro",
      "Calif. Procesamiento",
      "Calif. Acceso",
      "Calif. Seguridad",
      "Calif. Colaboración",
      "Color de cinta obtenida"
    )
    
    #rename columns
    colnames(all_data) <- all_TICometro_variables
    
    
  }
  
  
}
