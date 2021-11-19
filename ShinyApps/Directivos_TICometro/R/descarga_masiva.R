descarga_masiva <-
  function(db_connection,
           escuelas,
           fecha_de_aplicacion = "2021") {
    if (fecha_de_aplicacion == "2020") {
      # DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html

      ticometro_table <-
        dplyr::tbl(db_connection, "ticometro_resultados_main_table")

      print("funcionalidad no lista")
    } else if (fecha_de_aplicacion == "2021") {
      
      #* connect 2 specific table ------------------------------------------------

      # DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html

      ticometro_table <-
        dplyr::tbl(db_connection, "ticometro_resultados_2021")

      to_match <- paste(escuelas, collapse = "|")

      all_data <- ticometro_table %>%
        filter(grepl(to_match, institucion)) %>%
        select(1:18) %>%
        collect()


      #** Rename columns ----------------------------------------------------------


      # Pretty titles
      all_ticometro_variables <- c(
        # "Año del TICómetro",
        "Alumno",
        "Plantel",
        "Grupo",
        "Género",
        "Escuela de Procedencia",
        "Uso compartido de laptop o computadora",
        "Estabilidad de la red en casa",
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

      # rename columns
      colnames(all_data) <- all_ticometro_variables

      return(all_data)
    }
  }
