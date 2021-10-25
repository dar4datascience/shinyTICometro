#' handle_multiple_opcion_vars
#'
#' @description A fct function
#'
#' @return Returns a collect()ed query in the form of a tibble
#'
#' @import dplyr
#' @import stringr
#' @noRd
#' TEST PENDING!!!!! GOTTA CHECK EACH CASE: NULL group, group enp, multiple choice, 2 var to group

handle_multiple_choice_questions <-
  function(ticometro_table,
           select_schools,
           select_groups = NULL,
           multiple_opcion_var,
           grouping_var = NULL,
           fecha_de_aplicacion) {
    # No grouping 1 case --------------------------------------------------------
    
    
    #check if group is null or if selected schools are mixed to signal CCH or directivo variables
    if (is.null(select_groups) |
        (any(stringr::str_detect(select_schools, "ENP"))
         &
         any(stringr::str_detect(select_schools, "CCH")))) {
      #end of conditional
      
      #* No grouping 2 case -----------------------------------------------------
      
      
      if (is.null(grouping_var)) {
        #** If dispostivos electronicos ---------------------------------------------
        
        
        if (multiple_opcion_var == "dispositivos_electronicos") {
          count_var_4_directivos_and_cch <- ticometro_table %>%
            filter(institucion %in% select_schools,
                   fecha_aplicacion == fecha_aplicacion) %>%
            select(3, 22:31) %>%  #grab dispositivos tic multiple choice
            tidyr::pivot_longer(!institucion,
                                names_to = "dispositivos_electronicos",
                                values_to = "respuesta") %>%
            filter(respuesta == 1) %>%
            group_by(institucion, dispositivos_electronicos) %>%
            count(dispositivos_electronicos) %>%
            collect()
          
          
          # ***mutate collected -------------------------------------------------------
          
          
          tidy_multiple_choice_directivos_cch <-
            count_var_4_directivos_and_cch %>%
            mutate(
              dispositivos_electronicos = stringr::str_replace_all(dispositivos_electronicos, "_", " "),
              dispositivos_electronicos = stringr::str_replace_all(
                dispositivos_electronicos,
                "sin responder dispositivo x estudiante",
                "No declaró"
              )
            )
          
          #*** return object -----------------------------------------------------------
          
          
          return(tidy_multiple_choice_directivos_cch)
          
          #** if plataformas educativas -----------------------------------------------
          
          
        } else if (multiple_opcion_var == "plataformas_edu_known") {
          count_var_4_directivos_and_cch <- ticometro_table %>%
            filter(institucion %in% select_schools,
                   fecha_aplicacion == fecha_aplicacion) %>%
            select(3, 32:40) %>%  #grab eduplatform multiple choice
            tidyr::pivot_longer(!institucion,
                                names_to = "plataformas_edu_known",
                                values_to = "respuesta") %>%
            filter(respuesta == 1) %>%
            group_by(institucion, plataformas_edu_known) %>%
            count(plataformas_edu_known) %>%
            collect()
          
          # ***mutate collected -------------------------------------------------------
          
          
          tidy_multiple_choice_directivos_cch <-
            count_var_4_directivos_and_cch %>%
            mutate(
              plataformas_edu_known = stringr::str_replace_all(plataformas_edu_known, "_", " "),
              plataformas_edu_known = stringr::str_replace_all(
                plataformas_edu_known,
                "otra plataf educativa",
                "Otra plataforma educativa"
              ),
              plataformas_edu_known = stringr::str_replace_all(
                plataformas_edu_known,
                "sin responder plataf educa",
                "No declaró"
              )
            )
          
          #*** return object ----------------------------------------------------------
          
          
          return(tidy_multiple_choice_directivos_cch)
          
        }
        #second option if grouping var is declared
      } else{
        #logic for a 2 grouping variable
        
        #* Grouping 2 case ---------------------------------------------------------
        
        #** If dispostivos electronicos ---------------------------------------------
        
        if (multiple_opcion_var == "dispositivos_electronicos") {
          count_2vars_4_directivos_and_cch <- ticometro_table %>%
            filter(institucion %in% select_schools,
                   fecha_aplicacion == fecha_aplicacion) %>%
            select(3, 22:31, .data[[grouping_var]]) %>%  #grab dispositivos tic multiple choice
            tidyr::pivot_longer(!c(institucion, .data[[grouping_var]]),
                                names_to = "dispositivos_electronicos",
                                values_to = "respuesta") %>%
            filter(respuesta == 1) %>%
            group_by(institucion, .data[[grouping_var]], dispositivos_electronicos) %>%
            count(dispositivos_electronicos) %>%
            collect()
          
          
          # ***mutate collected -------------------------------------------------------
          
          tidy_2_vars_multiple_choice_directivos_cch <-
            count_2vars_4_directivos_and_cch %>%
            mutate(
              dispositivos_electronicos = stringr::str_replace_all(dispositivos_electronicos, "_", " "),
              dispositivos_electronicos = stringr::str_replace_all(
                dispositivos_electronicos,
                "sin responder dispositivo x estudiante",
                "No declaró"
              )
            )
          
          #*** return object -----------------------------------------------------------
          
          return(tidy_2_vars_multiple_choice_directivos_cch)
          
          #** if plataformas educativas -----------------------------------------------
          
        } else if (multiple_opcion_var == "plataformas_edu_known") {
          count_var_4_directivos_and_cch <- ticometro_table %>%
            filter(institucion %in% select_schools,
                   fecha_aplicacion == fecha_aplicacion) %>%
            select(3, .data[[grouping_var]], 32:40) %>%  #grab eduplatform multiple choice
            tidyr::pivot_longer(!c(institucion, .data[[grouping_var]]),
                                names_to = "plataformas_edu_known",
                                values_to = "respuesta") %>%
            filter(respuesta == 1) %>%
            group_by(institucion, .data[[grouping_var]], plataformas_edu_known) %>%
            count(plataformas_edu_known) %>%
            collect()
          
          # ***mutate collected -------------------------------------------------------
          
          
          tidy_multiple_choice_directivos_cch <-
            count_var_4_directivos_and_cch %>%
            mutate(
              plataformas_edu_known = stringr::str_replace_all(plataformas_edu_known, "_", " "),
              plataformas_edu_known = stringr::str_replace_all(
                plataformas_edu_known,
                "otra plataf educativa",
                "Otra plataforma educativa"
              ),
              plataformas_edu_known = stringr::str_replace_all(
                plataformas_edu_known,
                "sin responder plataf educa",
                "No declaró"
              )
            )
          
          #*** return object -----------------------------------------------------------
          
          return(tidy_multiple_choice_directivos_cch)
          
        }
        
      }
      
      
    } else{
      # Grouping 1 case ---------------------------------------------------------
      
      
      
      #* No grouping 2 case -----------------------------------------------------
      
      if (is.null(grouping_var)) {
        #** if plataforma known. No other options -----------------------------------------------------
        
        count_var_4_ENP <-
          ticometro_table %>%  #A conection pointing to the ticometro table
          filter(
            fecha_aplicacion == fecha_aplicacion,
            institucion %in% select_schools,
            grupo %in% select_groups
          ) %>%
          select(3, 4, 32:40) %>%
          tidyr::pivot_longer(!c(institucion, grupo),
                              names_to = "plataformas_edu_known",
                              values_to = "respuesta") %>%
          filter(respuesta == 1) %>%
          group_by(institucion, grupo, plataformas_edu_known) %>%
          count(plataformas_edu_known) %>%
          collect()
        
        # ***mutate collected -------------------------------------------------------
        
        tidy_multiple_choice_ENP <- count_var_4_ENP %>%
          mutate(
            plataformas_edu_known = stringr::str_replace_all(plataformas_edu_known, "_", " "),
            plataformas_edu_known = stringr::str_replace_all(
              plataformas_edu_known,
              "otra plataf educativa",
              "Otra plataforma educativa"
            ),
            plataformas_edu_known = stringr::str_replace_all(
              plataformas_edu_known,
              "sin responder plataf educa",
              "No declaró"
            )
          )
        
        print("you are looking for me")
        
        #*** return object -----------------------------------------------------------
        
        
        return(tidy_multiple_choice_ENP)
        
      } else{
        #logic for a 2 grouping variable
        
        #* Grouping 2 case ---------------------------------------------------------
        
        #** if plataforma known. No other options -----------------------------------------------------
        
        
        count_2vars_4_ENP <-
          ticometro_table %>%  #A conection pointing to the ticometro table
          filter(
            fecha_aplicacion == fecha_aplicacion,
            institucion %in% select_schools,
            grupo %in% select_groups
          ) %>%
          select(3, 4, 32:40, .data[[grouping_var]]) %>%
          filter() %>%
          tidyr::pivot_longer(!c(institucion, grupo, .data[[grouping_var]]),
                              names_to = "plataformas_edu_known",
                              values_to = "respuesta") %>%
          filter(respuesta == 1) %>%
          group_by(institucion, grupo, .data[[grouping_var]], plataformas_edu_known) %>%
          count(plataformas_edu_known) %>%
          collect()
        
        # ***mutate collected -------------------------------------------------------
        
        
        tidy_multiple_2choices_ENP <- count_2vars_4_ENP %>%
          mutate(
            plataformas_edu_known = stringr::str_replace_all(plataformas_edu_known, "_", " "),
            plataformas_edu_known = stringr::str_replace_all(
              plataformas_edu_known,
              "otra plataf educativa",
              "Otra plataforma educativa"
            ),
            plataformas_edu_known = stringr::str_replace_all(
              plataformas_edu_known,
              "sin responder plataf educa",
              "No declaró"
            )
          )
        
        #*** return object -----------------------------------------------------------
        
        return(tidy_multiple_2choices_ENP)
        
      }
      
    }
    
  } #END OF FUNCTION
