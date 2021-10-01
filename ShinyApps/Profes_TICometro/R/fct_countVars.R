#' countVars 
#'
#' @description A fct function
#' @import dplyr
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' FOR NOW GROUPING VAR CAN NEVER BE THE MULTIPLE CHOICE QUESTIONS
countVars <- function(db_connection, select_schools, select_groups = NULL, select_var, grouping_var = NULL, fecha_aplicacion = 2020){
  
  #DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html
  ticometro_table <- dplyr::tbl(db_connection, "ticometro_resultados_main_table")
  
  #handle CCH and directivos queries
  #check if group is null or if selected schools are mixed to signal CCH or directivo variables
  if (is.null(select_groups) | (any(stringr::str_detect(select_schools, "ENP"))
                                &
                                any(stringr::str_detect(select_schools, "CCH"))
  )
  ){ #end of conditional
    
    if (select_var == "plataformas_edu_known" | select_var == "dispositivos_electronicos"){
      
      #a collected query
      counted_df <- handle_multiple_choice_questions(ticometro_table,
                                                     select_schools,
                                                     select_groups = NULL,
                                                     multiple_opcion_var=select_var, grouping_var = grouping_var, fecha_aplicacion)
      
      print("im in cch logic multiple choice")
    }else{ #count regularly
      
      if(is.null(grouping_var)){
        
        counted_df <-  ticometro_table %>% 
          filter(institucion %in% select_schools,
                 fecha_aplicacion == fecha_aplicacion) %>% 
          select(institucion, .data[[select_var]]) %>% 
          group_by(institucion) %>% 
          count(.data[[select_var]]) %>% 
          collect()
        
        
      }else{ #count specially
        
        counted_df <-  ticometro_table %>% 
          filter(institucion %in% select_schools,
                 fecha_aplicacion == fecha_aplicacion) %>% 
          select(institucion, .data[[select_var]], .data[[grouping_var]]) %>% 
          group_by(institucion, .data[[grouping_var]]) %>% 
          count(.data[[select_var]]) %>% 
          collect()
      }
    }
    
    #END CCH AND DIRECTIVOS LOGIC
  }else{ #ENP LOGIC
    
    if (select_var == "plataformas_edu_known"){
      
      #a collected query
      counted_df <- handle_multiple_choice_questions(ticometro_table,
                                                     select_schools,
                                                     select_groups,
                                                     multiple_opcion_var = select_var,
                                                     grouping_var = grouping_var, fecha_aplicacion)
      
      print("im in enp logic multiple chocie")
      
    }else{#count normally
      
      if(is.null(grouping_var)){
        
        counted_df <-  ticometro_table %>% 
          filter(institucion %in% select_schools,
                 grupo %in% select_groups,
                 fecha_aplicacion == fecha_aplicacion) %>% 
          select(institucion, grupo, .data[[select_var]]) %>% 
          group_by(institucion, grupo) %>% 
          count(.data[[select_var]]) %>% 
          collect()
        
        
      }else{ #count specially
        
        counted_df <-  ticometro_table %>% 
          filter(institucion %in% select_schools,
                 grupo %in% select_groups, 
                 fecha_aplicacion == fecha_aplicacion) %>% 
          select(institucion, grupo, .data[[select_var]], .data[[grouping_var]]) %>% 
          group_by(institucion, grupo, .data[[grouping_var]]) %>% 
          count(.data[[select_var]]) %>% 
          collect()
      }
    }
    
  }
  
  #ADD INFO TO POPULATE PLOT
  add_info_to_counts <- counted_df %>%
    mutate(
      #respuesta = forcats::fct_reorder(.data[[select_var]], num_alumnos),
      "# alumnos" = as.numeric(n)
    ) %>%
    select(!c(n)) %>% 
    rename("Institución" = institucion)
  
  return(add_info_to_counts)
  
  
}
