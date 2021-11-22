#' countVars
#'
#' @description A fct function
#' @import dplyr
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' FOR NOW GROUPING VAR CAN NEVER BE THE MULTIPLE CHOICE QUESTIONS
count_vars <-
  function(db_connection,
           select_schools,
           select_groups,
           select_var,
           grouping_var = NULL,
           fecha_de_aplicacion = "2021") {


    # Connect depending on year -----------------------------------------------



    if (fecha_de_aplicacion == "2020") {

      # DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html

      ticometro_table <- dplyr::tbl(db_connection,
                                    "ticometro_resultados_main_table")
    } else if (fecha_de_aplicacion == "2021") {


      #* connect 2 specific table ------------------------------------------------

      # DECLARE CONNECTION: https://shiny.rstudio.com/articles/pool-dplyr.html

      ticometro_table <- dplyr::tbl(db_connection, "ticometro_resultados_2021")
    }
    # No grouping case --------------------------------------------------------

    # check if group is null or if selected schools are
    #mixed to signal CCH or directivo variables
    
    if (any(select_groups == "Todos")) {

      #* if multiple option question ---------------------------------------------

      if (select_var == "plataformas_edu_known") {
        # a collected query
        counted_df <-
          handle_multiple_choice_questions(
            ticometro_table,
            select_schools,
            select_groups = "Todos",
            multiple_opcion_var = select_var,
            grouping_var = grouping_var,
            fecha_de_aplicacion = fecha_de_aplicacion
          )

        print("im multiple choice no groups")
      } else {
        
        # count regularly
        #* if normal question ------------------------------------------------------


        #** No grouping 2 -----------------------------------------------------------


        if (is.null(grouping_var)) {
          counted_df <- ticometro_table %>%
            filter(institucion %in% select_schools) %>%
            select(institucion, .data[[select_var]]) %>%
            group_by(institucion) %>%
            count(.data[[select_var]]) %>%
            collect()
          print("isnull grouping_var")
        } else {
          
          # count specially

          #** Grouping 2 --------------------------------------------------------------

          counted_df <- ticometro_table %>%
            filter(
              institucion %in% select_schools
            ) %>%
            select(institucion, .data[[select_var]], .data[[grouping_var]]) %>%
            group_by(institucion, .data[[grouping_var]]) %>%
            count(.data[[select_var]]) %>%
            collect()
        }
      }
    } else {
      # Grouping case -----------------------------------------------------------
      print("esto es lo que buscas en select_groups")

      #* if multiple option quesiton ---------------------------------------------

      if (select_var == "plataformas_edu_known") {
        # a collected query
        counted_df <-
          handle_multiple_choice_questions(
            ticometro_table,
            select_schools,
            select_groups,
            multiple_opcion_var = select_var,
            grouping_var = grouping_var,
            fecha_de_aplicacion
          )

        print("im in grouped multiple choice platform edu")
        
      } else {
        # count normally

        #* If normal question ------------------------------------------------------

        #** No grouping 2 -----------------------------------------------------------


        if (is.null(grouping_var)) {
          counted_df <- ticometro_table %>%
            filter(
              institucion %in% select_schools,
              grupo %in% select_groups
            ) %>%
            select(institucion, grupo, .data[[select_var]]) %>%
            group_by(institucion, grupo) %>%
            count(.data[[select_var]]) %>%
            collect()
          
        } else {
          
          # count specially
          print("calculando para select_groups filter")
          
          # ** Grouping 2 ------------------------------------------------------------

          counted_df <- ticometro_table %>%
            filter(
              institucion %in% select_schools,
              grupo %in% select_groups
            ) %>%
            select(institucion, grupo, .data[[select_var]], .data[[grouping_var]]) %>%
            group_by(institucion, grupo, .data[[grouping_var]]) %>%
            count(.data[[select_var]]) %>%
            collect()
        }
      }
    }

    # Mutate df ---------------------------------------------------------------

    # clean_title <- clean_plot_titles(select_var)
    # ADD INFO TO POPULATE PLOT
    add_info_to_counts <- counted_df %>%
      mutate(
        "Num. alumnos" = as.numeric(n),
        institucion = forcats::fct_reorder(
          forcats::as_factor(institucion),
          `Num. alumnos`
        )
      ) %>%
      select(!c(n)) %>%
      rename("Institución" = institucion)


    # return object df --------------------------------------------------------


    return(add_info_to_counts)
  }
