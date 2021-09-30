# A function for counting per group the different categories in the TICometro
# @params con es la conexion de la tabla TICometro_db
# @params group_var es la variable por agrupar
# Descripcion: esta funcion es para el CCH que no tiene la variable grupo solo escuela CCH para profesores
library(dplyr)

count_variable_CCH <- function(con , select_schools, select_var, fecha_aplicacion = 2020){


  if (select_var == "plataformas_edu_known" | select_var == "dispositivos_electronicos"){

    counted_var <- count_multipleOpcion_vars_CCH(con, select_schools, select_var)

  }else{
  conn <- pool::poolCheckout(con)

  #QUERY DATABASE 4 VARIABLE
  counted_var <- DBI::dbWithTransaction(conn = conn,
                      DBI::dbGetQuery(conn = conn,
                                 glue::glue_sql('
                       SELECT institucion,
                       {`select_var`},
                       COUNT(*) AS n
                       FROM ticometro_resultados_main_table
                       WHERE institucion IN ({select_schools*})
                          AND fecha_aplicacion IN ({fecha_aplicacion*}) --FOR WHEN YOU WANT TO SELECT MULTIPLE fecha_aplicacionS
                       GROUP BY institucion,
                       {`select_var`};
                       ',
                       .con = conn)
  )
  )


  pool::poolReturn(conn)
  }
  #ADD INFO TO POPULATE PLOT
  add_info_to_counts <- counted_var %>%
    mutate(
      #respuesta = forcats::fct_reorder(.data[[select_var]], num_alumnos),
      select_var = stringr::str_replace_all(.data[[select_var]], "_", " "),
      num_alumnos = n,
      value_text = paste0("# de alumnos: ", n)
    ) %>%
    select(!c(n))

  return(add_info_to_counts)



}
