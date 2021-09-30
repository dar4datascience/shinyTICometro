# A function to get main vars ENP from the ticometro filtering escuela y grupo
get_vars_ENP <- function(con , select_schools, select_groups, fecha_aplicacion = 2020){

  conn <- pool::poolCheckout(con)

  #https://shiny.rstudio.com/articles/pool-advanced.html

  #QUERY DATABASE 4 VARIABLE
  main_vars_ENP <- DBI::dbWithTransaction(conn = conn,
                                        DBI::dbGetQuery(conn = conn,
                                    glue::glue_sql('
                       SELECT
                       num_alumno,
                       institucion,
                       grupo,
                       genero,
                       escuela_de_procedencia,
                       compartes_tic,
                       estabilidad_internet_4_clases,
                       internet_fuera_d_casa,
                       dispositivos_electronicos,
                       edad_uso_dispositivo,
                       plataformas_edu_known,
                       total_de_plataformas_por_estudiante,
                       cinta,
                       calif_checker,
                       calif_proces_admin_infor,
                       calif_acceso_informacion,
                       calif_seguridad,
                       calif_colabor_comunic
                       FROM ticometro_resultados_main_table
                       WHERE institucion IN ({select_schools*})
                          AND grupo IN ({select_groups*})
                          AND fecha_aplicacion IN ({fecha_aplicacion*}); --FOR WHEN YOU WANT TO SELECT MULTIPLE fecha_aplicacionS
                       ',
                       .con = conn)
  )
  )

  pool::poolReturn(conn)



  #Rename columns to display in a table
  #Pretty titles
  ENP_TICometro_variables<- c("Alumno",
                              "Plantel",
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
                              "Calif. Colaboración")


  #rename columns
  colnames(main_vars_ENP) <- ENP_TICometro_variables



  return(main_vars_ENP)

}
