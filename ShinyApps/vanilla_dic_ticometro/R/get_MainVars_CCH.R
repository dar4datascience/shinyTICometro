# A function to get main vars CCH from the ticometro filtering escuela
get_vars_CCH <- function(con , select_schools, fecha_aplicacion = 2020){

  conn <- pool::poolCheckout(con)

  #QUERY DATABASE 4 VARIABLE
  main_vars_CCH <- DBI::dbWithTransaction(conn = conn,
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
                       total_de_dispositivos_por_estudiante, -- variable exclusiva del CCH
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
                        AND fecha_aplicacion IN ({fecha_aplicacion*}); --FOR WHEN YOU WANT TO SELECT MULTIPLE fecha_aplicacionS
                       ',
                       .con = conn)
                                          )
  )

  pool::poolReturn(conn)



  #Rename columns to display in a table
  #Pretty titles
  CCH_TICometro_variables<- c("Alumno",
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
                              "Calif. Colaboración")


  #rename columns
  colnames(main_vars_CCH) <- CCH_TICometro_variables



  return(main_vars_CCH)

}
