count_multipleOpcion_vars_CCH <- function(con , select_schools, multiple_opcion_var, fecha_aplicacion = 2020) {
  ###########TO OBTAIN THE SQL COMPLICATED SQL QUERY DPLYR WAS USED TO TRANSLATE################
  # tbl_db %>%  A conection pointing to the ticometro table
  #   filter(fecha_aplicacion == 2020) %>%
  #   select(3, 32:40) %>%
  #   filter(institucion %in% select_schools) %>%
  #   tidyr::pivot_longer(!institucion,
  #                      names_to = plataformas_edu_known,
  #                      values_to = respuesta) %>%
  #   filter(respuesta == 1) %>%
  #   group_by(institucion, plataformas_edu_known) %>%
  #   count(plataformas_edu_known) %>%
  #   show_query()
  ##########################################################################################
  #
  #
  #
  #





  ################################################################################################

  if (multiple_opcion_var == "plataformas_edu_known"){

  conn <- pool::poolCheckout(con)

  #QUERY DATABASE 4 VARIABLE
  multiple_vars <- DBI::dbWithTransaction(conn = conn,
                                          DBI::dbGetQuery(
                                            conn = conn,
                                             glue::glue_sql('
SELECT "institucion", "plataformas_edu_known", COUNT(*) AS "n"
FROM (((((((((SELECT "institucion", \'moodle\' AS "plataformas_edu_known", "moodle" AS "respuesta"
              FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                    FROM "ticometro_resultados_main_table"
                    WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
              WHERE ("institucion" IN ({select_schools*})))
             UNION ALL
             (SELECT "institucion", \'edmodo\' AS "plataformas_edu_known", "edmodo" AS "respuesta"
               FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                     FROM "ticometro_resultados_main_table"
                     WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
               WHERE ("institucion" IN ({select_schools*}))))
            UNION ALL
            (SELECT "institucion", \'blackboard\' AS "plataformas_edu_known", "blackboard" AS "respuesta"
              FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                    FROM "ticometro_resultados_main_table"
                    WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
              WHERE ("institucion" IN ({select_schools*}))))
           UNION ALL
           (SELECT "institucion", \'google_classroom\' AS "plataformas_edu_known", "google_classroom" AS "respuesta"
             FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                   FROM "ticometro_resultados_main_table"
                   WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
             WHERE ("institucion" IN ({select_schools*}))))
          UNION ALL
          (SELECT "institucion", \'aula24\' AS "plataformas_edu_known", "aula24" AS "respuesta"
            FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                  FROM "ticometro_resultados_main_table"
                  WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
            WHERE ("institucion" IN ({select_schools*}))))
         UNION ALL
         (SELECT "institucion", \'use_un_aula_virtual_pero_no_recuerdo_su_nombre\' AS "plataformas_edu_known", "use_un_aula_virtual_pero_no_recuerdo_su_nombre" AS "respuesta"
           FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                 FROM "ticometro_resultados_main_table"
                 WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
           WHERE ("institucion" IN ({select_schools*}))))
        UNION ALL
        (SELECT "institucion", \'otra_plataf_educativa\' AS "plataformas_edu_known", "otra_plataf_educativa" AS "respuesta"
          FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                FROM "ticometro_resultados_main_table"
                WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
          WHERE ("institucion" IN ({select_schools*}))))
       UNION ALL
       (SELECT "institucion",\'no_las_conozco\' AS "plataformas_edu_known", "no_las_conozco" AS "respuesta"
         FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
               FROM "ticometro_resultados_main_table"
               WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
         WHERE ("institucion" IN ({select_schools*}))))
      UNION ALL
      (SELECT "institucion", \'sin_responder_plataf_educa\' AS "plataformas_edu_known", "sin_responder_plataf_educa" AS "respuesta"
        FROM (SELECT "institucion", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
              FROM "ticometro_resultados_main_table"
              WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
        WHERE ("institucion" IN ({select_schools*})))) "q02"
WHERE ("respuesta" = 1.0)
GROUP BY "institucion", "plataformas_edu_known";
 ',
                       .con = conn
                                            )
                                          ))

  pool::poolReturn(conn)

  return(multiple_vars)

  }else{
    if (multiple_opcion_var == "dispositivos_electronicos"){

      conn <- pool::poolCheckout(con)

      #QUERY DATABASE 4 VARIABLE
      multiple_vars <- DBI::dbWithTransaction(conn = conn,
                                              DBI::dbGetQuery(
                                                conn = conn,
                                                glue::glue_sql('
SELECT "institucion", "dispositivos_electronicos", COUNT(*) AS "n"
FROM ((((((((((SELECT "institucion", \'telefono_inteligente_con_recarga\' AS "dispositivos_electronicos", "telefono_inteligente_con_recarga" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*})))
UNION ALL
(SELECT "institucion", \'telefono_inteligente_con_renta_mensual\' AS "dispositivos_electronicos", "telefono_inteligente_con_renta_mensual" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'computadora_portatil_laptop_sin_internet\' AS "dispositivos_electronicos", "computadora_portatil_laptop_sin_internet" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'computadora_portatil_laptop_con_acceso_a_internet\' AS "dispositivos_electronicos", "computadora_portatil_laptop_con_acceso_a_internet" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'computadora_de_escritorio_sin_internet\' AS "dispositivos_electronicos", "computadora_de_escritorio_sin_internet" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'computadora_de_escritorio_con_acceso_a_internet\' AS "dispositivos_electronicos", "computadora_de_escritorio_con_acceso_a_internet" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'tableta_digital_sin_internet\' AS "dispositivos_electronicos", "tableta_digital_sin_internet" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'tableta_digital_con_acceso_a_internet\' AS "dispositivos_electronicos", "tableta_digital_con_acceso_a_internet" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'ninguno\' AS "dispositivos_electronicos", "ninguno" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*}))))
UNION ALL
(SELECT "institucion", \'sin_responder_dispositivo_x_estudiante\' AS "dispositivos_electronicos", "sin_responder_dispositivo_x_estudiante" AS "respuesta"
FROM (SELECT "institucion", "telefono_inteligente_con_recarga", "telefono_inteligente_con_renta_mensual", "computadora_portatil_laptop_sin_internet", "computadora_portatil_laptop_con_acceso_a_internet", "computadora_de_escritorio_sin_internet", "computadora_de_escritorio_con_acceso_a_internet", "tableta_digital_sin_internet", "tableta_digital_con_acceso_a_internet", "ninguno", "sin_responder_dispositivo_x_estudiante"
FROM "ticometro_resultados_main_table"
WHERE ("fecha_aplicacion"IN ({fecha_aplicacion*}))) "q01"
WHERE ("institucion" IN ({select_schools*})))) "q02"
WHERE ("respuesta" = 1.0)
GROUP BY "institucion", "dispositivos_electronicos";
 ',
.con = conn
                                                )
                                              ))

      pool::poolReturn(conn)

      return(multiple_vars)

    }
  }

}





