count_multipleOpcion_vars_ENP <- function(con, select_schools, select_groups, multiple_opcion_var = "plataformas_edu_known", fecha_aplicacion = 2020) {
  ###########TO OBTAIN THE SQL COMPLICATED SQL QUERY DPLYR WAS USED TO TRANSLATE################
  #tbl_db %>%
  #  filter(fecha_aplicacion == 2020) %>%
  #  select(3,4, 32:40) %>%
  #  filter(institucion %in% c("ENP 1"), grupo %in% c("401")) %>%
  #  tidyr::pivot_longer(!c(institucion, grupo),
  #                      names_to = "plataformas_edu_known",
  #                      values_to = "respuesta") %>%
  #  filter(respuesta == 1) %>%
  #  group_by(institucion, grupo, plataformas_edu_known) %>%
  #  count(plataformas_edu_known) %>% collect()
  #
  #
  #
  #
  #
  ################################################################################################
  conn <- pool::poolCheckout(con)

    #QUERY DATABASE 4 VARIABLE
    multiple_vars <- DBI::dbWithTransaction(conn = conn,
                                            DBI::dbGetQuery(
                                              conn = conn,
                                              glue::glue_sql('

SELECT "institucion", "grupo", "plataformas_edu_known", COUNT(*) AS "n"
FROM (((((((((SELECT "institucion", "grupo", \'moodle\' AS "plataformas_edu_known", "moodle" AS "respuesta"
              FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                    FROM "ticometro_resultados_main_table"
                    WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
              WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*}))))
             UNION ALL
             (SELECT "institucion", "grupo", \'edmodo\' AS "plataformas_edu_known", "edmodo" AS "respuesta"
               FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                     FROM "ticometro_resultados_main_table"
                     WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
               WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
            UNION ALL
            (SELECT "institucion", "grupo", \'blackboard\' AS "plataformas_edu_known", "blackboard" AS "respuesta"
              FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                    FROM "ticometro_resultados_main_table"
                    WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
              WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
           UNION ALL
           (SELECT "institucion", "grupo", \'google_classroom\' AS "plataformas_edu_known", "google_classroom" AS "respuesta"
             FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                   FROM "ticometro_resultados_main_table"
                   WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
             WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
          UNION ALL
          (SELECT "institucion", "grupo", \'aula24\' AS "plataformas_edu_known", "aula24" AS "respuesta"
            FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                  FROM "ticometro_resultados_main_table"
                  WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
            WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
         UNION ALL
         (SELECT "institucion", "grupo", \'use_un_aula_virtual_pero_no_recuerdo_su_nombre\' AS "plataformas_edu_known", "use_un_aula_virtual_pero_no_recuerdo_su_nombre" AS "respuesta"
           FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                 FROM "ticometro_resultados_main_table"
                 WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
           WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
        UNION ALL
        (SELECT "institucion", "grupo", \'otra_plataf_educativa\' AS "plataformas_edu_known", "otra_plataf_educativa" AS "respuesta"
          FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
                FROM "ticometro_resultados_main_table"
                WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
          WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
       UNION ALL
       (SELECT "institucion", "grupo", \'no_las_conozco\' AS "plataformas_edu_known", "no_las_conozco" AS "respuesta"
         FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
               FROM "ticometro_resultados_main_table"
               WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
         WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*})))))
      UNION ALL
      (SELECT "institucion", "grupo", \'sin_responder_plataf_educa\' AS "plataformas_edu_known", "sin_responder_plataf_educa" AS "respuesta"
        FROM (SELECT "institucion", "grupo", "moodle", "edmodo", "blackboard", "google_classroom", "aula24", "use_un_aula_virtual_pero_no_recuerdo_su_nombre", "otra_plataf_educativa", "no_las_conozco", "sin_responder_plataf_educa"
              FROM "ticometro_resultados_main_table"
              WHERE ("fecha_aplicacion" IN ({fecha_aplicacion*}))) "q01"
        WHERE (("institucion" IN ({select_schools*})) AND ("grupo" IN ({select_groups*}))))) "q02"
WHERE ("respuesta" = 1.0)
GROUP BY "institucion", "grupo", "plataformas_edu_known"
;
 ',
.con = conn
                                              )
                                            ))

    pool::poolReturn(conn)

    return(multiple_vars)

}
