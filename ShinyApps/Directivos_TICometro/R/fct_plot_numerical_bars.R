#' plot_numerical_bars ONLY 4 calificaciones del ticometro
#'
#' @description una funcion para graficar un histograma de las calificaciones del ticometro
#' @import RColorBrewer
#' @import ggplot2
#' @import dplyr
#' @import plotly
#' @return a plotly histogram with the var2plot
plot_numerical_vars <-
  function(df, var2plot, groupvar) {
    # No grouping variable ---------------------------------------------------


    if (groupvar == "Ninguno") {
      #* Data transform ----------------------------------------------------------
      print("im in no grouping plot numerical")
      plot_df <- df %>%
        ungroup() %>%
        select(`Institución`, .data[[var2plot]]) %>%
        mutate(respuesta = round(signif(.data[[var2plot]], 2))) %>%
        select(!.data[[var2plot]]) %>%
        group_by(`Institución`, respuesta)

      # *Ggplot it --------------------------------------------------------------


      p <- ggplot(
        dplyr::arrange(plot_df, `Institución`),
        aes(
          x = respuesta,
          fill = `Institución`,
          text = paste0(
            "Institución: ",
            fill,
            "</br></br>Calificación: ",
            signif(x, 2),
            "</br>Num. de Alumnos: ",
            ..count..
          )
        )
      ) +
        geom_histogram(
          bins = 5,
          lwd = 0.5,
          color = "white",
          position = position_stack(reverse = TRUE)
        ) +
        theme(
          axis.text.y = element_text("Num. de Alumnos"),
          axis.text.x = element_text("Calificación"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )

      #* Plotly it ---------------------------------------------------------------


      fig <- ggplotly(p, tooltip = "text")


      fig <- plotly::config(
        fig,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "pan2d",
          "select2d",
          "lasso2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "toggleSpikelines",
          "toImage"
        )
      ) %>%
        plotly::layout(
          autosize = T,
          margin = list(autoexpand = TRUE)
        ) %>%
        plotly::layout(
          title = clean_plot_titles(var2plot),
          font = list(family = "Arial"),
          legend = list(title = list(text = "")),
          hoverlabel = list(bgcolor = "white"),
          xaxis = list(title = "Num. de Alumnos"),
          yaxis = list(title = "Calificación")
          # ,font=list(size = 30)
        )


      return(fig)
    } else {
      # Caso grouping variable --------------------------------------------------
      print("im in grouping")



      #* Data transform ----------------------------------------------------------
      plot_df <- df %>%
        ungroup() %>%
        select(`Institución`, .data[[groupvar]], .data[[var2plot]]) %>%
        mutate(respuesta = round(signif(.data[[var2plot]], 2))) %>%
        group_by(`Institución`, .data[[groupvar]], respuesta) %>%
        count(respuesta) %>%
        mutate(
          "Num. de Alumnos" = as.numeric(n),
          `Institución` = forcats::fct_reorder(
            `Institución`,
            desc(`Num. de Alumnos`)
          )
        ) %>%
        select(!c(n))

      # *Ggplot it --------------------------------------------------------------

      p <- plot_df %>%
        ggplot(aes(
          y = `Num. de Alumnos`,
          x = respuesta,
          fill = .data[[groupvar]],
          text = .data[[groupvar]]
        )) +
        geom_col() +
        facet_wrap(~`Institución`,
          ncol = 1
        ) +
        theme(
          axis.text.x = element_text("Num. de Alumnos"),
          axis.text.y = element_text("Calificación"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )


      # p <- p + scale_fill_brewer(palette = "RdYlBu", guide = "legend")

      #* Plotly it ---------------------------------------------------------------

      fig <- ggplotly(p, tooltip = c("y", "x", "fill"))


      fig <- plotly::config(
        fig,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "pan2d",
          "select2d",
          "lasso2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "toggleSpikelines",
          "toImage"
        )
      ) %>%
        plotly::layout(
          autosize = T,
          bargap = 0.3,
          margin = list(autoexpand = TRUE)
        ) %>%
        plotly::layout(
          title = clean_plot_titles(var2plot),
          font = list(family = "Arial"),
          legend = list(title = list(text = "")),
          hoverlabel = list(bgcolor = "white"),
          xaxis = list(title = "Num. de Alumnos"),
          yaxis = list(title = "Calificación")
          # ,font=list(size = 30)
        )


      return(fig)

      prnt("grafica x grupos")
    }
  }
