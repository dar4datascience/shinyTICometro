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
    
    
    if (groupvar == "Todos") {
      #* Data transform ----------------------------------------------------------
      print("im in no grouping plot numerical")
      plot_df <- df %>%
        ungroup() %>%
        mutate(respuesta = round(.data[[var2plot]])) %>%
        select(!.data[[var2plot]]) %>%
        group_by(`Institución`, respuesta) %>%
        mutate(`Num. alumnos` = sum(`Num. alumnos`)) %>%
        distinct(`Institución`, respuesta, `Num. alumnos`)
      
      
      # *Ggplot it --------------------------------------------------------------
      
      p <- ggplot(
        dplyr::arrange(plot_df, `Num. alumnos`),
        aes(
          y = `Num. alumnos`,
          x = respuesta,
          fill = `Institución`,
          text = paste0(
            "Plantel: ",
            `Institución`,
            "</br></br>Calificación: ",
            signif(respuesta, 2),
            "</br>Núm. de Alumnos: ",
            `Num. alumnos`
          )
        )
      )  +
        geom_col() +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      #* Plotly it ---------------------------------------------------------------
      
      
      fig <- ggplotly(p, tooltip = "text")
      
      
      fig <- plotly::config(
        fig,
        displaylogo = FALSE
        #,modeBarButtonsToRemove = c(
        #  "pan2d",
        #  "select2d",
        #  "lasso2d",
        #  "autoScale2d",
        #  "hoverClosestCartesian",
        #  "hoverCompareCartesian",
        #  "toggleSpikelines",
        #  "toImage"
        #)
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
          yaxis = list(title = "Núm. de Alumnos"),
          xaxis = list(title = "Calificación")
          # ,font=list(size = 30)
        )
      
      
      return(fig)
    } else {
      # Caso grouping variable --------------------------------------------------
      print("im in grouping")
      
      
      
      #* Data transform ----------------------------------------------------------
      plot_df <- df %>%
        ungroup() %>%
        mutate(respuesta = round(.data[[var2plot]])) %>%
        select(!.data[[var2plot]]) %>%
        group_by(`Institución`,  .data[[groupvar]], respuesta) %>%
        mutate(`Num. alumnos` = sum(`Num. alumnos`)) %>%
        distinct(`Institución`,
                 .data[[groupvar]],
                 respuesta,
                 `Num. alumnos`)
      
      # *Ggplot it --------------------------------------------------------------
      
      p <- plot_df %>%
        ggplot(aes(
          y = `Num. alumnos`,
          x = respuesta,
          fill = .data[[groupvar]],
          text = paste0(
            "Grupo: ",
            .data[[groupvar]],
            "</br></br>Calificación: ",
            signif(respuesta, 2),
            "</br>Núm. de Alumnos: ",
            `Num. alumnos`
          )
        )) +
        geom_col() +
        facet_wrap(~`Institución`,
                   ncol = 1
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
        )
      
      
      # p <- p + scale_fill_brewer(palette = "RdYlBu", guide = "legend")
      
      #* Plotly it ---------------------------------------------------------------
      
      fig <- ggplotly(p, tooltip = c("y", "x", "fill"))
      
      
      fig <- plotly::config(
        fig,
        displaylogo = FALSE
        #,modeBarButtonsToRemove = c(
        # "pan2d",
        #"select2d",
        #"lasso2d",
        #"autoScale2d",
        #"hoverClosestCartesian",
        #"hoverCompareCartesian",
        #"toggleSpikelines",
        #"toImage"
        # )
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
          yaxis = list(title = "Núm. de Alumnos"),
          xaxis = list(title = "Calificación")
          # ,font=list(size = 30)
        )
      
      
      return(fig)
      
      print("grafica x grupos")
    }
  }
