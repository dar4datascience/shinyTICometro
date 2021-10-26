#' plot_numerical_bars ONLY 4 calificaciones del ticometro
#'
#' @description una funcion para graficar un histograma de las calificaciones del ticometro
#' @import RColorBrewer
#' @import ggplot2
#' @import dplyr
#' @import plotly
#' @return a plotly histogram with the var2plot
plot_numerical_vars <-
  function(df, var2plot, groupvar = "ninguno") {
    # No grouping variable ---------------------------------------------------
    
    
    if (groupvar == "ninguno") {
      #* Data transform ----------------------------------------------------------
      
      plot_df <- df %>%
        select(`Institución`, .data[[var2plot]]) %>%
        mutate(var2plot = round(signif(.data[[var2plot]], 2))) %>% 
        count(.data[[var2plot]]) %>%
        mutate("Num. de Alumnos" = as.numeric(n),
               `Institución` = forcats::fct_reorder(`Institución`,
                                                    desc(`Num. de Alumnos`)
               )
        ) %>%
        select(!c(n))
      
      # *Ggplot it --------------------------------------------------------------
      
      
      p <- ggplot(
        dplyr::arrange(plot_df, `Institución`),
        aes(
          x = .data[[var2plot]],
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
        plotly::layout(autosize = T,
                       margin = list(autoexpand = TRUE)) %>%
        plotly::layout(
          title = clean_plot_titles(var2plot) ,
          font = list(family = "Arial"),
          legend = list(title = list(text = '')),
          yaxis = list(title = 'Num. de Alumnos'),
          xaxis = list(title = 'Calificación')
          #,font=list(size = 30)
        )
      
      
      return(fig)
      
      
    } else{
      # Caso grouping variable --------------------------------------------------
      
      #* Data transform ----------------------------------------------------------
      plot_df <- df %>%
        select(`Institución`, .data[[groupvar]], .data[[var2plot]]) %>%
        mutate(var2plot = round(signif(.data[[var2plot]], 2))) %>% 
        group_by(`Institución`, .data[[groupvar]], .data[[var2plot]]) %>%
        count(.data[[var2plot]]) %>%
        mutate("Num. de Alumnos" = as.numeric(n),
               `Institución` = forcats::fct_reorder(`Institución`,
                                                    desc(`Num. de Alumnos`)
               )
               ) %>%
        select(!c(n))
      
      # *Ggplot it --------------------------------------------------------------
      
      p <- plot_df %>%
        ggplot(aes(
          y = `Num. de Alumnos`,
          x = .data[[var2plot]],
          fill = .data[[groupvar]],
          text = .data[[groupvar]]
        )) +
        geom_col() +
        facet_wrap(~ `Institución`) +
        theme(
          axis.text.y = element_text("Num. de Alumnos"),
          axis.text.x = element_text("Calificación")
        )
      
      
      #p <- p + scale_fill_brewer(palette = "RdYlBu", guide = "legend")
      
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
          legend = list(title = list(text = ''))
          #,font=list(size = 30)
        )
      
      
      return(fig)
      
      prnt("grafica x grupos")
    }
    
    
  }
