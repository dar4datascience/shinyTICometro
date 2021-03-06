#' plot_numerical_bars ONLY 4 calificaciones del ticometro
#'
#' @description una funcion para graficar un histograma de las calificaciones del ticometro
#' @import RColorBrewer
#' @import ggplot2
#' @import dplyr
#' @import plotly
#' @return a plotly histogram with the var2plot
plot_numerical_vars <- function(df, var2plot, groupvar = "ninguno"){
  
  #caso directivos y cch
  if(groupvar == "ninguno"){
    
    #SIMPLE histogram PLOT
    plot_df <- df %>%
      select(`Institución`, .data[[var2plot]]) %>%
      mutate(
        "clean_calif" = round(signif(.data[[var2plot]], 2))
      ) %>%
      group_by(`Institución`, .data[["clean_calif"]]) %>%
      count(.data[["clean_calif"]]) %>%
      mutate(
        "# alumnos" = as.numeric(n)
      ) %>% 
    select(!c(n)) 
    
    p <- plot_df%>% 
     ggplot(aes(y = `# alumnos`,
                     x = `clean_calif`,
                     fill = `Institución`,
                     text = `Institución`)
      ) +
      geom_col() +
      theme(axis.text.y= element_text("# de Alumnos"), 
            axis.text.x= element_text("Calificación")
      ) 
    
    #p <- p + scale_fill_brewer(palette = "RdYlBu", guide = "legend")
    
    fig <- ggplotly(p, tooltip = c("y","x", "fill"))
    
    
    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c("pan2d",
                                                     "select2d",
                                                     "lasso2d",
                                                     "hoverClosestCartesian",
                                                     "hoverCompareCartesian",
                                                     "toggleSpikelines",
                                                     "toImage")) %>%
      plotly::layout(autosize = T,
                     bargap=0.3,
                     margin=list(autoexpand = TRUE)) %>%      
      plotly::layout(title = clean_plot_titles(var2plot)
                     #,font=list(size = 30)
      )
    
   
    return(fig)
    
    #caso enp
  }else{
    
    #GROUPED histogram PLOT
    plot_df <- df %>%
      select(`Institución`, .data[[groupvar]], .data[[var2plot]]) %>%
      mutate(
        "clean_calif" = round(signif(.data[[var2plot]], 2))
      ) %>%
      group_by(`Institución`, .data[[groupvar]], .data[["clean_calif"]]) %>%
      count(.data[["clean_calif"]]) %>%
      mutate(
        "# alumnos" = as.numeric(n)
      ) %>% 
      select(!c(n))
    
    
   p <- plot_df %>%
      ggplot(aes(y = `# alumnos`,
                 x = `clean_calif`,
                 fill = .data[[groupvar]],
                 text = .data[[groupvar]])
      ) +
      geom_col() +
      facet_wrap(~`Institución`) +
      theme(axis.text.y= element_text("# de Alumnos"), 
            axis.text.x= element_text("Calificación")
      ) 
   
   
   #p <- p + scale_fill_brewer(palette = "RdYlBu", guide = "legend")
    
    fig <- ggplotly(p, tooltip = c("y","x", "fill"))
    
    
    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c("pan2d",
                                                     "select2d",
                                                     "lasso2d",
                                                     "hoverClosestCartesian",
                                                     "hoverCompareCartesian",
                                                     "toggleSpikelines",
                                                     "toImage")) %>%
      plotly::layout(autosize = T,
                     bargap=0.3,
                     margin=list(autoexpand = TRUE)) %>%      
      plotly::layout(title = clean_plot_titles(var2plot)
                     #,font=list(size = 30)
      )
    
      
    return(fig)
    
    prnt("grafica x grupos")
  }
  
  
}





