#' plot_categorical_vars 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @export
#' @import plotly
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @noRd

# df: a counted df producted by fct_countVar_CCH
# var2plot: a string input to put as the title of plot
#Takes a counted variable in a dataframe
plot_categorical_vars <- function(df, var2fill, groupvar = "ninguno"){
  
  
  
  if(groupvar == "ninguno"){
    
    p <- df %>% ggplot(aes(y = `# alumnos`,
                           x = `Institución`,
                           fill = .data[[var2fill]],
                           text = .data[[var2fill]])
    ) +
      geom_col() +
      coord_flip() +
      theme(axis.text.x= element_text("# de Alumnos"), 
            axis.text.y= element_text("Institución")
      )
    
    fig <- ggplotly(p, tooltip = c("y","x", "fill"))
    
    
    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c("pan2d",
                                                     "select2d",
                                                     "lasso2d",
                                                     "resetScale2d",
                                                     "hoverCompareCartesian",
                                                     "toggleSpikelines",
                                                     "toImage")) %>%
      plotly::layout(autosize = T, margin=list(autoexpand = TRUE)) %>%      
      plotly::layout(title = clean_plot_titles(var2fill)
                     #,font=list(size = 30)
      ) %>%
      plotly::layout(xaxis = list(
        categoryorder = "total ascending"),
        yaxis = list(
          categoryorder = "total ascending")
      ) 
    
    
    
    return(fig)
    
  }else{
    
    #ggplot(caso_muchos_enp_count, aes(x=grupo, y=as.numeric(num_alumnos), label = institucion, fill=plataformas_edu_known)) + geom_col() + coord_flip() + facet_wrap(~institucion)
    
    #GROUPED BAR PLOT
    p <- df %>% ggplot(aes(y = `# alumnos`,
                           x = .data[[groupvar]],
                           fill = .data[[var2fill]],
                           text = .data[[var2fill]])
    ) +
      geom_col() +
      facet_wrap(~`Institución`) +
      coord_flip()  +
      theme(axis.text.x= element_text("# de Alumnos"), 
            axis.text.y= element_text("Grupo")
      )
    
    
    fig <- ggplotly(p, tooltip = c("y","x", "fill"))
    
    
    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c("pan2d",
                                                     "select2d",
                                                     "lasso2d",
                                                     "resetScale2d",
                                                     "hoverCompareCartesian",
                                                     "toggleSpikelines",
                                                     "toImage")) %>%
      plotly::layout(autosize = T, margin=list(autoexpand = TRUE)) %>%      
      plotly::layout(title = clean_plot_titles(var2fill)
                     #,font=list(size = 30)
      ) %>%
      plotly::layout(xaxis = list(
        categoryorder = "total ascending"),
        yaxis = list(
          categoryorder = "total ascending")
      ) 
    
    
    
    return(fig)
    
  }
  
  
  
}
