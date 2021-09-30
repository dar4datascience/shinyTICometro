#' plot_numerical_bars ONLY 4 calificaciones del ticometro
#'
#' @description una funcion para graficar un histograma de las calificaciones del ticometro
#' @import RColorBrewer
#' @import ggplot2
#' @import plotly
#' @return a plotly histogram with the var2plot
#'
plot_numerical_vars <- function(df, var2plot, groupvar = "ninguno"){
  
  #caso directivos y cch
  if(groupvar == "ninguno"){
    
    #SIMPLE histogram PLOT
    fig <- df %>%
      select(institucion, .data[[var2plot]]) %>% 
      mutate(
        "clean_calif" = round(signif(.data[[var2plot]], 2))
      ) %>% 
      count(.data[["clean_calif"]]) %>% 
      mutate(
        num_alumnos = as.numeric(n)
      ) %>% 
      plotly::plot_ly(x = ~.data[["clean_calif"]],
                      y = ~.data[["# de alumnos: "]],
                      color = ~`Institución`,
                      text = ~`Institución`,
                      colors = colorRampPalette(brewer.pal(11, "RdYlBu"))(14),
                      type = "bar",
                      hovertemplate = paste('<b>Respuesta</b>: <b>%{x}</b>',
                                            '<br><b># Alumnos</b>: <b>%{y}</b>',
                                            '<br><b>Institución</b>: <b>%{text}</b>',
                                            '<extra></extra>') #to remove trace0
      ) %>%
      #for readability
      plotly::layout(barmode = "stack",
                     bargap=0.3) %>%
      plotly::layout(autosize = T, margin=list(autoexpand = TRUE)) %>%
      plotly::layout(title = var2plot
                     #,font=list(size = 30)
      ) %>%
      plotly::layout(xaxis = list(title = 'Calificación'),
                     yaxis = list(title = '# de Alumnos')) 
    #%>% 2 change font sizes
    # plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
    #               yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22))) %>%
    #plotly::layout(hoverlabel = list(font=list(size=25)))
    
    
    fig <- plotly::config(fig,
                          displaylogo = FALSE,
                          displayModeBar = TRUE,
                          modeBarButtonsToRemove = c("pan2d", "hoverClosestCartesian", "hoverCompareCartesian")
    )
    
    return(fig)
    
    #caso enp
  }else{
    
    #GROUPED histogram PLOT
    fig <- df %>%
      select(institucion, .data[[groupvar]], .data[[var2plot]]) %>% 
      mutate(
        "clean_calif" = round(signif(.data[[var2plot]], 2))
      ) %>% 
      count(.data[[var2plot]]) %>% 
      mutate(
        num_alumnos = as.numeric(n)
      ) %>% 
      plotly::plot_ly(x = ~.data[["clean_calif"]],
                      y = ~.data[["# de alumnos: "]],
                      text = ~.data[[groupvar]],
                      color = ~.data[[groupvar]],
                      colors = colorRampPalette(brewer.pal(11, "RdYlBu"))(14),
                      type = "bar",
                      hovertemplate = paste('<b>Calificación</b>: <b>%{x}</b>',
                                            '<br><b># Alumnos</b>: <b>%{y}</b>',
                                            '<br><b>Grupo</b>: <b>%{text}</b>',
                                            '<extra></extra>') #to remove trace0
      ) %>%
      #for readability
      plotly::layout(barmode = "stack",
                     bargap=0.3) %>%
      plotly::layout(autosize = T, margin=list(autoexpand = TRUE)) %>%
      plotly::layout(title = var2plot
                     #,font=list(size = 30)
      ) %>%
      plotly::layout(xaxis = list(title = 'Calificación'),
                     yaxis = list(title = '# de Alumnos')) 
    #%>% 2 change font sizes
    # plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
    #               yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22))) %>%
    #plotly::layout(hoverlabel = list(font=list(size=25)))
    
    
    fig <- plotly::config(fig,
                          displaylogo = FALSE,
                          displayModeBar = TRUE,
                          modeBarButtonsToRemove = c("pan2d",
                                                     "select2d"
                          ))
    
    
    return(fig)
  }
  
  
}







