

plot_numerical_vars <- function(df, var2plot, groupvar = "ninguno"){

  if(groupvar == "ninguno"){

    #SIMPLE histogram PLOT
    fig <- df %>%
      plotly::plot_ly(x = ~.data[[var2plot]],
                text = ~institucion,
                color = ~institucion,
                type = "histogram",
                hovertemplate = paste('<b>Respuesta</b>: <b>%{x}</b>',
                                      '<br><b># Alumnos</b>: <b>%{y}</b>',
                                      '<br><b>Institución</b>: <b>%{text}</b>',
                                      '<extra></extra>') #to remove trace0
                ) %>%
      #for readability
      plotly::layout(barmode = "stack",
                     bargap=0.4) %>%
      plotly::layout(autosize = T, margin=list(autoexpand = TRUE)) %>%
      plotly::layout(title = var2plot,
                     font=list(size = 30)) %>%
      plotly::layout(xaxis = list(title = ''),
                     yaxis = list(title = '# de Alumnos'))
    #%>% 2 change font sizes
     # plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
      #               yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22))) %>%
      #plotly::layout(hoverlabel = list(font=list(size=25)))

    fig <- plotly::config(fig, displaylogo = FALSE,
                          #see more on https://plotly.com/r/configuration-options/
                          modeBarButtonsToRemove = c(
                                                     "toggleSpikelines",
                                                     "hoverCompareCartesian"))

    return(fig)

  }else{

      #GROUPED histogram PLOT
    fig <- df %>%
      plotly::plot_ly(x = ~.data[[var2plot]],
                      text = ~.data[[groupvar]],
                      color = ~.data[[groupvar]],
                      type = "histogram",
                      hovertemplate = paste('<b>Respuesta</b>: <b>%{x}</b>',
                                            '<br><b># Alumnos</b>: <b>%{x}</b>',
                                            '<br><b>Grupo</b>: <b>%{text}</b>',
                                            '<extra></extra>') #to remove trace0
                      ) %>%
                        #for readability
                        plotly::layout(barmode = "stack",
                                       bargap=0.4) %>%
      plotly::layout(autosize = T, margin=list(autoexpand = TRUE)) %>%                        plotly::layout(title = var2plot,
                                       font=list(size = 30)) %>%
                        plotly::layout(xaxis = list(title = ''),
                                       yaxis = list(title = '# de Alumnos'))
      #%>% 2 change font sizes
      # plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
      #               yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22))) %>%
      #plotly::layout(hoverlabel = list(font=list(size=25)))
      # plotly::layout(hoverlabel = list(font=list(size=25)))

    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c(
                                                     "toggleSpikelines"))


    return(fig)
  }


}





