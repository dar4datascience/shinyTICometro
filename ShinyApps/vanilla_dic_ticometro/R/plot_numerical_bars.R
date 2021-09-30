

plot_numerical_vars <- function(df, var2plot, groupvar = "ninguno"){

  if(groupvar == "ninguno"){

    #SIMPLE histogram PLOT
    fig <- df %>%
      plotly::plot_ly(x = ~.data[[var2plot]],
                color = ~institucion,
                text = ~institucion,
                type = "histogram",
                hovertemplate = paste('<b>Respuesta</b>: <b>%{x}</b>',
                                      '<br><b># Alumnos</b>: <b>%{y}</b>',
                                      '<br><b>Institucion</b>: <b>%{text}</b>',
                                      '<extra></extra>') #to remove trace0
                ) %>%
      #for readability
      plotly::layout(bargap=0.1) %>%
      plotly::layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
      plotly::layout(title = var2plot,
                     font=list(size = 30)) %>%
      plotly::layout(xaxis = list(title = ''),
                     yaxis = list(title = '# de Alumnos')) %>%
      plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
                     yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22))) %>%
      plotly::layout(hoverlabel = list(font=list(size=25)))

    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c("zoom2d",
                                                     "toggleSpikelines",
                                                     "hoverClosestCartesian",
                                                     "hoverCompareCartesian",
                                                     "hoverClosestGl2d","pan2d",
                                                     "lasso2d", "zoomIn2d", "zoomOut2d",
                                                     "autoScale2d"))

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
                                       bargap=0.1) %>%
                        plotly::layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
                        plotly::layout(title = var2plot,
                                       font=list(size = 30)) %>%
                        plotly::layout(xaxis = list(title = ''),
                                       yaxis = list(title = '# de Alumnos')) %>%
                        plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
                                       yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22))) %>%
      plotly::layout(hoverlabel = list(font=list(size=25)))

    fig <- plotly::config(fig, displaylogo = FALSE,
                          modeBarButtonsToRemove = c("zoom2d",
                                                     "toggleSpikelines",
                                                     "hoverClosestCartesian",
                                                     "hoverCompareCartesian",
                                                     "hoverClosestGl2d","pan2d",
                                                     "lasso2d", "zoomIn2d", "zoomOut2d",
                                                     "autoScale2d"))


    return(fig)
  }


}





