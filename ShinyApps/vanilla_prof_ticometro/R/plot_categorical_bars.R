# df: a counted df producted by fct_countVar_CCH
# var2plot: a string input to put as the title of plot
#Takes a counted variable in a dataframe
plot_categorical_vars <- function(df, var2plot, groupvar = "ninguno"){

    if(groupvar == "ninguno"){

      #SIMPLE BAR PLOT
      fig <- df %>%
        #plotly for dynamic plotting ~ notation points from dataframe to a particular column
        plotly::plot_ly(y = ~.data[[var2plot]],
                        #volteamos los ejes para mostrar la informacion mas facil
                        x = ~num_alumnos,
                        color = ~institucion,
                        type = "bar",
                        orientation = "h",
                        text = ~institucion,
                        hovertemplate = paste('<b>Respuesta</b>: <b>%{y}</b>',
                                              '<br><b>Institución</b>: <b>%{text}</b> <br>',
                                              '<b># Alumnos</b>: <b>%{x:,}</b> ', #notation 4 , after big numbers
                                              '<extra></extra>') #to remove trace0
        ) %>%
        #for readability
        plotly::layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
        plotly::layout(title = var2plot,
                       font=list(size = 30)) %>%
        plotly::layout(xaxis = list(title = '# de Alumnos',
                                    categoryorder = "total ascending"),
                       yaxis = list(title = '',
                                    categoryorder = "total ascending")
                       ) %>%
        plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
                       yaxis = list( tickfont = list(size = 22))) %>%
        plotly::layout(hoverlabel = list(font=list(size=15)))

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

      #GROUPED BAR PLOT
      fig <- df %>%
        #plotly for dynamic plotting ~ notation points from dataframe to a particular column
        plotly::plot_ly(y = ~.data[[var2plot]],
                        #volteamos los ejes para mostrar la informacion mas facil
                        x = ~num_alumnos,
                        color = ~.data[[groupvar]],
                        type = "bar",
                        orientation = "h",
                        text = ~.data[[groupvar]],
                        hovertemplate = paste('<b>Respuesta</b>: <b>%{y}</b>',
                                              '<br><b>Grupo</b>: <b>%{text}</b> <br>',
                                              '<b># Alumnos</b>: <b>%{x:,}</b> ', #notation 4 , after big numbers
                                              '<extra></extra>') #to remove trace0
        ) %>%
        #for readability
        plotly::layout(barmode = "stack")%>%
        plotly::layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
        plotly::layout(title = 'compartes TIC', font=list(size = 30)) %>%
        plotly::layout(xaxis = list(title = '# de Alumnos',
                                    categoryorder = "total ascending"),
                       yaxis = list(title = '')) %>%
        plotly::layout(xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
                       yaxis = list( tickfont = list(size = 22))) %>%
        plotly::layout(hoverlabel = list(font=list(size=15)))

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
