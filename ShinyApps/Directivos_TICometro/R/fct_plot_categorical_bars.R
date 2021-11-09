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
plot_categorical_vars <-
  function(df, var2fill, groupvar = "ninguno") {

# Define own palette ------------------------------------------------------
   # my_palette <-  scale_fill_manual(values = c("#758830",
    #                         "#ac6882",
     #                        "#c27356",
      #                       "#6c86c3",
       #                      "#9b5163",
        #                     "#8395b4",
         #                    "#e66aa4",
          #                   "#55874a",
           #                  "#a58713",
            #                 "#7a7a7a"))
   

    
    # Caso no group variable --------------------------------------------------
    
    
    if (any(groupvar == "ninguno")) {
      # *Caso se elija cinta ----------------------------------------------------
      
      
      if (var2fill == "cinta") {
        # **Order levels of cinta -------------------------------------------------
        
        
        df[["cinta"]] <-
          factor(df[["cinta"]], levels = c("Cinta negra",
                                           "Cinta azul",
                                           "Cinta anaranjada",
                                           "Cinta blanca")
                 )
        #df[["cinta"]] <- forcats::fct_rev(df[["cinta"]])
        
        # **Ggplot it --------------------------------------------------------------
        
        p <- df %>% ggplot(aes(
          y = `Num. alumnos`,
          x = `Institución`,
          fill = `cinta`
        )) +
          geom_col() +
          coord_flip() +
          theme(
            axis.text.x = element_text("Num. de Alumnos"),
            axis.text.y = element_text("Institución"),
            legend.title = element_blank(),
            plot.margin = margin(0.5, 0, 0.5, 0, "cm")
          ) +
          scale_fill_manual(
            values = c(
              "Cinta blanca" = "#bcbcbc",
              "Cinta anaranjada" = "#ffa500",
              "Cinta azul" = "#0000ff",
              "Cinta negra" = "#000000"
            )
          )

        #** Plotly it ---------------------------------------------------------------
        
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
          plotly::layout(autosize = T,
                         yaxis = list(automargin = TRUE)) %>%
          plotly::layout(
            title = clean_plot_titles(var2fill),
            legend = list(title = list(text = '')),
            hoverlabel = list(bgcolor = "white")
            )
            #,font=list(size = 30))
            
            
            
            return(fig)
            
            } else{
              #* Caso no elijo cinta  --------------------------------------------------
              
  print("im in no cinta no group")
# Mutate to reorder factors -----------------------------------------------

              ordered_df <- df %>%
                mutate(
                  respuesta = forcats::fct_reorder(
                    forcats::as_factor(.data[[var2fill]]),
                                                   desc(`Num. alumnos`)
                  ),
                  `Institución` = forcats::fct_reorder(`Institución`,
                                                       desc(`Num. alumnos`)
                  )
                ) %>% 
                select(!c(.data[[var2fill]]))
                
              
              # **Ggplot it --------------------------------------------------------------
              
              p <- ordered_df %>% ggplot(aes(
                y = `Num. alumnos`,
                x = `Institución`,
                fill = respuesta
              )) +
                geom_col() +
                coord_flip() +
                theme(
                  axis.text.x = element_text("Num. de Alumnos"),
                  axis.text.y = element_text("Institución"),
                  plot.margin = margin(0.5, 0, 0.5, 0, "cm")
                ) #+ my_palette
              
              # **Plotly it -------------------------------------------------------------
              
              
              fig <-
                ggplotly(p, tooltip = c("y", "x", "fill"))
              
              
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
                plotly::layout(title = clean_plot_titles(var2fill),
                               legend = list(title = list(text = '')),
                               hoverlabel = list(bgcolor = "white")
                )
                               #,font=list(size = 30))
                               
                               
                               # **return object ---------------------------------------------------------
                               
                               return(fig)
              
                               }
      
            } else{
              # Caso grouping variable --------------------------------------------------
              
              # *Caso se elija cinta ----------------------------------------------------
              print("im in grupo case")
              if (var2fill == "cinta") {
                # **Order levels of cinta -------------------------------------------------
                
                
                df[["cinta"]] <-
                  factor(df[["cinta"]], levels = c("Cinta negra",
                                                   "Cinta azul",
                                                   "Cinta anaranjada",
                                                   "Cinta blanca"))  
                
                # **Ggplot it --------------------------------------------------------------
                
                p <- df %>% ggplot(aes(
                  y = `Num. alumnos`,
                  x = `Institución`,
                  fill = `cinta`
                )) +
                  geom_col() +
                  facet_wrap(~.data[[groupvar]]) +
                  coord_flip()  +
                  theme(axis.text.x = element_text("Num. de Alumnos"),
                        axis.text.y = element_text("Grupo"),
                        plot.margin = margin(0.5, 0, 0.5, 0, "cm")) +
                  scale_fill_manual(
                    values = c(
                      "Cinta blanca" = "#bcbcbc",
                      "Cinta anaranjada" = "#ffa500",
                      "Cinta azul" = "#0000ff",
                      "Cinta negra" = "#000000"
                    )
                  )
                #** Plotly it ---------------------------------------------------------------
                
                fig <-
                  ggplotly(p, tooltip = c("y", "x", "fill"))
                
                
                fig <- plotly::config(
                  fig,
                  displaylogo = FALSE,
                  modeBarButtonsToRemove = c(
                    "pan2d",
                    "select2d",
                    "lasso2d",
                    "hoverCompareCartesian",
                    "toggleSpikelines",
                    "toImage"
                  )
                ) %>%
                  plotly::layout(autosize = T,
                                 margin = list(autoexpand = TRUE)) %>%
                  plotly::layout(title = clean_plot_titles(var2fill),
                                 legend = list(title = list(text = '')),
                                 hoverlabel = list(bgcolor = "white")
                                 )
                #,font=list(size = 30))
                
                
                
                # **return object ---------------------------------------------------------
                
                
                return(fig)
                
              } else{
                #* Caso no elijo cinta  --------------------------------------------------

# Mutate to reorder factors -----------------------------------------------

                
                ordered_df <- df %>%
                  mutate(
                    respuesta = forcats::fct_reorder(
                      forcats::as_factor(.data[[var2fill]]),
                                                     desc(`Num. alumnos`)
                    ),
                    `Institución` = forcats::fct_reorder(`Institución`,
                                                         desc(`Num. alumnos`)
                    )
                  ) %>% 
                  select(!c(.data[[var2fill]]))
                
                # **Ggplot it --------------------------------------------------------------
                
                p <-
                  ordered_df %>% ggplot(aes(
                    y = `Num. alumnos`,
                    x = `Institución`,
                    fill = respuesta
                  )) +
                  geom_col() +
                  facet_wrap(~.data[[groupvar]]) +
                  coord_flip()  +
                  theme(axis.text.x = element_text("Num. de Alumnos"),
                        axis.text.y = element_text("Grupo"),
                        plot.margin = margin(0.5, 0, 0.5, 0, "cm")) #+my_palette
                # **Plotly it -------------------------------------------------------------
                
                fig <-
                  ggplotly(p, tooltip = c("y", "x", "fill"))
                
                
                fig <-
                  plotly::config(
                    fig,
                    displaylogo = FALSE,
                    modeBarButtonsToRemove = c(
                      "pan2d",
                      "select2d",
                      "lasso2d",
                      "hoverCompareCartesian",
                      "toggleSpikelines",
                      "toImage"
                    )
                  ) %>%
                  plotly::layout(autosize = T,
                                 margin = list(autoexpand = TRUE)) %>%
                  plotly::layout(title = clean_plot_titles(var2fill)) %>%
                  
                  #,font=list(size = 30)) %>%
                  plotly::layout(
                    xaxis = list(categoryorder = "total ascending"),
                    yaxis = list(categoryorder = "total ascending"),
                    legend = list(title = list(text = '')),
                    hoverlabel = list(bgcolor = "white")
                  )
                
                
                # **return object ---------------------------------------------------------
                
                
                return(fig)
              }
              
              
            }
    
    
    
      }
