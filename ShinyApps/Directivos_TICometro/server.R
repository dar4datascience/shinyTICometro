##### TICometro 4 Directivos Server 2021#####

grupos_y_escuelas <- grupos_y_escuelas_2021()


# Lista Escuelas ENP
ENP_escuelas <- get_escuelas_ticometro(instituto = "ENP")

# ESCUELAS CCH

# ESCUELAS CCH
CCH_escuelas <- get_escuelas_ticometro(instituto = "CCH")


# CONNECT OUTSIDE OF THE SERVER FUNCTION ----------------------------------

db_connection <- connect2database()

print("connected 2 database")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Mensaje de Desconexion --------------------------------------------------
  
  
  sever::sever(
    html = sever_default(
      title = "Error: Interrupción del procesamiento",
      subtitle = "Disculpe las molestias. Si esta pantalla continua apareciendo, favor de comunicarse con el administrador del sitio.",
      button = "Actualizar",
      button_class = "info"
    ),
    bg_color = "white",
    color = "black"
  )

  
  # Alert max number of grupos ----------------------------------------------
  
  
  
  observeEvent(input$grupo_select,{
    
    if (length(input$grupo_select) == 5) {
    shinyalert(
      title = "Máximo número de grupos seleccionados",
      text = "Sólo puedes escoger hasta 5 grupos",
      size = "m", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Comprendo",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    }
    
    
    
  })
  
  
  ################### DIRECTIVOS LOGIC BEGINS   ######################################################
  
  #* Init reactive Values ----------------------------------------------------
  
  
  # changing inputs values captured by event reactive
  reactive_directivos_selectors <- reactiveValues()
  
  #* Consider initial state --------------------------------------------------
  
  # ESTADO INICIAL DE LA APPLICACION
  # IF the action button has not been pressed assing initial values to reactive ENP
  observe(if (input$activa_consulta == 0) {
    # ISOLATE so it takes no dependency when changed
    reactive_directivos_selectors$escuelasPicked <-
      isolate(input$escuelas_directivos_picked)
    reactive_directivos_selectors$plotvarPicked <-
      isolate(input$plot_directivo_var)
    reactive_directivos_selectors$gruposPicked <-
      isolate(input$grupo_select)
  })
  
  #* Observe Main Event push consulta button ---------------------------------
  
  
  #** Event cascade1: inputs to reactiveValues --------------------------------
  observeEvent(input$activa_consulta,{
    if (length(input$escuelas_directivos_picked) == 0 |
        length(input$grupo_select) == 0) {
      shinyalert(
        title = "Selecciona al menos 1 plantel y un 1 grupo",
        text = "La consulta no se va a activar hasta cumplir los requisitos mínimos",
        size = "m", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Comprendo",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  # observe button press 4 changing values
  observeEvent(input$activa_consulta, {
    
    validate(
      need(input$grupo_select,
           'Selecciona al menos 1 grupo!'),
      need(input$escuelas_directivos_picked,
           'Selecciona al menos un plantel!')
    )
    
    # assign selector variables to reactivelist
    reactive_directivos_selectors$escuelasPicked <-
      input$escuelas_directivos_picked
    reactive_directivos_selectors$plotvarPicked <-
      input$plot_directivo_var
    reactive_directivos_selectors$gruposPicked <-
      input$grupo_select
    print(reactive_directivos_selectors$gruposPicked)
  })
  

#** update grupos -----------------------------------------------------------

  reactiveGrupos <- reactiveValues()
  observeEvent(input$escuelas_directivos_picked,{
    validate(
      need(input$escuelas_directivos_picked,
           "Escoja uno o más grupos")
    )
    
    reactiveGrupos$grupos_y_escuelas <- grupos_y_escuelas %>% 
      filter(institucion %in% input$escuelas_directivos_picked)
    print("escuelas are picked")
    print(input$escuelas_directivos_picked)
    print("selections are filtered")
 
    reactiveGrupos$grupos_enp <- grupos_y_escuelas %>% 
      filter(institucion %in% input$escuelas_directivos_picked,
             stringr::str_detect(institucion, "ENP")) %>% 
      distinct(grupo) %>% 
      pull(grupo) %>% 
      as.character()
    
    print("grupo enp")
    print(reactiveGrupos$grupos_enp)
    
    reactiveGrupos$grupos_cch <- grupos_y_escuelas %>% 
      filter(institucion %in% input$escuelas_directivos_picked,
             stringr::str_detect(institucion, "CCH")) %>% 
      distinct(grupo) %>% 
      pull(grupo) %>% 
      as.character()
      
   
    print("grupos cch") 
    print(reactiveGrupos$grupos_cch)
    
    updateSelectizeInput(session,
                         'grupo_select',
                         label = "Grupo:",
                         choices = list("Todos",
                                        c(reactiveGrupos$grupos_enp,
                                        reactiveGrupos$grupos_cch)
                                        ),
                         selected = "Todos",
                         options = list(placeholder = 'Escoja uno o más grupos',
                                        maxItems = 5),
                         server = TRUE)  
    
  })
  
  

#** Select and Deselect All  -------------------------------------------------

  observeEvent(input$selectall, {
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0) {
        # Escuelas
        
        updateActionButton(session,
                           inputId = "selectall",
                           label = "Limpiar",
                           icon = NULL)
        
        updateSelectizeInput(session,
          inputId = "escuelas_directivos_picked",
          label = "Escuela y/o plantel:",
          choices = list(
            "ENP" = c(ENP_escuelas$escuela_name),
            "CCH" = c(CCH_escuelas$escuela_name)
          ),
          selected = c(ENP_escuelas$escuela_name,
                       CCH_escuelas$escuela_name),
          options = list(
            placeholder = 'Escoja uno o más planteles',
            plugins = list("remove_button")
          )
        )
        
        
        #grupos
        
        updateSelectizeInput(session,
                             'grupo_select',
                             label = "Grupo:",
                             choices = list("Todos",
                                            c(reactiveGrupos$grupos_enp,
                                              reactiveGrupos$grupos_cch)
                             ),
                             selected = c("Todos"),
                             options = list(placeholder = 'Escoja uno o más grupos',
                                            maxItems = 5,
                                            plugins = list("remove_button")),
                             server = TRUE) 
        
      } else {
        
        updateActionButton(session,
                           inputId = "selectall",
                           label = "Seleccionar a todos los planteles",
                           icon = NULL)
        
        #escuelas
        updateSelectizeInput(session,
                             inputId = "escuelas_directivos_picked",
                             label = "Escuela y/o plantel:",
                             choices = list(
                               "ENP" = c(ENP_escuelas$escuela_name),
                               "CCH" = c(CCH_escuelas$escuela_name)
                             ),
                             selected = c(""),
                             options = list(
                               placeholder = 'Escoja uno o más planteles',
                               plugins = list("remove_button")
                             )
        )
        #grupos
        updateSelectizeInput(session,
                             'grupo_select',
                             label = "Grupo:",
                             choices = list("Todos",
                                            c(reactiveGrupos$grupos_enp,
                                              reactiveGrupos$grupos_cch)
                             ),
                             selected = "",
                             options = list(placeholder = 'Escoja uno o más grupos',
                                            maxItems = 5,
                                            plugins=list("remove_button")),
                             server = TRUE) 
        
      }}
  })
  
  #** Event cascade2: count ---------------------------------------------------
  
  
  # reactive tabulated. everytime the reactivelist changes this changes too
  reactive_directivos_tabulated_data <- reactive({
    count_vars(
      db_connection = db_connection,
      select_schools = reactive_directivos_selectors$escuelasPicked,
      select_groups = reactive_directivos_selectors$gruposPicked,
      select_var = reactive_directivos_selectors$plotvarPicked,
      fecha_de_aplicacion = "2021"
    )
  })
  
  
  #** Event cascade3: pull hoja de datos --------------------------------------
  
  
  # get main vars 4 cch
  reactive_directivos_main_data <- reactive(
    get_mainvars_4_planteles(
      db_connection = db_connection,
      select_schools = reactive_directivos_selectors$escuelasPicked,
      select_groups = reactive_directivos_selectors$gruposPicked,
      fecha_de_aplicacion = "2021"
    )
  )
  
  #* Init reactive values of computations ------------------------------------
  
  
  data_directivos <- reactiveValues()
  tabulated_directivos <- reactiveValues()
  
  #* Reactive computations to reactiveValues ---------------------------------
  
  # This allows for easier manipulations of inner data
  observeEvent(reactive_directivos_main_data(),{
    req(reactive_directivos_main_data())
    
    data_directivos$data <- reactive_directivos_main_data()
    data_directivos$mean_calif <-
      round(mean(data_directivos$data$`Calificación TICómetro`,
                 na.rm = TRUE),
            2)
    tabulated_directivos$data <-
      reactive_directivos_tabulated_data()
    
    data_directivos$mode_cinta <-
      data_directivos$data$`Color de cinta obtenida` %>%
      forcats::as_factor(.) %>%
      forcats::fct_count(.) %>%
      arrange(desc(n)) %>%
      slice_head(n = 1)
    
    # print("deberia ser un solo valor")
    print(data_directivos$mode_cinta)
    
    
    data_directivos$cinta_a_mostrar <- case_when(
      data_directivos$mode_cinta[1] == "Cinta blanca" ~ "cinta_blanca.png",
      data_directivos$mode_cinta[1] == "Cinta anaranjada" ~ "cinta_naranja.png",
      data_directivos$mode_cinta[1] == "Cinta azul" ~ "cinta_azul.png",
      data_directivos$mode_cinta[1] == "Cinta negra" ~ "cinta_negra.png"
    )
    # print(reactive_directivos_tabulated_data())
    
    # colnames(tabulated_directivos$data)[2] <- clean_plot_titles(reactive_directivos_selectors$plotvarPicked)
  })
  
  observeEvent(tabulated_directivos$data,{
    if (any(isolate(reactive_directivos_selectors$gruposPicked) == "Todos")) {
      colnames(tabulated_directivos$data)[2] <-
        clean_plot_titles(reactive_directivos_selectors$plotvarPicked)
    } else {
      colnames(tabulated_directivos$data)[c(2, 3)] <- c("Grupo",
                                                        clean_plot_titles(reactive_directivos_selectors$plotvarPicked))
    }
  })
  
  
  #** Event cascade4: Download Handler csv ------------------------------------
  
  
  output$download_tabulado <- downloadHandler(
    filename = function() {
      paste("datos-tabulados-ticometro-miSeleccion-",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      data.table::fwrite(tabulated_directivos$data, file)
    },
    contentType = "text/csv"
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datos-ticometro-miSeleccion-",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      data.table::fwrite(data_directivos$data, file, bom = TRUE)
    },
    contentType = "text/csv"
  )
  
  
  
  #* Event cascade5: Render value boxes ------------------------------------------------------
  
  
  # VALUE BOX FOR # OF ALUMNOS SELECCIONADOS
  num_alumnos_selected_directivos <-
    reactive(prettyNum(nrow(data_directivos$data),
                       big.mark = ","))
  
  
  output$value_box_directivos <- bs4Dash::renderbs4ValueBox({
    req(num_alumnos_selected_directivos())
    
    bs4Dash::valueBox(
      value = tags$p(num_alumnos_selected_directivos(),
                     style = "font-size: 3rem;"),
      subtitle = tags$p("Alumnos",
                        style = "font-size: 2rem;"),
      color = "success",
      icon = htmltools::tagAppendAttributes(icon("user-friends"),
                                            style = "color:white;")
      #font-size: 50px;
      # href = "#" #Referencia directo a la pagina principal de la aplicacion
    )
  })
  
  output$mode_box_directivos <- bs4Dash::renderbs4ValueBox({
    req(data_directivos$cinta_a_mostrar)
    bs4Dash::valueBox(
      value = tags$img(
        id = "cinta mas comun",
        alt = paste0(data_directivos$cinta_a_mostrar),
        src = data_directivos$cinta_a_mostrar,
        width = "40%",
        style = "display: block;
  margin-left: auto;
  margin-right: auto;"
      ),
  # tags$p(data_directivos$mode_cinta[1]),
  subtitle = tags$p(
    "Cinta más común de los alumnos",
    style = "font-size: 1.5rem;
                        color: black !important;
                        margin-top: 0;
margin-bottom: 0px !important;
text-align: center;"
  ),
color = "white"
    )
  })
  
  #* Event cascade6: Render tables -------------------------------------------
  
  
  output$tabulated_vars_directivos <- reactable::renderReactable({
    req(tabulated_directivos$data)
    reactable::reactable(
      tabulated_directivos$data,
      defaultSorted = list(`Num. alumnos` = "desc"),
      defaultColDef = reactable::colDef(
        align = "center",
        style = JS(
          "function(rowInfo, colInfo, state) {
       // Highlight sorted columns
       for (var i = 0; i < state.sorted.length; i++) {
         if (state.sorted[i].id === colInfo.id) {
           return { background: 'rgba(0, 0, 0, 0.03)' }
         }
       }
     }"
        )
      ),
     filterable = TRUE,
     outlined = TRUE,
     highlight = TRUE,
     compact = TRUE,
     theme = reactable::reactableTheme(
       style = list(fontFamily = "Arial",
                    fontSize = "20px"),
       color = "#000000"
     )
    ) # end of reactable
  }) # end of render
  
  
  
  output$main_vars_directivos <- reactable::renderReactable({
    reactable::reactable(
      data_directivos$data,
      defaultColDef = colDef(
        width = 130,
        align = "center",
        style = JS(
          "function(rowInfo, colInfo, state) {
       // Highlight sorted columns
       for (var i = 0; i < state.sorted.length; i++) {
         if (state.sorted[i].id === colInfo.id) {
           return { background: 'rgba(0, 0, 0, 0.03)' }
         }
       }
     }"
        )
      ),
     filterable = TRUE,
     resizable = TRUE,
     outlined = TRUE,
     highlight = TRUE,
     compact = TRUE,
     theme = reactable::reactableTheme(
       style = list(fontFamily = "Arial",
                    fontSize = "15px"),
       color = "#000000"
     )
    ) # end of reactable
  }) # end of render
  
  
  #* Event cascade7: Render plots --------------------------------------------
  
  
  output$directivos_plot <- renderPlotly({
    # THIS FUNCTION ONLY TAKES DEPENDENCY ON reactive_directivos_tabulated_data
    # everything else is isolated
    # ONLY PLOT HISTOGRAMS ON calificaciones variables
   # my_plotly_plots <-
    req(reactive_directivos_selectors$gruposPicked)
    
      if (any(reactive_directivos_selectors$gruposPicked == "Todos")) {
        if (grepl(
          "calif",
          isolate(reactive_directivos_selectors$plotvarPicked),
          ignore.case = FALSE
        )) {
          plot_numerical_vars(
            reactive_directivos_tabulated_data(),
            isolate(reactive_directivos_selectors$plotvarPicked),
            groupvar = "Todos"
          )
        } else {
          if (isolate(input$plot_directivo_var) == "edad_uso_dispositivo") {
            filtered_edad <- reactive_directivos_tabulated_data() %>%
              mutate(edad_uso_dispositivo = as.numeric(.data[["edad_uso_dispositivo"]])) %>%
              filter(between(.data[["edad_uso_dispositivo"]], 1, 17))
            
            print("printing filtered edad")
            
            plot_categorical_vars(
              filtered_edad,
              isolate(reactive_directivos_selectors$plotvarPicked),
              groupvar = "Todos"
            )
          } else {
            plot_categorical_vars(
              reactive_directivos_tabulated_data(),
              isolate(reactive_directivos_selectors$plotvarPicked),
              groupvar = "Todos"
            )
          }
        }
      } else {
        if (grepl(
          "calif",
          isolate(reactive_directivos_selectors$plotvarPicked),
          ignore.case = FALSE
        )) {
          plot_numerical_vars(
            reactive_directivos_tabulated_data(),
            isolate(reactive_directivos_selectors$plotvarPicked),
            groupvar = "grupo"
          )
        } else {
          if (isolate(input$plot_directivo_var) == "edad_uso_dispositivo") {
            filtered_edad <- reactive_directivos_tabulated_data() %>%
              mutate(edad_uso_dispositivo = as.numeric(.data[["edad_uso_dispositivo"]])) %>%
              filter(between(.data[["edad_uso_dispositivo"]], 1, 17))
            
            print("printing filtered edad")
            
            plot_categorical_vars(
              filtered_edad,
              isolate(reactive_directivos_selectors$plotvarPicked),
              groupvar = "grupo"
            )
          } else {
            plot_categorical_vars(
              reactive_directivos_tabulated_data(),
              isolate(reactive_directivos_selectors$plotvarPicked),
              groupvar = "grupo"
            )
          }
        }
      }
    
  #  browsable(div(
   #   style = "display: flex;
    #    flex-wrap: wrap;
     #   justify-content: center",
      #div(htmltools::tagList(my_plotly_plots))
  #  ))
  }) # end of render plotly
  
  
  
  # Event Descarga Masiva ---------------------------------------------------
  
  mi_descarga_masiva <- reactiveValues()
  
  observeEvent(input$massive_download_button, {
    if (length(input$massive_data_selectors) == 0) {
      shinyalert(
        title = "Selecciona al menos 1 plantel",
        text = "La descarga no puede proseguir.",
        size = "m", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Comprendo",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  observeEvent(input$massive_data_selectors, {
    print(input$massive_data_selectors)
    # * funcion to download ---------------------------------------------------
    mi_descarga_masiva$datos <-
      descarga_masiva(
        db_connection = db_connection,
        input$massive_data_selectors,
        fecha_de_aplicacion = "2021"
      )
    
    mi_descarga_masiva$names <- paste(input$massive_data_selectors,
                                      collapse = "-")
    print(head(mi_descarga_masiva$datos))
  })
  
  output$massive_download_button <- downloadHandler(
    filename = function() {
      paste("datos-ticometro-",
            mi_descarga_masiva$names,
            "-",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      data.table::fwrite(mi_descarga_masiva$datos,
                         file,
                         bom = TRUE)
    },
    contentType = "text/csv"
  )
  
  
  #### DIRECTIVOS LOGIC ENDS  #####
}
