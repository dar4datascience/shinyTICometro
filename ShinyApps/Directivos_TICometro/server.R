##### TICometro 4 Directivos Server 2021#####




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
    
    observeEvent(input$stop, {
        stopApp()
    })
    
    # Alert diferent measurement ----------------------------------------------
    
    
    observeEvent(input$plot_directivo_var, {
        if (input$plot_directivo_var == "dispositivos_electronicos") {
            shinyalert::shinyalert(
                title = "Observación metodológica",
                text = "En las ENPs sólo seleccionaron una opción.",
                size = "l",
                closeOnEsc = TRUE,
                closeOnClickOutside = TRUE,
                html = FALSE,
                type = "warning",
                showConfirmButton = TRUE,
                showCancelButton = FALSE,
                confirmButtonText = "Comprendo",
                confirmButtonCol = "#3020E0",
                timer = 0,
                imageUrl = "",
                animation = TRUE
            )
        }
    })
    
    ################### DIRECTIVOS LOGIC BEGINS   ######################################################
    
    #* Init reactive Values ----------------------------------------------------
    
    
    #changing inputs values captured by event reactive
    reactive_Directivos_var_selectors <- reactiveValues()
    
    #* Consider initial state --------------------------------------------------
    
    #ESTADO INICIAL DE LA APPLICACION
    #IF the action button has not been pressed assing initial values to reactive ENP
    observe(if (input$activa_consulta == 0) {
        #ISOLATE so it takes no dependency when changed
        reactive_Directivos_var_selectors$escuelasPicked <-
            isolate(input$escuelas_directivos_picked)
        reactive_Directivos_var_selectors$plotvarPicked <-
            isolate(input$plot_directivo_var)
        
    })
    
    #* Observe Main Event push consulta button ---------------------------------
    
    
    #** Event cascade1: inputs to reactiveValues --------------------------------
    
    
    #observe button press 4 changing values
    observeEvent(input$activa_consulta, {
        #assign selector variables to reactivelist
        reactive_Directivos_var_selectors$escuelasPicked <-
            input$escuelas_directivos_picked
        reactive_Directivos_var_selectors$plotvarPicked <-
            input$plot_directivo_var
        
    })
    
    
    #** Event cascade2: count ---------------------------------------------------
    
    
    #reactive tabulated. everytime the reactivelist changes this changes too
    reactive_Directivos_tabulated_data <- reactive({
        countVars(
            db_connection = db_connection,
            select_schools = reactive_Directivos_var_selectors$escuelasPicked,
            select_var = reactive_Directivos_var_selectors$plotvarPicked,
            fecha_de_aplicacion = "2021"
        )
    })
    
    
    #** Event cascade3: pull hoja de datos --------------------------------------
    
    
    #get main vars 4 cch
    reactive_Directivos_main_data <- reactive(
        get_mainVars_4_planteles(
            db_connection = db_connection,
            select_schools = reactive_Directivos_var_selectors$escuelasPicked,
            fecha_de_aplicacion = "2021"
        )
    )
    
    #* Init reactive values of computations ------------------------------------
    
    
    data_directivos <- reactiveValues()
    tabulated_directivos <- reactiveValues()
    
    #* Reactive computations to reactiveValues ---------------------------------
    
    #This allows for easier manipulations of inner data
    observe({
        data_directivos$data <- reactive_Directivos_main_data()
        data_directivos$mean_calif <-
            round(mean(
                data_directivos$data$`Calificación TICómetro`,
                na.rm = TRUE
            ),
            2)
        tabulated_directivos$data <-
            reactive_Directivos_tabulated_data()
        
    })
    
    
    #** Event cascade4: Download Handler csv ------------------------------------
    
    
    # Descarga handler csvs
    
    output$downloadtabulado <- downloadHandler(
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
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("datos-ticometro-miSeleccion-",
                  Sys.Date(),
                  ".csv",
                  sep = "")
        },
        content = function(file) {
            data.table::fwrite(data_directivos$data, file)
        },
        contentType = "text/csv"
    )
    
    
    
    #* Event cascade5: Render value boxes ------------------------------------------------------
    
    
    #VALUE BOX FOR # OF ALUMNOS SELECCIONADOS
    num_alumnos_selected_directivos <-
        reactive(prettyNum(nrow(data_directivos$data),
                           big.mark = ","))
    
    
    output$value_box_Directivos <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = tags$p(num_alumnos_selected_directivos(),
                           style = "font-size: 2.5rem;"),
            subtitle = tags$p("Alumnos",
                              style = "font-size: 1.5rem;"),
            color = "success",
            icon = htmltools::tagAppendAttributes(icon("user-friends"),
                                                  style = "color:white;")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })
    
    output$average_box_Directivos <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = tags$p(data_directivos$mean_calif,
                           style = "font-size: 2.5rem;"),
            subtitle = tags$p("Calificación promedio",
                              style = "font-size: 1.5rem;"),
            color = "success",
            icon = htmltools::tagAppendAttributes(icon("user-graduate"),
                                                  style = "color:white;")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })
    
    #* Event cascade6: Render tables -------------------------------------------
    
    
    output$TabulatedVars_Directivos <- reactable::renderReactable({
        reactable::reactable(
            tabulated_directivos$data,
            defaultSorted = list(`# alumnos` = "desc"),
            defaultColDef = reactable::colDef(align = "center"),
            filterable = TRUE,
            outlined = TRUE,
            highlight = TRUE,
            compact = TRUE,
            theme = reactable::reactableTheme(
                style = list(fontFamily = "MyriadProBold",
                             fontSize = "20px"),
                color = "#000000"
            )
        )#end of reactable
    }) # end of render
    
    
    
    output$MainVars_Directivos <- DT::renderDataTable(
        reactive_Directivos_main_data(),
        rownames = FALSE,
        #Enable download button 1
        options = list(
            dom = "lfrtip",
            # customize the length menu
            lengthMenu = list(c(10, 20, 50), # declare values
                              c(10, 20, 50)),
            # end of lengthMenu customization
            #To center columns
            columnDefs = list(list(
                className = 'dt-center',
                targets = "_all"
            )),
            scrollY = '50vh',
            scrollX = TRUE,
            scrollCollapse = TRUE
        ) # end of options
    )   # end of renderDatatable
    
    #* Event cascade7: Render plots --------------------------------------------
    
    
    output$Directivos_plot <- plotly::renderPlotly({
        #THIS FUNCTION ONLY TAKES DEPENDENCY ON reactive_Directivos_tabulated_data
        #everything else is isolated
        #ONLY PLOT HISTOGRAMS ON calificaciones variables
        if (grepl(
            "calif",
            isolate(reactive_Directivos_var_selectors$plotvarPicked),
            ignore.case = FALSE
        )) {
            plot_numerical_vars(
                tabulated_directivos$data,
                isolate(
                    reactive_Directivos_var_selectors$plotvarPicked
                )
            )
            
            
        } else{
            if (isolate(input$plot_directivo_var) == "edad_uso_dispositivo") {
                filtered_edad <- tabulated_directivos$data %>%
                    filter(between(edad_uso_dispositivo, 1, 17))
                
                print('printing filtered edad')
                print(filtered_edad)
                
                plot_categorical_vars(
                    filtered_edad,
                    isolate(
                        reactive_Directivos_var_selectors$plotvarPicked
                    )
                )
                
            } else{
                plot_categorical_vars(
                    tabulated_directivos$data,
                    isolate(
                        reactive_Directivos_var_selectors$plotvarPicked
                    )
                )
            }
        }
    })
    
    
    #### DIRECTIVOS LOGIC ENDS  #####
    
    
    
}
