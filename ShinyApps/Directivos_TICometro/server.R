######################################
###### TICometro 4 Directivos##########
######################################

#REVERSE LIST: used for putting neat names to plots

# CONNECT OUTSIDE OF THE SERVER FUNCTION
db_connection <- connect2database()

print("connected 2 database")

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    #mensaje de desconexion
    sever::sever(html = sever_default(
        title = "Error: Interrupción del procesamiento",
        subtitle = "Disculpe las molestias. Si esta pantalla continua apareciendo, favor de comunicarse con el administrador del sitio.",
        button = "Actualizar",
        button_class = "info"
    ),
    bg_color = "white", color = "black")

    observeEvent(input$stop, {
        stopApp()
    })

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

    #############################################################################################
    ################### DIRECTIVOS LOGIC BEGINS   ######################################################
    #############################################################################################

    #changing inputs values captured by event reactive
    reactive_Directivos_var_selectors <- reactiveValues()

    #ESTADO INICIAL DE LA APPLICACION
    #IF the action button has not been pressed assing initial values to reactive ENP
    observe(if (input$activa_consulta == 0) {
        #ISOLATE so it takes no dependency when changed
        reactive_Directivos_var_selectors$escuelasPicked <-
            isolate(input$escuelas_directivos_picked)
        reactive_Directivos_var_selectors$plotvarPicked <-
            isolate(input$plot_directivo_var)

    })

    #observe button press 4 changing values
    observeEvent(input$activa_consulta, {
        #assign selector variables to reactivelist
        reactive_Directivos_var_selectors$escuelasPicked <-
            input$escuelas_directivos_picked
        reactive_Directivos_var_selectors$plotvarPicked <-
            input$plot_directivo_var

    })


    #reactive tabulated. everytime the reactivelist changes this changes too
    reactive_Directivos_tabulated_data <- reactive({
        countVars(
            db_connection,
            select_schools = reactive_Directivos_var_selectors$escuelasPicked,
            select_var = reactive_Directivos_var_selectors$plotvarPicked
        )
    })


    #get main vars 4 cch
    reactive_Directivos_main_data <- reactive(
        get_mainVars_4_planteles(db_connection,
                     reactive_Directivos_var_selectors$escuelasPicked)
    )

    data_directivos <- reactiveValues()

    observe({
        data_directivos$data <- reactive_Directivos_main_data()
        print(data_directivos$data$`Calificación TICómetro`)
        data_directivos$mean_calif <- round(mean(data_directivos$data$`Calificación TICómetro`, na.rm=TRUE), 2)

    })





    #VALUE BOX FOR # OF ALUMNOS SELECCIONADOS
    num_alumnos_selected_CCH <-
        reactive(prettyNum(
            nrow(reactive_Directivos_main_data()),
            big.mark = ","
        ))


    output$value_box_Directivos <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = num_alumnos_selected_CCH(),
            width = 4,
            subtitle = "Alumnos seleccionados",
            color = "success",
            icon = icon("user-friends")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })

    output$average_box_Directivos <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = data_directivos$mean_calif,
            width = 4,
            subtitle = "Calificación promedio",
            color = "success",
            icon = icon("user-graduate")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })

    output$TabulatedVars_Directivos <- DT::renderDataTable(
        reactive_Directivos_tabulated_data(),
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

    ) # end of renderDatatable



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

    output$Directivos_plot <- plotly::renderPlotly({
        #THIS FUNCTION ONLY TAKES DEPENDENCY ON reactive_Directivos_tabulated_data
        #everything else is isolated
        #ONLY PLOT HISTOGRAMS ON calificaciones variables
        if (grepl("calif",
                  isolate(reactive_Directivos_var_selectors$plotvarPicked),
                  ignore.case = FALSE)) {
            plot_numerical_vars(
                reactive_Directivos_tabulated_data(),
                isolate(reactive_Directivos_var_selectors$plotvarPicked)
            )

        } else{

            plot_categorical_vars(
                reactive_Directivos_tabulated_data(),
                isolate(reactive_Directivos_var_selectors$plotvarPicked)
            )
        }
    })
    #############################################################################################
    ##################### CCH LOGIC ENDS  ########################################################
    #############################################################################################



}
