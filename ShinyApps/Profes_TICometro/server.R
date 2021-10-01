######################################
###### TICometro 4 Profesores##########
######################################
#Expected inputs
# VARIABLES 4 ENP
# escuelasENP_picked: for consulta enp profes
# grupoENP_select
# plotENP_var
# activa_consulta_ENP


# VARIABLES 4 CCH
# escuelasCCH_picked
# activa_consulta_CCH
# plotCCH_var
#
# Outputs: bar plot, DT counts, DT mainVars, value box #alumns selected, value box cinta mas comun


#CONNECT ONCE TO DATABASE
db_connection <- connect2database()


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
    
 
    
    #############################################################################################
    ################### ENP LOGIC BEGINS   ######################################################
    #############################################################################################
    
    #changing inputs values captured by event reactive
    reactive_ENP_var_selectors <- reactiveValues()
    
    #ESTADO INICIAL DE LA APPLICACION
    #IF the action button has not been pressed assing initial values to reactive ENP
    observe(if (input$activa_consulta_enp == 0) {
        #ISOLATE so it takes no dependency when changed
        reactive_ENP_var_selectors$escuelasPicked <-
            isolate(input$escuelas_enp_picked)
        reactive_ENP_var_selectors$gruposPicked <-
            isolate(input$grupo_enp_select)
        reactive_ENP_var_selectors$plotvarPicked <-
            isolate(input$plot_enp_var)
        
    })
    
    #observe button press 4 changing values
    observeEvent(input$activa_consulta_enp, {
        #assign selector variables to reactivelist
        reactive_ENP_var_selectors$escuelasPicked <-
            input$escuelas_enp_picked
        reactive_ENP_var_selectors$gruposPicked <-
            input$grupo_enp_select
        reactive_ENP_var_selectors$plotvarPicked <-
            input$plot_enp_var
        
        
    })
    
    #reactive tabulated. everytime the reactivelist changes this changes too
    reactive_ENP_tabulated_data <- reactive({
        countVars(
            db_connection,
            reactive_ENP_var_selectors$escuelasPicked,
            reactive_ENP_var_selectors$gruposPicked,
            reactive_ENP_var_selectors$plotvarPicked
        )
    })
    
    #reactive main data to show
    reactive_ENP_main_data <- reactive({
        get_mainVars_4_planteles(
            db_connection,
            reactive_ENP_var_selectors$escuelasPicked,
            reactive_ENP_var_selectors$gruposPicked
        )
    })
    
    data_enp <- reactiveValues()
    
    observe({
        data_enp$data <- reactive_ENP_main_data()
        
        data_enp$mode_cinta <- data_enp$data $`Color de cinta obtenida` %>%
            forcats::as_factor(.) %>%
            forcats::fct_count(.) %>% 
            arrange(desc(n)) %>% 
            pull(f) 
        
        print("deberia ser un solo valor")
        print(data_enp$mode_cinta[1])
        
    })
   
    #VALUE BOX FOR # OF ALUMNOS SELECCIONADOS
    num_alumnos_selected_ENP <-
        reactive(prettyNum(nrow(reactive_ENP_main_data()),
                           big.mark = ","))
    
    output$value_box_ENP <- bs4Dash::renderbs4ValueBox({
        bs4Dash::bs4ValueBox(
            value = num_alumnos_selected_ENP(),
            width = 2,
            subtitle = "Alumnos seleccionados",
            color = "success",
            icon = icon("user-friends")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })
    
    output$average_box_ENP <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = data_enp$mode_cinta[1],
            width = 4,
            subtitle = "Cinta más común",
            color = "success",
            icon = icon("user-graduate")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })
    
    output$TabulatedVars_ENP <- DT::renderDataTable(
        reactive_ENP_tabulated_data(),
        #!!!!remove redundant column used for plot
        rownames = FALSE,
        #Enable download button 1
        extensions = 'Buttons',
        options = list(
            dom = "Blfrtip",
            #controls if searchbar, buttons, and other things appear
            buttons = list(
                "copy",
                list(
                    extend = "collection"
                    ,
                    #csv breaks encoding
                    buttons = c("csv")
                    ,
                    text = "Descargue selección de datos"
                )
            ) # end of buttons customization
            
            # customize the length menu
            ,
            lengthMenu = list(
                c(10, 20, 50), # declare values
                c(10, 20, 50)), # declare titles),
            # end of lengthMenu customization
            #To center columns
            columnDefs = list(list(
                className = 'dt-center',
                targets = "_all"
            )),
            scrollY = '50vh',
            scrollX = TRUE,
            scrollCollapse = TRUE
        ) # END GLOBAL OPTIONS
    ) #END DATA TABLE
    
    
    output$MainVars_ENP <- DT::renderDataTable(
        reactive_ENP_main_data(),
        rownames = FALSE,
        #Enable download button 1
        extensions = c('Buttons'),
        options = list(
            dom = "Blfrtip",
            buttons = list(
                "copy",
                list(
                    extend = "collection",
                    #csv breaks encoding
                    buttons = c("csv"),
                    text = "Descargue selección de datos"
                )
            ),
            # end of buttons customization
            # customize the length menu
            lengthMenu = list(c(10, 20, 50), # declare values
                              c(10, 20, 50)),
            # declare titles strings # end of lengthMenu customization
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
    
    output$ENP_plot <- plotly::renderPlotly({
        #THIS FUNCTION ONLY TAKES DEPENDENCY ON reactive_ENP_tabulated_data
        #everything else is isolated
        
        #ONLY PLOT HISTOGRAMS ON calificaciones variables
        if (grepl("calif",
                  isolate(reactive_ENP_var_selectors$plotvarPicked),
                  ignore.case = FALSE)) {
            plot_numerical_vars(
                reactive_ENP_tabulated_data(),
                isolate(reactive_ENP_var_selectors$plotvarPicked),
                "grupo"
            )
            
        } else{
            plot_categorical_vars(
                reactive_ENP_tabulated_data(),
                isolate(reactive_ENP_var_selectors$plotvarPicked),
                "grupo"
            )
        }
    })
    
    #############################################################################################
    ##################### ENP LOGIC ENDS  ########################################################
    #############################################################################################
    
    #############################################################################################
    ################### CCH LOGIC BEGINS   ######################################################
    #############################################################################################
    
    #changing inputs values captured by event reactive
    reactive_CCH_var_selectors <- reactiveValues()
    
    #ESTADO INICIAL DE LA APPLICACION
    #IF the action button has not been pressed assing initial values to reactive ENP
    observe(if (input$activa_consulta_cch == 0) {
        #ISOLATE so it takes no dependency when changed
        reactive_CCH_var_selectors$escuelasPicked <-
            isolate(input$escuelas_cch_picked)
        reactive_CCH_var_selectors$plotvarPicked <-
            isolate(input$plot_cch_var)
        
    })
    
    #observe button press 4 changing values
    observeEvent(input$activa_consulta_cch, {
        #assign selector variables to reactivelist
        reactive_CCH_var_selectors$escuelasPicked <-
            input$escuelas_cch_picked
        reactive_CCH_var_selectors$plotvarPicked <-
            input$plot_cch_var
        
    })
    
    
    #reactive tabulated. everytime the reactivelist changes this changes too
    reactive_CCH_tabulated_data <- reactive({
        countVars(
            db_connection,
            reactive_CCH_var_selectors$escuelasPicked,
            select_var = reactive_CCH_var_selectors$plotvarPicked
        )
    })
    
    
    #reactive main data to show
    reactive_CCH_main_data <- reactive({
        get_mainVars_4_planteles(db_connection,
                     reactive_CCH_var_selectors$escuelasPicked)
    })
    
    data_cch <- reactiveValues()
    
    observe({
        data_cch$data <- reactive_CCH_main_data()
        data_cch$mode_cinta <- data_cch$data$`Color de cinta obtenida` %>%
            forcats::as_factor(.) %>%
            forcats::fct_count(.) %>% 
            arrange(desc(n)) %>% 
            pull(f) 
        #%>%  as.factor(.) %>%  forcats::fct_infreq(.)
        print("deberia ser un solo valor")
        print(data_cch$mode_cinta[1])
        print(class(data_cch$mode_cinta[1]))
        
    })
    
    
    #VALUE BOX FOR # OF ALUMNOS SELECCIONADOS
    num_alumnos_selected_CCH <-
        reactive(prettyNum(
            nrow(reactive_CCH_main_data()),
            big.mark = ","
        ))
    
    
    output$value_box_CCH <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = num_alumnos_selected_CCH(),
            width = 2,
            subtitle = "Alumnos seleccionados",
            color = "success",
            icon = icon("user-friends")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })
    
    output$average_box_CCH <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = data_cch$mode_cinta[1],
            width = 4,
            subtitle = "Cinta más común",
            color = "success",
            icon = icon("user-graduate")
            # href = "#" #Referencia directo a la pagina principal de la aplicacion
        )
    })
    
    output$TabulatedVars_CCH <- DT::renderDataTable(
        reactive_CCH_tabulated_data(),
        rownames = FALSE,
        #Enable download button 1
        extensions = c('Buttons'),
        options = list(
            dom = "Blfrtip",
            buttons = list(
                "copy",
                list(
                    extend = "collection",
                    #csv breaks encoding ?????
                    buttons = c("csv"),
                    text = "Descargue selección de datos"
                )
            ),
            # end of buttons customization
            #To center columns
            columnDefs = list(list(
                className = 'dt-center',
                targets = "_all"
            )),
            #scrollY = '50vh',
            scrollX = TRUE,
            scrollCollapse = TRUE
        ) # end of options
        
    ) # end of renderDatatable
    
    
    
    output$MainVars_CCH <- DT::renderDataTable(
        reactive_CCH_main_data(),
        rownames = FALSE,
        #Enable download button 1
        extensions = c('Buttons'),
        options = list(
            dom = "Blfrtip",
            buttons = list(
                "copy",
                list(
                    extend = "collection",
                    #csv breaks encoding
                    buttons = c("csv"),
                    text = "Descargue selección de datos"
                )
            ),
            # end of buttons customization
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
    
    output$CCH_plot <- plotly::renderPlotly({
        #THIS FUNCTION ONLY TAKES DEPENDENCY ON reactive_CCH_tabulated_data
        #everything else is isolated
        
        #ONLY PLOT HISTOGRAMS ON calificaciones variables
        if (grepl("calif",
                  isolate(reactive_CCH_var_selectors$plotvarPicked),
                  ignore.case = FALSE)) {
            plot_numerical_vars(
                reactive_CCH_tabulated_data(),
                isolate(reactive_CCH_var_selectors$plotvarPicked)
            )
            
        } else{
            plot_categorical_vars(
                reactive_CCH_tabulated_data(),
                isolate(reactive_CCH_var_selectors$plotvarPicked)
            )
        }
    })
    #############################################################################################
    ##################### CCH LOGIC ENDS  ########################################################
    #############################################################################################
    
    
    
}

