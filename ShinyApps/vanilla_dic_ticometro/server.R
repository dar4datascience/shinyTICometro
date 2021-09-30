######################################
###### TICometro 4 Directivos##########
######################################


#libraries needed
library(shinyWidgets)
library(shinycssloaders)
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(plotly)))



myToastOptions <- list(
    positionClass = "toast-top-center",
    progressBar = FALSE,
    timeOut = 3000,
    closeButton = TRUE,

    # same as defaults
    newestOnTop = TRUE,
    preventDuplicates = FALSE,
    showDuration = 9000,
    hideDuration = 1000,
    extendedTimeOut = 1000,
    showEasing = "linear",
    hideEasing = "linear",
    showMethod = "fadeIn",
    hideMethod = "fadeOut"
)


#REVERSE LIST: used for putting neat names to plots

reverse_TICometro_variables <- list(
    "num_alumno" = "Alumno",
    "institucion" = "Plantel",
    "grupo" = "Grupo",
    "genero" = "Género",
    "escuela_de_procedencia" = "Escuela de Procedencia",
    "compartes_tic" = "Uso compartido de laptop o computadora",
    "estabilidad_internet_4_clases" = "Estabilidad de la red en casa",
    "internet_fuera_d_casa" = "Conexión a Internet fuera de casa",
    "dispositivos_electronicos" = "Dispositivo TIC de acceso",
    "total_de_dispositivos_por_estudiante" = "# de Dispositivos TIC",
    "edad_uso_dispositivo" = "Edad de primer uso de TIC",
    "plataformas_edu_known" = "Plataformas Educativas que conoce el estudiante",
    "total_de_plataformas_por_estudiante" = "# de Plataformas Educativas que conoce el estudiante",
    "cinta" = "Color de cinta obtenida",
    "calif_checker" = "Calificación TICómetro",
    "calif_proces_admin_infor" = "Calif. Procesamiento",
    "calif_acceso_informacion" = "Calif. Acceso",
    "calif_seguridad" = "Calif. Seguridad",
    "calif_colabor_comunic" = "Calif. Colaboración"
)

disconnected <- tagList(
    h1("Whoah there..."),
    p("Something went terribly wrong!"),
    sever::reload_button("REFRESH", class = "warning")
)

img_url <-
    "https://i.pinimg.com/280x280_RS/45/d4/01/45d40177e1ce8015e9e4dd2a2115ed36.jpg"

# CONNECT OUTSIDE OF THE SERVER FUNCTION
db_connection <- connect2database()

print("connected 2 database")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    sever::sever(html = disconnected,
                 bg_image  = img_url,
                 color = "black")

    observeEvent(input$stop, {
        stopApp()
    })

    observeEvent(input$activa_consulta, {

        if (all(input$plot_cch_var) == "dispositivos_electronicos") {
            showToast(
                "warning",
                "En las ENP sólo seleccionarion una opción.",
                .options = myToastOptions
            )
        }
    })

    observeEvent(input$plot_enp_var, {

        if (all(input$plot_enp_var) == "dispositivos_electronicos") {
            shinyalert(
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
    ################### CCH LOGIC BEGINS   ######################################################
    #############################################################################################

    #changing inputs values captured by event reactive
    reactive_CCH_var_selectors <- reactiveValues()

    #ESTADO INICIAL DE LA APPLICACION
    #IF the action button has not been pressed assing initial values to reactive ENP
    observe(if (input$activa_consulta == 0) {
        #ISOLATE so it takes no dependency when changed
        reactive_CCH_var_selectors$escuelasPicked <-
            isolate(input$escuelas_cch_picked)
        reactive_CCH_var_selectors$plotvarPicked <-
            isolate(input$plot_cch_var)

    })

    #observe button press 4 changing values
    observeEvent(input$activa_consulta, {
        #assign selector variables to reactivelist
        reactive_CCH_var_selectors$escuelasPicked <-
            input$escuelas_cch_picked
        reactive_CCH_var_selectors$plotvarPicked <-
            input$plot_cch_var

    })


    #reactive tabulated. everytime the reactivelist changes this changes too
    reactive_CCH_tabulated_data <- reactive({
        count_variable_CCH(
            db_connection,
            reactive_CCH_var_selectors$escuelasPicked,
            reactive_CCH_var_selectors$plotvarPicked
        )
    }) %>% bindCache(input$escuelas_cch_picked, input$plot_cch_var)


    #get main vars 4 cch
    reactive_CCH_main_data <- reactive({
        get_vars_CCH(db_connection,
                     reactive_CCH_var_selectors$escuelasPicked)
    }) %>% bindCache(input$escuelas_cch_picked)

    #VALUE BOX FOR # OF ALUMNOS SELECCIONADOS
    num_alumnos_selected_CCH <-
        reactive(prettyNum(
            nrow(reactive_CCH_main_data()),
            big.mark = ","
        ))


    output$value_box_CCH <- bs4Dash::renderbs4ValueBox({
        bs4Dash::valueBox(
            value = num_alumnos_selected_CCH(),
            width = 4,
            subtitle = "Alumnos seleccionados",
            color = "success",
            icon = icon("user-friends")
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
    )   # end of renderDatatable

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
