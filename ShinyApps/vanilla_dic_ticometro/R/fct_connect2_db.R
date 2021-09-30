connect2database <- function() {
  #First Function automatically connects using local credentials by creating environment variables
  #https://shiny.rstudio.com/articles/pool-advanced.html
  
  ticometro_db_connection <- pool::dbPool(
    drv = odbc::odbc(),
    Driver = 'PostgreSQL',
    Server = "132.248.10.243",
    Database = "resultados",
    UID = Sys.getenv("R_resultadosTIC_db_user"),
    #environment variable for user
    PWD = Sys.getenv("R_resultadosTIC_db_user_psswrd"),
    encoding = "UTF-8"
    #,    minSize = 2, 
    #idleTimeout = 30000
  )

    return(ticometro_db_connection)
  
  
}

