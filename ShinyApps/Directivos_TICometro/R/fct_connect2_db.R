connect2database <- function() {
  #First Function automatically connects using local credentials by creating environment variables
  #https://shiny.rstudio.com/articles/pool-advanced.html

# Declare a pool connection -----------------------------------------------

#This is a pointer to the db. The other functions call the tables explicitly  
  ticometro_db_connection <- pool::dbPool(
    drv = odbc::odbc(),
    Driver = 'PostgreSQL ANSI',
    Server = "132.248.10.243",
    Database = "resultados",
    UID = Sys.getenv("R_resultadosTIC_db_user"),
    #environment variable for user
    PWD = Sys.getenv("R_resultadosTIC_db_user_psswrd"),
    encoding = "UTF-8",
    minSize = 2,
    idleTimeout = 30000
  )

# Ensure connection is of right locale ------------------------------------

  
  DBI::dbGetQuery(ticometro_db_connection, 
                  "SET client_encoding = 'UTF8';")
 

# Return db connection object ---------------------------------------------

 
  return(ticometro_db_connection)
  
  
  
}

