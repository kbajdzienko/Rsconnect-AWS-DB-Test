aws_guide_aurora <- shiny::tags$span(
  "Endpoint (",
  shiny::tags$a(
    "How to find Endpoint Address - Example", 
    href = 'https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Connecting.html#Aurora.Connecting.AuroraPostgreSQL',
    target = '_blank'),
  ")"
)


#' Show Error Message
#'
#' @param msg 
#'
#' @return html
#' @export
#'
showError <- function(msg){
  shiny::showModal(
    shiny::modalDialog(
      title = 'Error',
      size = "l",
      p(msg)
    )
  )
}


#' Open con & Execute SQL Query
#'
#' @param sql 
#'
#' @return data frame
#' @export
#'
dbQuery <- function(sql, DRIVER, ENDPOINT, PORT, DBNAME, USER, PASSWORD){
  
  con <- make_con(DRIVER, ENDPOINT, PORT, DBNAME, USER, PASSWORD)
  
  df <- odbc::dbGetQuery(con, sql)
  
  odbc::dbDisconnect(con)
  
  return(df)
  
}

#' Connect 
#'
#' @return connection object
#' @export
#'
make_con <- function(DRIVER, ENDPOINT, PORT, DBNAME, USER, PASSWORD){
  
  con <- odbc::dbConnect(
    odbc::odbc(),
    driver = DRIVER,
    host = ENDPOINT,
    port = PORT,
    database = DBNAME,
    user = USER,
    password = PASSWORD,
    timeout = 5
  )
  
  return(con)
  
}
