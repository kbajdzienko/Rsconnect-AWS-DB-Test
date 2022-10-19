aws_guide_aurora <- shiny::tags$span(
  "Endpoint (",
  shiny::tags$a(
    "How to find Endpoint Address", 
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
dbQuery <- function(sql, ENDPOINT, PORT, DBNAME, USER, PASSWORD){
  
  con <- make_con(ENDPOINT, PORT, DBNAME, USER, PASSWORD)
  
  df <- DBI::dbGetQuery(con, sql)
  
  DBI::dbDisconnect(con)
  
  return(df)
  
}

#' Connect to AWS Aurora
#'
#' @return connection object
#' @export
#'
make_con <- function(ENDPOINT, PORT, DBNAME, USER, PASSWORD){
  
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = ENDPOINT,
    port = PORT,
    dbname = DBNAME,
    user = USER,
    password = PASSWORD
  )
  
  shiny::showNotification(paste("DB Connection Success for",USER))
  
  return(con)
  
}