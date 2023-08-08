library(shiny)
library(waiter)

library(DBI)
library(odbc)


source("global.R")


ui <- navbarPage(
    title = 'AWS Datasource Connection Test',
                     
    tags$head(
        waiter::useWaiter(),
        waiter::waiterShowOnLoad(spin_fading_circles())
        ),
                 
                 
    
    tabPanel(
        "Home",
        sidebarLayout(
            sidebarPanel(
                h3("Help"),
                p(
                    "This is a POC shiny app to test connection to ODBC datasources on AWS.",
                    "It should help you validate if the RSCONNECT server is able to connect with data provider in your AWS account."
                ),
                p(
                    "Source code is available at:",a(href = "https://github.com/kbajdzienko/Rsconnect-AWS-DB-Test/")
                ),
                p(
                    "Note that DEV accounts cannot connect to PROD accounts."
                ),
                p(
                    "Fill the connection arguments and click Connect DB to test the connection.
                On successfull connection, the app will return query result on the right panel."
                ),
                p(
                    tags$b("Important Note:"),
                    "If the connection takes too long to establish it would indicate that the database port is blocked between Posit Connect account and your AWS account. That requries FireWall change IT ticket.",
                    "Alternatively check the VPC security group Inbound rules inside your account".
                ),
                h3("Connection Arguments"),
                selectInput(
                    inputId = 'driver',
                    label = 'Data Source Type',
                    choices = unique(odbc::odbcListDrivers()[,"name"])
                ),
                textInput(
                    inputId = 'endpoint',
                    label = aws_guide_aurora,
                    value = ""
                ),
                textInput(
                    inputId = 'port',
                    label = 'Port',
                    value = ''
                ),
                textInput(
                    inputId = 'dbname',
                    label = 'Database Name',
                    value = 'dev'
                ),
                textInput(
                    inputId = 'schema',
                    label = 'Schema Name',
                    value = 'public'
                ),
                textInput(
                    inputId = 'user', 
                    label = "User"
                ),
                passwordInput(
                    inputId = 'password', 
                    label = "Password or Token"
                ),
                textInput(
                    inputId = 'query',
                    label = "Query",
                    value = "select 1"
                ),
                actionButton(
                    inputId = 'db.connect',
                    label = "Connect",
                    icon = icon('database'),
                    class = 'btn-success btn-block'
                )
            ),
            mainPanel(dataTableOutput('table'))
        ) #end sidebarLayout
    )
)

server <- function(input, output, session) {
    
    rv <- reactiveVal()
    
    waiter::waiter_hide()
    
    observeEvent(input$db.connect, {
        DRIVER <- req(input$driver)
        ENDPOINT <- req(input$endpoint)
        PORT <- req(input$port)
        DBNAME <- req(input$dbname)
        SCHEMA <- req(input$schema)
        USER <- req(input$user)
        PASSWORD <- req(input$password)
        
        query <- req(input$query)
        
        waiter_show(
            html = tagList(
                waiter::spin_6(),
                paste("Connecting to", ENDPOINT)
            )
        )
        
        df <- tryCatch(
            dbQuery(query, DRIVER, ENDPOINT, PORT, DBNAME, USER, PASSWORD),
            error = function(e) {
                paste(e, collapse = '\n')
            }
        )
        
        waiter_hide()
        
        
        
        if ('error' %in% class(df)) {
            showError(df$message)
        } else if('character' %in% class(df)) {
            showError(df)
        } else if ('data.frame' %in% class(df)) {
            rv(df)
        }
        
        
    })
    
    
    output$table <- renderDataTable(
        req(rv()),
        options = list(
            filter = 'none',
            dom = 't'
        )
    )
    
    
    
}

shinyApp(ui, server)
