library(shiny)
library(waiter)

library(DBI)
library(RPostgres)
library(odbc)
library(RJDBC)

library(shinyAce)

source("global.R")


ui <- navbarPage(
    
    waiter::useWaiter(),
    waiter::waiterShowOnLoad(spin_fading_circles()),
    
    titlePanel('AWS Aurora Connection Test'),
    tabPanel(
        "Simple",
        sidebarLayout(
            sidebarPanel(
                h3("Help"),
                p(
                    "This is a POC shiny app to test connection to AWS Aurora, PostgreSQL."
                ),
                p(
                    "Fill the connection arguments and click Connect DB to test the connection.
                On successfull connection, the app will return a table with current connection information on the right panel."
                ),
                h3("Connection Arguments"),
                textInput(
                    inputId = 'endpoint',
                    label = aws_guide_aurora
                ),
                textInput(
                    inputId = 'port',
                    label = 'Port',
                    value = 5432
                ),
                textInput(
                    inputId = 'dbname',
                    label = 'Database Name',
                    value = 'postgres'
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
                actionButton(
                    inputId = 'db.connect',
                    label = "Connect",
                    icon = icon('database'),
                    class = 'btn-success btn-block'
                )
            ),
            mainPanel(dataTableOutput('table'))
        ), #end sidebarLayout
    ),
    tabPanel(
        'Advanced',
        fluidRow(
            h2("Source Code"),
            aceEditor("code", mode = "r", height = "200px", value = ""),
            actionButton("eval", "Evaluate"),
            verbatimTextOutput("output"),
            verbatimTextOutput('errors')
        )
    )
    
)

server <- function(input, output, session) {
    
    rv <- reactiveVal()
    
    waiter::waiter_hide()
    
    observeEvent(input$db.connect, {
        ENDPOINT <- req(input$endpoint)
        PORT <- req(input$port)
        DBNAME <- req(input$dbname)
        SCHEMA <- req(input$schema)
        USER <- req(input$user)
        PASSWORD <- req(input$password)
        
        query <- paste0(
            "select current_catalog, current_schema, current_role, current_timestamp"
        )
        
        waiter_show(
            html = tagList(
                waiter::spin_6(),
                paste("Connecting to", ENDPOINT)
            )
        )
        
        df <- tryCatch(
            dbQuery(query, ENDPOINT, PORT, DBNAME, USER, PASSWORD),
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
    
    output$output <- renderPrint({
        input$eval
        eval(parse(text = isolate(input$code)))
    })
    
    output$errors <- renderPrint({
        input$eval
        tryCatch(expr = eval(parse(text = isolate(input$code))),
                 error = function(e) paste(e, collapse = '\n'))
    })
    
}

shinyApp(ui, server)