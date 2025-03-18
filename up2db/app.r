librarian::shelf(
  DBI, dplyr, DT, JohnCoene/firebase, here,
  # readr
  RPostgres, shiny, tidyr)

if (Sys.getenv("FIREBASE_API_KEY") == "")
  readRenviron(here(".Renviron"))
# private: [.Renviron](https://drive.google.com/file/d/1ljlmJv-RRnJesXZ2qqBKvLRLZy23yWEG/view?usp=sharing)

source(here("libs/db.R"))
# write table net_uuids from database to csv
# dbGetQuery(con, "SELECT * FROM net_uuids") |> 
#   slice(1:100) |> 
#   write_csv(here("up2db/test_net_uuids.csv"))

# define signin
# signin <- modalDialog(
#   title = "Login",
#   actionButton("google", "Google", icon = icon("google"), class = "btn-danger"),
#   # actionButton("github", "Github", icon = icon("github")),
#   footer = NULL)

tbls <- dbListTables(con) |> 
  sort()
# |> setdiff(c())
#   TODO: add tables to disallow

# UI definition
ui <- fluidPage(
  titlePanel("Database Table Update Tool"),
  
  useFirebase(), # import dependencies
  firebaseUIContainer(),
  # reqSignin(
  #   h4("Logged in!"),
  suppressWarnings(
    reqSignin(actionButton("signout", "Sign out"))),
  uiOutput("welcome"),
  
  suppressWarnings(
    reqSignin(
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Choose CSV File",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          
          selectInput("table", "Target Table Name", tbls),
          
          actionButton("validate", "Validate Data", class = "btn-primary"),
          actionButton("update", "Update Database", class = "btn-success"),
          
          hr(),
          
          h4("Connection Status:"),
          verbatimTextOutput("connection_status"),
          
          hr(),
          
          h4("Validation Results:"),
          verbatimTextOutput("validation_results")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Data Preview", DTOutput("preview")),
            tabPanel("Schema Comparison", 
                     h4("Database Schema:"),
                     verbatimTextOutput("db_schema"),
                     h4("CSV Schema:"),
                     verbatimTextOutput("csv_schema"))
          )
        )
      ) ))
)

# Server logic
server <- function(input, output, session) {
  # showModal(signin)
  
  # f <- FirebaseSocial$new()
  
  # observeEvent(input$google, {
  #   f$launch_google()
  # })
  
  # observeEvent(input$github, {
  #   f$launch_github()
  # })
  
  # observe({
  #   f$req_sign_in()
  #   removeModal()
  # })
  
  output$welcome <- renderUI({
    f$req_sign_in() # require sign in
    
    user <- f$get_signed_in() # get logged in user info
    # print(user)
    
    h4("Welcome,", user$response$displayName)
  })
  
  observeEvent(input$signout, {
    f$sign_out()
  })
  
  f <- FirebaseUI$
    # instantiate
    new("session")$           
    # set terms of service
    set_tos_url(
      "https://termly.io/resources/templates/terms-of-service-template/")$
    # set privacy policy
    set_privacy_policy_url(
      "https://www.privacypolicytemplate.net/")$ 
    # define providers
    set_providers(            
      # email = TRUE,
      google = TRUE
    )$
    # launch
    launch() 

  # s <- Storage$new() # initialise
  # a <- Analytics$new()$launch() # TOOD: add user analytics
  
  # Database connection
  db_con <- reactive({
    f$req_sign_in()
    
    emails_allowed <- tbl(con, "users") |> 
      pull(email)
    
    u <- f$get_signed_in()$response
    # names(u)
    # [1] "uid"             "email"           "emailVerified"   "displayName"    
    # [5] "isAnonymous"     "photoURL"        "providerData"    "stsTokenManager"
    # [9] "createdAt"       "lastLoginAt"     "apiKey"          "appName"     
    if (!u$email %in% emails_allowed)
      safeError(stop(glue(
        "Sorry {u$email} is not in the list of allowed users.")))
    
    return(con)
  })
  
  # Connection status output
  output$connection_status <- renderText({
    if (is.null(db_con())) {
      "❌ Database connection failed"
    } else {
      "✅ Connected to database"
    }
  })
  
  # Data reading and validation
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  # Data preview
  output$preview <- renderDT({
    req(data())
    datatable(head(data(), 100), options = list(scrollX = TRUE))
  })
  
  # Get database schema
  db_schema <- reactive({
    req(input$table)
    req(db_con())
    
    query <- sprintf("
      SELECT column_name, data_type 
      FROM INFORMATION_SCHEMA.COLUMNS 
      WHERE table_schema = 'public' 
      AND table_name = '%s'
    ", input$table)
    
    dbGetQuery(db_con(), query)
  })
  
  # Schema comparison outputs
  output$db_schema <- renderPrint({
    req(db_schema())
    print(db_schema())
  })
  
  output$csv_schema <- renderPrint({
    req(data())
    data_types <- sapply(data(), class)
    data.frame(
      column_name = names(data_types),
      data_type = unname(data_types)
    )
  })
  
  # Validation logic
  validate_data <- eventReactive(input$validate, {
    req(data(), input$table, db_schema())
    
    # Initialize validation results
    results <- list(
      status = TRUE,
      messages = character()
    )
    
    # 1. Check column names
    db_cols <- db_schema()$column_name
    csv_cols <- names(data())
    
    missing_cols <- setdiff(db_cols, csv_cols)
    extra_cols <- setdiff(csv_cols, db_cols)
    
    if (length(missing_cols) > 0) {
      results$status <- FALSE
      results$messages <- c(results$messages,
                          sprintf("Missing required columns: %s", 
                                  paste(missing_cols, collapse = ", ")))
    }
    
    if (length(extra_cols) > 0) {
      results$messages <- c(results$messages,
                          sprintf("Extra columns will be ignored: %s",
                                  paste(extra_cols, collapse = ", ")))
    }
    
    # 2. Check for required special columns
    if ("geom" %in% db_cols && !("longitude" %in% csv_cols || "latitude" %in% csv_cols)) {
      results$status <- FALSE
      results$messages <- c(results$messages,
                          "Geometry column requires longitude and latitude")
    }
    
    # 3. Basic data validation
    df <- data()
    if ("longitude" %in% names(df)) {
      invalid_lon <- sum(!between(df$longitude, -180, 180), na.rm = TRUE)
      if (invalid_lon > 0) {
        results$status <- FALSE
        results$messages <- c(results$messages,
                            sprintf("%d invalid longitude values", invalid_lon))
      }
    }
    
    if ("latitude" %in% names(df)) {
      invalid_lat <- sum(!between(df$latitude, -90, 90), na.rm = TRUE)
      if (invalid_lat > 0) {
        results$status <- FALSE
        results$messages <- c(results$messages,
                            sprintf("%d invalid latitude values", invalid_lat))
      }
    }
    
    results
  })
  
  # Validation results output
  output$validation_results <- renderText({
    req(validate_data())
    results <- validate_data()
    
    status_text <- if(results$status) "✅ Validation passed" else "❌ Validation failed"
    messages_text <- paste(results$messages, collapse = "\n")
    
    paste(status_text, messages_text, sep = "\n")
  })
  
  # Database update logic
  observeEvent(input$update, {
    req(validate_data())
    results <- validate_data()
    
    if (!results$status) {
      showModal(modalDialog(
        title = "Error",
        "Cannot update database. Please fix validation errors first.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Proceed with update
    tryCatch({
      # Begin transaction
      dbBegin(db_con())
      
      # Clear existing data
      dbExecute(db_con(), sprintf("TRUNCATE TABLE %s;", input$table))
      
      # Prepare data frame
      df <- data()
      
      # Handle special columns
      if ("geom" %in% db_schema()$column_name) {
        df$geom <- NA
      }
      
      # Write data
      dbWriteTable(db_con(), input$table, df, append = TRUE, row.names = FALSE)
      
      # Update geometry if needed
      if ("geom" %in% db_schema()$column_name) {
        dbExecute(db_con(), 
                 sprintf("UPDATE %s SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326);",
                         input$table))
      }
      
      # Commit transaction
      dbCommit(db_con())
      
      showModal(modalDialog(
        title = "Success",
        "Database updated successfully!",
        easyClose = TRUE
      ))
    }, error = function(e) {
      # Rollback on error
      dbRollback(db_con())
      
      showModal(modalDialog(
        title = "Error",
        paste("Database update failed:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Cleanup connection on session end
  session$onSessionEnded(function() {
    if (!is.null(db_con())) {
      dbDisconnect(db_con())
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)