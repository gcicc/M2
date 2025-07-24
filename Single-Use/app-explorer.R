library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyBS)

# Define UI
ui <- fluidPage(
  titlePanel("Clinical Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      bsCollapse(
        id = "sidebarCollapse",
        open = "Filter Data",  # Set the default open panel
        bsCollapsePanel("Filter Data", uiOutput("filter_ui")),
        bsCollapsePanel("Select Columns", uiOutput("columns_ui")),
        bsCollapsePanel("Plot Variables",
                        selectInput("xcol", "X-axis", choices = NULL),
                        selectInput("ycol", "Y-axis", choices = NULL),
                        selectInput("color", "Color", choices = NULL),
                        selectInput("facet1", "Facet 1", choices = NULL),
                        selectInput("facet2", "Facet 2", choices = NULL),
                        selectInput("scales", "Scales", choices = c("fixed", "free", "free_x", "free_y"))
        )
      ),
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Plots", plotOutput("plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file and convert columns
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    # Convert character columns that should be numeric
    numeric_cols <- c("First5Percent_Mean", "BVA", "Tertile1_Max", "Tertile2_Mean", "Tertile2_Max", "Tertile3_Mean", "Tertile3_Max", "All_Mean", "All_Max", "IELCOUNT", "MO", "GISS", "Agg_Histology", "VCIEL", "Disease_Burden", "tTG_IgA", "DGP_IgA", "DGP_IgG", "DQ2", "DQ8", "GIP")
    df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(as.character(x)))
    
    return(df)
  })
  
  # Update select inputs for plot options based on the uploaded data
  observe({
    req(data())
    df <- data()
    updateSelectInput(session, "xcol", choices = names(df))
    updateSelectInput(session, "ycol", choices = names(df))
    updateSelectInput(session, "color", choices = c("None", names(df)))
    updateSelectInput(session, "facet1", choices = c("None", names(df)))
    updateSelectInput(session, "facet2", choices = c("None", names(df)))
  })
  
  # Dynamically generate UI for filtering based on the data
  output$filter_ui <- renderUI({
    req(data())
    df <- data()
    exclude_cols <- c("SUBJID", "DOB", "ENRDATE", "RANDDATE", "OBSDATE.VCE", "CapsuleID", "OBSDATE.histology", "OBSDATE.symptoms", "earliest", "latest")
    filter_ui <- lapply(setdiff(names(df), exclude_cols), function(col) {
      if (is.factor(df[[col]]) || is.character(df[[col]])) {
        selectInput(col, paste("Filter by", col), choices = unique(df[[col]]), selected = unique(df[[col]]), multiple = TRUE)
      }
    })
    do.call(tagList, filter_ui)
  })
  
  # UI for selecting columns to display
  output$columns_ui <- renderUI({
    req(data())
    df <- data()
    default_columns <- c("STUDYID", "SUBJID", "ARM", "SEX", "DOB", "RACE", "ETHNICITY",  "SEROSTATUS", "HISTINJURY", "PPIHIST2ANT", "OBSDATE.VCE", "VISIT", "READER", "First5Percent_Mean", "BVA", "Tertile1_Max", "Tertile2_Mean", "Tertile2_Max", "Tertile3_Mean", "Tertile3_Max", "All_Mean", "All_Max", "Retest.Flag", "VHCD", "IELCOUNT", "MO", "GISS", "Agg_Histology", "VCIEL", "Disease_Burden", "tTG_IgA", "DGP_IgA", "DGP_IgG", "DQ2", "DQ8", "GIP")
    checkboxGroupInput("columns", "Select Columns to Display", choices = names(df), selected = intersect(names(df), default_columns))
  })
  
  # Filtered data based on user input
  filtered_data <- reactive({
    df <- data()
    for (col in names(df)) {
      if (!is.null(input[[col]])) {
        df <- df %>% filter(get(col) %in% input[[col]])
      }
    }
    df
  })
  
  is_numeric_columns <- function(df) {
    sapply(df, is.numeric)
  }
  # Render data table with two decimal places for numeric columns
  output$data_table <- renderDataTable({
    req(input$columns)
    datatable(
      filtered_data()[, input$columns, drop = FALSE],
      options = list(columnDefs = list(list(
        targets = which(sapply(filtered_data(), is.numeric)), 
        render = JS("function(data, type, row, meta) { 
        if (type === 'display' && data != null) {
          return parseFloat(data).toFixed(2); 
        } else {
          return data; 
        }
      }")
      )))
    ) %>% formatRound(c(13:20, 22:35), 2)
  })
  # Render summary statistics
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Render plots with user-selected options
  output$plot <- renderPlot({
    req(input$xcol, input$ycol)
    df <- filtered_data()
    
    p <- ggplot(df, aes_string(x = input$xcol, y = input$ycol)) +
      geom_point(aes_string(color = ifelse(input$color == "None", NULL, input$color))) +
      theme_minimal()
    
    # Add facets based on user input
    if (input$facet1 != "None" && input$facet2 != "None") {
      p <- p + facet_grid(as.formula(paste(input$facet1, "~", input$facet2)), scales = input$scales)
    } else if (input$facet1 != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet1)), scales = input$scales)
    }
    
    print(p)
  })
  
  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, names(data()), selected = unique(data()[[names(data())]]))
  })
}

# Run the app
shinyApp(ui = ui, server = server)