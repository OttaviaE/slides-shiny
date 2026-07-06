library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "dataset",
        "Choose a dataset:",
        choices = c(
          "rock" = 1,
          "pressure" = 2,
          "cars" = 3,
          "Upload data" = 4
        )
      ),
      
      conditionalPanel(
        condition = "input.dataset == '4'",
        fileInput(
          "file",
          "",
          accept = "csv"
        ),
        radioButtons(
          "sep",
          "Choose column separator",
          choices = c(";" = ";", "," = ",")
        )
      ),
      
      actionButton(
        "load",
        "Load dataset"
      ),
      
      uiOutput("x_ui"),
      uiOutput("y_ui"),
      
      actionButton(
        "analyze",
        "Update analysis"
      )
      
    ),
    
    mainPanel(
      plotOutput("graph"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session){
  
  # -----------------------
  # 1. LOAD DATASET
  # -----------------------
  
  dataInput <- eventReactive(input$load, {
    
    if (input$dataset == 1) {
      rock
    } else if (input$dataset == 2) {
      pressure
    } else if (input$dataset == 3) {
      cars
    } else {
      req(input$file)
      read.csv(input$file$datapath, sep = input$sep)
    }
    
  })
  
  # -----------------------
  # 2. VARIABLE NAMES
  # -----------------------
  
  vars <- reactive({
    names(dataInput())
  })
  
  # -----------------------
  # 3. DYNAMIC UI
  # -----------------------
  
  output$x_ui <- renderUI({
    selectInput(
      "x",
      "X variable:",
      choices = vars(),
      selected = vars()[1]
    )
  })
  
  output$y_ui <- renderUI({
    selectInput(
      "y",
      "Y variable:",
      choices = vars(),
      selected = vars()[min(2, length(vars()))]
    )
  })
  
  # -----------------------
  # 4. REACTIVE DATA FOR ANALYSIS
  # -----------------------
  
  newdata <- eventReactive(input$analyze, {
    
    df <- dataInput()
    
    df[, c(input$x, input$y), drop = FALSE]
  })
  
  # -----------------------
  # 5. OUTPUTS
  # -----------------------
  
  output$graph <- renderPlot({
    df <- newdata()
    
    validate(need(!is.null(df), "Waiting for data"))
    
    x <- df[[1]]
    y <- df[[2]]
    
    # caso boxplot: x categorica
    if (is.factor(x) || is.character(x)) {
      boxplot(y ~ x,
              xlab = "Group",
              ylab = "Value")
    } else {
      plot(x, y)
    }
  })
  
  output$summary <- renderPrint({
    
    df <- newdata()
    
    summary(df)
  })
  
}

shinyApp(ui, server)