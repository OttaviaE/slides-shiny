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
        fileInput("file",  "",
                  accept = c("csv")),
        radioButtons("sep", 
                     "Choose column separator", 
                     c(";" = ";", "," = ","))
        
      ),
      
      actionButton("load", "Load dataset"),
      
      selectInput(
        "x",
        "X variable:",
        choices = NULL
      ),
      
      selectInput(
        "y",
        "Y variable:",
        choices = NULL
      ),
      
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
  dataInput <- eventReactive(input$load, {
    if(input$dataset == 1){
      rock
    } else if(input$dataset == 2){
      pressure
    } else if (input$dataset == 3){
      cars
    } else {
      read.csv(input$file$datapath, sep = input$sep)
    }
    
  })
  
  
  observeEvent(dataInput(),{
    
    vars <- names(dataInput())
    
    updateSelectInput(
      session,
      "x",
      choices = vars,
      selected = vars[1]
    )
    
    updateSelectInput(
      session,
      "y",
      choices = vars,
      selected = vars[min(2,length(vars))]
    )
    
  })
  
  analysisInput <- eventReactive(input$analyze,{
    
    req(dataInput())
    req(input$x,input$y)
    
    list(
      data = dataInput(),
      x = input$x,
      y = input$y
    )
    
  })
  
  ## Plot
  
  output$graph <- renderPlot({
    
    req(analysisInput())
    
    obj <- analysisInput()
    
    plot(
      obj$data[[obj$x]],
      obj$data[[obj$y]],
      xlab = obj$x,
      ylab = obj$y,
      pch = 19
    )
    
  })
  
  ## Summary
  
  output$summary <- renderPrint({
    
    req(analysisInput())
    
    obj <- analysisInput()
    
    summary(
      obj$data[,c(obj$x,obj$y)]
    )
    
  })
  
}

shinyApp(ui, server)