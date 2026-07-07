library(shiny)
library(tidyverse)

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", 
                                 "pressure",
                                 "cars")),
      actionButton("load", "Select dataset"),
    ), 
    mainPanel(plotOutput("graph"),
              downloadButton("downloadPlot", "Download plot"),
              verbatimTextOutput("summary"), 
              downloadButton("downloadSummary", "Dowload summary"))
  )
)

server = function(input, output){
  dataInput <- eventReactive(input$load, {
    switch(input$dataset,
           rock = rock,
           pressure = pressure,
           cars = cars)
  })
  output$graph <- renderPlot({
    df = dataInput()
    plot(df[, c(1:2)])
  })
  ddata <- reactive({
    df = dataInput()
    descript_data= df %>%
      summarise( m_1 = mean(df[,1]), sd_1 = sd(df[,1]),
                 m_2 = mean(df[,2]), sd_2 = sd(df[,2])
      )
    colnames(descript_data) <- gsub("1", colnames(df)[1], colnames(descript_data))
    colnames(descript_data) <- gsub("2", colnames(df)[2], colnames(descript_data))
    descript_data
  })  
  output$summary <- renderPrint({
    ddata()
  })
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste("descriptive-",input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.table(ddata(), file, sep = ",",
                  row.names = FALSE)
    }
  )
  output$downloadPlot <- downloadHandler(
    
    filename = function() {
      paste0("plot-", input$dataset, ".png")
    },
    
    content = function(file) {
      
      png(file, width = 800, height = 600)
      
      df <- dataInput()
      plot(df[, 1:2])
      
      dev.off()
    }
  )
}

shinyApp(ui, server)