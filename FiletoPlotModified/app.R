library(shiny)
library(shinyjs)

ui = fluidPage(
  useShinyjs(),
  titlePanel("Plot1 or Plot2?"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("my_choices", "Plot1 or Plot2",choices = c("Plot1", "Plot2"), selected = "Plot1"),width=2),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

server = function(input, output,session) {
  
  # hide plots on start
  hide("plot1");hide("plot2")
  
  output$plot1 <- renderPlot({plot(iris)})
  output$plot2 <- renderPlot({plot(mtcars)})
  
  observeEvent(input$my_choices,{
    
    if(is.null(input$my_choices)){
      hide("plot1"); hide("plot2")
    }
    
    else if(length(input$my_choices) == 1){
      if(input$my_choices == "Plot1"){
        show("plot1");hide("plot2")
      }
      if(input$my_choices == "Plot2"){
        hide("plot1");show("plot2")
      }
    }
    
    else{
      
      if(all(c("Plot1","Plot2") %in% input$my_choices)){
        show("plot1");show("plot2")
      }
    }
  },ignoreNULL = F)
}

shinyApp(ui, server)
