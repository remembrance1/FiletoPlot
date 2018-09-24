library(shiny)

#data
df <- iris

#ui
ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput(inputId = "Question",
                       label = "Choose the plots",
                       choices = c("Plot1", "Plot2", "Plot3"),
                       selected = "")),
  mainPanel(
    uiOutput('ui_plot') 
  )
)

#server
server <- function(input, output)
{
  # gen plot containers
  output$ui_plot <- renderUI({ 
    out <- list()
    if (length(input$Question)==0){return(NULL)}
    for (i in 1:length(input$Question)){
      out[[i]] <-  plotOutput(outputId = paste0("plot",i))
    }  
    return(out) 
  })
  
  # render plots
  observe({  
    for (i in 1:3){  
      local({  #because expressions are evaluated at app init
        ii <- i 
        output[[paste0('plot',ii)]] <- renderPlot({ 
          if (length(input$Question) > ii-1 ){  
            return(plot(runif(100)))
          } 
          NULL
        })
      })
    }                                  
    
  })
  
}

shinyApp(ui, server)