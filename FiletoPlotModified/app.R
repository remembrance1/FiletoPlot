library(shiny)
library(ggplot2)
library(shinyjs)
library(RColorBrewer)
library(corrplot)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  # Application title
  titlePanel("File to Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      # Horizontal line ----
      tags$hr(),
      
      checkboxInput('header', 'Header', TRUE),
      checkboxInput('nonnum', 'Drop Non-Numeric', FALSE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      #implementing dropdown column 
      selectInput('xcol', 'X Variable', ""),
      selectInput('ycol', 'Y Variable', "", selected = ""),
      selectInput('color', 'Colour', "", selected = ""),
      
      tags$hr(),
      
      checkboxGroupInput("my_choices", "Plots to Display",
                         choices = c("Scatterplot", "CorrMat"), selected = "Scatterplot")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Output: Data file ----
      plotOutput('Scatterplot'), #need to add in more plots into the UI
      plotOutput('CorrMat')
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'color', label = 'Colour',
                      choices = names(df), selected = names(df)[3])
    
    return(df)
  })
  
  # hide plots on start
  hide("Scatterplot");hide("CorrMat")
  
  output$Scatterplot <- renderPlot({
    ggplot(data = data(), aes_string(x = input$xcol, y = input$ycol, colour = input$color)) + 
      geom_point() +
      ggtitle("Scatterplot") +
      theme_bw() + 
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  output$CorrMat <- renderPlot({
    #need to change it to numeric, by dropping those columns that are non-numeric...
    corr <- cor(data(), use="complete", method="pearson")
    corrplot(corr, method="circle", sig.level = 0.0000112, type="upper", 
             tl.cex = 1, tl.col="black", order="hclust", col=brewer.pal(n=8, name="RdYlBu"))
  })
  
  observeEvent(input$my_choices,{
    
    if(is.null(input$my_choices)){
      hide("Scatterplot"); hide("CorrMat")
    }
    
    else if(length(input$my_choices) == 1){
      if(input$my_choices == "Scatterplot"){
        show("Scatterplot");hide("CorrMat")
      }
      if(input$my_choices == "CorrMat"){
        hide("Scatterplot");show("CorrMat")
      }
    }
    
    else{
      
      if(all(c("Scatterplot","CorrMat") %in% input$my_choices)){
        show("Scatterplot");show("CorrMat")
      }
    }
  },ignoreNULL = F)
}

shinyApp(ui, server)