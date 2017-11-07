library(shiny)
library(ggplot2)

#GUI
ui <- fluidPage(
  titlePanel("Diamonds Data"),
  h1("I am header 1"),
  a("click here",href="http://google.com"),
  sidebarLayout(
    sidebarPanel(
      helpText("This app is to visualize diamonds dataset"),
      textInput(inputId = "title", 
                label = "Chart title", 
                value = ""),
      selectInput(inputId = "pos",
                  label = "Position",
                  choices = list("stack","dodge"),
                  selected = "stack")
    ),
    mainPanel(
      #every widget make sure to have id name for input and output
      plotOutput(outputId = "plot")
      
    )
  )
)

#Server
server <- function(input, output) {
  output$plot = renderPlot({
    #more than one line of codes, we need the {}
    ggplot(diamonds, aes(x = cut, fill = clarity)) +
      geom_bar(position = input$pos)+
      ggtitle(input$title)
  })
  
}

#Run the app

shinyApp(ui, server)
