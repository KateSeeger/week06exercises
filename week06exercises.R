library(shiny)
library(tidyverse)
library(babynames)

ui <- fluidPage(sliderInput(inputId = "year", 
                            label = "Year Range",
                            min = 1890,
                            max = 2019, 
                            value = c(1890, 2019), 
                            sep = ""), 
                textInput(inputId = "name",
                          label = "Enter Name Here:", 
                          value = "Lisa", 
                          placeholder = "eg. Lisa"), 
                selectInput(inputId = "sex", 
                            label = "Sex :", 
                            choices = c(Male  = "M", Female = "F")),
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "timeplot")
                
)
server <- function(input, output) { 
  output$timeplot <- renderPlot({
    babynames %>% 
      filter(name == input$name, 
             sex == input$sex) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_line() +
      scale_x_continuous(limits = input$year) +
      theme_minimal()
  })
  
}
shinyApp(ui = ui, server = server)