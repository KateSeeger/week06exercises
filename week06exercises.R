library(tidyverse)     # for data cleaning and plotting
library(lubridate)     # for date manipulation
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps

#This app will also use the COVID data. Make sure you load that data 
#and all the libraries you need in the `app.R` file you create.
#Below, you will post a link to the app that you publish on shinyapps.io.
#You will create an app to compare states' cumulative number of COVID cases over time.
#The x-axis will be number of days since 20+ cases and the y-axis will be
#cumulative cases on the log scale (`scale_y_log10()`). We use number of days
#since 20+ cases on the x-axis so we can make better comparisons of the curve 
#trajectories. You will have an input box where the user can choose which states
#to compare (`selectInput()`) and have a submit button to click once the user has
#chosen all states they're interested in comparing. The graph should display
#a different line for each state,with labels either on the graph or in a legend.
#Color can be used if needed. 


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