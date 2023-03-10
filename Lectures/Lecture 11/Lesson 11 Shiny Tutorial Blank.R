#----------------------------------------
#
# Shiny App Tutorial for Econ 258 
#
# Tutorial code by: Ardina Hasanbasri 
# Full shiny graph example code by: Emily Wolhart
# 
#----------------------------------------

# Other shiny app refernce from RStudio: https://shiny.rstudio.com/tutorial/
# Shiny cheatsheet                     : https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#                                      : https://shiny.rstudio.com/reference/shiny/1.0.5/ 

#install.packages("shiny")
library(shiny)


###### Step 1: Basics of a shiny apps

# A shiny app has three components: 

  # 1: a user interface component  
  ui <- fluidPage()
  
  # 2: a server component  
  server <- function(input, output) {}
  
  # 3: a line of code to run the app  
  shinyApp(ui = ui, server = server)
  


###### Step 2: Let's talk about the first interface component
  
  ui <- fluidPage("Hello Econ 258 Class!")
  
  ui <- fluidPage(sliderInput(inputId="number", label="choose a number", min=0, max=100))
  server <- function(input, output) {}
  shinyApp(ui = ui, server = server)
  
  ui <- fluidPage(sliderInput(inputId="number", label="Choose a number", value=25, min=0, max=100))
  server <- function(input, output) {}
  shinyApp(ui = ui, server = server)


    
###### Step 4: Let's do a more complicated example
  
  library(ggplot2)
  
  data("mtcars")
  # Data on automobiles and their characteristics.
  # Let's look at the cars mpg by different characteristics.
  
  ui <- fluidPage(radioButtons("x_axis", "Graph mpg by:",
                               c("Weight (wt)", 
                                 "1/4 Mile (qsec)",
                                 "Displacement (disp)")), 
                  plotOutput("scatter_plot"))
  
  server <- function(input, output) {
    output$scatter_plot <- renderPlot({
      
      if (input$x_axis == "Weight (wt)") {
        ggplot(mtcars, aes(wt, mpg)) + geom_point(pch=21) + 
          labs(title = "Cars: MPG by Weight") + xlab("Weight (1000 lbs)") +
          ylab("Miles per gallon (mpg)")
        
      } else if (input$x_axis == "1/4 Mile (qsec)") {
        ggplot(mtcars, aes(qsec, mpg)) + geom_point(pch=21) + 
          labs(title = "Cars: MPG by 1/4 Mile Time") + xlab("1/4 Mile (sec)") +
          ylab("Miles per gallon (mpg)")
        
      } else if (input$x_axis == "Displacement (disp)") {
        ggplot(mtcars, aes(disp, mpg)) + geom_point(pch=21) + 
          labs(title = "Cars: MPG by Displacement") + xlab("Displacement (cubic inches)") +
          ylab("Miles per gallon (mpg)") 
        
      }
    }
    )}
  
  shinyApp(ui = ui, server = server)
  
  
###### Step 5: My own 
  
  #install.packages("gcookbook")
  library(shiny)
  library(ggplot2)
  
  data(uspopage, package="gcookbook")
  
  ui <- fluidPage(sliderInput(inputId = "num",
                              label = "Year",
                              value = 1, min = 1900, 2002))
  server <- function(input, output) {
    output$scatter_plot <- renderPlot({
      ggplot(uspopage, aes())
    })
  }
  
  shinyApp(ui = ui, server = server)
  

  
# example 
  ggplot(mtcars, 
         aes(x = wt, y = mpg, size = hp)) +
    geom_point(alpha = .5, 
               fill="cornflowerblue", 
               color="black", 
               shape=21) +
    scale_size_continuous(range = c(1, 14)) +
    labs(title = "Auto mileage by weight and horsepower",
         subtitle = "Motor Trend US Magazine (1973-74 models)",
         x = "Weight (1000 lbs)",
         y = "Miles/(US) gallon",
         size = "Gross horsepower") 