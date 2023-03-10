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
  
  ui <- fluidPage(sliderInput(inputId = "num", 
  label = "Choose a number", 
  value = 25, min = 1, max = 100))
  
  ui <- fluidPage(radioButtons(inputId = "col", 
                               label="What is your favourite color?", 
                               choices = c("Yellow", "Red", "Green")
                                ))
  
  # Now, let's add an output to the user interface. 
  ui <- fluidPage(radioButtons(inputId = "col", 
                               label="What is your favourite color?", 
                               choices = c("Yellow", "Red", "Green"), 
                               plotOutput("plot")))
  
  # It has not done anything yet because we have not told the server to do anything. 

  # I leave this code below so that I can run the app. 
  server <- function(input, output) {}
  shinyApp(ui = ui, server = server)
  

  
###### Step 3: Let's talk about the server
  
  # Be careful, if the code is incorrect, the app won't run well. 

  ui <- fluidPage(textInput(inputId="color", 
                            label="What is your favourite color?"), 
                               textOutput("textcolor"))
  
  server <- function(input, output) {
            output$textcolor <- renderText({"You have selected a color!"}) 
    }
  
  shinyApp(ui = ui, server = server)
  
  
## Another example  

  ui <- fluidPage(textInput(inputId="color", 
                            label="What is your favourite color?"), 
                  textOutput("textcolor"))
  
  server <- function(input, output) {
    output$textcolor <- renderText({paste("You have selected a color:", input$color)}) 
  }
  
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


###### Step 5: Let's upload this into a server! 

# To see Emily's full code: http://ardina-umich.shinyapps.io/Shiny_example_by_ewolhart

# Yellowdig task for the week! 
# Create a mini shiny app (just something simple, as long as you can do the basics)
# Upload it to shinyapps.io and share the link on yellowdig.

# You can use datasets too that is already available. 
# Check out dataset available from the visualization book https://rkabacoff.github.io/datavis/Data.html

# Check out also the Shiny gallery: https://shiny.rstudio.com/gallery/