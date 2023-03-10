#-------------------------
# Econ 258 Shiny Project
# Sharon Ma
# 03/22/2022
#-------------------------

# Example from "R for Data Science” (https://r4ds.had.co.nz/index.html)

library(shiny)

ui <- fluidPage(textInput("name", "What is your name?"),
                numericInput("age", "How old are you?", NA, min=0, max=150))

server <- function(input, output) {}

shinyApp(ui=ui, server=server)

# reference
server <- function(input, output) {
  output$textcolor <- renderText({paste("You have selected a color:", input$color)}) 
}

shinyApp(ui = ui, server = server)


# reference
ui <- fluidPage(textInput("name", "What is your name?"),
                numericInput("age", "How old are you?", NA, min=0, max=150))



# Mini Shiny Project: Zodiac Signs 
ui <- fluidPage(numericInput("birthday", "When is your birthday? (Format: MMDD", 
                             NA, min=0101, max=1231))

server <- function(input,output) {
  if(input$birthday>=1222 & input$birthday<=1231){
    output$textzodiac <- renderText({paste("Zodiac Sign: Capricorn")})
  }}

shinyApp(ui = ui, server = server)




