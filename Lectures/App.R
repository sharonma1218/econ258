#----------------------------------
# Econ 258 
# Mini Shiny Project: Zodiac Signs
# Sharon Ma
# 03/22/2022
#----------------------------------

ui <- fluidPage(numericInput("birthday", "When is your birthday? (Format: MMDD)", 
                             value=1111, min=0101, max=1231), textOutput("textzodiac"))

server <- function(input,output){
  output$textzodiac <- renderText({
    if(input$birthday>=1222 & input$birthday<=1231)
    {paste("Zodiac Sign: Capricorn")}
    else if(input$birthday>=0101 & input$birthday<=0119)
    {paste("Zodiac Sign: Capricorn")}
    else if(input$birthday>=0120 & input$birthday<=0218)
    {paste("Zodiac Sign: Aquarius")}
    else if(input$birthday>=0219 & input$birthday<=0320)
    {paste("Zodiac Sign: Pisces")}
    else if(input$birthday>=0321 & input$birthday<=0419)
    {paste("Zodiac Sign: Aries")}
    else if(input$birthday>=0420 & input$birthday<=0520)
    {paste("Zodiac Sign: Taurus")}
    else if(input$birthday>=0521 & input$birthday<=0620)
    {paste("Zodiac Sign: Gemini")}
    else if(input$birthday>=0621 & input$birthday<=0722)
    {paste("Zodiac Sign: Cancer")}
    else if(input$birthday>=0723 & input$birthday<=0822)
    {paste("Zodiac Sign: Leo")}
    else if(input$birthday>=0823 & input$birthday<=0922)
    {paste("Zodiac Sign: Virgo")}
    else if(input$birthday>=0923 & input$birthday<=1022)
    {paste("Zodiac Sign: Libra")}
    else if(input$birthday>=1023 & input$birthday<=1121)
    {paste("Zodiac Sign: Scorpio")}
    else if(input$birthday>=1122 & input$birthday<=1221)
    {paste("Zodiac Sign: Sagittarius")}
  })
}

shinyApp(ui = ui, server = server)




