library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

data <- mtcars

#classify the data set
data$mpgt[data$mpg<20]<-1
data$mpgt[data$mpg>20 & data$mpg<25]<-2
data$mpgt[data$mpg>25 & data$mpg<30]<-3
data$mpgt[data$mpg>30]<-4

data$hpf[data$hp<100]<-0
data$hpf[data$hp>100&data$hp<200]<-1
data$hpf[data$hp>200]<-2

data$mpgt<-factor(data$mpgt)
data$vs<-factor(data$vs)
data$cyl<-factor(data$cyl)
data$hpf<-factor(data$hpf)

#design the main menu
header <- dashboardHeader(title="Car Features")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard"))
  )
) 
frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Weight per Cylinders"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Weightpercyl", height = "300px")
  )
  ,box(
    title = "Weight per mpg"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Weightpermpg", height = "300px")
  ) 
  ,box(
    title = "Weight per hp"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Weightperhp", height = "300px")
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

ui <- dashboardPage(
  title = 'Example',
  header,
  sidebar,
  body,
  skin = "red"
)

server <- function(input, output) { 
  wt.cyl <- data %>% group_by(cyl) %>% summarise(value = sum(wt)) %>% filter(value==max(value))
  wt.mpgt <- data %>% group_by(mpgt) %>% summarise(value = sum(wt)) %>% filter(value==max(value))
  wt.hp <- data %>% group_by(hpf) %>% summarise(value = sum(wt)) %>% filter(value==max(value))

  output$value1 <- renderValueBox({
    valueBox(
      formatC(wt.cyl$value, format="d", big.mark=',')
      ,paste('Top Cylinder:',wt.cyl$cyl)
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(wt.hp$value, format="d", big.mark=',')
      ,paste('Top HP:',wt.hp$hpf)
      ,color = "green")
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(wt.mpgt$value, format="d", big.mark=',')
      ,paste('Top MPG:',wt.mpgt$mpgt)
      ,color = "yellow")   
  })
  
  output$Weightpercyl <- renderPlot({
    ggplot(data = data, 
           aes(x=cyl, y=wt, fill=vs)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Weight") + 
      xlab("Cylinders") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      theme_bw() +
      ggtitle("Weight per Cylinders") + labs(fill = "Engine") +
      scale_fill_manual(values=c("#2E5077",
                                 "#4DA1A9"))
  })
  output$Weightpermpg <- renderPlot({
    ggplot(data = data, 
           aes(x=mpgt, y=wt, fill=vs)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Weight") + 
      xlab("MPG") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      theme_bw() +
      ggtitle("Weight per MPG") + labs(fill = "Engine") +
      scale_fill_manual(values=c("#2E5077",
                                 "#4DA1A9"))
  })
  output$Weightperhp <- renderPlot({
    ggplot(data = data, 
           aes(x=hpf, y=wt, fill=vs)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Weight") + 
      xlab("HP") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      theme_bw() +
      ggtitle("Weight per HP") + labs(fill = "Engine")+
      scale_fill_manual(values=c("#2E5077",
                                 "#4DA1A9"))
  })
}
shinyApp(ui,server)
