library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        titlePanel(img(src="prospera.png", height = 128, width =272 )
                   
                   
                   ),
        
        sidebarLayout(
                position ="left",
                
                sidebarPanel( "sidebar panel"),
                mainPanel(     
                        
                        h4("Seleccione el Programa" ),
                      
                        tabsetPanel(
                                tabPanel("Con Corresponsabilidad", plotOutput("Con Corresponsabilidad")), 
                                tabPanel("Sin Corresponsabilidad", verbatimTextOutput("summary")), 
                                tabPanel("Transito", tableOutput("table"))
                        )
                
                
                        
      
                )
        
  
        )
 ))