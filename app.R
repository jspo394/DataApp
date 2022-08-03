#install.packages()
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)


ui<-dashboardPage(title= "DatApp", skin= "blue",dashboardHeader(title="DatApp"
                                               
                                  
                                  
                  ),
                  dashboardSidebar(
                    
                    sidebarMenu(id="sidebarID",
                                menuItem("Cargar documento",fileInput("GetFile", "Cargar Archivo")),
                                menuSubItem("Tableu",tabName = 'chart1'),
                                menuItem("Contenido",id = "chartsID",
                                         menuSubItem("Blog"),
                                         menuSubItem("Quienes somos")
                                         
                                )
                    )
                    
                    
                  ),
                  dashboardBody(
                    DT::dataTableOutput("infile"),plotOutput("fig"),
                    
                   
                  )
)

server <- function(input, output){
  
  text1<- reactive({
    infile <- input$GetFile
    if(is.null(infile)){
      return(NULL)
    }
    datafile<-read.csv(infile$datapath)
    return(datafile)
  })
  output$infile <- DT::renderDataTable({
    DT::datatable(text1())
  })
  
  output$fig <- renderPlot(
    ggplot(data = text1(), aes(primernombre, segundonombre))+
      geom_point()
  )

  
}
shinyApp(ui, server)