#install.packages()
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(openxlsx)
library(writexl)
library(stringr)
library(ggplot2)

ui<-dashboardPage(title= "DatApp", skin= "blue",
                  dashboardHeader(title="DatApp",
                                  dropdownMenu(type="messages",
                                               messageItem(from="Abner",
                                                           "Hola")
                                               
                                  )
                                  
                  ),
                  
                  dashboardSidebar(
                    
                    
                    sidebarMenu(id="sidebarID",
                                menuItem("Cargar documento",icon = icon("upload"),
                                         fileInput("file", "Csv o Excel", multiple = TRUE,accept = c(".csv", ".xlsx"))),
                                
                                menuItem("Descargar",icon = icon("download"),downloadButton("downloadData")),
                                menuItem("Tableu",icon = icon("dashboard"),href = "https://public.tableau.com/app/profile/javier.mauricio.bedoya.gonzalez/viz/Anlisis_Brecha_Digital_Colombia/Hoja1"),
                                menuItem("Contenido",id = "chartsID",icon = icon("rss"),
                                         menuSubItem("Blog"),
                                         menuSubItem("Quienes somos")
                                         
                                )
                    )
                    
                    
                  ),
                  dashboardBody(
                    DT::dataTableOutput("contents"),plotOutput("fig")
                    
                    
                  )
)

server <- function(input, output){
  
  text1 <- reactive({
    
    req(input$file)
    
    ext <- input$file$datapath
    ext <- str_remove(ext, ".*/0.")
    
    if(ext == "xlsx")
    {
      datafile <- read.xlsx(input$file$datapath)
    }else{
      datafile <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    }
  })
  
  output$contents <- DT::renderDataTable({
    DT::datatable(text1())
  })
  
  output$fig <- renderPlot(
    ggplot(data = text1(), aes(Nombre))+geom_bar()
  )
  output$downloadData <- downloadHandler(
    filename = "prueba.xlsx",
    content = function(file) {
      write_xlsx(text1(), file)
    }
  )
  
}
shinyApp(ui, server)