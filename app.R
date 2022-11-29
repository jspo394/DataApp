#install.packages()
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(openxlsx)
library(writexl)
library(stringr)
library(ggplot2)

#titulo del sitio
ui<-dashboardPage(title= "DatApp",
                  dashboardHeader(title="DatApp",tags$li(actionLink("openModal", label = "",
                                                                    icon = icon("address-book")),class = "dropdown")
                                  
                  ),
                  
                  dashboardSidebar(
                    
                    #menu
                    sidebarMenu(id="sidebarID",
                                
                                menuItem("Archivo a trabajar",icon = icon("database"),href="https://drive.google.com/file/d/11osYZHTN0Z6JK6uXgEh9JDv-aW-_8wk4/view?usp=share_link"),
                                menuItem("Cargar documento",icon = icon("upload"),
                                         fileInput("file", "Csv o Excel", multiple = TRUE,accept = c(".csv", ".xlsx"))),
                                
                                menuItem("Descargar",icon = icon("download"),downloadButton("downloadData")),
                                menuItem("Tableu",icon = icon("dashboard"),href = "https://public.tableau.com/app/profile/javier.mauricio.bedoya.gonzalez/viz/Anlisis_Brecha_Digital_Colombia/Hoja1")#,
                                # menuItem("Contenido",id = "chartsID",icon = icon("rss"),
                                #          menuSubItem("Blog"),
                                #          menuSubItem("Quienes somos")
                                #          
                                # )
                    )
                    
                    
                  ),
                  #muestra la tabla del contenido de la base de datos
                  dashboardBody(
                    DT::dataTableOutput("contents"),plotOutput("fig")
                    
                    
                  )
)

server <- function(input, output){
  
  
  #muestra el icono con información de los creadores
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Autores Dashboard",
                  p("Contacto:"),
                  p("Diana Marcela Velez Posso","-dmvelezp@unadvirtual.edu.co"),
                  p("Jhoan Sebastian Padilla Ortiz","-jspadillao@unadvirtual.edu.co"),
      )
    )
  })
  #limpia los datos
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
  #muestra la tabla
  output$contents <- DT::renderDataTable({
    DT::datatable(text1())
  })
  #muestra la grafica
  output$fig <- renderPlot(
    ggplot(data = text1(), aes(x=Cargo_identificado))+ geom_bar(width=0.2,colour="blue") + 
      ggtitle("Podemos observar que el cargo que más aparece es el de Desarrollador,
      esta base de datos hace parte del Estudio de identificación de brechas de
      capital humano para el sector TIC y se usó como insumo para la construcción
      del tablero de resultados del estudio: Tablero Desempeño Actual del Talento Humano")+ coord_flip()
  )
  #sirve para descargar el archivo trabajado
  output$downloadData <- downloadHandler(
    filename = "brechasTic.csv",
    content = function(file) {
    write_csv(text1(), file)
    }
  )
  
}
shinyApp(ui, server)