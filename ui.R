library(shiny)

shinyUI(pageWithSidebar(
  
  #### Título de la Aplicación ####
  headerPanel(title="Vientos", windowTitle="Aplicación en R para análisis de datos de viento"), 
  
  #### Sidebar ####
    sidebarPanel(
    
    imageOutput(outputId="logo", height=50), ### LOGO
    
    conditionalPanel(
      condition="input.tabs=='Tabla'", ### TABLA
      
      tags$hr(),
      
      wellPanel(
        strong('Cargar datos desde:'), br(), br(),
      selectInput('tipo_archivo', "", c("Archivo de texto"='.csv',
                                                            "Archivo de Microsoft Excel"='.xls',
                                                            "Base de datos"='mysql'))
      ),
      
      conditionalPanel(
        condition="input.tipo_archivo=='.csv'", ### CSV
        fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain')),
        
        wellPanel(
          strong('Opciones'),br(),br(),  
          checkboxInput('header', 'Cabecera', TRUE),
          selectInput('sep', 'Separadores',c('Coma'=',', 'Punto y coma'=';', 'Tabulador'='\t'),'Coma',FALSE),
          selectInput('quote', 'Comillas', c("Ninguna"='','Doble'='"','Simple'="'"),'Ninguna',FALSE),
          selectInput('dec', 'Separador decimal', c(Punto=".", Coma=","),'Punto',FALSE),
          numericInput('skip',"Leer desde línea:",value="0"),
          tags$hr(),
          strong('Formato de Fecha y Hora'),br(),  
          selectInput('formato_fecha',"",
                      multiple=FALSE,selected="ej. 06/28/2009 22:50:60",
                      choices=c("ej. 06/28/2009 22:50:60"="%m/%d/%Y %H:%M:%S",
                                "ej. 28/06/2009 22:50:60"="%d/%m/%Y %H:%M:%S",
                                "ej. 28/06/09 22:50:60"="%d/%m/%y %H:%M:%S",
                                "ej. 28-06-09 22:50:60"="%d-%m-%y %H:%M:%S"))
        )
        ),
      
      conditionalPanel(
        condition="input.tipo_archivo=='.xls'", ### XLS
        fileInput('file2', '', accept=c('application/vnd.ms-excel')),
        
        wellPanel(
          strong('Opciones'),br(),br(),
          checkboxInput('header2', 'Cabecera', TRUE),
          br(),
          textInput('sheet_name', 'Nombre de hoja', c(""))
          ), 
        
        tags$hr(),
        strong('Formato de Fecha y Hora'),br(),  
        selectInput('formato_fecha',"",
                    multiple=FALSE,selected="ej. 06/28/2009 22:50:60",
                    choices=c("ej. 06/28/2009 22:50:60"="%m/%d/%Y %H:%M:%S",
                              "ej. 28/06/2009 22:50:60"="%d/%m/%Y %H:%M:%S",
                              "ej. 28/06/09 22:50:60"="%d/%m/%y %H:%M:%S",
                              "ej. 28-06-09 22:50:60"="%d-%m-%y %H:%M:%S"))
        
        ),      
        
       conditionalPanel(
         condition="input.tipo_archivo=='mysql'", ### MySQL
        tags$hr(),
        imageOutput(outputId="logo2",height=80), ### LOGO
        radioButtons('estacion', 'Estación meteorológica:', c("Punta Brava"='DL02', "Isla de Flores"='DL01', "La Paloma"='DL03', "José Ignacio"='DL04')),
        tags$hr(),
        dateRangeInput('dates', 'Fechas', start=Sys.Date()-30, end=Sys.Date(), format="dd/mm/yyyy", separator="-", weekstart=1, language="es"),
        textInput('hora_inic', 'Hora inicial', c("00:00:00")),
        textInput('hora_fin', 'Hora final', c("23:59:59")),
        HTML("<button id=\"enviar_consulta\" type=\"button\" class=\"btn action-button btn-primary\">Enviar consulta</button>"),
        tags$hr(),
        downloadButton("descarga","Descargar datos",class='btn-success')      
       )
        ),
      
    conditionalPanel(
      condition="input.tabs=='Rosa de los vientos'", ### PLOT
      tags$hr(),
      
      wellPanel(
        strong('Configurar variables'), br(), br(),
      uiOutput("fecha_col"),
      uiOutput("vel_viento_col"),
      uiOutput("dir_viento_col")
      ),
      
      wellPanel(
        strong('Escala temporal'), br(), br(),
      selectInput("type", "", choices=c("Anual"="year", "Estacional"="season", "Mensual"="month", "Horaria"="hour", "Todos los datos"="default"), selected="Todos los datos")
      ),
      
      wellPanel(
        strong('Estadística'), br(), br(),
      selectInput("statistic", "", choices=c("Proporción de la frecuencia"="prop.count", "Contribución proporcional al promedio"="prop.mean", "Conteo absoluto"="abs.count"), selected="Proporción de la frecuencia")
      ),
      
      wellPanel(
        strong('Configuración de plot'), br(), br(),
      sliderInput("intervals","Intervalo:", value= 3, min= 1, max= 10, step=1),
      br(),
      sliderInput("breaks", "Quiebres:", value= 4, min= 1, max= 10, step=1),
      br(),
      sliderInput("angle","Ángulo:", value= 15, min= 1, max= 45)
      )
      
      ),
    conditionalPanel(
      condition="input.tabs=='Serie de tiempo'",
      tags$hr(),
      
      wellPanel(
        strong('Configurar variables'), br(), br(),
      uiOutput("fecha"),
      uiOutput("variables"))
      )
    ),
  
  #### Mainpanel ####
   
  mainPanel(
    tabsetPanel(id="tabs",
      tabPanel("Tabla", dataTableOutput("table")), # Tabla
      tabPanel("Rosa de los vientos", plotOutput("plot")), # Plot
      tabPanel("Serie de tiempo", htmlOutput("plot_ts")),
      tabPanel("Resumen resultados", verbatimTextOutput("summary")) # Resumen
      )
    )
))