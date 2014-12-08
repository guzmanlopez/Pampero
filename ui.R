library(shiny)

shinyUI(pageWithSidebar(
  
  #### Título de la Aplicación ####
  headerPanel(title="Pampero APP", windowTitle="Pampero: análisis de vientos"), 
  
  #### Sidebar ####
    sidebarPanel(
    
      HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/logos_viento_2.png"/></div>'), ### LOGO
    
    conditionalPanel(
      condition="input.tabs=='Tabla'", ### TABLA
      
      tags$hr(),
      
      wellPanel(
        strong('Cargar datos desde:'), br(), br(),
      selectInput('tipo_archivo', "", c("Archivo de texto"='.csv', "Archivo de Microsoft Excel"='.xls',"Base de datos"='mysql'))
      ),
      
      conditionalPanel(
        condition="input.tipo_archivo=='.csv'", ### CSV
        fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain')),
        
        wellPanel(
          strong('Opciones'),br(),br(),  
          checkboxInput('header', 'Cabecera', TRUE),
          selectInput('sep', 'Separador de campos',c('Coma'=',','Punto y coma'=';','Tabulador'='\t'),'Coma',FALSE),
          selectInput('quote', 'Delimitador de texto', c("Ninguno"='','Comillas dobles'='"','Comillas simples'="'"),'Ninguna',FALSE),
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
         HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/mariaDB_chico_dos.png"/></div>'), ### LOGO
        br(),
        radioButtons('estacion', 'Estación meteorológica:', c("Boya oceanográfica"='Boya Oceanográfica', "Pilote Norden"='Pilote Norden', "Torre Oyarvide"='Torre Oyarvide')),
        tags$hr(),
        dateRangeInput('dates', 'Fechas', start="2009-11-26", end="2011-04-27", format="dd/mm/yyyy", separator="-", weekstart=1, language="es"),
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
      selectInput("statistic", "", choices=c("Frecuencia"="prop.count", "Contribución al promedio"="prop.mean", "Conteo absoluto"="abs.count"), selected="Proporción de la frecuencia")
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
      tabPanel("Resumen", htmlOutput("summary")),
      tabPanel("Acerca de esta APP",
               h3(p(strong('Descripción'))),
               p(style="text-align:justify",'Esta aplicación web de R con Shiny se encuentra en desarrollo.'),
               p(style="text-align:justify",'La aplicación web Pampero está diseñada para permitirle al usuario visualizar y analizar de manera interactiva datos de viento. Está siendo desarrollada en el marco del Proyecto FREPLATA URU/09/G31 dentro del', em('"Programa de Monitoreo y Evaluación y Sistema de Información Integrado y establecido para la toma de decisiones y la Gestión del Río de la Plata y su Frente Marítimo".'),'El objetivo era generar una herramienta que permita a los usuarios analizar datos de viento provenientes de las estaciones meteorológicas de la Boya oceanográfica o de Pilote Norden.'),
               p(style="text-align:justify",'La mayor parte del software empleado para desarrollar esta aplicación es libre, eso quiere decir que garantiza al usuario la libertad de poder usarlo, estudiarlo, compartirlo (copiarlo) y modificarlo. El software R es un proyecto de software libre que es colaborativo y tiene muchos contribuyentes.'),
               tags$hr(),
               h3(p(strong('Guía de usuario'))),
               HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/PDF.png" alt="" style="width: 5%; height: 5%; float: left; margin-right:5px" /></div>'),
               br(),
               a('Pampero web app', href="https://github.com/guzmanlopez/Pampero/blob/master/Manual/Gu%C3%ADa%20de%20usuario%20Pampero%20web%20app.pdf?raw=true", target="_blank"),
               tags$hr(),
               h3(p(strong('Código fuente'))),
               HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/github-10-512.png" alt="" style="width: 5%; height: 5%; float: left; margin-right:5px" /></div>'),
               br(),
               a('Repositorio GitHub', href="https://github.com/guzmanlopez/Pampero", target="_blank"),
               tags$hr(),
               h3(p(strong('Referencias'))),
               p(style="text-align:justify",strong('R Core Team (2013).'),'R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL',a("http://www.R-project.org/", href="http://www.R-project.org/",target="_blank")),
               p(style="text-align:justify",strong('RStudio and Inc. (2013).'),'shiny: Web Application Framework for R. R package version 0.8.0.',a("http://CRAN.R-project.org/package=shiny", href="http://CRAN.R-project.org/package=shiny",target="_blank")),
               p(style="text-align:justify",strong('David Carslaw y Karl Ropkins (2013).'),'openair: Open-source tools for the analysis of air pollution data. R package version 0.9-0.',a("http://CRAN.R-project.org/package=openair",href="http://CRAN.R-project.org/package=openair",target="_blank")),
               p(style="text-align:justify",strong('Markus Gesmann & Diego de Castillo.'),'Using the Google Visualisation API with R. The R Journal, 3(2):40-44, December 2011.'),
               p(style="text-align:justify",strong('Jeffrey A. Ryan & Joshua M. Ulrich (2013).'),'xts: eXtensible Time Series. R package version 0.9-7.',a("http://r-forge.r-project.org/projects/xts/",href="http://r-forge.r-project.org/projects/xts/",target="_blank")),
               p(style="text-align:justify",strong('Karoly Antal. (2012).'),'gnumeric: Read data from files readable by gnumeric. R package version 0.7-2.',a("http://CRAN.R-project.org/package=gnumeric",href="http://CRAN.R-project.org/package=gnumeric",target="_blank")),
               p(style="text-align:justify",strong('R Special Interest Group on Databases (2013).'),'DBI: R Database Interface. R package version 0.2-7.',a("http://CRAN.R-project.org/package=DBI",href="http://CRAN.R-project.org/package=DBI",target="_blank")),
               p(style="text-align:justify",strong('David A. James y Saikat DebRoy (2011).'),'RMySQL: R interface to the MySQL database. R package version 0.8-0.',a("http://CRAN.R-project.org/package=RMySQL",href="http://CRAN.R-project.org/package=RMySQL",target="_blank")),
               tags$hr(),
               HTML('<div style="clear: left;"><img src="https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/foto_perfil.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
               strong('Autor'),
               p(a('Guzmán López', href="https://www.linkedin.com/pub/guzm%C3%A1n-l%C3%B3pez/59/230/812", target="_blank"),' - (Correo:',
                 a('guzilop@gmail.com', target="_blank"),' | ', 'Telegram:', a('@Guzman', href="https://telegram.me/Guzman", target="_blank"),')',
                 br(),
                 'Biólogo | Asistente para el manejo de información oceanográfica',br(),a('Proyecto FREPLATA - URU/09/G31',href="http://www.freplata.org/", target="_blank")),
               br()) # Acerca de este programa
                )
    )
  )
        )   