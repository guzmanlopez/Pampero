library(shiny)
library(openair)
library(gnumeric)
library(DBI)
library(RMySQL)
library(googleVis)

shinyServer(function(input, output) {

### Datos ####
  datasetInput <- reactive({
    
    if (is.null(input$file1) & is.null(input$file2) & input$enviar_consulta==0) return(NULL) 
    else 
      if (input$tipo_archivo=='.csv') mydata <- read.csv(input$file1$datapath, header=input$header, sep=input$sep, quote=input$quote, dec=input$dec, skip=input$skip) else
      if (input$tipo_archivo=='.xls') mydata <- read.gnumeric.sheet(file=input$file2$datapath,head=input$header2,sheet.name=input$sheet_name,drop.empty.rows="all",drop.empty.columns="none")
    if (input$tipo_archivo=='mysql') mydata <- if (input$enviar_consulta==0) return(NULL)
    else isolate({op <- options("useFancyQuotes")
               options(useFancyQuotes = FALSE)
               estacion <- sQuote(input$estacion)
               fecha_inic <- sQuote(input$dates[1])
               fecha_fin <- sQuote(input$dates[2])
               hora_inic <- sQuote(input$hora_inic)
               hora_fin <- sQuote(input$hora_fin)
               
               con <- dbConnect(MySQL(), user="", password="", dbname="", host="localhost") # conectar
               rs <- dbSendQuery(con, statement=paste("SELECT dlgId, fechaData, horaData, mag0, mag1","FROM dbtable_values","WHERE fechaData BETWEEN", fecha_inic, "AND", fecha_fin, "AND dlgId =",estacion, "AND horaData BETWEEN", hora_inic, "AND", hora_fin,"ORDER BY horaData",sep=" "))
               data <- fetch(rs, n=-1)
               dbClearResult(res=rs)
               on.exit(dbDisconnect(con)) # desconectar
               data <- cbind(data[,1],
                             rep(x=if(input$estacion=="DL01") "Isla de Flores" else
                               if(input$estacion=="DL02") "Punta Brava" else
                                 if(input$estacion=="DL03") "La Paloma" else
                                   if(input$estacion=="DL04") "José Ignacio",nrow(data)),data[,2],data[,3],data[,5],data[,4])
               colnames(data) <- c("Canal","Estación","Fecha","Hora","VEL_kn","DIR_grad")
               data <- as.data.frame(data)
               #assign(make.names("viento_base_de_datos"),data,envir= globalenv()) #data global env paste nomb estacion fecha hora
               return(data)
               })
    
    return(mydata)
    
    })
  
### Logos ####
  output$logo <- renderImage({
      
      filename <- "/home/guzman/Documentos/FREPLATA/R/GUI/logos_viento_app.png"
      
      list(src = filename,
           contentType = 'image/png')
    }, deleteFile = FALSE)
  output$logo2 <- renderImage({
    
    filename <- "/home/guzman/Imágenes/Iconos/mariaDB_chico_dos.png"
    
    list(src = filename,
         contentType = 'image/png')
  }, deleteFile = FALSE)
  
### Plot ####
output$plot <- renderPlot(width=1000, height=500, {
  
  if (input$tipo_archivo=='mysql') {
    
    mydata <- datasetInput()
    
    mydata[,input$velviento_var] <- as.numeric(as.character(mydata[,input$velviento_var]))
    mydata[,input$dirviento_var] <- as.numeric(as.character(mydata[,input$dirviento_var]))
    
    # Formato fechas
    mydata[,input$date_var] <- paste(substr(x=mydata$Fecha,start=1,stop=4),
                                     substr(x=mydata$Fecha,start=6,stop=7),
                                     substr(x=mydata$Fecha,start=9,stop=10),sep="-")
    
    fechayhora <- as.POSIXct(paste(mydata$Fecha,mydata$Hora, sep=" "))
    mydata <- cbind(mydata, date=fechayhora)
  } 
  else {
    
    mydata <- datasetInput()
    mydata <- cbind(mydata, date=mydata[,input$date_var])
  }
  
  plot(windRose(mydata=mydata, ws=input$velviento_var, wd=input$dirviento_var, type=input$type, statistic=input$statistic, angle=input$angle, paddle=FALSE, hemisphere="southern", ws.int=input$intervals, breaks=input$breaks, key.footer="Velocidad del viento (nudos)"))
  
})

### Encontrar columnas para Rosa de los vientos ####
  output$fecha_col <- renderUI({
    if(is.null(datasetInput())) return("")
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    busqueda <- grep(pattern="fecha", x=colnames, ignore.case=TRUE, value=FALSE)
    
    if(length(busqueda)==0) select <- colnames[1]
    if(length(busqueda)==1) select <- colnames[busqueda]
    if(length(busqueda) > 1) select <- colnames[busqueda[1]]
  
    # Create the checkboxes and select them all by default
    selectInput("date_var", "Fecha:",choices=colnames, selected=select)
  })
  output$vel_viento_col <- renderUI({
    if(is.null(datasetInput())) return("")
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    busqueda <- grep(pattern="vel", x=colnames, ignore.case=TRUE, value=FALSE)
    
    if(length(busqueda)==0) select <- colnames[1]
    if(length(busqueda)==1) select <- colnames[busqueda]
    if(length(busqueda) > 1) select <- colnames[busqueda[1]]
    
    # Create the checkboxes and select them all by default
    selectInput("velviento_var", "Velocidad de viento:",choices=colnames, selected=select)
  })
  output$dir_viento_col <- renderUI({
    if(is.null(datasetInput())) return("")
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    busqueda <- grep(pattern="dir", x=colnames, ignore.case=TRUE, value=FALSE)
    
    if(length(busqueda)==0) select <- colnames[1]
    if(length(busqueda)==1) select <- colnames[busqueda]
    if(length(busqueda) > 1) select <- colnames[busqueda[1]]
    
    # Create the checkboxes and select them all by default
    selectInput("dirviento_var", "Dirección de viento:",choices=colnames, selected=select)
  })

  ### Serie de Tiempo: Annotated Time Line ####
  output$plot_ts <- renderGvis({
    if (input$tipo_archivo=='mysql') {
      
      mydata <- datasetInput()
      
      mydata[,input$velviento_var_ts] <- as.numeric(as.character(mydata[,input$velviento_var_ts]))
      
      # Formato fechas
      mydata[,input$date_var_ts] <- paste(substr(x=mydata$Fecha,start=1,stop=4),
                                       substr(x=mydata$Fecha,start=6,stop=7),
                                       substr(x=mydata$Fecha,start=9,stop=10),sep="-")
      
      fechayhora <- as.POSIXct(paste(mydata$Fecha,mydata$Hora, sep=" "))
      mydata <- cbind(mydata, date=fechayhora)
      mydata[,"date"] <- as.POSIXct(mydata[,"date"]) 
      
    } 
    else {
      
      mydata <- datasetInput()
      mydata <- cbind(Fecha=strptime(x=mydata[,input$date_var_ts],format=input$formato_fecha), mydata[,-1])
      
      mydata_reshape <- reshape(data=mydata, varying=input$var, v.names="Valores", timevar="Variable", times=input$var, direction="long", idvar="Valores_ID", drop=setdiff(names(mydata),c("Fecha",input$var)))
    }
      
    return(gvisAnnotatedTimeLine(data=mydata_reshape, datevar="Fecha", numvar="Valores", idvar="Variable", options=list(gvis.language="es", dateFormat='HH:mm - dd/MM/yyyy', width=800, height=400)))
    
  })  
  
  ### Encontrar columnas para Serie de Tiempo ####
  output$fecha <- renderUI({
    if(is.null(datasetInput())) return("")
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    busqueda <- grep(pattern="fecha", x=colnames, ignore.case=TRUE, value=FALSE)
    
    if(length(busqueda)==0) select <- colnames[1]
    if(length(busqueda)==1) select <- colnames[busqueda]
    if(length(busqueda) > 1) select <- colnames[busqueda[1]]
    
    # Create the checkboxes and select them all by default
    selectInput("date_var_ts", "Fecha:",choices=colnames, selected=select)
  })
  output$variables <- renderUI({
    if(is.null(datasetInput())) return("")
    
    # Get the data set with the appropriate name
    colnames <- names(datasetInput())
    busqueda <- grep(pattern="vel", x=colnames, ignore.case=TRUE, value=FALSE)
    
    if(length(busqueda)==0) select <- colnames[1]
    if(length(busqueda)==1) select <- colnames[busqueda]
    if(length(busqueda) > 1) select <- colnames[busqueda[1]]
    
    # Create the checkboxes and select them all by default
    selectInput("var", "Variables", choices=colnames, selected=select, multiple=TRUE)
  })
  
### Resumen ####
  output$summary <- renderPrint({
    
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    mydata <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    type <- input$type
    stat <- input$statistic
    angle <- input$angle
    intervals <- input$intervals
    breaks <- input$breaks
    
  res <- results(windRose(mydata=mydata, ws="VEL_kn", wd="DIR_grad", type=type, statistic=stat, angle= angle, paddle=FALSE, hemisphere="southern", ws.int=intervals, breaks=breaks))
    
    return(res)
    
  })

### Tabla ####
  output$table <- renderDataTable({
    mydata <- datasetInput()
    return(mydata)
      })
  
  ### Descargar desde Base de Datos ####
  output$descarga <- downloadHandler(
    filename = function() {
      paste('viento-', if(input$estacion=="DL01") "Isla de Flores" else
                       if(input$estacion=="DL02") "Punta Brava" else
                        if(input$estacion=="DL03") "La Paloma" else
                          if(input$estacion=="DL04") "José Ignacio","-", input$dates[1], "-", input$dates[2], '.csv', sep='')
    },
    content = function(con) {
      write.csv(mydata, con)
    },
    contentType="text/csv"
    )
  
})



