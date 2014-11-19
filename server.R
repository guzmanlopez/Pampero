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
    else isolate({
      
      op <- options("useFancyQuotes")
      options(useFancyQuotes = FALSE)
      estacion <- sQuote(input$estacion)
      fecha_inic <- sQuote(paste(input$dates[1],input$hora_inic))
      fecha_fin <- sQuote(paste(input$dates[2],input$hora_fin))
      
      con <- dbConnect(MySQL(), user="", password="", dbname="oceano", host="localhost") # conectar
      rs <- dbSendQuery(con, statement=paste("select *","from METEO","where FECHA between", fecha_inic, "and", fecha_fin, "and ESTACION_NOMBRE=",estacion,"order by FECHA",sep=" "))
      data <- fetch(rs, n=-1)
      dbClearResult(res=rs)
      on.exit(dbDisconnect(con)) # desconectar
    
      if(length(data)==0) {
        data[1,1] <- NA
        data[2,1] <- "NO EXISTEN DATOS PARA LA CONSULTA REALIZADA"
        data[3,1] <- NA
        colnames(data) <- c("Mensajes")
      } 
              
      return(data)
      
      })
    
    return(mydata)
    
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

### Plot ####
output$plot <- renderPlot({
  
  if (input$tipo_archivo=='mysql') {
    
    mydata <- datasetInput()
    
    mydata[,input$velviento_var] <- as.numeric(as.character(mydata[,input$velviento_var]))
    mydata[,input$dirviento_var] <- as.numeric(as.character(mydata[,input$dirviento_var]))
    
    # Formato fechas
    #fechayhora <- as.POSIXct(mydata$Fecha)
    mydata <- cbind(mydata[,-1], date=mydata[,1])
  } 
  else {
    
    mydata <- datasetInput()
    mydata <- cbind(mydata, date=mydata[,input$date_var])
  }
  
  plot(windRose(mydata=mydata, ws=input$velviento_var, wd=input$dirviento_var, type=input$type, statistic=input$statistic, angle=input$angle, paddle=FALSE, hemisphere="southern", ws.int=input$intervals, breaks=input$breaks, key.footer="Velocidad del viento (nudos)"))
  
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

### Serie de Tiempo: Annotated Time Line ####
  output$plot_ts <- renderGvis({
    if (input$tipo_archivo=='mysql') {
      
      mydata <- datasetInput()
      mydata[,input$velviento_var_ts] <- as.numeric(as.character(mydata[,input$velviento_var_ts]))
      mydata[,input$date_var_ts] <- as.POSIXct(mydata[,input$date_var_ts])
                  
    }
    else {
      
      mydata <- datasetInput()
      mydata <- cbind(Fecha=strptime(x=mydata[,input$date_var_ts],format=input$formato_fecha), mydata[,-1])
  
    }
    
    mydata_reshape <- reshape(data=mydata, varying=input$var, v.names="Valores", timevar="Variable", times=input$var, direction="long", idvar="Valores_ID", drop=setdiff(names(mydata),c(input$date_var_ts,input$var)))
    
    return(gvisAnnotatedTimeLine(data=mydata_reshape, datevar=input$date_var_ts, numvar="Valores", idvar="Variable", options=list(gvis.language="es", dateFormat='HH:mm - dd/MM/yyyy', width=750, height=450, scaleType='allmaximized', scaleColumns='[0,1,2]')))
    
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

### Resumen
output$summary <- renderGvis({
  
    datos <- datasetInput()
    tabla_resumen <- function(datos){
      
      if(input$tipo_archivo=='mysql') {
        
      columnas <- colnames(datos)[-c(1:5)]
      mat <- matrix(nrow=length(columnas), ncol=6, dimnames=list(c(columnas),c("Promedio", "ds", "Mediana", "Máximo", "Mínimo", "n")))
      
      # Promedios
      promedios <- numeric()
      for(i in 1:length(columnas))
        promedios[i] <- mean(datos[,columnas[i]], na.rm=TRUE)
      
      # Ds
      ds <- numeric()
      for(i in 1:length(columnas))
        ds[i] <- sd(datos[,columnas[i]], na.rm=TRUE)
      
      # Mediana
      med <- numeric()
      for(i in 1:length(columnas))
        med[i] <- median(datos[,columnas[i]], na.rm=TRUE)
      
      # Máximo
      max <- numeric()
      for(i in 1:length(columnas))
        max[i] <- max(datos[,columnas[i]], na.rm=TRUE)
      
      # Mínimo
      min <- numeric()
      for(i in 1:length(columnas))
        min[i] <- min(datos[,columnas[i]], na.rm=TRUE)
      
      # n
      n <- numeric()
      for(i in 1:length(columnas))
        n[i] <- length(datos[,columnas[i]])
      
      # Cargar datos a matriz
      
      mat[1:nrow(mat),1] <- promedios
      mat[1:nrow(mat),2] <- ds
      mat[1:nrow(mat),3] <- med
      mat[1:nrow(mat),4] <- max
      mat[1:nrow(mat),5] <- min
      mat[1:nrow(mat),6] <- n
      
      return(round(mat, digits=2))
      
      }
      if(input$tipo_archivo!='mysql') {
        
        columnas <- colnames(datos)[c(-1,-2)]
        mat <- matrix(nrow=length(columnas), ncol=6, dimnames=list(c(columnas),c("Promedio", "ds", "Mediana", "Máximo", "Mínimo", "n")))
        
        # Promedios
        promedios <- numeric()
        for(i in 1:length(columnas))
          promedios[i] <- mean(datos[,columnas[i]], na.rm=TRUE)
        
        # Ds
        ds <- numeric()
        for(i in 1:length(columnas))
          ds[i] <- sd(datos[,columnas[i]], na.rm=TRUE)
        
        # Mediana
        med <- numeric()
        for(i in 1:length(columnas))
          med[i] <- median(datos[,columnas[i]], na.rm=TRUE)
        
        # Máximo
        max <- numeric()
        for(i in 1:length(columnas))
          max[i] <- max(datos[,columnas[i]], na.rm=TRUE)
        
        # Mínimo
        min <- numeric()
        for(i in 1:length(columnas))
          min[i] <- min(datos[,columnas[i]], na.rm=TRUE)
        
        # n
        n <- numeric()
        for(i in 1:length(columnas))
          n[i] <- length(datos[,columnas[i]])
        
        # Cargar datos a matriz
        
        mat[1:nrow(mat),1] <- promedios
        mat[1:nrow(mat),2] <- ds
        mat[1:nrow(mat),3] <- med
        mat[1:nrow(mat),4] <- max
        mat[1:nrow(mat),5] <- min
        mat[1:nrow(mat),6] <- n
        
        return(round(mat, digits=2))
        
      }
        
    }
    
    if(input$tipo_archivo=='mysql') {
    mat <- data.frame("Variable"=colnames(datos)[-c(1:5)], as.data.frame(tabla_resumen(datos=datos),row.names=FALSE))
    resumen_datos <- gvisTable(data=mat)
    return(resumen_datos)
    }
    if(input$tipo_archivo!='mysql') {
      mat <- data.frame("Variable"=colnames(datos)[c(-1,-2)], as.data.frame(tabla_resumen(datos=datos),row.names=FALSE))
      resumen_datos <- gvisTable(data=mat)
      return(resumen_datos)
    }
    
}
                             )
  
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