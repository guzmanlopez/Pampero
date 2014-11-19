Pampero
===

Aplicación web de Shiny para visualizar y analizar de manera interactiva datos de viento.

Requisitos
===

```R
library(shiny) 
library(openair) 
library(gnumeric) 
library(googleVis)
```

Para conexión a base de datos:

```R
library(DBI)
library(RMySQL)
```

Ejecutar app desde R: 
===

```R
runGitHub("Pampero", "guzmanlopez")
```

Capturas de pantalla
===

Cargar datos:
![Image](https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/captura-de-pantalla-01.png)

Rosa de los vientos:
![Image](https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/captura-de-pantalla-02.png)

Rosa de los vientos estacional:
![Image](https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/captura-de-pantalla-03.png)

Línea de tiempo:
![Image](https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/captura-de-pantalla-04.png)

Estadísticas básicas:
![Image](https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/captura-de-pantalla-05.png)

Acerca de la app:
![Image](https://raw.githubusercontent.com/guzmanlopez/Pampero/master/Figuras/captura-de-pantalla-06.png)