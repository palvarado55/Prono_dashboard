
library(shiny)
library(dplyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)
library(lubridate)

shinyServer(function(input, output, session) {
  
  output$notifications <- renderMenu({

   dropdownMenu(type = "notifications",
                notificationItem( paste("Generado el:", now()),
                                         icon("calendar"), status = "primary")   )
  })
  
  # "Cargar" archivo con la información del mapa
  load("C:/E/Shiny/12_app_mapa_CR/gadf.Rdata") 
  
  lg = -84.10  # longitud  San José E(+), O(-)
  lt =   9.93  # latitud   San José N(+), S(-)
  zm =   7.45   # zoom
  
  output$gaugeANG <- renderGvis({
    df <- data.frame(Label = "MW", value = 172.2)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
              yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Angostura <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -83.64, lat = 9.95)  %>% 
      addTiles()  %>% setView(   lng =  lg   , lat = lt   , zoom = zm)   })
  
  
  output$gaugeARE <- renderGvis({
    df <- data.frame(Label = "MW", value = 157.4)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
              yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Arenal <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -85.0, lat = 10.47)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  
  output$gaugeCAC <- renderGvis({
    df <- data.frame(Label = "MW", value = 148.8)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
              yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Cachi <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -83.81, lat =  9.84)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  
  output$gaugeCAR <- renderGvis({
    df <- data.frame(Label = "MW", value =  87.9)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Cariblanco <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -84.18, lat = 10.31)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  output$gaugePBL <- renderGvis({
    df <- data.frame(Label = "MW", value =  38.2)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$PBlancas <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -84.58, lat = 10.38)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  
  output$gaugePIR <- renderGvis({
    df <- data.frame(Label = "MW", value = 140.3)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Pirris <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -84.11, lat =  9.64)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  
  output$gaugeREV <- renderGvis({
    df <- data.frame(Label = "MW", value = 305.5)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Reventazon <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -83.59, lat = 10.07)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  output$gaugeRMC <- renderGvis({
    df <- data.frame(Label = "MW", value = 135.2)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$RMacho <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -83.85, lat =  9.77)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  output$gaugeTO1 <- renderGvis({
    df <- data.frame(Label = "MW", value =  23.2)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Toro1 <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -84.28, lat = 10.24)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  output$gaugeTO2 <- renderGvis({
    df <- data.frame(Label = "MW", value =  65.7)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Toro2 <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -84.26, lat = 10.26)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  output$gaugeVGA <- renderGvis({
    df <- data.frame(Label = "MW", value =  97.4)  
    gvisGauge(df, options = list(min = 0, max = 300, greenFrom = 0 , greenTo = 100,
    yellowFrom = 100 , yellowTo = 200, redFrom = 200 , redTo = 400))      })
  
  output$Vgarita <- renderLeaflet({
    leaflet(gadf) %>% addCircles(lng = -84.355, lat =  9.93)  %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
  output$gaugePRE <- renderGvis({
    df <- data.frame(Label = "MW", value =  1372)  
    gvisGauge(df, options = list(min = 0, max = 1500, greenFrom = 0 , greenTo = 500,
    yellowFrom = 500 , yellowTo = 1000, redFrom = 1000 , redTo = 1500))      })
  
  output$Plantas <- renderLeaflet({
    leaflet(gadf) %>% addCircles(
                      lng=c(-83.64, -85.001, -83.81, -84.18, -84.58 , -84.11,
                            -83.59, -83.85 , -84.28, -84.26, -84.355           ),
                      lat=c(  9.95,  10.47 ,   9.84,  10.31,  10.38 ,   9.64,
                             10.07,   9.77 ,  10.24,  10.26,   9.93))   %>% 
      addTiles()  %>% setView(   lng =  lg  , lat = lt   , zoom = zm)   })
  
})
