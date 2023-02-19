###################################
#    Machi Portal Web             #
#                                 #
#  Created by: Santiago Bula M.   #
#  Last modification: 07 02 2023  #
# #################################

# Required libraries

library(shiny)
library(shinydashboard)
library(DT)
library(fresh) # Check on internet why it doesn't work
library(bslib)
library(quantmod)
library(ggplot2)
#install.packages("TTR")
library(TTR)
library(dygraphs)
library(dplyr)
library(tidyverse)



# Skeleton 

ui <- dashboardPage( 
                 # Logo
  dashboardHeader(title= tags$img(src='images/logo-machi.png', height='50',width='120'), 
                  
                  # Notification center
                  dropdownMenuOutput("msgOutput"),
                  dropdownMenu(type = "notifications", 
                               notificationItem(
                                 text = "Tu activo de intel según el RSI esta 
                                 sobrecomprado, cierra posición" ,
                                 icon = icon("laptop"),
                                 status ="success"
                                 ),  
                               notificationItem(
                                 text = "El precio de Café sigue bullish, esta alerta de tu posición" ,
                                 icon = icon("coffee"),
                                 status ="warning"
                               )
                               )
                  ), 
  dashboardSidebar( 
    sidebarMenu(
      # Serach bar form
      #sidebarSearchForm("searchText","buttonSearch","Search"),
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      # Home tab
      menuItem("Inicio", tabName = "home", icon = icon("house-flag")),
      # Teamwork tab
      menuItem("Nosotros", icon = icon("people-group"), tabName = "nosotros"),
      # Stock tabs
      menuItem("Revisión de activos", icon = icon("money-bill-trend-up"),
               menuSubItem("Acciones", tabName = "stock1"),
               menuSubItem("Fundamentales", tabName = "stock2"),
               menuSubItem("Zona Forex", tabName = "stock3")
               ),
      # Dashboard monitor tab
      menuItem("Machi monitor", icon = icon("dashboard"), tabName = "machiMonitor",
               badgeLabel = "PRO", badgeColor = "blue")
    )
  ),
  
  dashboardBody(
    # CSS Style
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              box(img(src="images/intro.jpg", align = "right",
                      height='700px',width='600px'), background="black"
              ),
              box(img(src="images/Sticker3.gif", align = "left",
                      height='700px',width='600px'), background="black")
              ),
      # Teamwork tab
      tabItem("nosotros", fluidPage(
        img(src="images/our-team.jpg", align = "top",height='800px',width='1350px')
      )),
      # Stock tabs
      
      # Stock visor
      tabItem("stock1",
              fluidRow(box(
                title = "Inputs", solidHeader = TRUE, width=12,
                "Seleccione la fecha desde donde quiere generar el gráfico hasta hoy", 
                br(), 
                "Al inicio le mostrará un error pero después de ingresar el ticket 
                del activo cargará un grafico para su analisis, recuerde asegurarse 
                de que este bien escrito el ticket",
                dateInput("date1", "Fecha:", value = "2020-02-28"),
                textInput("text1", "Ticket del activo:", value = "TSLA")),
              ), fluidRow(box(
                title = "Gráfico del activo", solidHeader = TRUE, width = 12,
                collapsible = TRUE,
                dygraphOutput("candleStick1", height = 450 )
              ))),
      
      # Fundamentals Visor
      tabItem("stock2",
              h1(span(textOutput("titolo"), style="color:#fff"), align="left"), 
              # Ho spento 2 ore cercando questo
              
              fluidRow(column(12, box(width = NULL,  title = "Indicadores Económicos", solidHeader = T, 
                            status = "primary", 
                            # A static valueBox
                            valueBox(textOutput("cafe"), "Precio del café", icon = icon("coffee")),
                            valueBox(textOutput("interesUS"), "Tasa de interés EE.UU", icon = icon("money-bill-trend-up")), 
                            #valueBox("10,7%", "Tasa de Desempleo", icon = icon("face-unamused")),
                            valueBox(textOutput("TRM"), "Tasa de cambio", icon = icon("dollar-sign"))))
                       ),
              
              fluidRow(column(12, box( width = NULL, title = "Activos en vivo", collapsible = T,
                                       solidHeader = TRUE, status = "info",
              HTML('<!-- TradingView Widget BEGIN -->
                        <div class="tradingview-widget-container">
                          <div class="tradingview-widget-container__widget"></div>
                          <div class="tradingview-widget-copyright"><a href="https://es.tradingview.com/markets/" rel="noopener" target="_blank"><span class="blue-text">Mercados hoy</span></a> por TradingView</div>
                          <script type="text/javascript" src="https://s3.tradingview.com/external-embedding/embed-widget-ticker-tape.js" async>
                          {
                          "symbols": [
                            {
                              "proName": "FOREXCOM:SPXUSD",
                              "title": "S&P 500"
                            },
                            {
                              "proName": "FOREXCOM:NSXUSD",
                              "title": "US 100"
                            },
                            {
                              "proName": "FX_IDC:EURUSD",
                              "title": "EUR/USD"
                            },
                            {
                              "proName": "BITSTAMP:BTCUSD",
                              "title": "Bitcoin"
                            },
                            {
                              "proName": "BITSTAMP:ETHUSD",
                              "title": "Ethereum"
                            }
                          ],
                          "showSymbolLogo": true,
                          "colorTheme": "light",
                          "isTransparent": true,
                          "displayMode": "adaptive",
                          "locale": "es"
                        }
                          </script>
                        </div>
                        <!-- TradingView Widget END -->')
                       ))),
              
              fluidRow(column(12, box( width = NULL, height = 470, title = "Principales titulares", collapsible = T,
                                       solidHeader = TRUE, status = "info",
                                       HTML('
                                            <!-- TradingView Widget BEGIN -->
                                            <div class="tradingview-widget-container">
                                              <div class="tradingview-widget-container__widget"></div>
                                              <div class="tradingview-widget-copyright"><a href="https://in.tradingview.com/key-events/" rel="noopener" target="_blank"><span class="blue-text">Daily news roundup</span></a> by TradingView</div>
                                              <script type="text/javascript" src="https://s3.tradingview.com/external-embedding/embed-widget-timeline.js" async>
                                              {
                                              "feedMode": "all_symbols",
                                              "colorTheme": "light",
                                              "isTransparent": true,
                                              "displayMode": "regular",
                                              "width": "100%",
                                              "height": "450",
                                              "locale": "in"
                                            }
                                              </script>
                                            </div>
                                            <!-- TradingView Widget END -->
                                            ')
              )))
              ,
              ),
      
      
      tabItem("stock3",
              h1(span(textOutput("titolo2"), style="color:#fff"), align="left"),
              
              fluidRow(column(12, box( width = NULL, title = "Música", collapsible = T,
                                       solidHeader = TRUE, status = "info",
              HTML('<iframe style="border-radius:12px" 
                   src="https://open.spotify.com/embed/playlist/37i9dQZF1DX3O0tIbqe92W?utm_source=generator&theme=0" 
                   width="100%" height="152" frameBorder="0" allowfullscreen="" allow="autoplay; 
                   clipboard-write; encrypted-media; fullscreen; picture-in-picture" loading="lazy"></iframe>')))),
              
              fluidRow(column(12, box( width = NULL, title = "Mercado Forex", collapsible = T,
                                       solidHeader = TRUE, status = "info",
                                       HTML('<!-- TradingView Widget BEGIN -->
                                            <div class="tradingview-widget-container">
                                              <div class="tradingview-widget-container__widget"></div>
                                              <div class="tradingview-widget-copyright"><a href="https://es.tradingview.com/markets/currencies/" rel="noopener" target="_blank"><span class="blue-text">Mercado de divisas</span></a> por TradingView</div>
                                              <script type="text/javascript" src="https://s3.tradingview.com/external-embedding/embed-widget-forex-cross-rates.js" async>
                                              {
                                              "width": 1160,
                                              "height": 500,
                                              "currencies": [
                                                "EUR",
                                                "USD",
                                                "COP",
                                                "GBP",
                                                "CHF",
                                                "AUD",
                                                "MXN",
                                                "CLP",
                                                "JPY"
                                              ],
                                              "isTransparent": true,
                                              "colorTheme": "light",
                                              "locale": "es"
                                            }
                                              </script>
                                            </div>
                                            <!-- TradingView Widget END -->'))))
        
      ),
      
      # Machi monitor
      tabItem(tabName = "machiMonitor",
              fluidRow(
      infoBox("tendencia del mercado", "Bearish", color = "blue", fill = T,
              icon = icon("money-bill-transfer")),
      infoBoxOutput("status"),
      infoBoxOutput("position")
              ),
      fluidPage(
        box(dygraphOutput("candleStick2", height = 450 )),
        box(dygraphOutput("candleStick3", height = 450))
      ),
              fluidRow(
                box(title = "Análitca del activo",  width = 12, autoHeight=T, status = "primary", 
                solidHeader = T, collapsible = T, dataTableOutput("stockTable"))
              )
              )
    )
    
  )
)

# Soul

server <- function(input, output){
  output$stockTable <- renderDataTable({
    # EMA ----
    # Medias moviles doble crossover method "Exponenciales" 
    ticket <- input$text1
    from <- input$date1
    periodicidad <- "daily"   #"weekly", "monthly"
    EMA1 <- 20
    EMA2 <- 50
    
    # MACD & Signal Line
    
    rapida <- 12 
    lenta <- 26 
    signo <- 9
    
    activo <- getSymbols(Symbols = ticket,src = "yahoo", from =from, to=Sys.Date(),
                         periodicity = periodicidad, auto.assign = FALSE)
    PRECIO <- activo[,4]
    
    EMA <- function (price,n){
      ema <- c()
      ema[1:(n-1)] <- NA
      ema[n]<- mean(price[1:n])
      beta <- 2/(n+1)
      for (i in (n+1):length(price)){
        ema[i]<-beta * price[i] + 
          (1-beta) * ema[i-1]
      }
      ema <- reclass(ema,price)
      return(ema)
    }
    
    MA1 <- EMA(PRECIO,EMA1)
    MA2 <- EMA(PRECIO,EMA2)
    
    # MACD Function
    MACD <- function (precio,Short,Large,sig){
      MACD <- na.omit(EMA(precio,Short) - EMA(precio,Large))
      signal <- EMA(MACD,sig)
      tabla <- cbind(MACD,signal)
      colnames(tabla) <- c("MACD","signal")
      return(tabla)
    }
    
    MACD_data <- MACD(precio = PRECIO,Short =rapida ,Large = lenta,sig = signo)
    
    # venta ---- 
    siganls_sell<- function(ma1,ma2){
      
      signal_sell <- c()
      inicio <- (nrow(ma2)-nrow(filter(as.data.frame(ma2),V1!="NA")))
      ma2[inicio,1] <- 0
      for (i in (inicio+1):nrow(MA2)) {
        if ((ma1[i,1]<ma2[i,1])&&(ma1[i-1,1]>ma2[i-1,1])) {
          signal_sell[i] <- 1
        }else{
          signal_sell[i] <- 0
        }
      }
      x <- cbind(ma1,ma2,signal_sell)
      return(x)
    }
    
    venta <- siganls_sell(ma1 = MA1,ma2 = MA2)
    # compra ----
    siganls_buy <- function(ma1,ma2){
      
      signal_buy <- c()
      inicio <- (nrow(ma2)-nrow(filter(as.data.frame(ma2),V1!="NA")))
      ma2[inicio,1] <- 0
      for (i in (inicio+1):nrow(MA2)) {
        if ((ma1[i,1]>ma2[i,1])&&(ma1[i-1,1]<ma2[i-1,1])) {
          signal_buy[i] <- 1
        }else{
          signal_buy[i] <- 0
        }
      }
      x <- cbind(ma1,ma2,signal_buy)
      return(x)
    }
    
    
    compra <- siganls_buy(ma1 = MA1,ma2 = MA2)
    # venta MACD ----
    siganls_sell_MACD <- function(macd){
      
      signal_sell <- c()
      inicio <- (nrow(macd)-nrow(filter(as.data.frame(macd[,2]),signal!="NA")))
      macd[inicio,2] <- 0
      for (i in (inicio+1):nrow(macd)) {
        if ((macd[i,1]<macd[i,2])&&(macd[i-1,1]>macd[i-1,2])) {
          signal_sell[i] <- 1
        }else{
          signal_sell[i] <- 0
        }
      }
      x <- cbind(macd,signal_sell)
      return(x)
    }
    
    venta_MACD <- siganls_sell_MACD(macd = abs(MACD_data))
    # compra MACD ----
    siganls_buy_MACD <- function(macd){
      
      signal_buy <- c()
      inicio <- (nrow(macd)-nrow(filter(as.data.frame(macd[,2]),signal!="NA")))
      macd[inicio,2] <- 0
      for (i in (inicio+1):nrow(macd)) {
        if ((macd[i,1]>macd[i,2])&&(macd[i-1,1]<macd[i-1,2])) {
          signal_buy[i] <- 1
        }else{
          signal_buy[i] <- 0
        }
      }
      x <- cbind(macd,signal_buy)
      
      return(x)
    }
    
    compra_MACD <- siganls_buy_MACD(macd = abs(MACD_data))
    
    
    # tabla ----
    TABLA <- merge(activo,venta,join = "inner")
    TABLA <- merge(TABLA,compra,join = "inner")
    TABLA <- merge(TABLA,venta_MACD,join = "inner")
    TABLA <- merge(TABLA,compra_MACD,join = "inner")
    
    final <- select(as.data.frame(TABLA),c(1,4,9,12,15,18))
    colnames(final) <- c("Precio_apertura","Precio_cierre","sig_SELL_MA","sig_BUY_MA",
                         "MACD_Sell","MACD_Buy")
    final$sig_SELL_MA <- gsub(1,"SELL",final$sig_SELL_MA)
    final$sig_SELL_MA <- gsub(0,"nada",final$sig_SELL_MA)
    
    final$sig_BUY_MA <- gsub(1,"BUY",final$sig_BUY_MA)
    final$sig_BUY_MA <- gsub(0,"nada",final$sig_BUY_MA)
    
    final$MACD_Sell <- gsub(1,"SELL",final$MACD_Sell)
    final$MACD_Sell <- gsub(0,"nada",final$MACD_Sell)
    
    final$MACD_Buy <- gsub(1,"BUY",final$MACD_Buy)
    final$MACD_Buy <- gsub(0,"nada",final$MACD_Buy)
    
    final
    
  })
  
  output$status <- renderInfoBox({
    infoBox("Status del stock", "Sobrevendido", color="teal", fill = T,
            icon = icon("comments-dollar"))
  })
  
  output$position <- renderInfoBox({
    infoBox("Posición recomendable", "Nada" , color="aqua", fill = T, 
            icon = icon("arrow-down-up-across-line"))
  })
  
  output$titolo <- renderText({"Noticias | Actualidad en los mercados 📰"})
  
  output$titolo2 <- renderText({"Zona Forex | Mercado de divisas y música 🎸"})
  
  output$cafe <- renderText({
    
    library(quantmod) 
    yahoo_symbols <- c("KC")
    qts_env <- new.env()
    
    getSymbols(yahoo_symbols, 
               env = qts_env,
               to = Sys.Date(), 
               periodicity = "daily")
    
    shares_kc <- do.call(merge, eapply(qts_env, Ad))
    shares_kc <- na.omit(shares_kc)
    tail(shares_kc,n=1)
    
  })
  
  output$TRM <- renderText({
   
    # Load American Dollar to Colombian Peso exchange rate data
    library(quantmod) 
    currency_pair <- "USD/COP"
    getSymbols(Symbols = currency_pair, to=Sys.Date(), src = "oanda")
    tail(USDCOP, n=1)
    
  })
  
  output$interesUS <- renderText({
    
    library(quantmod) 
    getSymbols(Symbols = "FEDFUNDS", to=Sys.Date(), src = "FRED")
    tail(FEDFUNDS, n=1)
    
  }) 
  
  # Chart 1 Dygraph ---- 
  
  output$candleStick1 <- renderDygraph({
    
    # Medias moviles doble crossover method "Exponenciales" 
    ticket <- input$text1
    from <- input$date1
    periodicidad <- "daily"   #"weekly", "monthly"
    EMA1 <- 20
    EMA2 <- 50
    
    # MACD & Signal Line
    
    rapida <- 12 
    lenta <- 26 
    signo <- 9
    
    activo <- getSymbols(Symbols = ticket,src = "yahoo", from =from, to=Sys.Date(),
                         periodicity = periodicidad, auto.assign = FALSE)
    PRECIO <- activo[,4]
    ventana=120 #relacionado con la periodicidad 
    
    EMA <- function (price,n){
      ema <- c()
      ema[1:(n-1)] <- NA
      ema[n]<- mean(price[1:n])
      beta <- 2/(n+1)
      for (i in (n+1):length(price)){
        ema[i]<-beta * price[i] + 
          (1-beta) * ema[i-1]
      }
      ema <- reclass(ema,price)
      return(ema)
    }
    
    MA1 <- EMA(PRECIO,EMA1)
    MA2 <- EMA(PRECIO,EMA2)
    
    
    x <- cbind(activo[,c(1:4)],MA1,MA2)
    cola <- tail(x, n=ventana)
    grap<-dygraph(cola)
    dyCandlestick(grap)%>%
      dyRangeSelector()
    
  })
  
  # Chart 2 Dygraph ----
  
  output$candleStick2 <- renderDygraph({
    
    # Input elements
    
    ticket <- input$text1
    from <- input$date1
    periodicidad <- "daily"   #"weekly", "monthly"
    EMA1 <- 20
    EMA2 <- 50
    
    # MACD & Signal Line
    
    rapida <- 12 
    lenta <- 26 
    signo <- 9
    
    activo <- getSymbols(Symbols = ticket,src = "yahoo", from =from, to=Sys.Date(),
                         periodicity = periodicidad, auto.assign = FALSE)
    PRECIO <- activo[,4]
    
    
    MACD <- function (precio,Short,Large,sig){
      MACD <- na.omit(EMA(precio,Short) - EMA(precio,Large))
      signal <- EMA(MACD,sig)
      tabla <- cbind(MACD,signal)
      colnames(tabla) <- c("MACD","signal")
      return(tabla)
    }
    
    
    MACD_data <- MACD(precio = PRECIO,Short =rapida ,Large = lenta,sig = signo)
    
    dygraph(MACD_data)%>%
      dyRangeSelector()
    
  })
  
  # Chart 3 Dygraph ----
  
  output$candleStick3<- renderDygraph({
    
    ticket <- input$text1
    from <- input$date1
    periodicidad <- "daily"   #"weekly", "monthly"
    EMA1 <- 20
    EMA2 <- 50
    
    activo <- getSymbols(Symbols = ticket,src = "yahoo", from =from, to=Sys.Date(),
                         periodicity = periodicidad, auto.assign = FALSE)
    PRECIO <- activo[,4]
    
    MM_RSI <- 14
    
    Sobrecompra <- 70
    Sobreventa <- 30
    myRSI <- function (PRECIO,n){
      N <- length(PRECIO)
      U <- rep(0,N)
      D <- rep(0,N)
      rsi <- rep(NA,N)
      Lprice <- Lag(PRECIO,1)
      for (i in 2:N){
        if (PRECIO[i]>=Lprice[i]){
          U[i] <- 1
        } else {
          D[i] <- 1
        }
        if (i>n){
          AvgUp <- mean(U[(i-n+1):i])
          AvgDn <- mean(D[(i-n+1):i])
          rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
        }
      }
      rsi <- reclass(rsi, PRECIO)
      return(rsi)
    }
    
    rsi <- myRSI(Cl(PRECIO), MM_RSI)
    RSI <- cbind(rsi,Sobrecompra,Sobreventa)
    #Gráfico
    dygraph(RSI)%>%
      dyRangeSelector()
    
  })
  
}

# Being
shinyApp(ui, server) 





