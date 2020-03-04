library(shiny)
library(forecast)
library(timeSeries)
library(basicTrendline)
library(deseasonalize)
library(tseries)
library(ggplot2)

#library(RODBC)

#channel <- odbcConnect(dsn="Rconnect", uid="Abishek",pwd="oracle")
#k<- sqlQuery(channel, "SELECT * FROM ECSDATA")

#odbcClose(channel)
#END

#YEAR <- k$YEAR
#QUARTER <- k$QUARTER
#TOTAL <- k$TOTAL
#EC <- k$ECOMMERCE


url<-"E:/STUDY/PROJECTS/R WITH ORACLE/ecs.csv"
k<-read.csv(url,header=TRUE,sep=",")
YEAR <- k$year
QUARTER <- k$Quarter
TOTAL <- k$total
EC <- k$e.commerce

#ggplot(data=TOTAL,mapping=aes())

a<-data.frame(YEAR,QUARTER,TOTAL,EC)
t<- ts(a,frequency = 4)
plot(YEAR,TOTAL)

ui<-fluidPage(
   headerPanel(" E- COMMERCE VS TOTAL ECONOMY PREDICTION AND ANALYSIS "),
   tabsetPanel(
    tabPanel("Index numbers",
             sidebarPanel(
               selectInput("bp","select the base period",YEAR),
               selectInput("cp","select the comparision period",YEAR),
               actionButton("index","compute")
             ),
             mainPanel(
               titlePanel("PERCENTAGE CHANGE IN ECONOMY AND E- COMMERCE"),
               fluidRow(
                 p("index numbers are a comparision between base and
                        current period they show in percentage how much the
                        value is increased or decreased it is either price
                        based or quantity based"),
                 p(""),
                 hr(style="border-color: purple;")
               ),
               fluidRow(
                 column("ECONOMY IN BASE PERIOD",width=3),
                 column(textOutput("ebp"),width=3),
                 column("ECONOMY IN COMPARISION PERIOD",width = 3),
                 column(textOutput ("ecp"),width = 3)
               ),
               hr(style="border-color: purple;"),
               fluidRow(
                 column("E-commerce IN BASE PERIOD",width=3),
                 column(textOutput("ecbp"),width=3),
                 column("E-commerce IN COMPARISION PERIOD",width = 3),
                 column(textOutput("eccp"),width = 3)
               ),
               hr(style="border-color: purple;"),
               fluidRow(
                 column(8,plotOutput("indexplot"),offset = 2)
               )
             )
             ), 
    tabPanel("Analysis",
             sidebarPanel(
               selectInput("anal","SELECT ANY ANALIZATION METHOD",
                           c("TREND","DESEASONALIZE","CYCLIC","TIMESERIES",
                             "EXPONENTIAL SMOOTHING")),
               actionButton("anago","GO")
               ),
               mainPanel(
                 titlePanel("ANALYZING THE DATA USING GRAPH"),
                 fluidRow(
                   
                 textOutput("anaprint"),
                   p("HERE WE COMPARE BOTH TOTAL ECONOMY VALUES AND E-COMMERCE VALUES FOR BETTER UNDERSTANDING .",
                     br(),
                   "THE TOTAL ECONOMY IS A SUM OF ALL TYPES OF COMMERCE ACTIVITIES WHICH INCLUDE E-COMMERCE TOO.",
                   br(),
                   "LEGEND:",
                     br(),
                   "RED INDICATES TOTAL ECONOMY.",
                   br(),
                   "BLUE INDICATES E-COMMERCE.")
                 ),
                 fluidRow(
                   titlePanel("COMPARING E-COMMERCE AND TOTAL ECONOMY"),
                 plotOutput("anaplotc")
                 ),
                 fluidRow(
                   titlePanel("TOTAL ECONOMY GRAPH"),
                   
                 plotOutput("anaplot")
                 ),
                 fluidRow(
                   titlePanel("E-COMMERCE"),
                   plotOutput("anaplot1")
                 )
                  )
             ),
      tabPanel("Prediction",
              sidebarPanel(
                selectInput("box","SELECT THE DATA TO BE PREDICTED",
                                   c("TOTAL ECONOMY","E-COMMERCE")),
               selectInput("pred","SELECT ANY PREDICTION METHOD",
                           c("HOLT WINTER'S METHOD","ARIMA METHOD"
                             ,"FORECAST")),
               actionButton("predgo","GO")
              ),
             mainPanel(
               titlePanel("PREDICTING FUTURE VALUES"),
               fluidRow(
                 plotOutput("preprint")
               )
              )
          )
)
)


server<-function(input,output)
{
  observeEvent(input$index,{
               b<-input$bp
               c<-input$cp
               
               bpe=0
               j=1
               for (i in YEAR){
                 if(i == b)
                 {
                   bpe=bpe+TOTAL[j]
                 }
                 j=j+1
               }
               
               cpe=0
               j=1
               for (i in YEAR){
                 if(i == c)
                 {
                   cpe=cpe+TOTAL[j]
                 }
                 j=j+1
               }

               k1<-(cpe/bpe)*100

               bpec=0
               j=1
               for (i in YEAR){
                 if(i == b)
                 {
                   bpec=bpec+EC[j]
                 }
                 j=j+1
               }

               cpec=0
               j=1
               for (i in YEAR){
                 if(i == c)
                 {
                   cpec = cpec+EC[j]
                 }
                 j=j+1
               }
               
               k2<-(cpec/bpec)*100
               
               x<-c(c(100,k1),c(100,k2))
               y<-c(b,c,b,c)
               
               output$ebp<-renderText({ "100" })
               output$ecp<-renderText({ k1 })
               output$ecbp<-renderText({ "100" })
               output$eccp<-renderText({ k2 })
               
               output$indexplot<-renderPlot({
                 barplot(x,xlab = "years base-period and current period",
                         ylab = "percentage change",axes = TRUE,
                         col=c("blue","red"),
                         main = "CHANGE IN ECONOMY AND E-COMMERCE")
               })
})
  observeEvent(input$anago,{
    a<-data.frame(TOTAL)
    aa<-data.frame(EC)
    t<-ts(a,frequency = 4)
    tt<-ts(aa,frequency = 4)
    df<-data.frame(YEAR,t,tt)
    m<-decompose(t)
    mm<-decompose(tt)
               
               trend<-function()
               {
                 daf<-data.frame(m$trend,mm$trend)
                 output$anaplotc<-renderPlot({
                   plot(m$trend,type ="l",col="red", ylim=c(1000,1300000))
                   par(new=TRUE)
                   plot(mm$trend,type= "l",col="blue", ylim=c(1000,1300000),legend=c())
                 })
                 
                 output$anaplot<-renderPlot(
                   plot(mm$trend)
                 )
                 
                 output$anaplot1<-renderPlot(
                   plot(mm$trend)
                 )
               }
               
               deseason<-function()
               { 
                 output$anaplotc<-renderPlot({
                   plot(t-m$seasonal,type ="l",col="red", ylim=c(1000,1300000))
                   par(new=TRUE)
                   plot(tt-mm$seasonal,type= "l",col="blue",legend=c(), ylim=c(1000,1300000))
                 })
                 
                 output$anaplot<-renderPlot(
                 plot(t-m$seasonal) 
                 )
                 
                 output$anaplot1<-renderPlot(
                   plot(tt-mm$seasonal) 
                 )
               }
               
               cyclic<-function()
               {
                 output$anaplotc<-renderPlot({
                   plot(m$random,type ="l",col="red", ylim=c(-28000,40000))
                   par(new=TRUE)
                   plot(mm$random,type= "l",col="blue",legend=c(), ylim=c(-28000,40000))
                 })
                 
                 output$anaplot<-renderPlot(
                 plot(m$random)
                 )
                 
                 output$anaplot1<-renderPlot(
                   plot(mm$random)
                 )
               }
               
               time<-function()
               {
                 output$anaplotc<-renderPlot({
                   plot.ts(t,type ="l",col="red", ylim=c(1000,1300000))
                   par(new=TRUE)
                   plot.ts(tt,type= "l",col="blue",legend=c(), ylim=c(1000,1300000))
                 })
                 
                 output$anaplot<-renderPlot(
                 plot.ts(t)
                 )
                 
                 output$anaplot1<-renderPlot(
                   plot.ts(tt)
                 )
               }
               
               es<-function()
               {
                 output$anaplotc<-renderPlot({
                   movin<-ma(t,order = 4)
                   moviin<-ma(tt,order = 4)
                   plot(movin,type="l",col="red", ylim=c(1000,1300000))
                   par(new=TRUE)
                   plot(moviin,type= "l",col="blue",legend=c(), ylim=c(1000,1300000))
                 })
                 
                 output$anaplot<-renderPlot({
                 movin<-ma(t,order = 4)
                 plot(movin)
                 })
                 
                 output$anaplot1<-renderPlot({
                   moviin<-ma(tt,order = 4)
                   plot(moviin)
                 })
               }
               
               output$anaprint<-renderText({ paste("WE CAN COMPARE THE DIAGRAMS 
                                                   TO FIND THE VALUE 
                                                   OF ",input$anal,".") })
               if (input$anal == "TREND")
               {
                 trend();
               }
               else if(input$anal == "DESEASONALIZE")
               {
                 deseason()
               }
               else if(input$anal == "CYCLIC")
               {
                 cyclic()
               }
               else if(input$anal == "TIMESERIES")
               {
                 time()
               }
               else if(input$anal == "EXPONENTIAL SMOOTHING")
               {
                 es()
               }
  })
  
  observeEvent(input$predgo,{
    a<-data.frame(TOTAL)
    aa<-data.frame(EC)
    t<-ts(a,frequency = 4)
    tt<-ts(aa,frequency = 4)
    df<-data.frame(YEAR,t,tt)
    m<-decompose(t)
    mm<-decompose(tt)

    holtt<-function()
    {
      output$preprint<- renderPlot({
      model<-hw(t,h=10,seasonal = "additive")
      plot(model)
      })
    }

    holte<-function()
    {
      output$preprint<- renderPlot({
        model<-hw(tt,h=10,seasonal = "additive")
        plot(model)
      })
    }

    at<-function()
    {
      output$preprint<-renderPlot({
        fit<-auto.arima(t)
        plot(forecast(fit,h=10))
      })
    }

    ae<-function()
    {
      output$preprint<-renderPlot({
        fit<-auto.arima(tt)
        plot(forecast(fit,h=10))
      })
    }

    ft<-function()
    {
      output$preprint<-renderPlot({
        plot(forecast(t,h=10))
      })
    }

    fe<-function()
    {
      output$preprint<-renderPlot({
        plot(forecast(tt,h=10))
      })
    }

    if(input$box == "TOTAL ECONOMY" & input$pred == "HOLT WINTER'S METHOD")
    {
      holtt()
    }
    else if(input$box == "TOTAL ECONOMY" & input$pred == "ARIMA METHOD")
    {
      at()
    }
    else if(input$box == "TOTAL ECONOMY" & input$pred == "FORECAST")
    {
      ft()
    }
    else if(input$box == "E-COMMERCE" & input$pred == "HOLT WINTER'S METHOD")
    {
      holte()
    }
    else if(input$box == "E-COMMERCE" & input$pred == "ARIMA METHOD")
    {
      ae()
    }
    else if(input$box == "E-COMMERCE" & input$pred == "FORECAST")
    {
      fe()
    }

  })

}
shinyApp(ui = ui, server = server)
############## REPORT #####################
