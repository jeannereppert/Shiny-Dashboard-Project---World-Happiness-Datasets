library(plyr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(DT)
library(data.table)
library(rworldmap)
library(plotly)
packageVersion('plotly')
library(ggplot2)
library(shinyWidgets)
library(pairsD3)

#read files into shiny dashboard

Happiness_2015 <- read.csv("2015.csv", header = TRUE, sep = ",")
Happiness_2016 <- read.csv("2016.csv", header = TRUE, sep = ",")
Happiness_2017 <- read.csv("2017.csv", header = TRUE, sep = ",")

#some data cleaning including correcting some values, dropping and merging categories
Country_2015 <- Happiness_2016[,c(1:2)]
Happiness_2017 <- merge(Country_2015, Happiness_2017, all.y=TRUE)
Happiness_2017 <- Happiness_2017[order(Happiness_2017$Happiness.Rank),]
Happiness_2017 <- (setattr(Happiness_2017, "row.names", c(1:155)))
Happiness_2017$Country[c(33,71)]<-c("Taiwan","Hong Kong")
Happiness_2017$Region[c(33,71)]<-c("Southeastern Asia","Southeastern Asia")
Happiness_2017$Region[c(113,139,155)]<-c("Sub-Saharan Africa", "Sub-Saharan Africa", "Sub-Saharan Africa")
Happiness_2017 <-Happiness_2017 %>% mutate_if(is.numeric, round, digits=5)
Happiness_2015 <- Happiness_2015[,-5]
Happiness_2016 <- Happiness_2016[,c(-5,-6)]
Happiness_2017 <- Happiness_2017[,c(-5,-6)]

#message data can be imported and loaded on the server side (the same is true for notifications and status bar)
messageData <- data.frame(
  from = c("Admininstrator", "Jeanne"),
  message = c(
    "Try clicking on mapping the data.",
    "Upload your own data"
  ),
  stringsAsFactors = FALSE
)
#start of the shiny server function
shinyServer(function(input, output){

#outputting the message into the dashboard header
output$messageMenu <- renderMenu({

  msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]],
                 message = row[["message"]])
    })
  dropdownMenu(type = "messages", .list = msgs)
})

#info boxes are rendered and can also contain calculated values as well as text
output$about<- renderInfoBox({
  infoBox(title = tags$b("About the data:"),
          subtitle = tags$b("The World Happiness Data is a survey by the United Nations.
                            Citizens from more than a hundred and fifty countries
                            rate their own perceived level of happiness on a scale of 
                            1 to 10 based on categories including the economy, government trust, freedom, 
                            generosity, and family life. A weighted average of these categories is then
                            taken to create a happiness score."),
          icon=icon("angle-double-right"), color = "navy")
})

output$source<- renderInfoBox({
  infoBox(title = tags$b("Data Source:"),
          subtitle = tags$b("These datasets were downloaded from the kaggle website.
                            Click here to connect to the datasets."),
          href = "https://www.kaggle.com/unsdsn/world-happiness",
          icon=icon("angle-double-right"), color = "navy")

})
#value boxes have a slightly different format than info boxes and work in a similar manner
output$mean<- renderValueBox({
  happinessdata = get(input$datachoice)
  mean_c <- round(mean(happinessdata[,{print(input$column)}]),2)
  infoBox(title = "Mean",
          value = mean_c,
          subtitle = "Mean of selected dataset and column",
          fill = TRUE,
          color = "maroon",
          icon = icon("globe"))
})

output$median<- renderValueBox({
  happinessdata = get(input$datachoice)
  median_c <- round(median(happinessdata[,{print(input$column)}]),2)
  infoBox(title = "Median",
          value = median_c,
          subtitle = "Median of selected dataset and column",
          fill = TRUE,
          color = "purple",
          icon = icon("globe"))
})

output$sd<- renderValueBox({
  happinessdata = get(input$datachoice)
  sd_c <- round(sd(happinessdata[,{print(input$column)}]),2)
  infoBox(title = "Standard Deviation",
          value = sd_c,
          subtitle = "Standard deviation of selected dataset and column",
          fill = TRUE,
          color = "green",
          icon = icon("globe"))
})

output$max<- renderValueBox({
  happinessdata = get(input$datachoice)
  max_c <- round(max(happinessdata[,{print(input$column)}]),2)
  infoBox(title = "Maximum Value",
          value = max_c,
          subtitle = "Maximum value of selected dataset and column",
          fill = TRUE,
          color = "teal",
          icon = icon("globe"))
})

#uses the data frame (dt) program to render a data table, this is a tabbed output so there are 3 tabels rendered
output$mydatatable1 <- renderDataTable({
  Happiness_2015
})

output$mydatatable2 <- renderDataTable({
  Happiness_2016
})

output$mydatatable3 <- renderDataTable({
  Happiness_2017
})

#from rworldmap package, each dataset is joined to the Country data using the country column and then results are plotted
output$plotmap1 <- renderPlot({
  sPDF <- joinCountryData2Map(Happiness_2015
                               ,joinCode = "NAME"
                               ,nameJoinColumn = "Country")
  mapCountryData(sPDF,nameColumnToPlot={print(input$cat)})
})

output$plotmap2 <- renderPlot({
  sPDF <- joinCountryData2Map(Happiness_2016
                              ,joinCode = "NAME"
                              ,nameJoinColumn = "Country")
  mapCountryData(sPDF,nameColumnToPlot={print(input$cat)})
})

output$plotmap3 <- renderPlot({
  sPDF <- joinCountryData2Map(Happiness_2017
                              ,joinCode = "NAME"
                              ,nameJoinColumn = "Country")
  mapCountryData(sPDF,nameColumnToPlot={print(input$cat)})
})

#an example of a plotly histogram along with bin inputs, variable and dataset choice
output$hist <- renderPlotly({
  histdataset = get(input$histdatachoice)
  ggplot(histdataset, aes_string(x=input$histaxischoice)) + geom_histogram(bins = input$bins)
  
})

#an example of a plotly scatter plot with options to choose dataset and variables
output$scatter <- renderPlotly({
  scatterhappinessdata = get(input$scatterdatachoice)
  ggplot(scatterhappinessdata,aes_string(x=input$xaxiss,y=input$yaxiss, color = "Region")) + geom_point()
  
})
#an example of a plotly boxplot with options to choose dataset and variables
output$boxplot <- renderPlotly({
  boxplothappinessdata = get(input$boxplotdatachoice)
  ggplot(boxplothappinessdata, aes_string(x=boxplothappinessdata$Region, y=input$boxyaxis, 
  color = "Region")) + geom_boxplot() + theme(legend.position="left") + theme(axis.text.x = element_text(angle = 45)) 
})
#an example of a a paired scatterplot using the pairsD3 package with data choices
output$pairs <- renderPairsD3({
  pairsdataset = get(input$pairsdatachoice)
  pairsD3(pairsdataset[,5:11],group=pairsdataset[,2], cex = 1.5, leftmar = 25)
  
})

#shiny dashboard can also be used to download and process other data sets, this code
#gives options for file formats, and then returns a summary statement and information 
#about the data, the image is stored in the WWW file that is included as a folder within
#the overall folder, the format can be used to place images throughout the dashboard
#the image serves as a placeholder until the dataset is loaded
data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()}
  read.table(file=file1$datapath,sep=input$sep,header=input$header,
             stringsAsFactors = input$stringAsFactors)
})
output$filedf<- renderTable({
  if(is.null(data())){return ()}
  input$file
})
output$sum <- renderTable({
  if(is.null(data())){return()}
  summary(data())
})
output$table <- renderTable({
  if(is.null(data())){return()}
  data()
})
output$tb <- renderUI({
  if(is.null(data()))
    h5("Powered by", tags$img(src = "RStudio-Ball.png", height=200, width=200, alt = "Alternate Text"))
  else
    tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data",
                tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
})

})

