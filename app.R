# Made by Riley Waters, Nov 2017 for STAT406 Environmentrics
# Rocky Cities Environmetrics App

# Yes, The whole thing is written in this one file. Sorry
#Hit Run App in the upper right of this screen to run it.
#You might need to open in browser and zoom out a bit to get the best experience


# Automatically makes sure all required packages are installed and attached
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2, plyr)

# Read in the cleaned csv
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
monthList <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
locList <- c("VAN", "KEL", "CAL")
DataAll <- read.csv("all_CLEANED.csv", header = TRUE)
DataAll$Date.Time <- as.Date(DataAll$Date.Time)
DataAll$Month <- factor(DataAll$Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = monthList);
DataAll$City <- factor(DataAll$City, levels = c(1,2,3), labels = locList);


#=====================================================================UI=====================================================================
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Rocky Cities App"),
    dashboardSidebar(
      sidebarMenu(
        
        # Tab names and icons
        menuItem("About", tabName = "about", icon = icon("question-circle-o")),
        menuItem("Data Explorer", tabName = "dataTable", icon = icon("table")),
        menuItem("Temperature", tabName = "temperature", icon = icon("thermometer-three-quarters"),
                 menuSubItem("Monthly", tabName = "t1"),
                 menuSubItem("Monthly Advanced", tabName = "t2"),
                 menuSubItem("Density", tabName = "t3")
                 ),
        menuItem("Prediction Models", tabName = "predictionModels", icon = icon("eye")),
        menuItem("Forecast", tabName = "forecast", icon = icon("line-chart")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("desktop"))
    )
  ),
  dashboardBody(
    tags$head(
      # Font of the top-right header
      tags$style(
        HTML('.main-header .logo {
              font-family: "Source Sans Pro";
              font-size: 24px;'
        )
      )
    ),
    tabItems(
      
      # About content
      tabItem(tabName = "about",
              fluidRow(
                h1("Rocky Cities", 
                 span("Environmetrics App", style = "font-weight: 300"), 
                 style = "font-family: 'Source Sans Pro';
                          color: #fff;
                          text-align: center;
                          background-image: url('texturebg.png');
                          padding: 20px"
                ),
                br()
              ),
              fluidRow(
                column(6, offset = 3,
                 p(style = "font-family: 'Source Sans Pro'; font-size: 18px",
                   "This is an application for interacting with historical weather data from Vancouver BC, Kelowna BC, and Calgary AB. 
                   Despite being close in location, these cities experience unique climates due their positions relative to the Pacific Ocean, Okanagan Valley, Rocky Mountains, and Canadian Praries.
                   Data was collected by",
                   a("Climate Canada", href = "http://climate.weather.gc.ca/"),
                   "at VANCOUVER INTL, KELOWNA, and CALGARY INTL weather stations. For detailed information about each variable, please visit the",
                   a("glossary", href = "http://climate.weather.gc.ca/glossary_e.html"), ".",
                   br(),br(),
                   "By clicking the tabs on the left, you can:"
                 ),
                 tags$ul(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                   tags$li("Sort and filter the data tables"), 
                   tags$li("Interact with data visualizations"), 
                   tags$li("Make predictions for future metrics")
                 ),
                 br(),
                 tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                        "App created by Riley Waters for STAT406 Environmentrics.",
                        br(),
                        "Data cleaned and compiled by Riley Waters. Last updated on November 23, 2017."
                 )
                )
              ),
              br(),br(),
              div(img(src = "skyline.jpg"), style="text-align: center;")
      ),
      
      # Data Explorer tab content
      tabItem(tabName = "dataTable",
              fluidRow(
                column(3, selectInput("tableCity","City:", c("All",unique(as.character(DataAll$City))))
                ),
                column(3, selectInput("tableYear","Year:", c("All",unique(as.character(DataAll$Year))))
                ),
                column(3,selectInput("tableMonth","Month:", c("All",unique(as.character(DataAll$Month))))
                ),
                column(3,selectInput("tableDay","Day:", c("All",unique(as.character(DataAll$Day))))
                )
              ),
              fluidRow(
                DT::dataTableOutput("tableOut")
              ),
              br(),br(),
              fluidRow(
                tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                       "Max.Temp, Min.Temp, Mean.Temp, Heat.Deg.Days, Cool.Deg.Days:", tags$strong("Celcius"),
                       br(),
                       "Total.Rain, Total.Precip:", tags$strong("mm"),
                       br(),
                       "Snow.on.Grnd, Total.Snow:", tags$strong("cm"),
                       br(),
                       "Spd.of.MaxGust:", tags$strong("km/h")
                )
              )
      ),
      
      
      
      # Temperatures tab content
      tabItem(tabName = "t1",
              fluidPage(
                column(width = 6,
                  box(title = "Data Sample Selector", status = "warning", collapsible = FALSE, collapsed = FALSE,width = 12,
                      sliderInput(
                        "range", "Years of data to sample from:", min = 1900, 
                          max = 2017, value = c(1900,2017), sep=""
                      ),
                      selectInput("opt.mmm", "Select which temperature set to visualize:",
                                  list("Mean Temperature" = "meanT",
                                       "Max Temperature" = "maxT", 
                                       "Min Temperature" = "minT"),                 
                                  selected="meanT")
                      
                  ),
                  box(title = "Temperatures Chart by Month", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,width = 12,
                      plotOutput("t1.1.Out")
                  )
                  
                ),
                
                column(width = 6,
                  box(title = "Temperature Graph by Month", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                      plotOutput("t1.2.Out", height=500)
                  ),
                  box(title = "Absolute Max and Min", status = "primary", solidHeader = TRUE,width = 12,
                      h4(style = "font-familt:Source Sans Pro", textOutput("t1.21.Out")),
                      h4(style = "font-familt:Source Sans Pro", textOutput("t1.22.Out"))
                  )
                )
              )
      ),
      
      # Next Tab
      tabItem(tabName = "t2",
              fluidRow(
                column(width = 6,
                  box(title = "Data Sample Selector", status = "warning", collapsible = FALSE, collapsed = FALSE,width = 12,
                      sliderInput(
                        "range2", "Years of data to sample from:", min = 1900, 
                        max = 2017, value = c(1900,2017), sep=""
                      )
                  ),
                  box(title = "Box Grid", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                      plotOutput("t1.4.Out", height=600)
                  )
                ),
                column(width = 6,
                  box(title = "Averages of Daily Highs and Lows", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                      plotOutput("t1.3.Out", height = 700)
                  )
                )
              )
      ),
      
      # Next Tab
      tabItem(tabName = "t3"
      ),
      
      # Forecasting tab content
      tabItem(tabName = "forecast"
        
      ),
      
      # Dashboard content
      tabItem(tabName = "dashboard"
              
      )
      
    )
  )
)


#=====================================================================SERVER=====================================================================
server <- function(input, output) {
  
  # returns a summarized dataframe with only City, Month, and Temperatures
  # MeanDMax and MeanDMin are the averages of the maximum and minimum temperatures
  summarized.df <- reactive({
    df <- subset(DataAll, Year >= input$range[1] & Year <= input$range[2])
    monthly.df <- ddply(df,.(City, Month), summarize,    
                        meanT= round(mean(Mean.Temp, na.rm = TRUE),1) ,
                        maxT = round(max(Max.Temp, na.rm = TRUE),0) ,    
                        minT = round(min(Min.Temp, na.rm = TRUE),0),
                        MeanDmax = round(mean(Max.Temp, na.rm = TRUE),1),
                        MeanDmin = round(mean(Min.Temp, na.rm = TRUE),1))
    return(monthly.df) 
  })
  summarized2.df <- reactive({
    df <- subset(DataAll, Year >= input$range2[1] & Year <= input$range2[2])
    monthly.df <- ddply(df,.(City, Month), summarize,    
                        meanT= round(mean(Mean.Temp, na.rm = TRUE),1) ,
                        maxT = round(max(Max.Temp, na.rm = TRUE),0) ,    
                        minT = round(min(Min.Temp, na.rm = TRUE),0),
                        MeanDmax = round(mean(Max.Temp, na.rm = TRUE),1),
                        MeanDmin = round(mean(Min.Temp, na.rm = TRUE),1))
    return(monthly.df) 
  })
  # Data table content
  output$tableOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {
      if(input$tableCity == "VAN"){
        tableData <- subset(DataAll, DataAll$City == "VAN")}
      else if(input$tableCity == "KEL"){
        tableData <- subset(DataAll, DataAll$City == "KEL")}
      else if(input$tableCity == "CAL"){
        tableData <- subset(DataAll, DataAll$City == "CAL")}
      else{
        tableData <- DataAll
      }
      if (input$tableYear != "All") {
        tableData <- tableData[tableData$Year == input$tableYear,]
      }
      if (input$tableMonth != "All") {
        tableData <- tableData[tableData$Month == input$tableMonth,]
      }
      if (input$tableDay != "All") {
        tableData <- tableData[tableData$Day == input$tableDay,]
      }
      tableData
    }
  ))
  
  # T1.1 Content
  fun1.1 <- function(df, str.column.to.plot) {
    smalldf<-df
    if(str.column.to.plot == "meanT")
      colPlot<-smalldf$meanT
    if(str.column.to.plot == "minT")
      colPlot<-smalldf$minT
    if(str.column.to.plot == "maxT")
      colPlot<-smalldf$maxT
    p<- ggplot(data=smalldf, aes(factor(Month, levels=monthList), City, color=meanT))
    p<-p + geom_text(size=10, label=as.character(round(colPlot, 0)))
    p<- p + scale_color_gradient(low="blue", high="orange")
    p <- p + theme(panel.background = element_rect(fill= "transparent"))
    p<- p+xlab("Month")
    return(p)
  }
  output$t1.1.Out <- renderPlot({
    smalldf <- summarized.df()
    mPlot <- fun1.1(smalldf,input$opt.mmm)
    print(mPlot)
  })
  
  #T1.2 Content
  fun1.2 <- function(df, str.column.to.plot) {
    if(str.column.to.plot == "meanT")
      str<-"Mean Temperature(C)"
    if(str.column.to.plot == "maxT")
      str<-"Max Temperature(C)"
    if(str.column.to.plot == "minT")
      str<-"Min Temperature(C)"
    p<- ggplot(data=df, aes_string(x="Month", y=str.column.to.plot, group="City", color="City"))  
    p<- p+geom_point(size=5)
    p<- p+geom_line(size=1, alpha=0.9)
    p<- p + geom_hline(yintercept=c(-40,-30,-20,-10,0,10,20,30,40))
    p<- p + theme(panel.background = element_rect(fill= "transparent"))
    p<- p+ylab(paste(str))
    p<- p+xlab("Month")
    p<- p+labs(title=paste(str, "by Month"))
    return(p)
  }
  output$t1.2.Out <- renderPlot({    
    smalldf <- summarized.df()
    mPlot <- fun1.2(smalldf, input$opt.mmm) #meanT = 3, maxT=4, minT=5
    print(mPlot)    
  })
  
  # T1.21 Content
  output$t1.21.Out <- renderPrint({
    smalldf <- summarized.df()
    maxMax <- max(smalldf$maxT)
    rowMax <- which(grepl(maxMax, DataAll$Max.Temp))
    maxDate <- DataAll[rowMax[1],2]
    maxName <- DataAll[rowMax[1],1]
    cat("The highest high between", input$range[1], "and", input$range[2],"is", maxMax, " Celsius, occuring on",paste(maxDate), "in", paste(maxName))
  })
  output$t1.22.Out <- renderPrint({
    smalldf <- summarized.df()
    minMin <- min(smalldf$minT)
    rowMin <- which(grepl(minMin, DataAll$Min.Temp))
    minDate <- DataAll[rowMin[1],2]
    minName <- DataAll[rowMin[1],1]
    cat("The lowest low between", input$range[1], "and", input$range[2],"is", minMin, " Celsius, occuring on",paste(minDate),"in", paste(minName))
  })
  
  # T1.3 Content
  fun1.3 <- function(df) {
    p <- ggplot(df) 
    p <- p + geom_linerange(aes(x=City, 
                                ymin=MeanDmin, ymax=MeanDmax, 
                                color=City, size=3)) + coord_flip()
    p <- p + facet_grid(Month ~ .)
    p <- p + xlab("Mean of Daily_Minimum Temperature to Mean of Daily_Maximum Temperature") 
    p <- p + theme( #eliminate background, gridlines, and chart border
      plot.background = element_blank()
      ,panel.background = element_blank()
      ,panel.grid.major = element_line(colour="blue", size=0.5)
      ,panel.grid.minor = element_line(colour="black", size=0.3)
      ,axis.ticks=element_blank()
      ,axis.title.y=element_blank()
      ,axis.text.x =element_text(colour="grey20",angle=0,hjust=.5,vjust=.5,face="plain")
      ,legend.position="none"
    )
    return(p)
  }
  output$t1.3.Out <- renderPlot({
    monthly.df <- summarized2.df()    
    MMbar <- fun1.3(monthly.df)
    print(MMbar)
  }, height=700)
  
  # T1.4 Content
  output$t1.4.Out <- renderPlot({    
    smalldf <- summarized2.df()
    p <- ggplot(data=smalldf, aes(x = factor(Month, levels=monthList),
                                  y=meanT,
                                  ymin=minT, ymax=maxT))
    p <- p + geom_crossbar(width=0.2, fill="red")
    p <- p + geom_text(data=smalldf, aes(y=maxT+5, label=maxT), color="red")
    p <- p + geom_text(data=smalldf, aes(y=minT-5, label=minT), color="blue")
    p <- p + facet_grid(City ~ .)
    p <- p + xlab("Month") + ylab("Temperature")
    print(p)
  })  
}

shinyApp(ui, server)