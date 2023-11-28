library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(scales)

df <- read.csv("Daily Particulate Air Quality.csv")

df1 <- select(df, Date, Daily.Mean.PM2.5.Concentration.ug.m3, Daily.PM2.5.AQI.Value, COUNTY)
df1 <- df1 %>% filter(!is.na(Daily.Mean.PM2.5.Concentration.ug.m3))

df2.5 <- df1 %>%
  group_by(Date, COUNTY) %>%
  summarize(avg_Daily_Mean_PM2.5_Concentration = mean(Daily.Mean.PM2.5.Concentration.ug.m3),
            avg_Daily_PM2.5_AQI_Value = mean(Daily.PM2.5.AQI.Value))
df2.5$Datef <- as.Date(df2.5$Date, format = "%m/%d/%Y")
df2.5$Year <- year(df2.5$Datef)
df2.5sg <- df2.5 %>% 
  mutate(Month = month(as.Date(Date, "%m/%d/%Y"))) %>% 
  mutate(Season = ifelse(Month %in% c(3, 4, 5), "Spring",
                         ifelse(Month %in% c(6, 7, 8), "Summer",
                                ifelse(Month %in% c(9, 10, 11), "Fall",
                                       ifelse(Month %in% c(12, 1, 2), "Winter"))))) %>%
  group_by(COUNTY, Season, Year) %>% 
  summarise(Percent = sum(ifelse(avg_Daily_PM2.5_AQI_Value <= 50, 1, 0))/n()*100)

df2 <- select(df, Date, Daily.Mean.PM10.Concentration.ug.m3, Daily.PM10.AQI.Value, COUNTY)
df2 <- df2 %>% filter(!is.na(Daily.Mean.PM10.Concentration.ug.m3))
df10 <- df2 %>%
  group_by(Date, COUNTY) %>%
  summarize(avg_Daily_Mean_PM10_Concentration = mean(Daily.Mean.PM10.Concentration.ug.m3),
            avg_Daily_PM10_AQI_Value = mean(Daily.PM10.AQI.Value))
df10$Datef <- as.Date(df10$Date, format = "%m/%d/%Y")
df10$Year <- year(df10$Datef)
df10sg <- df10 %>% 
  mutate(Month = month(as.Date(Date, "%m/%d/%Y"))) %>% 
  mutate(Season = ifelse(Month %in% c(3, 4, 5), "Spring",
                         ifelse(Month %in% c(6, 7, 8), "Summer",
                                ifelse(Month %in% c(9, 10, 11), "Fall",
                                       ifelse(Month %in% c(12, 1, 2), "Winter"))))) %>%
  group_by(COUNTY, Season, Year) %>% 
  summarise(Percent = sum(ifelse(avg_Daily_PM10_AQI_Value <= 50, 1, 0))/n()*100)



ui <- dashboardPage(
  dashboardHeader(title = "Arizona Air Quality Dashboard"),
  dashboardSidebar(
    label = p("Air quality in different Arizona counties between 2017 and 2022 measured through fine particles concentration in the air. United States Environment Protection Agency (EPA) has categorized good air quality at ≤ 12.0",HTML("µg/m<sup>3</sup>"),", 24-hour average fine particle concentration for particulate matter 2.5",HTML("(PM<sub>2.5</sub>)")," and ≤ 54.0",HTML("µg/m<sup>3</sup>"),", 24-hour average for particulate matter 10",HTML("(PM<sub>10</sub>)"),".", style = "text-align: justify; padding: 5px;"),  
    selectInput("countyInput", label = "Select a County",
                choices = unique(df$COUNTY)),
    selectInput("yearInput", label = "Select a Year",
                choices = unique(df2.5$Year)),
    icon = icon("info-circle", style = "padding: 5px;"),
    label = p("1. Data on both",HTML("PM<sub>2.5</sub>")," and",HTML("PM<sub>10</sub>")," not available for all counties as not all stations measured both types of particulate matter.", br(),"2. Data from different stations have been averaged for each county, separately for",HTML("PM<sub>2.5</sub>")," and",HTML("PM<sub>10</sub>"),".", br(),"3. For Cochise County, data on",HTML("PM<sub>2.5</sub>"),"  available till 2019.",br(),"4. Seasons based on meteorological definition.", style = "text-align: justify; padding: 5px;"),
    label = p("Data Source:",br(), "https://www.epa.gov/outdoor-air-quality-data/air-quality-index-daily-values-report", style = "text-align: justify; padding: 5px;")),
  dashboardBody(
    fluidRow(
      column(12, verbatimTextOutput("seasonOutput"))
      ),
    fluidRow(
      column(12, verbatimTextOutput("seasonOutput2"))
    ),
    box(
      title = "Particulate Matter 2.5",
      width = 12,
      height = 500,
      solidHeader = TRUE,
      status = "primary",
      collapsible = FALSE,
      fluidRow(
        column(width = 6, plotlyOutput("plotly1")),
        column(width = 6, plotlyOutput("plotly2"))
      ),
      fluidRow(
        column(6, verbatimTextOutput("C1")),
        column(6, verbatimTextOutput("C2"))
      ),
    ),
    box(
      title = "Particulate Matter 10",
      width = 12,
      height = 500,
      solidHeader = TRUE,
      status = "primary",
      collapsible = FALSE,
      fluidRow(
        column(width = 6, plotlyOutput("plotly3")),
        column(width = 6, plotlyOutput("plotly4"))
      ),
      fluidRow(
        column(6, verbatimTextOutput("C3")),
        column(6, verbatimTextOutput("C4"))
      ),
    )
  )
)

max_pm <- function() {
  df <- plotData4()
  max(df$avg_Daily_Mean_PM10_Concentration)
}


server <- function(input, output) {
  
  plotData <- reactive({
    subset(df2.5sg, COUNTY == input$countyInput & Year == input$yearInput)
  })

  plotData2 <- reactive({
    subset(df2.5, COUNTY == input$countyInput & Year == input$yearInput)
  })
  
  plotData3 <- reactive({
    subset(df10sg, COUNTY == input$countyInput & Year == input$yearInput)
  })
  
  plotData4 <- reactive({
    subset(df10, COUNTY == input$countyInput & Year == input$yearInput)
  })
 

   
 output$seasonOutput <- renderPrint({
   county_data <- plotData() %>%
     filter(COUNTY == input$countyInput & Year == input$yearInput)
   
   if(nrow(county_data) == 0) {
     return(NA)
   }
   max_pct <- max(plotData()$Percent)
   max_seasons <- plotData() %>%
     filter(Percent == max_pct) %>%
     pull(Season)
     
   if (length(max_seasons) == 1) {
     cat("Season with the highest percentage of days with good air quality in", input$countyInput, "County in", input$yearInput, "based on PM₂.₅ concentration is:", max_seasons)
   } else {
     cat("Seasons with the highest percentage of days with good air quality in", input$countyInput, "County in", input$yearInput, "based on PM₂.₅ concentration are:", paste(max_seasons, collapse = ", "))
     }
   })

 output$seasonOutput2 <- renderPrint({
   county_data <- plotData3() %>%
     filter(COUNTY == input$countyInput & Year == input$yearInput)
   
   if(nrow(county_data) == 0) {
     return(NA)
   }
   max_pct <- max(plotData3()$Percent)
   max_seasons <- plotData3() %>%
     filter(Percent == max_pct) %>%
     pull(Season)
   
   if (length(max_seasons) == 1) {
     cat("Season with the highest percentage of days with good air quality in", input$countyInput, "County in", input$yearInput, "based on PM₁₀ concentration is:", max_seasons)
   } else {
     cat("Seasons with the highest percentage of days with good air quality in", input$countyInput, "County in", input$yearInput, "based on PM₁₀ concentration are:", paste(max_seasons, collapse = ", "))
   }
 })
 
 output$C1 <- renderPrint({cat("Figure 1a: Bar plot shows percentage of days in each season with average daily PM₂.₅ concentration at most 12 µg/m³.")})
 output$C2 <- renderPrint({cat("Figure 1b: Line plot shows daily average PM concentration. Green dashed line is the maximum breakpoint for good air quality.")})
 output$C3 <- renderPrint({cat("Figure 2a: Bar plot shows percentage of days in each season with average daily PM₁₀ concentration at most 54 µg/m³.")})
 output$C4 <- renderPrint({cat("Figure 2b: Line plot shows daily average PM concentration. Green dashed line is the maximum breakpoint for good air quality.")})
 
  output$plotly1 <- renderPlotly({
    p <- plot_ly(plotData(), x = ~Percent, y = ~Season, type = "bar", marker = list(color = "#229954"))
    p <- p %>% layout(
      xaxis = list(title = "% Days", titlefont = list(size = 14), range = c(0, 100)),
      yaxis = list(title = "Season", categoryorder = "array", categoryarray = c("Winter", "Fall", "Summer", "Spring"), titlefont = list(size = 14)),
      title = list(
        text = "Percentage of Days with Good Air Quality by Season",
        font = list(size = 14)),
      margin = list(l = 50, r = 50, b = 40, t = 90, pad = 4),
      bargap = 0.75
    )
  })
  
  output$plotly2 <- renderPlotly({
    p <- plot_ly(plotData2(), x = ~Datef, y = ~avg_Daily_Mean_PM2.5_Concentration, type = "scatter", mode = "line", name = "Day's average")
    p <- p %>% add_trace(y = c(12), type = "scatter", mode = "line", line = list(color = "#229954", dash = "dash"), name = "EPA standard")
    p <- p %>% layout(
      xaxis = list(title = "Date", titlefont = list(size = 14)),
      yaxis = list(title = "µg/m<sup>3</sup>", titlefont = list(size = 14)),
      title = list(
        text = "Daily Average PM<sub>2.5</sub> Concentration", font = list(size = 14)),
      margin = list(l = 50, r = 50, b = 50, t = 90, pad = 4)
    )
  })
  
  output$plotly3 <- renderPlotly({
    p <- plot_ly(plotData3(), x = ~Percent, y = ~Season, type = "bar", marker = list(color = "#229954"))
    p <- p %>% layout(
      xaxis = list(title = "% Days", titlefont = list(size = 14), range = c(0, 100)),
      yaxis = list(title = "Season", categoryorder = "array", categoryarray = c("Winter", "Fall", "Summer", "Spring"), titlefont = list(size = 14)),
      title = list(
        text = "Percentage of Days with Good Air Quality by Season",
        font = list(size = 14)),
      margin = list(l = 50, r = 50, b = 40, t = 90, pad = 4),
      bargap = 0.75
    )
  })
  
  
  output$plotly4 <- renderPlotly({
    p <- plot_ly(plotData4(), x = ~Datef, y = ~avg_Daily_Mean_PM10_Concentration, type = "scatter", mode = "line", name = "Day's average")
    p <- p %>% add_trace(y = c(54), type = "scatter", mode = "line", line = list(color = "#229954", dash = "dash"), name = "EPA standard")
    p <- p %>% layout(
      xaxis = list(title = "Date", titlefont = list(size = 14)),
      yaxis = list(title = "µg/m<sup>3</sup>", titlefont = list(size = 14)),
      title = list(
        text = "Daily Average PM<sub>10</sub> Concentration", font = list(size = 14)),
      margin = list(l = 50, r = 50, b = 50, t = 90, pad = 4)
    )
  })
  

  
  
  
  observe(print(plotData4()))
}

shinyApp(ui, server)