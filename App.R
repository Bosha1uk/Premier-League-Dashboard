#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(plotly)
library(rsconnect)
#rsconnect::deployApp('path/to/your/app')

source("combinedStats.R")
source("1920.R")

# Define UI
ui <- fluidPage(HTML('<meta name="viewport" content="width=1024">'), 
   
  dashboardPage(
    dashboardHeader(title = "Premier League Data", titleWidth = 300),
    dashboardSidebar(
                     selectInput("Team", "Team: ", c("All", plDataAll3$HomeTeam)), width = 300),
                     #selectInput("Matchday", "Matchday: ", c("Latest", plFixturesResultsData1819$Matchday)), width = 300),
    
    dashboardBody(uiOutput("Overview"))
    )
  )

# Define server logic
server <- function(input, output) {

  ############
  ### Tabs ###
  ############
  
  output$Overview <- renderUI({
    if(input$Team == "All")
    {
      tabsetPanel(
        tabPanel("Table", #selectInput("Season", "Season: ", c("2019/20", "2018/19", "2017/18", "2016/17", "2015/16")),
                 dateRangeInput(inputId = "date",
                                strong("Date Range"),
                                start = "2019-08-01", end = Sys.Date(),
                                min = "2015-08-01", max =Sys.Date()),
                 #actionButton("s1920", "2019/20"),
                 tabsetPanel(

                     tabPanel("Overall", uiOutput("TableDate")),
                     tabPanel("Home",  uiOutput("HomeTableDate")),
                     tabPanel("Away",  uiOutput("AwayTableDate")),
                     tabPanel("1st Half of Match", uiOutput("Tablehalf1Match")),
                     tabPanel("2nd Half of Match", uiOutput("Tablehalf2Match")),
                     tabPanel("Big 6", uiOutput("Top6")),
                     tabPanel("Excluding Big 6", uiOutput("ExTop6")),
                     tabPanel("Against Big 6", uiOutput("AgTop6")),
                     tabPanel("Big 6 vs the rest", uiOutput("Top6VS"))
                     )
                   ),
        tabPanel("Results", dateInput(inputId = "fDate",
                                           strong("Date:"),
                                      value=max(plDataAll$Date),
                                      min = "2015-08-01", max =Sys.Date()),uiOutput("FixturesResults")),
        tabPanel("Stats 19/20", tabsetPanel(
          tabPanel("Overview", uiOutput('Stats')),
          tabPanel("Goals Scored", uiOutput('GoalsScored')),
          tabPanel("Goals Conceeded", uiOutput('GoalsConceeded')),
          tabPanel("Clean Sheets", uiOutput('CleanSheets'))
          )
          ),
        tabPanel("Graphs 19/20", 
                 fluidRow(column(width=12, box(plotlyOutput("goals_scored"), title = "Goals Scored per Team", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px"))
                 , column(width=12, box(plotlyOutput("goals_conceded"), title = "Goals Conceeded per Team", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px"))
                 , column(width=12, box(plotlyOutput("goal_difference"), title = "Goal Difference per Team", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px"))
                 , column(width=12, box(plotlyOutput("Average_Shots"), title = "Average Shots per Game", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px"))
                 , column(width=12, box(plotlyOutput("Average_Shots_Faced"), title = "Average Shots Faced per Game", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px"))
                 , column(width=12, box(plotlyOutput("Average_Shots_Comp"), title = "Average Shots Comparison per Game", background = "blue", solidHeader = TRUE, collapsible = TRUE, width = "1920px"))
        ))
      )
    }
    else
    {
      tabsetPanel(
        tabPanel("Overview 19/20", #dateRangeInput(inputId = "date",
                              #              strong("Date Range"),
                               #             start = "2019-08-01", end = Sys.Date(),
                                #            min = "2015-08-01", max =Sys.Date()), 
                 uiOutput("TeamStats")),
        #tabPanel("Goals", uiOutput("pGoals")),
        tabPanel("Results", uiOutput("ClubFixturesResults")),
        tabPanel("Form", sliderInput("rows1", "Matches:", min=1, max=100, value=10,
                                     step=1),
        tabsetPanel(
          tabPanel("Overall",  uiOutput("ClubLastMatchOverall")),
          tabPanel("Home",  uiOutput("ClubLastMatchHome")),
          tabPanel("Away",  uiOutput("ClubLastMatchAway"))
        )),
        #tabPanel("Graphs", box(plotlyOutput("Average_Shots"))),
        tabPanel("Season Comparison", sliderInput("rows2", "Matches:", min=1, max=38, value=13,
                                                  step=1),
                 fluidRow(column(width=12, box(uiOutput("ClubComparison1920"), title = "Season 19/20", solidHeader = TRUE, collapsible = TRUE, width = "1920px")),
                                 column(width=12, box(uiOutput("ClubComparison1819"), title = "Season 18/19", solidHeader = TRUE, collapsible = TRUE, width = "1920px")))
                 
      )
      )
    }
  })
  
  sliderValuesOverall <- reactive({input$rows1})
  
  sliderValuesComp <- reactive({input$rows2})
  
  sliderValuesDate <- reactive({input$date})
  
  homeTableFilter <- function(y) {
    x <- y %>%
      dplyr::filter(HomeTeam == input$Team) %>%
      dplyr::filter(between(row_number(), 1, input$rows1)) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG), CS = sum(HOME_CS)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    x <- x[order(-x$PTS, -x$GD, -x$GF),]
  }
  
  awayTableFilter <- function(y) {
    x <- y %>%
      dplyr::filter(AwayTeam == input$Team) %>%
      dplyr::filter(between(row_number(), 1, input$rows1)) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG), CS = sum(AWAY_CS)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    x <- x[order(-x$PTS, -x$GD, -x$GF),]
  }
  
  ##############
  ### Graphs ###
  ##############

  output$MonthlyPerformance <- renderPlotly({
    if(input$Season == "2019/20")
    {
      
      plMonth1920$MONTH <- factor(plMonth1920$MONTH, levels = c("August", "September", "October", "November"
                                                                , "December", "January", "February", "March"
                                                                , "April", "May"))
      
      plot_ly(plMonth1920[plMonth1920$TEAM == input$Team,],
              x = ~MONTH, 
              y = ~AVG_PPG, 
              
              type = 'bar', text = ~AVG_PPG_P, textposition = 'auto',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                            categoryorder = "array",
                            categoryarray = ~AVG_PPG),
               yaxis = list(title = "", range = c(0, 3), fixedrange=TRUE))
      
      
    }
    else if(input$Season == "2018/19")
    {
      
      plMonth1819$MONTH <- factor(plMonth1819$MONTH, levels = c("August", "September", "October", "November"
                                                                , "December", "January", "February", "March"
                                                                , "April", "May"))
      
      plot_ly(plMonth1819[plMonth1819$TEAM == input$Team,],
              x = ~MONTH, 
              y = ~AVG_PPG, 
              
              type = 'bar', text = ~AVG_PPG_P, textposition = 'auto',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(xaxis = list(title = "", tickangle = -90,
                            categoryorder = "array",
                            categoryarray = ~AVG_PPG),
               yaxis = list(title = "", range = c(0, 3)))
      
      
    }
  })
  
  output$MonthlyPoints <- renderPlotly({
    if(input$Season == "2019/20")
    {
      plot_ly(plMonth1920[plMonth1920$TEAM == input$Team,],
            x = ~MONTH, 
            y = ~PTS, 
            
            type = 'bar', text = ~PTS, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                          categoryorder = "array",
                          categoryarray = ~PTS),
             yaxis = list(title = "", range = c(0, 21), fixedrange=TRUE))
    }
    else if(input$Season == "2018/19")
    {
      plot_ly(plMonth1819[plMonth1819$TEAM == input$Team,],
              x = ~MONTH, 
              y = ~PTS, 
              
              type = 'bar', text = ~PTS, textposition = 'auto',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                            categoryorder = "array",
                            categoryarray = ~PTS),
               yaxis = list(title = "", range = c(0, 21), fixedrange=TRUE))
    }
  }) 
  
  output$goals_scored <- renderPlotly({
    
    plot_ly(plTable1920, x = ~TEAM, 
                                      y = ~GF, 
                                      
                                      type = 'bar', text = ~GF, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
             categoryorder = "array",
             categoryarray = ~GF),
             yaxis = list(title = "", fixedrange=TRUE))
  })
  
  output$goals_conceded <- renderPlotly({
    plot_ly(plTable1920, x = ~TEAM, 
              y = ~GA, 
              
              type = 'bar', text = ~GA, textposition = 'auto',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                            categoryorder = "array",
                            categoryarray = ~GA),
               yaxis = list(title = "", fixedrange=TRUE))
  })
  
  output$goal_difference <- renderPlotly({
    plot_ly(plTable1920, x = ~TEAM, 
              y = ~GD, 
              
              type = 'bar', text = ~GD, textposition = 'auto',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                            categoryorder = "array",
                            categoryarray = ~GD),
               yaxis = list(title = "", fixedrange=TRUE))
    })
  
  output$stats_comparison <- renderPlotly({
    plot_ly(plStats1920, x = ~ST, 
              type = 'bar', fixedrange=TRUE)
    })
  
  output$Average_Shots <- renderPlotly({
    
    plot_ly(plAShotsGraph1920, x = ~TEAM, 
            y = ~AVG_SHOTS, 
            
            type = 'bar', text = ~AVG_SHOTS, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                          categoryorder = "array",
                          categoryarray = ~AVG_SHOTS),
             yaxis = list(title = "", fixedrange=TRUE))
    
  })
  
  output$Average_Shots_Faced <- renderPlotly({
    
    plot_ly(plAShotsFacedGraph1920, x = ~TEAM, 
            y = ~AVG_SHOTS_FACED, 
            
            type = 'bar', text = ~AVG_SHOTS_FACED, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                          categoryorder = "array",
                          categoryarray = ~AVG_SHOTS_FACED),
             yaxis = list(title = "", fixedrange=TRUE))
    
  })
  
  output$Average_Shots_Comp <- renderPlotly({
    
    plot_ly(plShotsComp1920, x = ~TEAM, 
            y = ~AVG_COMP, 
            
            type = 'bar', text = ~AVG_COMP, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(xaxis = list(title = "", tickangle = -90, fixedrange=TRUE,
                          categoryorder = "array",
                          categoryarray = ~-AVG_COMP),
             yaxis = list(title = "", fixedrange=TRUE))
    
  })
  
  
  #################
  ### Render UI ###
  #################
  
  output$Stats <- renderUI({
      tagList(
        valueBox("Most Goals Scored", value = tags$p(paste(plMaxGoalsScored1920$TEAM, " - ", plMaxGoalsScored1920$FTG), style = "font-size: 50%;"), width = 3),
        valueBox("Most Goals Conceeded", value = tags$p(paste(plMaxGoalsConceeded1920$TEAM, " - ", plMaxGoalsConceeded1920$FTGC), style = "font-size: 50%;"), width = 3),
        valueBox("Most Shots", value = tags$p(paste(plMaxShots1920$TEAM, " - ", plMaxShots1920$S), style = "font-size: 50%;"), width = 3),
        valueBox("Most Shots Faced", value = tags$p(paste(plMaxShotsFaced1920$TEAM, " - ", plMaxShotsFaced1920$SF), style = "font-size: 50%;"), width = 3),
        valueBox("Most Shots on Target", value = tags$p(paste(plMaxShotsTarget1920$TEAM, " - ", plMaxShotsTarget1920$ST), style = "font-size: 50%;"), width = 3),
        valueBox("Most Shots on Target Faced", value = tags$p(paste(plMaxShotsTargetFaced1920$TEAM, " - ", plMaxShotsTargetFaced1920$STF), style = "font-size: 50%;"), width = 3),
        valueBox("Most Yellow Cards", value = tags$p(paste(plMaxYellowCards1920$TEAM, " - ", plMaxYellowCards1920$Y), style = "font-size: 50%;"), width = 3),
        valueBox("Least Goals Scored", value = tags$p(paste(plMinGoalsScored1920$TEAM, " - ", plMinGoalsScored1920$FTG), style = "font-size: 50%;"), width = 3),
        valueBox("Least Goals Conceeded", value = tags$p(paste(plMinGoalsConceeded1920$TEAM, " - ", plMinGoalsConceeded1920$FTGC), style = "font-size: 50%;"), width = 3),
        valueBox("Least Shots", value = tags$p(paste(plMinShots1920$TEAM, " - ", plMinShots1920$S), style = "font-size: 55%;"), width = 3),
        valueBox("Least Shots on Target", value = tags$p(paste(plMinShotsTarget1920$TEAM, " - ", plMinShotsTarget1920$ST), style = "font-size: 50%;"), width = 3),
        valueBox("Least Yellow Cards", value = tags$p(paste(plMinYellowCards1920$TEAM, " - ", plMinYellowCards1920$Y), style = "font-size: 50%;"), width = 3)
      )
  })
  
  output$TeamStats <- renderUI({
    
      tagList(
        valueBox("Goals Scored", value = tags$p(paste(plStats1920$FTG[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Goals Conceeded", value = tags$p(paste(plStats1920$FTGC[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Shots", value = tags$p(paste(plStats1920$S[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Shot Conversion Rate", value = tags$p(paste(plStats1920$SCR[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Shots Faced", value = tags$p(paste(plStats1920$SF[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Shots on Target", value = tags$p(paste(plStats1920$ST[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Shots on Target Faced", value = tags$p(paste(plStats1920$STF[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Shots on Target Conversion Rate", value = tags$p(paste(plStats1920$STCR[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3),
        valueBox("Yellow Cards", value = tags$p(paste(plStats1920$Y[plStats1920$TEAM == input$Team]), style = "font-size: 50%;"), width = 3)
      )
  })
  
  output$GoalsScored <- renderUI({
    tagList(
        box(DT::dataTableOutput("goalsScored1920"), width = 4),
        box(DT::dataTableOutput("homegoalsScored1920"), width = 4),
        box(DT::dataTableOutput("awaygoalsScored1920"), width = 4))
  })
  
  output$GoalsConceeded <- renderUI({
    tagList(
        box(DT::dataTableOutput("goalsConceeded1920"), width = 4),
        box(DT::dataTableOutput("homegoalsConceeded1920"), width = 4),
        box(DT::dataTableOutput("awaygoalsConceeded1920"), width = 4))
  })
  
  output$CleanSheets <- renderUI({
      tagList(
        box(DT::dataTableOutput("cleanSheets1920"), width = 4),
        box(DT::dataTableOutput("homecleanSheets1920"), width = 4),
        box(DT::dataTableOutput("awaycleanSheets1920"), width = 4))
    })
  
  output$YellowCards <- renderUI({
    tagList(
        box(DT::dataTableOutput("YellowCards1920"), width = 4),
        box(DT::dataTableOutput("homeYellowCards1920"), width = 4),
        box(DT::dataTableOutput("awayYellowCards1920"), width = 4))
    })
  
  output$Tablehalf1Match <- renderUI({
    
    sliderValuesDate()
    
    plHomeHT1920 <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!is.na(HTHG)) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HT_HOME_WIN), D = sum(HT_DRAW), L = sum(HT_HOME_LOSS), GF = sum(HTHG), GA = sum(HTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    
    plAwayHT1920 <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!is.na(HTAG)) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HT_AWAY_WIN), D = sum(HT_DRAW), L = sum(HT_AWAY_LOSS), GF = sum(HTAG), GA = sum(HTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    
    plTableHT <- union_all(plHomeHT1920, plAwayHT1920)
    
    plTableHT[is.na(plTableHT)] <- 0
    plTableHT$HomeTeam <- if_else(plTableHT$HomeTeam == 0, plTableHT$AwayTeam, plTableHT$HomeTeam)
    plTableHT <- plTableHT %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 38, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTableHT <- plTableHT[order(-plTableHT$PTS, -plTableHT$GD, -plTableHT$GF),]
    
    tagList(
      datatable(plTableHT, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE))
    )
  })
  
  output$Tablehalf2Match <- renderUI({
    sliderValuesDate()
    
    plHomeFT1920 <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!is.na(FTHG)) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(FT_HOME_WIN), D = sum(FT_DRAW), L = sum(FT_HOME_LOSS), GF = sum(HTHG2), GA = sum(HTAG2)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    
    plAwayFT1920 <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!is.na(FTAG)) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(FT_AWAY_WIN), D = sum(FT_DRAW), L = sum(FT_AWAY_LOSS), GF = sum(HTAG2), GA = sum(HTHG2)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    
    plTableFT <- union_all(plHomeFT1920, plAwayFT1920)
    
    plTableFT[is.na(plTableFT)] <- 0
    plTableFT$HomeTeam <- if_else(plTableFT$HomeTeam == 0, plTableFT$AwayTeam, plTableFT$HomeTeam)
    plTableFT <- plTableFT %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 38, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTableFT <- plTableFT[order(-plTableFT$PTS, -plTableFT$GD, -plTableFT$GF),]
    
    tagList(
      datatable(plTableFT, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE))
    )
  })
  
  output$Top6 <- renderUI({
    
    sliderValuesDate()
    
    plTop6ResultsData <- plDataAll %>%
      dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & between(Date, input$date[1], input$date[2]))
    
    plHomeTableTop6Date <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    plHomeTableTop6Date <- plHomeTableTop6Date[order(-plHomeTableTop6Date$PTS, -plHomeTableTop6Date$GD, -plHomeTableTop6Date$GF),]
    
    plAwayTableTop6Date <- plDataAll %>%
      dplyr::filter(!is.na(FTAG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    plAwayTableTop6Date <- plAwayTableTop6Date[order(-plAwayTableTop6Date$PTS, -plAwayTableTop6Date$GD, -plAwayTableTop6Date$GF),]
    
    plTableTop6Date <- union_all(plHomeTableTop6Date, plAwayTableTop6Date)
    
    plTableTop6Date[is.na(plTableTop6Date)] <- 0
    plTableTop6Date$HomeTeam <- if_else(plTableTop6Date$HomeTeam == 0, plTableTop6Date$AwayTeam, plTableTop6Date$HomeTeam)
    plTableTop6Date <- plTableTop6Date %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 10, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTableTop6Date <- plTableTop6Date[order(-plTableTop6Date$PTS, -plTableTop6Date$GD, -plTableTop6Date$GF),]
    
    tagList(
      datatable(plTableTop6Date, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE)) ,
      datatable(plTop6ResultsData, options = list("pageLength" = 5, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 

      )
  })
  
  output$ExTop6 <- renderUI({
    sliderValuesDate()
    
    plHomeTableExTop6Date <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    plHomeTableExTop6Date <- plHomeTableExTop6Date[order(-plHomeTableExTop6Date$PTS, -plHomeTableExTop6Date$GD, -plHomeTableExTop6Date$GF),]
    
    plAwayTableExTop6Date <- plDataAll %>%
      dplyr::filter(!is.na(FTAG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    plAwayTableExTop6Date <- plAwayTableExTop6Date[order(-plAwayTableExTop6Date$PTS, -plAwayTableExTop6Date$GD, -plAwayTableExTop6Date$GF),]
    
    plTableExTop6Date <- union_all(plHomeTableExTop6Date, plAwayTableExTop6Date)
    
    plTableExTop6Date[is.na(plTableExTop6Date)] <- 0
    plTableExTop6Date$HomeTeam <- if_else(plTableExTop6Date$HomeTeam == 0, plTableExTop6Date$AwayTeam, plTableExTop6Date$HomeTeam)
    plTableExTop6Date <- plTableExTop6Date %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 26, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTableExTop6Date <- plTableExTop6Date[order(-plTableExTop6Date$PTS, -plTableExTop6Date$GD, -plTableExTop6Date$GF),]
    
    tagList(
      datatable(plTableExTop6Date, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE))
    )
  })
  
  output$AgTop6 <- renderUI({
    
    plHomeTableAgTop6Date <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    plHomeTableAgTop6Date <- plHomeTableAgTop6Date[order(-plHomeTableAgTop6Date$PTS, -plHomeTableAgTop6Date$GD, -plHomeTableAgTop6Date$GF),]
    
    plAwayTableAgTop6Date <- plDataAll %>%
      dplyr::filter(!is.na(FTAG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    plAwayTableAgTop6Date <- plAwayTableAgTop6Date[order(-plAwayTableAgTop6Date$PTS, -plAwayTableAgTop6Date$GD, -plAwayTableAgTop6Date$GF),]
    
    plTableAgTop6Date <- union_all(plHomeTableAgTop6Date, plAwayTableAgTop6Date)
    
    plTableAgTop6Date[is.na(plTableAgTop6Date)] <- 0
    plTableAgTop6Date$HomeTeam <- if_else(plTableAgTop6Date$HomeTeam == 0, plTableAgTop6Date$AwayTeam, plTableAgTop6Date$HomeTeam)
    plTableAgTop6Date <- plTableAgTop6Date %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 12, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTableAgTop6Date <- plTableAgTop6Date[order(-plTableAgTop6Date$PTS, -plTableAgTop6Date$GD, -plTableAgTop6Date$GF),]
    
    tagList(
      datatable(plTableAgTop6Date, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE))
    )
  })
  
  output$Top6VS <- renderUI({
    plHomeTableTop6RestDate <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    plHomeTableTop6RestDate <- plHomeTableTop6RestDate[order(-plHomeTableTop6RestDate$PTS, -plHomeTableTop6RestDate$GD, -plHomeTableTop6RestDate$GF),]
    
    plAwayTableTop6RestDate <- plDataAll %>%
      dplyr::filter(!is.na(FTAG) & between(Date, input$date[1], input$date[2])) %>%
      dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                    & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    plAwayTableTop6RestDate <- plAwayTableTop6RestDate[order(-plAwayTableTop6RestDate$PTS, -plAwayTableTop6RestDate$GD, -plAwayTableTop6RestDate$GF),]
    
    plTableTop6RestDate <- union_all(plHomeTableTop6RestDate, plAwayTableTop6RestDate)
    
    plTableTop6RestDate[is.na(plTableTop6RestDate)] <- 0
    plTableTop6RestDate$HomeTeam <- if_else(plTableTop6RestDate$HomeTeam == 0, plTableTop6RestDate$AwayTeam, plTableTop6RestDate$HomeTeam)
    plTableTop6RestDate <- plTableTop6RestDate %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 28, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTableTop6RestDate <- plTableTop6RestDate[order(-plTableTop6RestDate$PTS, -plTableTop6RestDate$GD, -plTableTop6RestDate$GF),]
    
    tagList(
      datatable(plTableTop6RestDate, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE))
    )
  })
  
  output$FixturesResults <- renderUI({
    tagList(

      datatable(plDataResults[plDataResults$Date == input$fDate,], options = list("pageLength" = 10, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
  })                                                                                      
  
  output$ClubFixturesResults <- renderUI({
    tagList(
      
      datatable(plDataResults[plDataResults$HomeTeam == input$Team | plDataResults$AwayTeam == input$Team,], options = list("pageLength" = 10, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
  })
  
  output$ClubLastMatchHome <- renderUI({
    
    sliderValuesOverall()
    
    plDataAllHomeTable <- homeTableFilter(plDataAll)
    
        tagList(
        valueBox("Played", value = tags$p(paste(plDataAllHomeTable$P), style = "font-size: 50%;"), width = 3),
        valueBox("Wins", value = tags$p(paste(plDataAllHomeTable$W), style = "font-size: 50%;"), width = 3),
        valueBox("Draws", value = tags$p(paste(plDataAllHomeTable$D), style = "font-size: 50%;"), width = 3),  
        valueBox("Losses", value = tags$p(paste(plDataAllHomeTable$L), style = "font-size: 50%;"), width = 3),
        valueBox("Points", value = tags$p(paste(plDataAllHomeTable$PTS), style = "font-size: 50%;"), width = 3),
        valueBox("Max Points", value = tags$p(paste(plDataAllHomeTable$P*3), style = "font-size: 50%;"), width = 3), 
        valueBox("Clean Sheets", value = tags$p(paste(plDataAllHomeTable$CS), style = "font-size: 50%;"), width = 3), 
        valueBox("Goals Scored", value = tags$p(paste(plDataAllHomeTable$GF), style = "font-size: 50%;"), width = 3), 
        valueBox("Goals Conceeded", value = tags$p(paste(plDataAllHomeTable$GA), style = "font-size: 50%;"), width = 3), 
                
        datatable(plDataAll[(plDataAll$HomeTeam == input$Team) & input$rows1,], options = list(paging =TRUE, pageLength = input$rows1, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
        
      )
    
  })
  
  output$ClubLastMatchAway <- renderUI({
    
    sliderValuesOverall()
    
    
    plDataAllAwayTable <- awayTableFilter(plDataAll)
    
    tagList(
      valueBox("Played", value = tags$p(paste(plDataAllAwayTable$P), style = "font-size: 50%;"), width = 3),
      valueBox("Wins", value = tags$p(paste(plDataAllAwayTable$W), style = "font-size: 50%;"), width = 3),
      valueBox("Draws", value = tags$p(paste(plDataAllAwayTable$D), style = "font-size: 50%;"), width = 3),  
      valueBox("Losses", value = tags$p(paste(plDataAllAwayTable$L), style = "font-size: 50%;"), width = 3),
      valueBox("Points", value = tags$p(paste(plDataAllAwayTable$PTS), style = "font-size: 50%;"), width = 3),
      valueBox("Max Points", value = tags$p(paste(plDataAllAwayTable$P*3), style = "font-size: 50%;"), width = 3), 
      valueBox("Clean Sheets", value = tags$p(paste(plDataAllAwayTable$CS), style = "font-size: 50%;"), width = 3),
      valueBox("Goals Scored", value = tags$p(paste(plDataAllAwayTable$GF), style = "font-size: 50%;"), width = 3), 
      valueBox("Goals Conceeded", value = tags$p(paste(plDataAllAwayTable$GA), style = "font-size: 50%;"), width = 3),
      datatable(plDataAll[(plDataAll$AwayTeam == input$Team) & input$rows1,], options = list(paging =TRUE, pageLength = input$rows1, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
    
  })
  
  output$ClubLastMatchOverall <- renderUI({
    
    sliderValuesOverall()

    plDataAllTable <- plDataAll %>%
      dplyr::filter(AwayTeam == input$Team | HomeTeam == input$Team) %>%
      dplyr::filter(between(row_number(), 1, input$rows1)) %>%
      mutate(VOLUME = 1) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN[HomeTeam == input$Team])+sum(AWAY_WIN[AwayTeam == input$Team]),
             D = sum(DRAW), L = sum(HOME_LOSS[HomeTeam == input$Team])+sum(AWAY_LOSS[AwayTeam == input$Team]),
             PTS = ((W * 3) + D), CS = sum(HOME_CS[HomeTeam == input$Team])+sum(AWAY_CS[AwayTeam == input$Team]),
             GF = sum(FTHG[HomeTeam == input$Team])+sum(FTAG[AwayTeam == input$Team]),
             GA = sum(FTAG[HomeTeam == input$Team])+sum(FTHG[AwayTeam == input$Team]))

    tagList(
      valueBox("Played", value = tags$p(paste(plDataAllTable$P), style = "font-size: 50%;"), width = 3),
      valueBox("Wins", value = tags$p(paste(plDataAllTable$W), style = "font-size: 50%;"), width = 3),
      valueBox("Draws", value = tags$p(paste(plDataAllTable$D), style = "font-size: 50%;"), width = 3),  
      valueBox("Losses", value = tags$p(paste(plDataAllTable$L), style = "font-size: 50%;"), width = 3),
      valueBox("Points", value = tags$p(paste(plDataAllTable$PTS), style = "font-size: 50%;"), width = 3),
      valueBox("Max Points", value = tags$p(paste(plDataAllTable$P*3), style = "font-size: 50%;"), width = 3),
      valueBox("Clean Sheets", value = tags$p(paste(plDataAllTable$CS), style = "font-size: 50%;"), width = 3),
      valueBox("Goals Scored", value = tags$p(paste(plDataAllTable$GF), style = "font-size: 50%;"), width = 3), 
      valueBox("Goals Conceeded", value = tags$p(paste(plDataAllTable$GA), style = "font-size: 50%;"), width = 3),
      datatable(plDataAll[(plDataAll$HomeTeam == input$Team | plDataAll$AwayTeam == input$Team) & input$rows1,], options = list(paging =TRUE, pageLength = input$rows1, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
    
  })
  
  output$TableDate <- renderUI({
    
    sliderValuesDate()
    
    plHomeTable1920Date <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(HomeTeam)
    plHomeTable1920Date <- plHomeTable1920Date[order(-plHomeTable1920Date$PTS, -plHomeTable1920Date$GD, -plHomeTable1920Date$GF),]
    
    plAwayTable1920Date <- plDataAll %>%
      dplyr::filter(!is.na(FTAG) & between(Date, input$date[1], input$date[2])) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      ungroup(AwayTeam)
    plAwayTable1920Date <- plAwayTable1920Date[order(-plAwayTable1920Date$PTS, -plAwayTable1920Date$GD, -plAwayTable1920Date$GF),]
    
    plTable1920Date <- union_all(plHomeTable1920Date, plAwayTable1920Date)
    
    plTable1920Date[is.na(plTable1920Date)] <- 0
    plTable1920Date$HomeTeam <- if_else(plTable1920Date$HomeTeam == 0, plTable1920Date$AwayTeam, plTable1920Date$HomeTeam)
    plTable1920Date <- plTable1920Date %>%
      select(-AwayTeam) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 38, 0),
             TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plTable1920Date <- plTable1920Date[order(-plTable1920Date$PTS, -plTable1920Date$GD, -plTable1920Date$GF),]
    
    
  
    tagList(
      datatable(plTable1920Date, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
    
  })
  
  output$HomeTableDate <- renderUI({
    
    sliderValuesDate()
    
    plHomeTable1920Date <- plDataAll %>%
      dplyr::filter(!is.na(FTHG) & between(Date, input$date[1], input$date[2])) %>%
      mutate(VOLUME = 1) %>%
      group_by(HomeTeam) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 19, 0)) %>%
      ungroup(HomeTeam) %>%
      mutate(TEAM = HomeTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plHomeTable1920Date <- plHomeTable1920Date[order(-plHomeTable1920Date$PTS, -plHomeTable1920Date$GD, -plHomeTable1920Date$GF),]

    tagList(
      datatable(plHomeTable1920Date, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
    
  })
  
  output$AwayTableDate <- renderUI({
    
    sliderValuesDate()
    
    plAwayTable1920Date <- plDataAll %>%
      dplyr::filter(!is.na(FTAG) & between(Date, input$date[1], input$date[2])) %>%
      mutate(VOLUME = 1) %>%
      group_by(AwayTeam) %>%
      summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
      mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
      mutate(AVG_PPG = round(PTS / P, 2),
             PROJECTED_POINTS = round(AVG_PPG * 19, 0)) %>%
      ungroup(AwayTeam) %>%
      mutate(TEAM = AwayTeam) %>%
      select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
    plAwayTable1920Date <- plAwayTable1920Date[order(-plAwayTable1920Date$PTS, -plAwayTable1920Date$GD, -plAwayTable1920Date$GF),]
    
    tagList(
      datatable(plAwayTable1920Date, options = list("pageLength" = 26, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
    )
    
  })
  
  output$pGoals <- renderUI({
    if(input$Season == "2019/20")
    {
      tagList(
        DT::dataTableOutput("playerGoals1920"))
    }
    else if(input$Season == "2018/19")
    {
      tagList(
        DT::dataTableOutput("playerGoals1819"))
    }
  })
  
  output$ClubComparison1920 <- renderUI({
    
    sliderValuesComp()
    
    plCompData1920 <- plDataAll2 %>%
      dplyr::filter(AwayTeam == input$Team | HomeTeam == input$Team) %>%
      dplyr::filter(Date >= "2019-08-01" & Date <= "2020-06-01") %>%
      dplyr::filter(between(row_number(), 1, input$rows2)) %>%
      mutate(VOLUME = 1) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN[HomeTeam == input$Team])+sum(AWAY_WIN[AwayTeam == input$Team]),
                D = sum(DRAW), L = sum(HOME_LOSS[HomeTeam == input$Team])+sum(AWAY_LOSS[AwayTeam == input$Team]),
                PTS = ((W * 3) + D), CS = sum(HOME_CS[HomeTeam == input$Team])+sum(AWAY_CS[AwayTeam == input$Team]),
                GF = sum(FTHG[HomeTeam == input$Team])+sum(FTAG[AwayTeam == input$Team]),
                GA = sum(FTAG[HomeTeam == input$Team])+sum(FTHG[AwayTeam == input$Team]),
                S = sum(HS[HomeTeam == input$Team])+sum(AS[AwayTeam == input$Team]),
                SF = sum(AS[HomeTeam == input$Team])+sum(HS[AwayTeam == input$Team]),
                ST = sum(HST[HomeTeam == input$Team])+sum(AST[AwayTeam == input$Team]),
                STF = sum(AST[HomeTeam == input$Team])+sum(HST[AwayTeam == input$Team]),
                C = sum(HC[HomeTeam == input$Team])+sum(AC[AwayTeam == input$Team]),
                CF = sum(AC[HomeTeam == input$Team])+sum(HC[AwayTeam == input$Team]))
    
    tagList(
      valueBox("Played", value = tags$p(paste(plCompData1920$P), style = "font-size: 50%;"), width = 3),
      valueBox("Wins", value = tags$p(paste(plCompData1920$W), style = "font-size: 50%;"), width = 3),
      valueBox("Draws", value = tags$p(paste(plCompData1920$D), style = "font-size: 50%;"), width = 3),  
      valueBox("Losses", value = tags$p(paste(plCompData1920$L), style = "font-size: 50%;"), width = 3),
      valueBox("Points", value = tags$p(paste(plCompData1920$PTS), style = "font-size: 50%;"), width = 3),
      valueBox("Max Points", value = tags$p(paste(plCompData1920$P*3), style = "font-size: 50%;"), width = 3),
      valueBox("Clean Sheets", value = tags$p(paste(plCompData1920$CS), style = "font-size: 50%;"), width = 3),
      valueBox("Goals Scored", value = tags$p(paste(plCompData1920$GF), style = "font-size: 50%;"), width = 3), 
      valueBox("Goals Conceeded", value = tags$p(paste(plCompData1920$GA), style = "font-size: 50%;"), width = 3),
      valueBox("Shots", value = tags$p(paste(plCompData1920$S), style = "font-size: 50%;"), width = 3),
      valueBox("Shots Faced", value = tags$p(paste(plCompData1920$SF), style = "font-size: 50%;"), width = 3), 
      valueBox("Shots on Target", value = tags$p(paste(plCompData1920$ST), style = "font-size: 50%;"), width = 3),
      valueBox("Shots on Target Faced", value = tags$p(paste(plCompData1920$STF), style = "font-size: 50%;"), width = 3),
      valueBox("Corners", value = tags$p(paste(plCompData1920$C), style = "font-size: 50%;"), width = 3), 
      valueBox("Corners Faced", value = tags$p(paste(plCompData1920$CF), style = "font-size: 50%;"), width = 3)
    )
  })
  
  output$ClubComparison1819 <- renderUI({
    
  sliderValuesComp()
    
   plCompData1819 <- plDataAll2 %>%
      dplyr::filter(AwayTeam == input$Team | HomeTeam == input$Team) %>%
      dplyr::filter(Date >= "2018-08-01" & Date <= "2019-06-01") %>%
      dplyr::filter(between(row_number(), 1, input$rows2)) %>%
      mutate(VOLUME = 1) %>%
      summarise(P = sum(VOLUME), W = sum(HOME_WIN[HomeTeam == input$Team])+sum(AWAY_WIN[AwayTeam == input$Team]),
                D = sum(DRAW), L = sum(HOME_LOSS[HomeTeam == input$Team])+sum(AWAY_LOSS[AwayTeam == input$Team]),
                PTS = ((W * 3) + D), CS = sum(HOME_CS[HomeTeam == input$Team])+sum(AWAY_CS[AwayTeam == input$Team]),
                GF = sum(FTHG[HomeTeam == input$Team])+sum(FTAG[AwayTeam == input$Team]),
                GA = sum(FTAG[HomeTeam == input$Team])+sum(FTHG[AwayTeam == input$Team]),
                S = sum(HS[HomeTeam == input$Team])+sum(AS[AwayTeam == input$Team]),
                SF = sum(AS[HomeTeam == input$Team])+sum(HS[AwayTeam == input$Team]),
                ST = sum(HST[HomeTeam == input$Team])+sum(AST[AwayTeam == input$Team]),
                STF = sum(AST[HomeTeam == input$Team])+sum(HST[AwayTeam == input$Team]),
                C = sum(HC[HomeTeam == input$Team])+sum(AC[AwayTeam == input$Team]),
                CF = sum(AC[HomeTeam == input$Team])+sum(HC[AwayTeam == input$Team]))
    
   tagList(
        valueBox("Played", value = tags$p(paste(plCompData1819$P), style = "font-size: 50%;"), width = 3),
        valueBox("Wins", value = tags$p(paste(plCompData1819$W), style = "font-size: 50%;"), width = 3),
        valueBox("Draws", value = tags$p(paste(plCompData1819$D), style = "font-size: 50%;"), width = 3),  
        valueBox("Losses", value = tags$p(paste(plCompData1819$L), style = "font-size: 50%;"), width = 3),
        valueBox("Points", value = tags$p(paste(plCompData1819$PTS), style = "font-size: 50%;"), width = 3),
        valueBox("Max Points", value = tags$p(paste(plCompData1819$P*3), style = "font-size: 50%;"), width = 3),
        valueBox("Clean Sheets", value = tags$p(paste(plCompData1819$CS), style = "font-size: 50%;"), width = 3),
        valueBox("Goals Scored", value = tags$p(paste(plCompData1819$GF), style = "font-size: 50%;"), width = 3), 
        valueBox("Goals Conceeded", value = tags$p(paste(plCompData1819$GA), style = "font-size: 50%;"), width = 3),
        valueBox("Shots", value = tags$p(paste(plCompData1819$S), style = "font-size: 50%;"), width = 3),
        valueBox("Shots Faced", value = tags$p(paste(plCompData1819$SF), style = "font-size: 50%;"), width = 3), 
        valueBox("Shots on Target", value = tags$p(paste(plCompData1819$ST), style = "font-size: 50%;"), width = 3),
        valueBox("Shots on Target Faced", value = tags$p(paste(plCompData1819$STF), style = "font-size: 50%;"), width = 3),
        valueBox("Corners", value = tags$p(paste(plCompData1819$C), style = "font-size: 50%;"), width = 3), 
        valueBox("Corners Faced", value = tags$p(paste(plCompData1819$CF), style = "font-size: 50%;"), width = 3)
    )
    
  })

  
  ###################
  ### Data Tables ###
  ###################
  
  output$goalsScored1920 = DT::renderDataTable({ 
    datatable(plGoalsScored1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$homegoalsScored1920 = DT::renderDataTable({ 
    datatable(plHomeGoalsScored1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$awaygoalsScored1920 = DT::renderDataTable({ 
    datatable(plAwayGoalsScored1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  
  output$goalsConceeded1920 = DT::renderDataTable({ 
    datatable(plGoalsConceeded1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$homegoalsConceeded1920 = DT::renderDataTable({ 
    datatable(plHomeGoalsConceeded1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$awaygoalsConceeded1920 = DT::renderDataTable({ 
    datatable(plAwayGoalsConceeded1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  
  output$cleanSheets1920 = DT::renderDataTable({ 
    datatable(plCleanSheets1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$homecleanSheets1920 = DT::renderDataTable({ 
    datatable(plHomeCleanSheets1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$awaycleanSheets1920 = DT::renderDataTable({ 
    datatable(plAwayCleanSheets1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  
  output$YellowCards1920 = DT::renderDataTable({ 
    datatable(plYellowCards1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$homeYellowCards1920 = DT::renderDataTable({ 
    datatable(plHomeYellowCards1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  output$awayYellowCards1920 = DT::renderDataTable({ 
    datatable(plAwayYellowCards1920, options = list("pageLength" = 50, searching = FALSE, editable = FALSE, lengthChange = FALSE)) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

