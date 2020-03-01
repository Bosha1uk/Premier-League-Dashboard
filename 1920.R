library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(tidyverse)


##################
## 2019/20 Data ##
##################

pldata1920$MONTH <- month(pldata1920$Date)

plFixturesResultsData1920 <- pldata1920 %>%
  select(2, 3, 5, 6, 4, 11)

pldata1920$Date <- as.Date(pldata1920$Date, "%d-%m-%y")

pldata1920 <- pldata1920 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0),
         HOME_CS = if_else(FTAG == 0, 1, 0),
         AWAY_CS = if_else(FTHG == 0, 1, 0))

pldata1920HT <- pldata1920 %>%
  mutate(HOME_WIN = if_else(HTHG > HTAG, 1, 0),
         AWAY_WIN = if_else(HTHG < HTAG, 1, 0),
         DRAW = if_else(HTHG == HTAG, 1, 0),
         HOME_LOSS = if_else(HTHG < HTAG, 1, 0),
         AWAY_LOSS = if_else(HTHG > HTAG, 1, 0))

pldata1920FT <- pldata1920 %>%
  mutate(HTHG2 = FTHG - HTHG,
         HTAG2 = FTAG - HTAG)

pldata1920FT <- pldata1920FT %>%
  mutate(HOME_WIN = if_else(HTHG2 > HTAG2, 1, 0),
         AWAY_WIN = if_else(HTHG2 < HTAG2, 1, 0),
         DRAW = if_else(HTHG2 == HTAG2, 1, 0),
         HOME_LOSS = if_else(HTHG2 < HTAG2, 1, 0),
         AWAY_LOSS = if_else(HTHG2 > HTAG2, 1, 0))

homeTable <- function(y)
{
  x <- y %>%
    dplyr::filter(!is.na(FTHG)) %>%
    mutate(VOLUME = 1) %>%
    group_by(HomeTeam) %>%
    summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
    mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
    ungroup(HomeTeam)
  x <- x[order(-x$PTS, -x$GD, -x$GF),]
}

plHomeTable1920 <- homeTable(pldata1920)

awayTable <- function(y)
{
  x <- y %>%
    dplyr::filter(!is.na(FTAG)) %>%
    mutate(VOLUME = 1) %>%
    group_by(AwayTeam) %>%
    summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
    mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
    ungroup(AwayTeam)
  x <- x[order(-x$PTS, -x$GD, -x$GF),]
}

plAwayTable1920 <- awayTable(pldata1920)

table <- function(y, z)
{
  
  x <- union_all(y, z)
  
  x[is.na(x)] <- 0
  x$HomeTeam <- if_else(x$HomeTeam == 0, x$AwayTeam, x$HomeTeam)
  x <- x %>%
    select(-AwayTeam) %>%
    group_by(HomeTeam) %>%
    summarise(P = sum(P), W = sum(W), D = sum(D), L = sum(L), GF = sum(GF), GA = sum(GA)) %>%
    mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
    mutate(AVG_PPG = round(PTS / P, 2),
           PROJECTED_POINTS = round(AVG_PPG * 38, 0),
           TEAM = HomeTeam) %>%
    select(TEAM, P, W, D, L, GF, GA, GD, PTS, AVG_PPG, PROJECTED_POINTS)
  x <- x[order(-x$PTS, -x$GD, -x$GF),]
  #colnames(x)[1] = "TEAM"
  #colnames(y)[1] = "TEAM"
  #colnames(z)[1] = "TEAM"
}

plTable1920 <- table(plHomeTable1920, plAwayTable1920)

plHomeStats1920 <- pldata1920 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(HS = sum(HS), HSF = sum(AS), HST = sum(HST), HSTF = sum(AST), FTHG = sum(FTHG),
            STR = paste(round(HST / HS * 100, 2), "%"), SCR = paste(round(FTHG / HS * 100, 2), "%"), 
            STCR = paste(round(FTHG / HST * 100, 2), "%"),
            HF = sum(HF), HC = sum(HC), HY = sum(HY), HR = sum(HR),
            FTHGC = sum(FTAG), HCS = sum(HOME_CS)) %>%
  ungroup(HomeTeam)

plHomeStats1920ext <- left_join(plHomeTable1920, plHomeStats1920, by = "HomeTeam")

plHomeStats1920ext <- plHomeStats1920ext %>%
  group_by(HomeTeam) %>%
  dplyr::mutate(AVG_S = round(HS / P, 2), AVG_SF = round(HSF / P, 2), AVG_ST = round(HST / P, 2), AVG_STF = round(HSTF / P, 2)
                , AVG_GSS = round(HS / GF, 2), AVG_GSC = round(HSF / GA, 2))

plAwayStats1920 <- pldata1920 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(AS = sum(AS), ASF = sum(HS), AST = sum(AST), ASTF = sum(HST), FTAG = sum(FTAG),
            STR = paste(round(AST / AS * 100, 2), "%"), SCR = paste(round(FTAG / AS * 100, 2), "%"), 
            STCR = paste(round(FTAG / AST * 100, 2), "%"),
            AF = sum(AF), AC = sum(AC), AY = sum(AY), AR = sum(AR),
            FTAGC = sum(FTHG), ACS = sum(AWAY_CS)) %>%
  ungroup(AwayTeam)

plAwayStats1920ext <- left_join(plAwayTable1920, plAwayStats1920, by = "AwayTeam")

plAwayStats1920ext <- plAwayStats1920ext %>%
  group_by(AwayTeam) %>%
  dplyr::mutate(AVG_S = round(AS / P, 2), AVG_SF = round(ASF / P, 2), AVG_ST = round(AST / P, 2), AVG_STF = round(ASTF / P, 2)
                , AVG_GSS = round(AS / GF, 2), AVG_GSC = round(ASF / GA, 2))


plStats1920 <- union_all(plHomeStats1920, plAwayStats1920)
plStats1920[is.na(plStats1920)] <- 0
plStats1920$HomeTeam <- if_else(plStats1920$HomeTeam == 0, plStats1920$AwayTeam, plStats1920$HomeTeam)

colnames(plStats1920)[1] = "TEAM"

plStats1920 <- plStats1920 %>%
  dplyr::group_by(TEAM) %>%
  dplyr::summarise(SEASON = "2019/20"
                   , S = sum(HS + AS)
                   , ST = sum(HST + AST)
                   , SF = sum(HSF + ASF)
                   , STF = sum(HSTF + ASTF)
                   , FTG = sum(FTHG + FTAG)
                   , FTGC = sum(FTAGC + FTHGC)
                   , STR = paste(round(ST / S * 100, 2), "%") 
                   , SCR = paste(round(FTG / S * 100, 2), "%")
                   , STCR = paste(round(FTG / ST * 100, 2), "%")
                   , F = sum(HF + AF)
                   , C = sum(HC + AC)
                   , Y = sum(HY + AY)
                   , R = sum(HR + AR)
                   , CS = sum(HCS + ACS)) %>%
  select(TEAM, SEASON, S, ST, SF, STF, FTG, FTGC, STR, SCR, STCR, F, C, Y, R, CS)

plStats1920ext <- left_join(plTable1920, plStats1920, by = "TEAM")

plStats1920ext <- plStats1920ext %>%
  group_by(TEAM) %>%
  dplyr::mutate(AVG_S = round(S / P, 2), AVG_SF = round(SF / P, 2), AVG_ST = round(ST / P, 2), AVG_STF = round(STF / P, 2)
                , AVG_GSS = round(S / GF, 2), AVG_GSC = round(SF / GA, 2))

plShots1920 <- plStats1920 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(-S)

plAShotsGraph1920 <- plShots1920 %>%
  dplyr::mutate(AVG_SHOTS = round(S / 11, 2))

plShotsFaced1920 <- plStats1920 %>%
  dplyr::select(TEAM, SF) %>%
  dplyr::arrange(-SF)

plAShotsFacedGraph1920 <- plShotsFaced1920 %>%
  dplyr::mutate(AVG_SHOTS_FACED = round(SF / 11, 2))

plShotsComp1920 <- plStats1920 %>%
  dplyr::select(TEAM, S, SF) %>%
  dplyr::mutate(AVG_SHOTS = round(S / 11, 2)
                , AVG_SHOTS_FACED = round(SF / 11, 2)
                , AVG_COMP = AVG_SHOTS - AVG_SHOTS_FACED) %>%
  dplyr::arrange(-AVG_COMP)

plMaxShots1920 <- plShots1920[1,]

plMinShots1920 <- plShots1920[20,]

plMaxShotsFaced1920 <- plShotsFaced1920[1,]

plMinShotsFaced1920 <- plShotsFaced1920[20,]

plHomeShots1920 <- plHomeStats1920 %>%
  dplyr::select(HomeTeam, HS) %>%
  dplyr::arrange(-HS)

plAwayShots1920 <- plAwayStats1920 %>%
  dplyr::select(AwayTeam, AS) %>%
  dplyr::arrange(-AS)

plMaxShotsTarget1920 <- plStats1920 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(-ST)

plMaxShotsTarget1920 <- plMaxShotsTarget1920[1,]

plMaxShotsTargetFaced1920 <- plStats1920 %>%
  dplyr::select(TEAM, STF) %>%
  dplyr::arrange(-STF)

plMaxShotsTargetFaced1920 <- plMaxShotsTargetFaced1920[1,]

plMinShots1920 <- plStats1920 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(S)

plMinShots1920 <- plMinShots1920[1,]

plMinShotsTarget1920 <- plStats1920 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(ST)

plMinShotsTargetFaced1920 <- plStats1920 %>%
  dplyr::select(TEAM, STF) %>%
  dplyr::arrange(STF)

plMinShotsTarget1920 <- plMinShotsTarget1920[1,]

plMinShotsTargetFaced1920 <- plMinShotsTargetFaced1920[1,]

plGoalsScored1920 <- plStats1920 %>%
  dplyr::select(TEAM, FTG) %>%
  dplyr::arrange(-FTG)

plHomeGoalsScored1920 <- plHomeStats1920 %>%
  dplyr::select(HomeTeam, FTHG) %>%
  dplyr::arrange(-FTHG)

plAwayGoalsScored1920 <- plAwayStats1920 %>%
  dplyr::select(AwayTeam, FTAG) %>%
  dplyr::arrange(-FTAG)

plMaxGoalsScored1920 <- plGoalsScored1920[1,]
plMinGoalsScored1920 <- plGoalsScored1920[20,]


plGoalsConceeded1920 <- plStats1920 %>%
  dplyr::select(TEAM, FTGC) %>%
  dplyr::arrange(-FTGC)

plHomeGoalsConceeded1920 <- plHomeStats1920 %>%
  dplyr::select(HomeTeam, FTHGC) %>%
  dplyr::arrange(-FTHGC)

plAwayGoalsConceeded1920 <- plAwayStats1920 %>%
  dplyr::select(AwayTeam, FTAGC) %>%
  dplyr::arrange(-FTAGC)

plMaxGoalsConceeded1920 <- plGoalsConceeded1920[1,]
plMinGoalsConceeded1920 <- plGoalsConceeded1920[20,]

plCleanSheets1920 <- plStats1920 %>%
  dplyr::select(TEAM, CS) %>%
  dplyr::arrange(-CS)

plHomeCleanSheets1920 <- plHomeStats1920 %>%
  dplyr::select(HomeTeam, HCS) %>%
  dplyr::arrange(-HCS)

plAwayCleanSheets1920 <- plAwayStats1920 %>%
  dplyr::select(AwayTeam, ACS) %>%
  dplyr::arrange(-ACS)

plMaxCleanSheets1920 <- plCleanSheets1920[1,]
plMinCleanSheets1920 <- plCleanSheets1920[20,]

plYellowCards1920 <- plStats1920 %>%
  dplyr::select(TEAM, Y) %>%
  dplyr::arrange(-Y)

plHomeYellowCards1920 <- plHomeStats1920 %>%
  dplyr::select(HomeTeam, HY) %>%
  dplyr::arrange(-HY)

plAwayYellowCards1920 <- plAwayStats1920 %>%
  dplyr::select(AwayTeam, AY) %>%
  dplyr::arrange(-AY)

plMaxYellowCards1920 <- plYellowCards1920[1,]
plMinYellowCards1920 <- plYellowCards1920[20,]


plCHPoints <- pldata1920 %>%
  select(HomeTeam, Date, HOME_WIN, DRAW, FTHG, FTAG) %>%
  group_by(HomeTeam) %>%
  mutate(HG = FTHG, HGC = FTAG) %>%
  ungroup()

colnames(plCHPoints)[1] = "TEAM" 

plCAPoints <- pldata1920 %>%
  select(AwayTeam, Date, AWAY_WIN, DRAW, FTHG, FTAG) %>%
  group_by(AwayTeam) %>%
  mutate(AG = FTAG, AGC = FTHG) %>%
  ungroup()

colnames(plCAPoints)[1] = "TEAM"

plCPoints <- union_all(plCHPoints, plCAPoints)

#plCPoints <- left_join(plCHPoints, plCAPoints, by = c("HomeTeam", "AwayTeam", "Date"))  

plCPoints[is.na(plCPoints)] <- 0

plCPoints <- plCPoints %>%
  dplyr::arrange(TEAM, Date) %>%
  mutate(MD = 1) %>%
  group_by(TEAM) %>%
  mutate(MD2 = cumsum(MD)) %>%
  ungroup() %>%
  select(-MD) %>%
  group_by(TEAM) %>%
  mutate(CHPOINTS = cumsum(HOME_WIN * 3 + (DRAW / 2))) %>%
  mutate(CAPOINTS = cumsum(AWAY_WIN * 3 + (DRAW / 2))) %>%
  mutate(CPOINTS = CHPOINTS + CAPOINTS) %>%
  mutate(CHG = cumsum(HG)) %>%
  mutate(CAG = cumsum(AG)) %>%
  mutate(CG = CHG + CAG) %>%
  mutate(CHGC = cumsum(HGC)) %>%
  mutate(CAGC = cumsum(AGC)) %>%
  mutate(CGC = CHGC + CAGC) %>%
  mutate(CGD = CG - CGC) %>%
  ungroup() %>%
  group_by(MD2) %>%
  mutate(RANKING = order(order(CPOINTS, CGD, CG, TEAM, decreasing=TRUE))) %>%
  ungroup()


