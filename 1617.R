library(dplyr)
library(readr)
library(tidyr)

pldata1617 <- read_csv("Premier League Table 1617.csv")

##################
## 2016/17 Data ##
##################


plFixturesResultsData1617 <- pldata1617 %>%
  select(1:5, 10)

pldata1617$Date <- as.Date(pldata1617$Date, "%d-%m-%y")

pldata1617 <- pldata1617 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0),
         HOME_CS = if_else(FTAG == 0, 1, 0),
         AWAY_CS = if_else(FTHG == 0, 1, 0))

plHomeTable1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

pldata1617half1 <- pldata1617 %>%
  dplyr::filter(Date <= '2017-01-01')

pldata1617half2 <- pldata1617 %>%
  dplyr::filter(Date > '2017-01-01')  

plHomeTable1617half1 <- pldata1617half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1617half1 <- pldata1617half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTable1617half2 <- pldata1617half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1617half2 <- pldata1617half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6Table1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6Table1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeExTop6Table1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayExTop6Table1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeAgTop6Table1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayAgTop6Table1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6RestTable1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6RestTable1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plTable1617 <- plHomeTable1617

plTable1617$P <- plHomeTable1617$P + plAwayTable1617$P
plTable1617$W <- plHomeTable1617$W + plAwayTable1617$W
plTable1617$D <- plHomeTable1617$D + plAwayTable1617$D
plTable1617$L <- plHomeTable1617$L + plAwayTable1617$L
plTable1617$GF <- plHomeTable1617$GF + plAwayTable1617$GF
plTable1617$GA <- plHomeTable1617$GA + plAwayTable1617$GA
plTable1617$GD <- plTable1617$GF - plTable1617$GA
plTable1617$PTS <- plHomeTable1617$PTS + plAwayTable1617$PTS
plTable1617$AVG_PPG <- round(plTable1617$PTS / plTable1617$P, 2)
plTable1617$PROJECTED_POINTS <- round(plTable1617$AVG_PPG * 38, 0)
colnames(plTable1617)[1] = "TEAM"
colnames(plHomeTable1617)[1] = "TEAM"
colnames(plAwayTable1617)[1] = "TEAM"

plTable1617half1 <- plHomeTable1617half1

plTable1617half1$P <- plHomeTable1617half1$P + plAwayTable1617half1$P
plTable1617half1$W <- plHomeTable1617half1$W + plAwayTable1617half1$W
plTable1617half1$D <- plHomeTable1617half1$D + plAwayTable1617half1$D
plTable1617half1$L <- plHomeTable1617half1$L + plAwayTable1617half1$L
plTable1617half1$GF <- plHomeTable1617half1$GF + plAwayTable1617half1$GF
plTable1617half1$GA <- plHomeTable1617half1$GA + plAwayTable1617half1$GA
plTable1617half1$GD <- plTable1617half1$GF - plTable1617half1$GA
plTable1617half1$PTS <- plHomeTable1617half1$PTS + plAwayTable1617half1$PTS
plTable1617half1$AVG_PPG <- round(plTable1617half1$PTS / plTable1617half1$P, 2)
colnames(plTable1617half1)[1] = "TEAM"
colnames(plHomeTable1617half1)[1] = "TEAM"
colnames(plAwayTable1617half1)[1] = "TEAM"

plTable1617half2 <- plHomeTable1617half2

plTable1617half2$P <- plHomeTable1617half2$P + plAwayTable1617half2$P
plTable1617half2$W <- plHomeTable1617half2$W + plAwayTable1617half2$W
plTable1617half2$D <- plHomeTable1617half2$D + plAwayTable1617half2$D
plTable1617half2$L <- plHomeTable1617half2$L + plAwayTable1617half2$L
plTable1617half2$GF <- plHomeTable1617half2$GF + plAwayTable1617half2$GF
plTable1617half2$GA <- plHomeTable1617half2$GA + plAwayTable1617half2$GA
plTable1617half2$GD <- plTable1617half2$GF - plTable1617half2$GA
plTable1617half2$PTS <- plHomeTable1617half2$PTS + plAwayTable1617half2$PTS
plTable1617half2$AVG_PPG <- round(plTable1617half2$PTS / plTable1617half2$P, 2)
colnames(plTable1617half2)[1] = "TEAM"
colnames(plHomeTable1617half2)[1] = "TEAM"
colnames(plAwayTable1617half2)[1] = "TEAM"

plTop6Table1617 <- plHomeTop6Table1617

plTop6Table1617$P <- plHomeTop6Table1617$P + plAwayTop6Table1617$P
plTop6Table1617$W <- plHomeTop6Table1617$W + plAwayTop6Table1617$W
plTop6Table1617$D <- plHomeTop6Table1617$D + plAwayTop6Table1617$D
plTop6Table1617$L <- plHomeTop6Table1617$L + plAwayTop6Table1617$L
plTop6Table1617$GF <- plHomeTop6Table1617$GF + plAwayTop6Table1617$GF
plTop6Table1617$GA <- plHomeTop6Table1617$GA + plAwayTop6Table1617$GA
plTop6Table1617$GD <- plTop6Table1617$GF - plTop6Table1617$GA
plTop6Table1617$PTS <- plHomeTop6Table1617$PTS + plAwayTop6Table1617$PTS
colnames(plTop6Table1617)[1] = "TEAM"
colnames(plHomeTop6Table1617)[1] = "TEAM"
colnames(plAwayTop6Table1617)[1] = "TEAM"

plExTop6Table1617 <- plHomeExTop6Table1617

plExTop6Table1617$P <- plHomeExTop6Table1617$P + plAwayExTop6Table1617$P
plExTop6Table1617$W <- plHomeExTop6Table1617$W + plAwayExTop6Table1617$W
plExTop6Table1617$D <- plHomeExTop6Table1617$D + plAwayExTop6Table1617$D
plExTop6Table1617$L <- plHomeExTop6Table1617$L + plAwayExTop6Table1617$L
plExTop6Table1617$GF <- plHomeExTop6Table1617$GF + plAwayExTop6Table1617$GF
plExTop6Table1617$GA <- plHomeExTop6Table1617$GA + plAwayExTop6Table1617$GA
plExTop6Table1617$GD <- plExTop6Table1617$GF - plExTop6Table1617$GA
plExTop6Table1617$PTS <- plHomeExTop6Table1617$PTS + plAwayExTop6Table1617$PTS
colnames(plExTop6Table1617)[1] = "TEAM"
colnames(plHomeExTop6Table1617)[1] = "TEAM"
colnames(plAwayExTop6Table1617)[1] = "TEAM"

plAgTop6Table1617 <- plHomeAgTop6Table1617

plAgTop6Table1617$P <- plHomeAgTop6Table1617$P + plAwayAgTop6Table1617$P
plAgTop6Table1617$W <- plHomeAgTop6Table1617$W + plAwayAgTop6Table1617$W
plAgTop6Table1617$D <- plHomeAgTop6Table1617$D + plAwayAgTop6Table1617$D
plAgTop6Table1617$L <- plHomeAgTop6Table1617$L + plAwayAgTop6Table1617$L
plAgTop6Table1617$GF <- plHomeAgTop6Table1617$GF + plAwayAgTop6Table1617$GF
plAgTop6Table1617$GA <- plHomeAgTop6Table1617$GA + plAwayAgTop6Table1617$GA
plAgTop6Table1617$GD <- plAgTop6Table1617$GF - plAgTop6Table1617$GA
plAgTop6Table1617$PTS <- plHomeAgTop6Table1617$PTS + plAwayAgTop6Table1617$PTS
colnames(plAgTop6Table1617)[1] = "TEAM"
colnames(plHomeAgTop6Table1617)[1] = "TEAM"
colnames(plAwayAgTop6Table1617)[1] = "TEAM"

plTop6RestTable1617 <- plHomeTop6RestTable1617

plTop6RestTable1617$P <- plHomeTop6RestTable1617$P + plAwayTop6RestTable1617$P
plTop6RestTable1617$W <- plHomeTop6RestTable1617$W + plAwayTop6RestTable1617$W
plTop6RestTable1617$D <- plHomeTop6RestTable1617$D + plAwayTop6RestTable1617$D
plTop6RestTable1617$L <- plHomeTop6RestTable1617$L + plAwayTop6RestTable1617$L
plTop6RestTable1617$GF <- plHomeTop6RestTable1617$GF + plAwayTop6RestTable1617$GF
plTop6RestTable1617$GA <- plHomeTop6RestTable1617$GA + plAwayTop6RestTable1617$GA
plTop6RestTable1617$GD <- plTop6RestTable1617$GF - plTop6RestTable1617$GA
plTop6RestTable1617$PTS <- plHomeTop6RestTable1617$PTS + plAwayTop6RestTable1617$PTS
colnames(plTop6RestTable1617)[1] = "TEAM"
colnames(plHomeTop6RestTable1617)[1] = "TEAM"
colnames(plAwayTop6RestTable1617)[1] = "TEAM"

plTable1617 <- plTable1617[order(-plTable1617$PTS, -plTable1617$GD, -plTable1617$GF),]

plTable1617half1 <- plTable1617half1[order(-plTable1617half1$PTS, -plTable1617half1$GD, -plTable1617half1$GF),]

plTable1617half2 <- plTable1617half2[order(-plTable1617half2$PTS, -plTable1617half2$GD, -plTable1617half2$GF),]

plHomeTable1617 <- plHomeTable1617[order(-plHomeTable1617$PTS, -plHomeTable1617$GD, -plHomeTable1617$GF),]

plAwayTable1617 <- plAwayTable1617[order(-plAwayTable1617$PTS, -plAwayTable1617$GD, -plAwayTable1617$GF),]

plTop6Table1617 <- plTop6Table1617[order(-plTop6Table1617$PTS, -plTop6Table1617$GD, -plTop6Table1617$GF),]

plExTop6Table1617 <- plExTop6Table1617[order(-plExTop6Table1617$PTS, -plExTop6Table1617$GD, -plExTop6Table1617$GF),]

plAgTop6Table1617 <- plAgTop6Table1617[order(-plAgTop6Table1617$PTS, -plAgTop6Table1617$GD, -plAgTop6Table1617$GF),]

plTop6RestTable1617 <- plTop6RestTable1617[order(-plTop6RestTable1617$PTS, -plTop6RestTable1617$GD, -plTop6RestTable1617$GF),]

plHomeStats1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(HS = sum(HS), HST = sum(HST), FTHG = sum(FTHG),
            STR = paste(round(HST / HS * 100, 2), "%"), SCR = paste(round(FTHG / HS * 100, 2), "%"), 
            STCR = paste(round(FTHG / HST * 100, 2), "%"),
            HF = sum(HF), HC = sum(HC), HY = sum(HY), HR = sum(HR),
            FTAG = sum(FTAG), HCS = sum(HOME_CS)) %>%
  ungroup(HomeTeam)

plAwayStats1617 <- pldata1617 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(AS = sum(AS), AST = sum(AST), FTAG = sum(FTAG),
            STR = paste(round(AST / AS * 100, 2), "%"), SCR = paste(round(FTAG / AS * 100, 2), "%"), 
            STCR = paste(round(FTAG / AST * 100, 2), "%"),
            AF = sum(AF), AC = sum(AC), AY = sum(AY), AR = sum(AR),
            FTHG = sum(FTHG), ACS = sum(AWAY_CS)) %>%
  ungroup(AwayTeam)

plStats1617 <- plHomeStats1617 %>%
  select(1)

plStats1617$SEASON = "2016/17"
plStats1617$S <- plHomeStats1617$HS + plAwayStats1617$AS
plStats1617$ST <- plHomeStats1617$HST + plAwayStats1617$AST
plStats1617$FTG <- plHomeStats1617$FTHG + plAwayStats1617$FTAG
plStats1617$FTGC <- plHomeStats1617$FTAG + plAwayStats1617$FTHG
plStats1617$STR = paste(round(plStats1617$ST / plStats1617$S * 100, 2), "%") 
plStats1617$SCR = paste(round(plStats1617$FTG / plStats1617$S * 100, 2), "%")
plStats1617$STCR = paste(round(plStats1617$FTG / plStats1617$ST * 100, 2), "%")
plStats1617$F = plHomeStats1617$HF + plAwayStats1617$AF
plStats1617$C = plHomeStats1617$HC + plAwayStats1617$AC
plStats1617$Y = plHomeStats1617$HY + plAwayStats1617$AY
plStats1617$R = plHomeStats1617$HR + plAwayStats1617$AR
plStats1617$CS = plHomeStats1617$HCS + plAwayStats1617$ACS
colnames(plStats1617)[1] = "TEAM"

plMaxShots1617 <- plStats1617 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(-S)

plMaxShots1617 <- plMaxShots1617[1,]

plMaxShotsTarget1617 <- plStats1617 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(-ST)

plMaxShotsTarget1617 <- plMaxShotsTarget1617[1,]

plMinShots1617 <- plStats1617 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(S)

plMinShots1617 <- plMinShots1617[1,]

plMinShotsTarget1617 <- plStats1617 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(ST)

plMinShotsTarget1617 <- plMinShotsTarget1617[1,]

plMaxGoalsScored1617 <- plStats1617 %>%
  dplyr::select(TEAM, FTG) %>%
  dplyr::arrange(-FTG)

plMaxGoalsScored1617 <- plMaxGoalsScored1617[1,]

plMaxGoalsConceeded1617 <- plStats1617 %>%
  dplyr::select(TEAM, FTGC) %>%
  dplyr::arrange(-FTGC)

plMaxGoalsConceeded1617 <- plMaxGoalsConceeded1617[1,]

plMinGoalsScored1617 <- plStats1617 %>%
  dplyr::select(TEAM, FTG) %>%
  dplyr::arrange(FTG)

plMinGoalsScored1617 <- plMinGoalsScored1617[1,]

plMinGoalsConceeded1617 <- plStats1617 %>%
  dplyr::select(TEAM, FTGC) %>%
  dplyr::arrange(FTGC)

plMinGoalsConceeded1617 <- plMinGoalsConceeded1617[1,]



remove(plHomeTable1617half1)
remove(plHomeTable1617half2)
remove(plAwayTable1617half1)
remove(plAwayTable1617half2)
remove(plHomeExTop6Table1617)
remove(plAwayExTop6Table1617)
remove(plHomeAgTop6Table1617)
remove(plAwayAgTop6Table1617)
remove(plHomeTop6RestTable1617)
remove(plAwayTop6RestTable1617)
remove(plHomeTop6Table1617)
remove(plAwayTop6Table1617)

