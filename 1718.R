library(dplyr)
library(readr)
library(tidyr)

pldata1718 <- read_csv("Premier League Table 1718.csv")

##################
## 2017/18 Data ##
##################


plFixturesResultsData1718 <- pldata1718 %>%
  select(1:5, 10)

pldata1718$Date <- as.Date(pldata1718$Date, "%d-%m-%y")

pldata1718 <- pldata1718 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0),
         HOME_CS = if_else(FTAG == 0, 1, 0),
         AWAY_CS = if_else(FTHG == 0, 1, 0))

plHomeTable1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

pldata1718half1 <- pldata1718 %>%
  dplyr::filter(Date <= '2017-12-24')

pldata1718half2 <- pldata1718 %>%
  dplyr::filter(Date > '2017-12-24')  

plHomeTable1718half1 <- pldata1718half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1718half1 <- pldata1718half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTable1718half2 <- pldata1718half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1718half2 <- pldata1718half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6Table1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6Table1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeExTop6Table1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayExTop6Table1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeAgTop6Table1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayAgTop6Table1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6RestTable1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6RestTable1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plTable1718 <- plHomeTable1718

plTable1718$P <- plHomeTable1718$P + plAwayTable1718$P
plTable1718$W <- plHomeTable1718$W + plAwayTable1718$W
plTable1718$D <- plHomeTable1718$D + plAwayTable1718$D
plTable1718$L <- plHomeTable1718$L + plAwayTable1718$L
plTable1718$GF <- plHomeTable1718$GF + plAwayTable1718$GF
plTable1718$GA <- plHomeTable1718$GA + plAwayTable1718$GA
plTable1718$GD <- plTable1718$GF - plTable1718$GA
plTable1718$PTS <- plHomeTable1718$PTS + plAwayTable1718$PTS
plTable1718$AVG_PPG <- round(plTable1718$PTS / plTable1718$P, 2)
plTable1718$PROJECTED_POINTS <- round(plTable1718$AVG_PPG * 38, 0)
colnames(plTable1718)[1] = "TEAM"
colnames(plHomeTable1718)[1] = "TEAM"
colnames(plAwayTable1718)[1] = "TEAM"

plTable1718half1 <- plHomeTable1718half1

plTable1718half1$P <- plHomeTable1718half1$P + plAwayTable1718half1$P
plTable1718half1$W <- plHomeTable1718half1$W + plAwayTable1718half1$W
plTable1718half1$D <- plHomeTable1718half1$D + plAwayTable1718half1$D
plTable1718half1$L <- plHomeTable1718half1$L + plAwayTable1718half1$L
plTable1718half1$GF <- plHomeTable1718half1$GF + plAwayTable1718half1$GF
plTable1718half1$GA <- plHomeTable1718half1$GA + plAwayTable1718half1$GA
plTable1718half1$GD <- plTable1718half1$GF - plTable1718half1$GA
plTable1718half1$PTS <- plHomeTable1718half1$PTS + plAwayTable1718half1$PTS
plTable1718half1$AVG_PPG <- round(plTable1718half1$PTS / plTable1718half1$P, 2)
colnames(plTable1718half1)[1] = "TEAM"
colnames(plHomeTable1718half1)[1] = "TEAM"
colnames(plAwayTable1718half1)[1] = "TEAM"

plTable1718half2 <- plHomeTable1718half2

plTable1718half2$P <- plHomeTable1718half2$P + plAwayTable1718half2$P
plTable1718half2$W <- plHomeTable1718half2$W + plAwayTable1718half2$W
plTable1718half2$D <- plHomeTable1718half2$D + plAwayTable1718half2$D
plTable1718half2$L <- plHomeTable1718half2$L + plAwayTable1718half2$L
plTable1718half2$GF <- plHomeTable1718half2$GF + plAwayTable1718half2$GF
plTable1718half2$GA <- plHomeTable1718half2$GA + plAwayTable1718half2$GA
plTable1718half2$GD <- plTable1718half2$GF - plTable1718half2$GA
plTable1718half2$PTS <- plHomeTable1718half2$PTS + plAwayTable1718half2$PTS
plTable1718half2$AVG_PPG <- round(plTable1718half2$PTS / plTable1718half2$P, 2)
colnames(plTable1718half2)[1] = "TEAM"
colnames(plHomeTable1718half2)[1] = "TEAM"
colnames(plAwayTable1718half2)[1] = "TEAM"

plTop6Table1718 <- plHomeTop6Table1718

plTop6Table1718$P <- plHomeTop6Table1718$P + plAwayTop6Table1718$P
plTop6Table1718$W <- plHomeTop6Table1718$W + plAwayTop6Table1718$W
plTop6Table1718$D <- plHomeTop6Table1718$D + plAwayTop6Table1718$D
plTop6Table1718$L <- plHomeTop6Table1718$L + plAwayTop6Table1718$L
plTop6Table1718$GF <- plHomeTop6Table1718$GF + plAwayTop6Table1718$GF
plTop6Table1718$GA <- plHomeTop6Table1718$GA + plAwayTop6Table1718$GA
plTop6Table1718$GD <- plTop6Table1718$GF - plTop6Table1718$GA
plTop6Table1718$PTS <- plHomeTop6Table1718$PTS + plAwayTop6Table1718$PTS
colnames(plTop6Table1718)[1] = "TEAM"
colnames(plHomeTop6Table1718)[1] = "TEAM"
colnames(plAwayTop6Table1718)[1] = "TEAM"

plExTop6Table1718 <- plHomeExTop6Table1718

plExTop6Table1718$P <- plHomeExTop6Table1718$P + plAwayExTop6Table1718$P
plExTop6Table1718$W <- plHomeExTop6Table1718$W + plAwayExTop6Table1718$W
plExTop6Table1718$D <- plHomeExTop6Table1718$D + plAwayExTop6Table1718$D
plExTop6Table1718$L <- plHomeExTop6Table1718$L + plAwayExTop6Table1718$L
plExTop6Table1718$GF <- plHomeExTop6Table1718$GF + plAwayExTop6Table1718$GF
plExTop6Table1718$GA <- plHomeExTop6Table1718$GA + plAwayExTop6Table1718$GA
plExTop6Table1718$GD <- plExTop6Table1718$GF - plExTop6Table1718$GA
plExTop6Table1718$PTS <- plHomeExTop6Table1718$PTS + plAwayExTop6Table1718$PTS
colnames(plExTop6Table1718)[1] = "TEAM"
colnames(plHomeExTop6Table1718)[1] = "TEAM"
colnames(plAwayExTop6Table1718)[1] = "TEAM"

plAgTop6Table1718 <- plHomeAgTop6Table1718

plAgTop6Table1718$P <- plHomeAgTop6Table1718$P + plAwayAgTop6Table1718$P
plAgTop6Table1718$W <- plHomeAgTop6Table1718$W + plAwayAgTop6Table1718$W
plAgTop6Table1718$D <- plHomeAgTop6Table1718$D + plAwayAgTop6Table1718$D
plAgTop6Table1718$L <- plHomeAgTop6Table1718$L + plAwayAgTop6Table1718$L
plAgTop6Table1718$GF <- plHomeAgTop6Table1718$GF + plAwayAgTop6Table1718$GF
plAgTop6Table1718$GA <- plHomeAgTop6Table1718$GA + plAwayAgTop6Table1718$GA
plAgTop6Table1718$GD <- plAgTop6Table1718$GF - plAgTop6Table1718$GA
plAgTop6Table1718$PTS <- plHomeAgTop6Table1718$PTS + plAwayAgTop6Table1718$PTS
colnames(plAgTop6Table1718)[1] = "TEAM"
colnames(plHomeAgTop6Table1718)[1] = "TEAM"
colnames(plAwayAgTop6Table1718)[1] = "TEAM"

plTop6RestTable1718 <- plHomeTop6RestTable1718

plTop6RestTable1718$P <- plHomeTop6RestTable1718$P + plAwayTop6RestTable1718$P
plTop6RestTable1718$W <- plHomeTop6RestTable1718$W + plAwayTop6RestTable1718$W
plTop6RestTable1718$D <- plHomeTop6RestTable1718$D + plAwayTop6RestTable1718$D
plTop6RestTable1718$L <- plHomeTop6RestTable1718$L + plAwayTop6RestTable1718$L
plTop6RestTable1718$GF <- plHomeTop6RestTable1718$GF + plAwayTop6RestTable1718$GF
plTop6RestTable1718$GA <- plHomeTop6RestTable1718$GA + plAwayTop6RestTable1718$GA
plTop6RestTable1718$GD <- plTop6RestTable1718$GF - plTop6RestTable1718$GA
plTop6RestTable1718$PTS <- plHomeTop6RestTable1718$PTS + plAwayTop6RestTable1718$PTS
colnames(plTop6RestTable1718)[1] = "TEAM"
colnames(plHomeTop6RestTable1718)[1] = "TEAM"
colnames(plAwayTop6RestTable1718)[1] = "TEAM"

plTable1718 <- plTable1718[order(-plTable1718$PTS, -plTable1718$GD, -plTable1718$GF),]

plTable1718half1 <- plTable1718half1[order(-plTable1718half1$PTS, -plTable1718half1$GD, -plTable1718half1$GF),]

plTable1718half2 <- plTable1718half2[order(-plTable1718half2$PTS, -plTable1718half2$GD, -plTable1718half2$GF),]

plHomeTable1718 <- plHomeTable1718[order(-plHomeTable1718$PTS, -plHomeTable1718$GD, -plHomeTable1718$GF),]

plAwayTable1718 <- plAwayTable1718[order(-plAwayTable1718$PTS, -plAwayTable1718$GD, -plAwayTable1718$GF),]

plTop6Table1718 <- plTop6Table1718[order(-plTop6Table1718$PTS, -plTop6Table1718$GD, -plTop6Table1718$GF),]

plExTop6Table1718 <- plExTop6Table1718[order(-plExTop6Table1718$PTS, -plExTop6Table1718$GD, -plExTop6Table1718$GF),]

plAgTop6Table1718 <- plAgTop6Table1718[order(-plAgTop6Table1718$PTS, -plAgTop6Table1718$GD, -plAgTop6Table1718$GF),]

plTop6RestTable1718 <- plTop6RestTable1718[order(-plTop6RestTable1718$PTS, -plTop6RestTable1718$GD, -plTop6RestTable1718$GF),]

plHomeStats1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(HS = sum(HS), HST = sum(HST), FTHG = sum(FTHG),
            STR = paste(round(HST / HS * 100, 2), "%"), SCR = paste(round(FTHG / HS * 100, 2), "%"), 
            STCR = paste(round(FTHG / HST * 100, 2), "%"),
            HF = sum(HF), HC = sum(HC), HY = sum(HY), HR = sum(HR),
            FTAG = sum(FTAG), HCS = sum(HOME_CS)) %>%
  ungroup(HomeTeam)

plAwayStats1718 <- pldata1718 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(AS = sum(AS), AST = sum(AST), FTAG = sum(FTAG),
            STR = paste(round(AST / AS * 100, 2), "%"), SCR = paste(round(FTAG / AS * 100, 2), "%"), 
            STCR = paste(round(FTAG / AST * 100, 2), "%"),
            AF = sum(AF), AC = sum(AC), AY = sum(AY), AR = sum(AR),
            FTHG = sum(FTHG), ACS = sum(AWAY_CS)) %>%
  ungroup(AwayTeam)

plStats1718 <- plHomeStats1718 %>%
  select(1)

plStats1718$SEASON = "2017/18"
plStats1718$S <- plHomeStats1718$HS + plAwayStats1718$AS
plStats1718$ST <- plHomeStats1718$HST + plAwayStats1718$AST
plStats1718$FTG <- plHomeStats1718$FTHG + plAwayStats1718$FTAG
plStats1718$FTGC <- plHomeStats1718$FTAG + plAwayStats1718$FTHG
plStats1718$STR = paste(round(plStats1718$ST / plStats1718$S * 100, 2), "%") 
plStats1718$SCR = paste(round(plStats1718$FTG / plStats1718$S * 100, 2), "%")
plStats1718$STCR = paste(round(plStats1718$FTG / plStats1718$ST * 100, 2), "%")
plStats1718$F = plHomeStats1718$HF + plAwayStats1718$AF
plStats1718$C = plHomeStats1718$HC + plAwayStats1718$AC
plStats1718$Y = plHomeStats1718$HY + plAwayStats1718$AY
plStats1718$R = plHomeStats1718$HR + plAwayStats1718$AR
plStats1718$CS = plHomeStats1718$HCS + plAwayStats1718$ACS
colnames(plStats1718)[1] = "TEAM"

plMaxShots1718 <- plStats1718 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(-S)

plMaxShots1718 <- plMaxShots1718[1,]

plMaxShotsTarget1718 <- plStats1718 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(-ST)

plMaxShotsTarget1718 <- plMaxShotsTarget1718[1,]

plMinShots1718 <- plStats1718 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(S)

plMinShots1718 <- plMinShots1718[1,]

plMinShotsTarget1718 <- plStats1718 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(ST)

plMinShotsTarget1718 <- plMinShotsTarget1718[1,]

plMaxGoalsScored1718 <- plStats1718 %>%
  dplyr::select(TEAM, FTG) %>%
  dplyr::arrange(-FTG)

plMaxGoalsScored1718 <- plMaxGoalsScored1718[1,]

plMaxGoalsConceeded1718 <- plStats1718 %>%
  dplyr::select(TEAM, FTGC) %>%
  dplyr::arrange(-FTGC)

plMaxGoalsConceeded1718 <- plMaxGoalsConceeded1718[1,]

plMinGoalsScored1718 <- plStats1718 %>%
  dplyr::select(TEAM, FTG) %>%
  dplyr::arrange(FTG)

plMinGoalsScored1718 <- plMinGoalsScored1718[1,]

plMinGoalsConceeded1718 <- plStats1718 %>%
  dplyr::select(TEAM, FTGC) %>%
  dplyr::arrange(FTGC)

plMinGoalsConceeded1718 <- plMinGoalsConceeded1718[1,]




remove(plHomeTable1718half1)
remove(plHomeTable1718half2)
remove(plAwayTable1718half1)
remove(plAwayTable1718half2)
remove(plHomeExTop6Table1718)
remove(plAwayExTop6Table1718)
remove(plHomeAgTop6Table1718)
remove(plAwayAgTop6Table1718)
remove(plHomeTop6RestTable1718)
remove(plAwayTop6RestTable1718)
remove(plHomeTop6Table1718)
remove(plAwayTop6Table1718)

