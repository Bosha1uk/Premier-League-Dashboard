library(dplyr)
library(readr)
library(tidyr)

pldata1415 <- read_csv("Premier League Table 1415.csv")
##################
## 2016/17 Data ##
##################


plFixturesResultsData1415 <- pldata1415 %>%
  select(2:6, 11)

pldata1415$Date <- as.Date(pldata1415$Date, "%d-%m-%y")

pldata1415 <- pldata1415 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0))

plHomeTable1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

pldata1415half1 <- pldata1415 %>%
  dplyr::filter(Date <= '2017-01-01')

pldata1415half2 <- pldata1415 %>%
  dplyr::filter(Date > '2017-01-01')  

plHomeTable1415half1 <- pldata1415half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1415half1 <- pldata1415half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTable1415half2 <- pldata1415half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1415half2 <- pldata1415half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6Table1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6Table1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeExTop6Table1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayExTop6Table1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeAgTop6Table1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayAgTop6Table1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6RestTable1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6RestTable1415 <- pldata1415 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plTable1415 <- plHomeTable1415

plTable1415$P <- plHomeTable1415$P + plAwayTable1415$P
plTable1415$W <- plHomeTable1415$W + plAwayTable1415$W
plTable1415$D <- plHomeTable1415$D + plAwayTable1415$D
plTable1415$L <- plHomeTable1415$L + plAwayTable1415$L
plTable1415$GF <- plHomeTable1415$GF + plAwayTable1415$GF
plTable1415$GA <- plHomeTable1415$GA + plAwayTable1415$GA
plTable1415$GD <- plTable1415$GF - plTable1415$GA
plTable1415$PTS <- plHomeTable1415$PTS + plAwayTable1415$PTS
plTable1415$AVG_PPG <- round(plTable1415$PTS / plTable1415$P, 2)
plTable1415$PROJECTED_POINTS <- round(plTable1415$AVG_PPG * 38, 0)
colnames(plTable1415)[1] = "TEAM"
colnames(plHomeTable1415)[1] = "TEAM"
colnames(plAwayTable1415)[1] = "TEAM"

plTable1415half1 <- plHomeTable1415half1

plTable1415half1$P <- plHomeTable1415half1$P + plAwayTable1415half1$P
plTable1415half1$W <- plHomeTable1415half1$W + plAwayTable1415half1$W
plTable1415half1$D <- plHomeTable1415half1$D + plAwayTable1415half1$D
plTable1415half1$L <- plHomeTable1415half1$L + plAwayTable1415half1$L
plTable1415half1$GF <- plHomeTable1415half1$GF + plAwayTable1415half1$GF
plTable1415half1$GA <- plHomeTable1415half1$GA + plAwayTable1415half1$GA
plTable1415half1$GD <- plTable1415half1$GF - plTable1415half1$GA
plTable1415half1$PTS <- plHomeTable1415half1$PTS + plAwayTable1415half1$PTS
plTable1415half1$AVG_PPG <- round(plTable1415half1$PTS / plTable1415half1$P, 2)
colnames(plTable1415half1)[1] = "TEAM"
colnames(plHomeTable1415half1)[1] = "TEAM"
colnames(plAwayTable1415half1)[1] = "TEAM"

plTable1415half2 <- plHomeTable1415half2

plTable1415half2$P <- plHomeTable1415half2$P + plAwayTable1415half2$P
plTable1415half2$W <- plHomeTable1415half2$W + plAwayTable1415half2$W
plTable1415half2$D <- plHomeTable1415half2$D + plAwayTable1415half2$D
plTable1415half2$L <- plHomeTable1415half2$L + plAwayTable1415half2$L
plTable1415half2$GF <- plHomeTable1415half2$GF + plAwayTable1415half2$GF
plTable1415half2$GA <- plHomeTable1415half2$GA + plAwayTable1415half2$GA
plTable1415half2$GD <- plTable1415half2$GF - plTable1415half2$GA
plTable1415half2$PTS <- plHomeTable1415half2$PTS + plAwayTable1415half2$PTS
plTable1415half2$AVG_PPG <- round(plTable1415half2$PTS / plTable1415half2$P, 2)
colnames(plTable1415half2)[1] = "TEAM"
colnames(plHomeTable1415half2)[1] = "TEAM"
colnames(plAwayTable1415half2)[1] = "TEAM"

plTop6Table1415 <- plHomeTop6Table1415

plTop6Table1415$P <- plHomeTop6Table1415$P + plAwayTop6Table1415$P
plTop6Table1415$W <- plHomeTop6Table1415$W + plAwayTop6Table1415$W
plTop6Table1415$D <- plHomeTop6Table1415$D + plAwayTop6Table1415$D
plTop6Table1415$L <- plHomeTop6Table1415$L + plAwayTop6Table1415$L
plTop6Table1415$GF <- plHomeTop6Table1415$GF + plAwayTop6Table1415$GF
plTop6Table1415$GA <- plHomeTop6Table1415$GA + plAwayTop6Table1415$GA
plTop6Table1415$GD <- plTop6Table1415$GF - plTop6Table1415$GA
plTop6Table1415$PTS <- plHomeTop6Table1415$PTS + plAwayTop6Table1415$PTS
colnames(plTop6Table1415)[1] = "TEAM"
colnames(plHomeTop6Table1415)[1] = "TEAM"
colnames(plAwayTop6Table1415)[1] = "TEAM"

plExTop6Table1415 <- plHomeExTop6Table1415

plExTop6Table1415$P <- plHomeExTop6Table1415$P + plAwayExTop6Table1415$P
plExTop6Table1415$W <- plHomeExTop6Table1415$W + plAwayExTop6Table1415$W
plExTop6Table1415$D <- plHomeExTop6Table1415$D + plAwayExTop6Table1415$D
plExTop6Table1415$L <- plHomeExTop6Table1415$L + plAwayExTop6Table1415$L
plExTop6Table1415$GF <- plHomeExTop6Table1415$GF + plAwayExTop6Table1415$GF
plExTop6Table1415$GA <- plHomeExTop6Table1415$GA + plAwayExTop6Table1415$GA
plExTop6Table1415$GD <- plExTop6Table1415$GF - plExTop6Table1415$GA
plExTop6Table1415$PTS <- plHomeExTop6Table1415$PTS + plAwayExTop6Table1415$PTS
colnames(plExTop6Table1415)[1] = "TEAM"
colnames(plHomeExTop6Table1415)[1] = "TEAM"
colnames(plAwayExTop6Table1415)[1] = "TEAM"

plAgTop6Table1415 <- plHomeAgTop6Table1415

plAgTop6Table1415$P <- plHomeAgTop6Table1415$P + plAwayAgTop6Table1415$P
plAgTop6Table1415$W <- plHomeAgTop6Table1415$W + plAwayAgTop6Table1415$W
plAgTop6Table1415$D <- plHomeAgTop6Table1415$D + plAwayAgTop6Table1415$D
plAgTop6Table1415$L <- plHomeAgTop6Table1415$L + plAwayAgTop6Table1415$L
plAgTop6Table1415$GF <- plHomeAgTop6Table1415$GF + plAwayAgTop6Table1415$GF
plAgTop6Table1415$GA <- plHomeAgTop6Table1415$GA + plAwayAgTop6Table1415$GA
plAgTop6Table1415$GD <- plAgTop6Table1415$GF - plAgTop6Table1415$GA
plAgTop6Table1415$PTS <- plHomeAgTop6Table1415$PTS + plAwayAgTop6Table1415$PTS
colnames(plAgTop6Table1415)[1] = "TEAM"
colnames(plHomeAgTop6Table1415)[1] = "TEAM"
colnames(plAwayAgTop6Table1415)[1] = "TEAM"

plTop6RestTable1415 <- plHomeTop6RestTable1415

plTop6RestTable1415$P <- plHomeTop6RestTable1415$P + plAwayTop6RestTable1415$P
plTop6RestTable1415$W <- plHomeTop6RestTable1415$W + plAwayTop6RestTable1415$W
plTop6RestTable1415$D <- plHomeTop6RestTable1415$D + plAwayTop6RestTable1415$D
plTop6RestTable1415$L <- plHomeTop6RestTable1415$L + plAwayTop6RestTable1415$L
plTop6RestTable1415$GF <- plHomeTop6RestTable1415$GF + plAwayTop6RestTable1415$GF
plTop6RestTable1415$GA <- plHomeTop6RestTable1415$GA + plAwayTop6RestTable1415$GA
plTop6RestTable1415$GD <- plTop6RestTable1415$GF - plTop6RestTable1415$GA
plTop6RestTable1415$PTS <- plHomeTop6RestTable1415$PTS + plAwayTop6RestTable1415$PTS
colnames(plTop6RestTable1415)[1] = "TEAM"
colnames(plHomeTop6RestTable1415)[1] = "TEAM"
colnames(plAwayTop6RestTable1415)[1] = "TEAM"

plTable1415 <- plTable1415[order(-plTable1415$PTS, -plTable1415$GD, -plTable1415$GF),]

plTable1415half1 <- plTable1415half1[order(-plTable1415half1$PTS, -plTable1415half1$GD, -plTable1415half1$GF),]

plTable1415half2 <- plTable1415half2[order(-plTable1415half2$PTS, -plTable1415half2$GD, -plTable1415half2$GF),]

plHomeTable1415 <- plHomeTable1415[order(-plHomeTable1415$PTS, -plHomeTable1415$GD, -plHomeTable1415$GF),]

plAwayTable1415 <- plAwayTable1415[order(-plAwayTable1415$PTS, -plAwayTable1415$GD, -plAwayTable1415$GF),]

plTop6Table1415 <- plTop6Table1415[order(-plTop6Table1415$PTS, -plTop6Table1415$GD, -plTop6Table1415$GF),]

plExTop6Table1415 <- plExTop6Table1415[order(-plExTop6Table1415$PTS, -plExTop6Table1415$GD, -plExTop6Table1415$GF),]

plAgTop6Table1415 <- plAgTop6Table1415[order(-plAgTop6Table1415$PTS, -plAgTop6Table1415$GD, -plAgTop6Table1415$GF),]

plTop6RestTable1415 <- plTop6RestTable1415[order(-plTop6RestTable1415$PTS, -plTop6RestTable1415$GD, -plTop6RestTable1415$GF),]


remove(plHomeTable1415half1)
remove(plHomeTable1415half2)
remove(plAwayTable1415half1)
remove(plAwayTable1415half2)
remove(plHomeExTop6Table1415)
remove(plAwayExTop6Table1415)
remove(plHomeAgTop6Table1415)
remove(plAwayAgTop6Table1415)
remove(plHomeTop6RestTable1415)
remove(plAwayTop6RestTable1415)
remove(plHomeTop6Table1415)
remove(plAwayTop6Table1415)

