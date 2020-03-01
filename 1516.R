library(dplyr)
library(readr)
library(tidyr)

pldata1516 <- read_csv("Premier League Table 1516.csv")

##################
## 2016/17 Data ##
##################


plFixturesResultsData1516 <- pldata1516 %>%
  select(2:6, 11)

pldata1516$Date <- as.Date(pldata1516$Date, "%d-%m-%y")

pldata1516 <- pldata1516 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0))

plHomeTable1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

pldata1516half1 <- pldata1516 %>%
  dplyr::filter(Date <= '2017-01-01')

pldata1516half2 <- pldata1516 %>%
  dplyr::filter(Date > '2017-01-01')  

plHomeTable1516half1 <- pldata1516half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1516half1 <- pldata1516half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTable1516half2 <- pldata1516half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1516half2 <- pldata1516half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6Table1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6Table1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeExTop6Table1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayExTop6Table1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeAgTop6Table1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayAgTop6Table1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6RestTable1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6RestTable1516 <- pldata1516 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plTable1516 <- plHomeTable1516

plTable1516$P <- plHomeTable1516$P + plAwayTable1516$P
plTable1516$W <- plHomeTable1516$W + plAwayTable1516$W
plTable1516$D <- plHomeTable1516$D + plAwayTable1516$D
plTable1516$L <- plHomeTable1516$L + plAwayTable1516$L
plTable1516$GF <- plHomeTable1516$GF + plAwayTable1516$GF
plTable1516$GA <- plHomeTable1516$GA + plAwayTable1516$GA
plTable1516$GD <- plTable1516$GF - plTable1516$GA
plTable1516$PTS <- plHomeTable1516$PTS + plAwayTable1516$PTS
plTable1516$AVG_PPG <- round(plTable1516$PTS / plTable1516$P, 2)
plTable1516$PROJECTED_POINTS <- round(plTable1516$AVG_PPG * 38, 0)
colnames(plTable1516)[1] = "TEAM"
colnames(plHomeTable1516)[1] = "TEAM"
colnames(plAwayTable1516)[1] = "TEAM"

plTable1516half1 <- plHomeTable1516half1

plTable1516half1$P <- plHomeTable1516half1$P + plAwayTable1516half1$P
plTable1516half1$W <- plHomeTable1516half1$W + plAwayTable1516half1$W
plTable1516half1$D <- plHomeTable1516half1$D + plAwayTable1516half1$D
plTable1516half1$L <- plHomeTable1516half1$L + plAwayTable1516half1$L
plTable1516half1$GF <- plHomeTable1516half1$GF + plAwayTable1516half1$GF
plTable1516half1$GA <- plHomeTable1516half1$GA + plAwayTable1516half1$GA
plTable1516half1$GD <- plTable1516half1$GF - plTable1516half1$GA
plTable1516half1$PTS <- plHomeTable1516half1$PTS + plAwayTable1516half1$PTS
plTable1516half1$AVG_PPG <- round(plTable1516half1$PTS / plTable1516half1$P, 2)
colnames(plTable1516half1)[1] = "TEAM"
colnames(plHomeTable1516half1)[1] = "TEAM"
colnames(plAwayTable1516half1)[1] = "TEAM"

plTable1516half2 <- plHomeTable1516half2

plTable1516half2$P <- plHomeTable1516half2$P + plAwayTable1516half2$P
plTable1516half2$W <- plHomeTable1516half2$W + plAwayTable1516half2$W
plTable1516half2$D <- plHomeTable1516half2$D + plAwayTable1516half2$D
plTable1516half2$L <- plHomeTable1516half2$L + plAwayTable1516half2$L
plTable1516half2$GF <- plHomeTable1516half2$GF + plAwayTable1516half2$GF
plTable1516half2$GA <- plHomeTable1516half2$GA + plAwayTable1516half2$GA
plTable1516half2$GD <- plTable1516half2$GF - plTable1516half2$GA
plTable1516half2$PTS <- plHomeTable1516half2$PTS + plAwayTable1516half2$PTS
plTable1516half2$AVG_PPG <- round(plTable1516half2$PTS / plTable1516half2$P, 2)
colnames(plTable1516half2)[1] = "TEAM"
colnames(plHomeTable1516half2)[1] = "TEAM"
colnames(plAwayTable1516half2)[1] = "TEAM"

plTop6Table1516 <- plHomeTop6Table1516

plTop6Table1516$P <- plHomeTop6Table1516$P + plAwayTop6Table1516$P
plTop6Table1516$W <- plHomeTop6Table1516$W + plAwayTop6Table1516$W
plTop6Table1516$D <- plHomeTop6Table1516$D + plAwayTop6Table1516$D
plTop6Table1516$L <- plHomeTop6Table1516$L + plAwayTop6Table1516$L
plTop6Table1516$GF <- plHomeTop6Table1516$GF + plAwayTop6Table1516$GF
plTop6Table1516$GA <- plHomeTop6Table1516$GA + plAwayTop6Table1516$GA
plTop6Table1516$GD <- plTop6Table1516$GF - plTop6Table1516$GA
plTop6Table1516$PTS <- plHomeTop6Table1516$PTS + plAwayTop6Table1516$PTS
colnames(plTop6Table1516)[1] = "TEAM"
colnames(plHomeTop6Table1516)[1] = "TEAM"
colnames(plAwayTop6Table1516)[1] = "TEAM"

plExTop6Table1516 <- plHomeExTop6Table1516

plExTop6Table1516$P <- plHomeExTop6Table1516$P + plAwayExTop6Table1516$P
plExTop6Table1516$W <- plHomeExTop6Table1516$W + plAwayExTop6Table1516$W
plExTop6Table1516$D <- plHomeExTop6Table1516$D + plAwayExTop6Table1516$D
plExTop6Table1516$L <- plHomeExTop6Table1516$L + plAwayExTop6Table1516$L
plExTop6Table1516$GF <- plHomeExTop6Table1516$GF + plAwayExTop6Table1516$GF
plExTop6Table1516$GA <- plHomeExTop6Table1516$GA + plAwayExTop6Table1516$GA
plExTop6Table1516$GD <- plExTop6Table1516$GF - plExTop6Table1516$GA
plExTop6Table1516$PTS <- plHomeExTop6Table1516$PTS + plAwayExTop6Table1516$PTS
colnames(plExTop6Table1516)[1] = "TEAM"
colnames(plHomeExTop6Table1516)[1] = "TEAM"
colnames(plAwayExTop6Table1516)[1] = "TEAM"

plAgTop6Table1516 <- plHomeAgTop6Table1516

plAgTop6Table1516$P <- plHomeAgTop6Table1516$P + plAwayAgTop6Table1516$P
plAgTop6Table1516$W <- plHomeAgTop6Table1516$W + plAwayAgTop6Table1516$W
plAgTop6Table1516$D <- plHomeAgTop6Table1516$D + plAwayAgTop6Table1516$D
plAgTop6Table1516$L <- plHomeAgTop6Table1516$L + plAwayAgTop6Table1516$L
plAgTop6Table1516$GF <- plHomeAgTop6Table1516$GF + plAwayAgTop6Table1516$GF
plAgTop6Table1516$GA <- plHomeAgTop6Table1516$GA + plAwayAgTop6Table1516$GA
plAgTop6Table1516$GD <- plAgTop6Table1516$GF - plAgTop6Table1516$GA
plAgTop6Table1516$PTS <- plHomeAgTop6Table1516$PTS + plAwayAgTop6Table1516$PTS
colnames(plAgTop6Table1516)[1] = "TEAM"
colnames(plHomeAgTop6Table1516)[1] = "TEAM"
colnames(plAwayAgTop6Table1516)[1] = "TEAM"

plTop6RestTable1516 <- plHomeTop6RestTable1516

plTop6RestTable1516$P <- plHomeTop6RestTable1516$P + plAwayTop6RestTable1516$P
plTop6RestTable1516$W <- plHomeTop6RestTable1516$W + plAwayTop6RestTable1516$W
plTop6RestTable1516$D <- plHomeTop6RestTable1516$D + plAwayTop6RestTable1516$D
plTop6RestTable1516$L <- plHomeTop6RestTable1516$L + plAwayTop6RestTable1516$L
plTop6RestTable1516$GF <- plHomeTop6RestTable1516$GF + plAwayTop6RestTable1516$GF
plTop6RestTable1516$GA <- plHomeTop6RestTable1516$GA + plAwayTop6RestTable1516$GA
plTop6RestTable1516$GD <- plTop6RestTable1516$GF - plTop6RestTable1516$GA
plTop6RestTable1516$PTS <- plHomeTop6RestTable1516$PTS + plAwayTop6RestTable1516$PTS
colnames(plTop6RestTable1516)[1] = "TEAM"
colnames(plHomeTop6RestTable1516)[1] = "TEAM"
colnames(plAwayTop6RestTable1516)[1] = "TEAM"

plTable1516 <- plTable1516[order(-plTable1516$PTS, -plTable1516$GD, -plTable1516$GF),]

plTable1516half1 <- plTable1516half1[order(-plTable1516half1$PTS, -plTable1516half1$GD, -plTable1516half1$GF),]

plTable1516half2 <- plTable1516half2[order(-plTable1516half2$PTS, -plTable1516half2$GD, -plTable1516half2$GF),]

plHomeTable1516 <- plHomeTable1516[order(-plHomeTable1516$PTS, -plHomeTable1516$GD, -plHomeTable1516$GF),]

plAwayTable1516 <- plAwayTable1516[order(-plAwayTable1516$PTS, -plAwayTable1516$GD, -plAwayTable1516$GF),]

plTop6Table1516 <- plTop6Table1516[order(-plTop6Table1516$PTS, -plTop6Table1516$GD, -plTop6Table1516$GF),]

plExTop6Table1516 <- plExTop6Table1516[order(-plExTop6Table1516$PTS, -plExTop6Table1516$GD, -plExTop6Table1516$GF),]

plAgTop6Table1516 <- plAgTop6Table1516[order(-plAgTop6Table1516$PTS, -plAgTop6Table1516$GD, -plAgTop6Table1516$GF),]

plTop6RestTable1516 <- plTop6RestTable1516[order(-plTop6RestTable1516$PTS, -plTop6RestTable1516$GD, -plTop6RestTable1516$GF),]


remove(plHomeTable1516half1)
remove(plHomeTable1516half2)
remove(plAwayTable1516half1)
remove(plAwayTable1516half2)
remove(plHomeExTop6Table1516)
remove(plAwayExTop6Table1516)
remove(plHomeAgTop6Table1516)
remove(plAwayAgTop6Table1516)
remove(plHomeTop6RestTable1516)
remove(plAwayTop6RestTable1516)
remove(plHomeTop6Table1516)
remove(plAwayTop6Table1516)

