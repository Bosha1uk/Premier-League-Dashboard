library(dplyr)
library(readr)
library(tidyr)

pldata1314 <- read_csv("Premier League Table 1314.csv")

##################
## 2016/17 Data ##
##################


plFixturesResultsData1314 <- pldata1314 %>%
  select(2:6, 11)

pldata1314$Date <- as.Date(pldata1314$Date, "%d-%m-%y")

pldata1314 <- pldata1314 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0))

plHomeTable1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

pldata1314half1 <- pldata1314 %>%
  dplyr::filter(Date <= '2017-01-01')

pldata1314half2 <- pldata1314 %>%
  dplyr::filter(Date > '2017-01-01')  

plHomeTable1314half1 <- pldata1314half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1314half1 <- pldata1314half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTable1314half2 <- pldata1314half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1314half2 <- pldata1314half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6Table1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6Table1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeExTop6Table1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayExTop6Table1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeAgTop6Table1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayAgTop6Table1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6RestTable1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6RestTable1314 <- pldata1314 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plTable1314 <- plHomeTable1314

plTable1314$P <- plHomeTable1314$P + plAwayTable1314$P
plTable1314$W <- plHomeTable1314$W + plAwayTable1314$W
plTable1314$D <- plHomeTable1314$D + plAwayTable1314$D
plTable1314$L <- plHomeTable1314$L + plAwayTable1314$L
plTable1314$GF <- plHomeTable1314$GF + plAwayTable1314$GF
plTable1314$GA <- plHomeTable1314$GA + plAwayTable1314$GA
plTable1314$GD <- plTable1314$GF - plTable1314$GA
plTable1314$PTS <- plHomeTable1314$PTS + plAwayTable1314$PTS
plTable1314$AVG_PPG <- round(plTable1314$PTS / plTable1314$P, 2)
plTable1314$PROJECTED_POINTS <- round(plTable1314$AVG_PPG * 38, 0)
colnames(plTable1314)[1] = "TEAM"
colnames(plHomeTable1314)[1] = "TEAM"
colnames(plAwayTable1314)[1] = "TEAM"

plTable1314half1 <- plHomeTable1314half1

plTable1314half1$P <- plHomeTable1314half1$P + plAwayTable1314half1$P
plTable1314half1$W <- plHomeTable1314half1$W + plAwayTable1314half1$W
plTable1314half1$D <- plHomeTable1314half1$D + plAwayTable1314half1$D
plTable1314half1$L <- plHomeTable1314half1$L + plAwayTable1314half1$L
plTable1314half1$GF <- plHomeTable1314half1$GF + plAwayTable1314half1$GF
plTable1314half1$GA <- plHomeTable1314half1$GA + plAwayTable1314half1$GA
plTable1314half1$GD <- plTable1314half1$GF - plTable1314half1$GA
plTable1314half1$PTS <- plHomeTable1314half1$PTS + plAwayTable1314half1$PTS
plTable1314half1$AVG_PPG <- round(plTable1314half1$PTS / plTable1314half1$P, 2)
colnames(plTable1314half1)[1] = "TEAM"
colnames(plHomeTable1314half1)[1] = "TEAM"
colnames(plAwayTable1314half1)[1] = "TEAM"

plTable1314half2 <- plHomeTable1314half2

plTable1314half2$P <- plHomeTable1314half2$P + plAwayTable1314half2$P
plTable1314half2$W <- plHomeTable1314half2$W + plAwayTable1314half2$W
plTable1314half2$D <- plHomeTable1314half2$D + plAwayTable1314half2$D
plTable1314half2$L <- plHomeTable1314half2$L + plAwayTable1314half2$L
plTable1314half2$GF <- plHomeTable1314half2$GF + plAwayTable1314half2$GF
plTable1314half2$GA <- plHomeTable1314half2$GA + plAwayTable1314half2$GA
plTable1314half2$GD <- plTable1314half2$GF - plTable1314half2$GA
plTable1314half2$PTS <- plHomeTable1314half2$PTS + plAwayTable1314half2$PTS
plTable1314half2$AVG_PPG <- round(plTable1314half2$PTS / plTable1314half2$P, 2)
colnames(plTable1314half2)[1] = "TEAM"
colnames(plHomeTable1314half2)[1] = "TEAM"
colnames(plAwayTable1314half2)[1] = "TEAM"

plTop6Table1314 <- plHomeTop6Table1314

plTop6Table1314$P <- plHomeTop6Table1314$P + plAwayTop6Table1314$P
plTop6Table1314$W <- plHomeTop6Table1314$W + plAwayTop6Table1314$W
plTop6Table1314$D <- plHomeTop6Table1314$D + plAwayTop6Table1314$D
plTop6Table1314$L <- plHomeTop6Table1314$L + plAwayTop6Table1314$L
plTop6Table1314$GF <- plHomeTop6Table1314$GF + plAwayTop6Table1314$GF
plTop6Table1314$GA <- plHomeTop6Table1314$GA + plAwayTop6Table1314$GA
plTop6Table1314$GD <- plTop6Table1314$GF - plTop6Table1314$GA
plTop6Table1314$PTS <- plHomeTop6Table1314$PTS + plAwayTop6Table1314$PTS
colnames(plTop6Table1314)[1] = "TEAM"
colnames(plHomeTop6Table1314)[1] = "TEAM"
colnames(plAwayTop6Table1314)[1] = "TEAM"

plExTop6Table1314 <- plHomeExTop6Table1314

plExTop6Table1314$P <- plHomeExTop6Table1314$P + plAwayExTop6Table1314$P
plExTop6Table1314$W <- plHomeExTop6Table1314$W + plAwayExTop6Table1314$W
plExTop6Table1314$D <- plHomeExTop6Table1314$D + plAwayExTop6Table1314$D
plExTop6Table1314$L <- plHomeExTop6Table1314$L + plAwayExTop6Table1314$L
plExTop6Table1314$GF <- plHomeExTop6Table1314$GF + plAwayExTop6Table1314$GF
plExTop6Table1314$GA <- plHomeExTop6Table1314$GA + plAwayExTop6Table1314$GA
plExTop6Table1314$GD <- plExTop6Table1314$GF - plExTop6Table1314$GA
plExTop6Table1314$PTS <- plHomeExTop6Table1314$PTS + plAwayExTop6Table1314$PTS
colnames(plExTop6Table1314)[1] = "TEAM"
colnames(plHomeExTop6Table1314)[1] = "TEAM"
colnames(plAwayExTop6Table1314)[1] = "TEAM"

plAgTop6Table1314 <- plHomeAgTop6Table1314

plAgTop6Table1314$P <- plHomeAgTop6Table1314$P + plAwayAgTop6Table1314$P
plAgTop6Table1314$W <- plHomeAgTop6Table1314$W + plAwayAgTop6Table1314$W
plAgTop6Table1314$D <- plHomeAgTop6Table1314$D + plAwayAgTop6Table1314$D
plAgTop6Table1314$L <- plHomeAgTop6Table1314$L + plAwayAgTop6Table1314$L
plAgTop6Table1314$GF <- plHomeAgTop6Table1314$GF + plAwayAgTop6Table1314$GF
plAgTop6Table1314$GA <- plHomeAgTop6Table1314$GA + plAwayAgTop6Table1314$GA
plAgTop6Table1314$GD <- plAgTop6Table1314$GF - plAgTop6Table1314$GA
plAgTop6Table1314$PTS <- plHomeAgTop6Table1314$PTS + plAwayAgTop6Table1314$PTS
colnames(plAgTop6Table1314)[1] = "TEAM"
colnames(plHomeAgTop6Table1314)[1] = "TEAM"
colnames(plAwayAgTop6Table1314)[1] = "TEAM"

plTop6RestTable1314 <- plHomeTop6RestTable1314

plTop6RestTable1314$P <- plHomeTop6RestTable1314$P + plAwayTop6RestTable1314$P
plTop6RestTable1314$W <- plHomeTop6RestTable1314$W + plAwayTop6RestTable1314$W
plTop6RestTable1314$D <- plHomeTop6RestTable1314$D + plAwayTop6RestTable1314$D
plTop6RestTable1314$L <- plHomeTop6RestTable1314$L + plAwayTop6RestTable1314$L
plTop6RestTable1314$GF <- plHomeTop6RestTable1314$GF + plAwayTop6RestTable1314$GF
plTop6RestTable1314$GA <- plHomeTop6RestTable1314$GA + plAwayTop6RestTable1314$GA
plTop6RestTable1314$GD <- plTop6RestTable1314$GF - plTop6RestTable1314$GA
plTop6RestTable1314$PTS <- plHomeTop6RestTable1314$PTS + plAwayTop6RestTable1314$PTS
colnames(plTop6RestTable1314)[1] = "TEAM"
colnames(plHomeTop6RestTable1314)[1] = "TEAM"
colnames(plAwayTop6RestTable1314)[1] = "TEAM"

plTable1314 <- plTable1314[order(-plTable1314$PTS, -plTable1314$GD, -plTable1314$GF),]

plTable1314half1 <- plTable1314half1[order(-plTable1314half1$PTS, -plTable1314half1$GD, -plTable1314half1$GF),]

plTable1314half2 <- plTable1314half2[order(-plTable1314half2$PTS, -plTable1314half2$GD, -plTable1314half2$GF),]

plHomeTable1314 <- plHomeTable1314[order(-plHomeTable1314$PTS, -plHomeTable1314$GD, -plHomeTable1314$GF),]

plAwayTable1314 <- plAwayTable1314[order(-plAwayTable1314$PTS, -plAwayTable1314$GD, -plAwayTable1314$GF),]

plTop6Table1314 <- plTop6Table1314[order(-plTop6Table1314$PTS, -plTop6Table1314$GD, -plTop6Table1314$GF),]

plExTop6Table1314 <- plExTop6Table1314[order(-plExTop6Table1314$PTS, -plExTop6Table1314$GD, -plExTop6Table1314$GF),]

plAgTop6Table1314 <- plAgTop6Table1314[order(-plAgTop6Table1314$PTS, -plAgTop6Table1314$GD, -plAgTop6Table1314$GF),]

plTop6RestTable1314 <- plTop6RestTable1314[order(-plTop6RestTable1314$PTS, -plTop6RestTable1314$GD, -plTop6RestTable1314$GF),]


remove(plHomeTable1314half1)
remove(plHomeTable1314half2)
remove(plAwayTable1314half1)
remove(plAwayTable1314half2)
remove(plHomeExTop6Table1314)
remove(plAwayExTop6Table1314)
remove(plHomeAgTop6Table1314)
remove(plAwayAgTop6Table1314)
remove(plHomeTop6RestTable1314)
remove(plAwayTop6RestTable1314)
remove(plHomeTop6Table1314)
remove(plAwayTop6Table1314)

