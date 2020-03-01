library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

pldata1819 <- read_csv("Premier League Table 1819 2.csv")

plplayergoals1819 <- read_csv("PL Player Stats.csv")
plplayerassists1819 <- read_csv("PL Player Assists.csv")

##################
## 2018/19 Data ##
##################

pldata1819$MONTH <- month(pldata1819$Date)

plFixturesResultsData1819 <- pldata1819 %>%
  select(2, 3, 5, 6, 4, 11)

 

plTop6ResultsData1819 <- plFixturesResultsData1819 %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea"))
  

pldata1819$Date <- as.Date(pldata1819$Date, "%d-%m-%y")

pldata1819 <- pldata1819 %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0),
         HOME_CS = if_else(FTAG == 0, 1, 0),
         AWAY_CS = if_else(FTHG == 0, 1, 0))

pldata1819HT <- pldata1819 %>%
  mutate(HOME_WIN = if_else(HTHG > HTAG, 1, 0),
         AWAY_WIN = if_else(HTHG < HTAG, 1, 0),
         DRAW = if_else(HTHG == HTAG, 1, 0),
         HOME_LOSS = if_else(HTHG < HTAG, 1, 0),
         AWAY_LOSS = if_else(HTHG > HTAG, 1, 0))

pldata1819FT <- pldata1819 %>%
  mutate(HTHG2 = FTHG - HTHG,
         HTAG2 = FTAG - HTAG)

pldata1819FT <- pldata1819FT %>%
  mutate(HOME_WIN = if_else(HTHG2 > HTAG2, 1, 0),
         AWAY_WIN = if_else(HTHG2 < HTAG2, 1, 0),
         DRAW = if_else(HTHG2 == HTAG2, 1, 0),
         HOME_LOSS = if_else(HTHG2 < HTAG2, 1, 0),
         AWAY_LOSS = if_else(HTHG2 > HTAG2, 1, 0))

plHomeMonth1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam, MONTH) %>%
  summarise(P = sum(VOLUME), PTS = ((sum(HOME_WIN) * 3) + sum(DRAW)), AVG_PPG = PTS / sum(VOLUME)) %>%
  ungroup(HomeTeam)

plAwayMonth1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam, MONTH) %>%
  summarise(P = sum(VOLUME), PTS = ((sum(AWAY_WIN) * 3) + sum(DRAW)), AVG_PPG = PTS / sum(VOLUME)) %>%
  ungroup(AwayTeam)

plMonth1819 <- plHomeMonth1819

plMonth1819$P = plHomeMonth1819$P + plAwayMonth1819$P
plMonth1819$PTS = plHomeMonth1819$PTS + plAwayMonth1819$PTS
plMonth1819$AVG_PPG = round(plMonth1819$PTS / plMonth1819$P, 2)
plMonth1819$AVG_PPG_P = paste(plMonth1819$AVG_PPG, "(", plMonth1819$P, ")")
colnames(plMonth1819)[1] = "TEAM"

plHomeTable1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

pldata1819half1 <- pldata1819 %>%
  dplyr::filter(Date <= '2018-12-27')

pldata1819half2 <- pldata1819 %>%
  dplyr::filter(Date > '2018-12-27')  

plHomeTable1819half1 <- pldata1819half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1819half1 <- pldata1819half1 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTable1819half2 <- pldata1819half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayTable1819half2 <- pldata1819half2 %>%
  mutate(VOLUME = 1) %>%
  #filter(Matchday <= 17) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6Table1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6Table1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeExTop6Table1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayExTop6Table1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeAgTop6Table1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayAgTop6Table1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(!AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHomeTop6RestTable1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam) 

plAwayTop6RestTable1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  dplyr::filter(AwayTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")
                & !HomeTeam %in% c("Man City", "Man United", "Liverpool", "Tottenham", "Arsenal", "Chelsea")) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plTable1819 <- plHomeTable1819

plTable1819$P <- plHomeTable1819$P + plAwayTable1819$P
plTable1819$W <- plHomeTable1819$W + plAwayTable1819$W
plTable1819$D <- plHomeTable1819$D + plAwayTable1819$D
plTable1819$L <- plHomeTable1819$L + plAwayTable1819$L
plTable1819$GF <- plHomeTable1819$GF + plAwayTable1819$GF
plTable1819$GA <- plHomeTable1819$GA + plAwayTable1819$GA
plTable1819$GD <- plTable1819$GF - plTable1819$GA
plTable1819$PTS <- plHomeTable1819$PTS + plAwayTable1819$PTS
plTable1819$AVG_PPG <- round(plTable1819$PTS / plTable1819$P, 2)
plTable1819$PROJECTED_POINTS <- round(plTable1819$AVG_PPG * 38, 0)
colnames(plTable1819)[1] = "TEAM"


plTable1819half1 <- plHomeTable1819half1

plTable1819half1$P <- plHomeTable1819half1$P + plAwayTable1819half1$P
plTable1819half1$W <- plHomeTable1819half1$W + plAwayTable1819half1$W
plTable1819half1$D <- plHomeTable1819half1$D + plAwayTable1819half1$D
plTable1819half1$L <- plHomeTable1819half1$L + plAwayTable1819half1$L
plTable1819half1$GF <- plHomeTable1819half1$GF + plAwayTable1819half1$GF
plTable1819half1$GA <- plHomeTable1819half1$GA + plAwayTable1819half1$GA
plTable1819half1$GD <- plTable1819half1$GF - plTable1819half1$GA
plTable1819half1$PTS <- plHomeTable1819half1$PTS + plAwayTable1819half1$PTS
plTable1819half1$AVG_PPG <- round(plTable1819half1$PTS / plTable1819half1$P, 2)
colnames(plTable1819half1)[1] = "TEAM"
colnames(plHomeTable1819half1)[1] = "TEAM"
colnames(plAwayTable1819half1)[1] = "TEAM"

plTable1819half2 <- plHomeTable1819half2

plTable1819half2$P <- plHomeTable1819half2$P + plAwayTable1819half2$P
plTable1819half2$W <- plHomeTable1819half2$W + plAwayTable1819half2$W
plTable1819half2$D <- plHomeTable1819half2$D + plAwayTable1819half2$D
plTable1819half2$L <- plHomeTable1819half2$L + plAwayTable1819half2$L
plTable1819half2$GF <- plHomeTable1819half2$GF + plAwayTable1819half2$GF
plTable1819half2$GA <- plHomeTable1819half2$GA + plAwayTable1819half2$GA
plTable1819half2$GD <- plTable1819half2$GF - plTable1819half2$GA
plTable1819half2$PTS <- plHomeTable1819half2$PTS + plAwayTable1819half2$PTS
plTable1819half2$AVG_PPG <- round(plTable1819half2$PTS / plTable1819half2$P, 2)
colnames(plTable1819half2)[1] = "TEAM"
colnames(plHomeTable1819half2)[1] = "TEAM"
colnames(plAwayTable1819half2)[1] = "TEAM"

plTop6Table1819 <- plHomeTop6Table1819

plTop6Table1819$P <- plHomeTop6Table1819$P + plAwayTop6Table1819$P
plTop6Table1819$W <- plHomeTop6Table1819$W + plAwayTop6Table1819$W
plTop6Table1819$D <- plHomeTop6Table1819$D + plAwayTop6Table1819$D
plTop6Table1819$L <- plHomeTop6Table1819$L + plAwayTop6Table1819$L
plTop6Table1819$GF <- plHomeTop6Table1819$GF + plAwayTop6Table1819$GF
plTop6Table1819$GA <- plHomeTop6Table1819$GA + plAwayTop6Table1819$GA
plTop6Table1819$GD <- plTop6Table1819$GF - plTop6Table1819$GA
plTop6Table1819$PTS <- plHomeTop6Table1819$PTS + plAwayTop6Table1819$PTS
colnames(plTop6Table1819)[1] = "TEAM"
colnames(plHomeTop6Table1819)[1] = "TEAM"
colnames(plAwayTop6Table1819)[1] = "TEAM"

plExTop6Table1819 <- plHomeExTop6Table1819

plExTop6Table1819$P <- plHomeExTop6Table1819$P + plAwayExTop6Table1819$P
plExTop6Table1819$W <- plHomeExTop6Table1819$W + plAwayExTop6Table1819$W
plExTop6Table1819$D <- plHomeExTop6Table1819$D + plAwayExTop6Table1819$D
plExTop6Table1819$L <- plHomeExTop6Table1819$L + plAwayExTop6Table1819$L
plExTop6Table1819$GF <- plHomeExTop6Table1819$GF + plAwayExTop6Table1819$GF
plExTop6Table1819$GA <- plHomeExTop6Table1819$GA + plAwayExTop6Table1819$GA
plExTop6Table1819$GD <- plExTop6Table1819$GF - plExTop6Table1819$GA
plExTop6Table1819$PTS <- plHomeExTop6Table1819$PTS + plAwayExTop6Table1819$PTS
colnames(plExTop6Table1819)[1] = "TEAM"
colnames(plHomeExTop6Table1819)[1] = "TEAM"
colnames(plAwayExTop6Table1819)[1] = "TEAM"

plAgTop6Table1819 <- plHomeAgTop6Table1819

plAgTop6Table1819$P <- plHomeAgTop6Table1819$P + plAwayAgTop6Table1819$P
plAgTop6Table1819$W <- plHomeAgTop6Table1819$W + plAwayAgTop6Table1819$W
plAgTop6Table1819$D <- plHomeAgTop6Table1819$D + plAwayAgTop6Table1819$D
plAgTop6Table1819$L <- plHomeAgTop6Table1819$L + plAwayAgTop6Table1819$L
plAgTop6Table1819$GF <- plHomeAgTop6Table1819$GF + plAwayAgTop6Table1819$GF
plAgTop6Table1819$GA <- plHomeAgTop6Table1819$GA + plAwayAgTop6Table1819$GA
plAgTop6Table1819$GD <- plAgTop6Table1819$GF - plAgTop6Table1819$GA
plAgTop6Table1819$PTS <- plHomeAgTop6Table1819$PTS + plAwayAgTop6Table1819$PTS
colnames(plAgTop6Table1819)[1] = "TEAM"
colnames(plHomeAgTop6Table1819)[1] = "TEAM"
colnames(plAwayAgTop6Table1819)[1] = "TEAM"

plTop6RestTable1819 <- plHomeTop6RestTable1819

plTop6RestTable1819$P <- plHomeTop6RestTable1819$P + plAwayTop6RestTable1819$P
plTop6RestTable1819$W <- plHomeTop6RestTable1819$W + plAwayTop6RestTable1819$W
plTop6RestTable1819$D <- plHomeTop6RestTable1819$D + plAwayTop6RestTable1819$D
plTop6RestTable1819$L <- plHomeTop6RestTable1819$L + plAwayTop6RestTable1819$L
plTop6RestTable1819$GF <- plHomeTop6RestTable1819$GF + plAwayTop6RestTable1819$GF
plTop6RestTable1819$GA <- plHomeTop6RestTable1819$GA + plAwayTop6RestTable1819$GA
plTop6RestTable1819$GD <- plTop6RestTable1819$GF - plTop6RestTable1819$GA
plTop6RestTable1819$PTS <- plHomeTop6RestTable1819$PTS + plAwayTop6RestTable1819$PTS
colnames(plTop6RestTable1819)[1] = "TEAM"
colnames(plHomeTop6RestTable1819)[1] = "TEAM"
colnames(plAwayTop6RestTable1819)[1] = "TEAM"

plTable1819 <- plTable1819[order(-plTable1819$PTS, -plTable1819$GD, -plTable1819$GF),]

plTable1819half1 <- plTable1819half1[order(-plTable1819half1$PTS, -plTable1819half1$GD, -plTable1819half1$GF),]

plTable1819half2 <- plTable1819half2[order(-plTable1819half2$PTS, -plTable1819half2$GD, -plTable1819half2$GF),]

plHomeTable1819 <- plHomeTable1819[order(-plHomeTable1819$PTS, -plHomeTable1819$GD, -plHomeTable1819$GF),]

plAwayTable1819 <- plAwayTable1819[order(-plAwayTable1819$PTS, -plAwayTable1819$GD, -plAwayTable1819$GF),]

plTop6Table1819 <- plTop6Table1819[order(-plTop6Table1819$PTS, -plTop6Table1819$GD, -plTop6Table1819$GF),]

plExTop6Table1819 <- plExTop6Table1819[order(-plExTop6Table1819$PTS, -plExTop6Table1819$GD, -plExTop6Table1819$GF),]

plAgTop6Table1819 <- plAgTop6Table1819[order(-plAgTop6Table1819$PTS, -plAgTop6Table1819$GD, -plAgTop6Table1819$GF),]

plTop6RestTable1819 <- plTop6RestTable1819[order(-plTop6RestTable1819$PTS, -plTop6RestTable1819$GD, -plTop6RestTable1819$GF),]


remove(plHomeTable1819half1)
remove(plHomeTable1819half2)
remove(plAwayTable1819half1)
remove(plAwayTable1819half2)
remove(plHomeExTop6Table1819)
remove(plAwayExTop6Table1819)
remove(plHomeAgTop6Table1819)
remove(plAwayAgTop6Table1819)
remove(plHomeTop6RestTable1819)
remove(plAwayTop6RestTable1819)
remove(plHomeTop6Table1819)
remove(plAwayTop6Table1819)


 
plHomeHT <- pldata1819HT %>%
  dplyr::filter(!is.na(HTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(HTHG), GA = sum(HTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayHT <- pldata1819HT %>%
  dplyr::filter(!is.na(HTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(HTAG), GA = sum(HTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plHTTable1819 <- plHomeHT

plHTTable1819$P <- plHomeHT$P + plAwayHT$P
plHTTable1819$W <- plHomeHT$W + plAwayHT$W
plHTTable1819$D <- plHomeHT$D + plAwayHT$D
plHTTable1819$L <- plHomeHT$L + plAwayHT$L
plHTTable1819$GF <- plHomeHT$GF + plAwayHT$GF
plHTTable1819$GA <- plHomeHT$GA + plAwayHT$GA
plHTTable1819$GD <- plHTTable1819$GF - plHTTable1819$GA
plHTTable1819$PTS <- plHomeHT$PTS + plAwayHT$PTS
colnames(plHTTable1819)[1] = "TEAM"
colnames(plHomeHT)[1] = "TEAM"
colnames(plAwayHT)[1] = "TEAM"

plHTTable1819 <- plHTTable1819[order(-plHTTable1819$PTS, -plHTTable1819$GD, -plHTTable1819$GF),]

plHomeFT <- pldata1819FT %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(P = sum(VOLUME), W = sum(HOME_WIN), D = sum(DRAW), L = sum(HOME_LOSS), GF = sum(FTHG), GA = sum(FTAG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(HomeTeam)

plAwayFT <- pldata1819FT %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(P = sum(VOLUME), W = sum(AWAY_WIN), D = sum(DRAW), L = sum(AWAY_LOSS), GF = sum(FTAG), GA = sum(FTHG)) %>%
  mutate(GD = GF - GA, PTS = ((W * 3) + D)) %>%
  ungroup(AwayTeam)

plFTTable1819 <- plHomeFT

plFTTable1819$P <- plHomeFT$P + plAwayFT$P
plFTTable1819$W <- plHomeFT$W + plAwayFT$W
plFTTable1819$D <- plHomeFT$D + plAwayFT$D
plFTTable1819$L <- plHomeFT$L + plAwayFT$L
plFTTable1819$GF <- plHomeFT$GF + plAwayFT$GF
plFTTable1819$GA <- plHomeFT$GA + plAwayFT$GA
plFTTable1819$GD <- plFTTable1819$GF - plFTTable1819$GA
plFTTable1819$PTS <- plHomeFT$PTS + plAwayFT$PTS
colnames(plFTTable1819)[1] = "TEAM"
colnames(plHomeFT)[1] = "TEAM"
colnames(plAwayFT)[1] = "TEAM"

plFTTable1819 <- plFTTable1819[order(-plFTTable1819$PTS, -plFTTable1819$GD, -plFTTable1819$GF),]

plHomeStats1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTHG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(HomeTeam) %>%
  summarise(HS = sum(HS), HSF = sum(AS), HST = sum(HST), HSTF = sum(AST), FTHG = sum(FTHG),
            STR = paste(round(HST / HS * 100, 2), "%"), SCR = paste(round(FTHG / HS * 100, 2), "%"), 
            STCR = paste(round(FTHG / HST * 100, 2), "%"),
            HF = sum(HF), HC = sum(HC), HY = sum(HY), HR = sum(HR),
            FTHGC = sum(FTAG), HCS = sum(HOME_CS)) %>%
  ungroup(HomeTeam)

plHomeStats1819ext <- left_join(plHomeTable1819, plHomeStats1819, by = "HomeTeam")

plHomeStats1819ext <- plHomeStats1819ext %>%
  group_by(HomeTeam) %>%
  dplyr::mutate(AVG_S = round(HS / P, 2), AVG_SF = round(HSF / P, 2), AVG_ST = round(HST / P, 2), AVG_STF = round(HSTF / P, 2)
                , AVG_GSS = round(HS / GF, 2), AVG_GSC = round(HSF / GA, 2))

plAwayStats1819 <- pldata1819 %>%
  dplyr::filter(!is.na(FTAG)) %>%
  mutate(VOLUME = 1) %>%
  group_by(AwayTeam) %>%
  summarise(AS = sum(AS), ASF = sum(HS), AST = sum(AST), ASTF = sum(HST), FTAG = sum(FTAG),
            STR = paste(round(AST / AS * 100, 2), "%"), SCR = paste(round(FTAG / AS * 100, 2), "%"), 
            STCR = paste(round(FTAG / AST * 100, 2), "%"),
            AF = sum(AF), AC = sum(AC), AY = sum(AY), AR = sum(AR),
            FTAGC = sum(FTHG), ACS = sum(AWAY_CS)) %>%
  ungroup(AwayTeam)

plAwayStats1819ext <- left_join(plAwayTable1819, plAwayStats1819, by = "AwayTeam")

plAwayStats1819ext <- plAwayStats1819ext %>%
  group_by(AwayTeam) %>%
  dplyr::mutate(AVG_S = round(AS / P, 2), AVG_SF = round(ASF / P, 2), AVG_ST = round(AST / P, 2), AVG_STF = round(ASTF / P, 2)
                , AVG_GSS = round(AS / GF, 2), AVG_GSC = round(ASF / GA, 2))

plStats1819 <- union_all(plHomeStats1819, plAwayStats1819)
plStats1819[is.na(plStats1819)] <- 0
plStats1819$HomeTeam <- if_else(plStats1819$HomeTeam == 0, plStats1819$AwayTeam, plStats1819$HomeTeam)

colnames(plStats1819)[1] = "TEAM"

plStats1819 <- plStats1819 %>%
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

plShots1819 <- plStats1819 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(-S)

plShotsFaced1819 <- plStats1819 %>%
  dplyr::select(TEAM, SF) %>%
  dplyr::arrange(-SF)

plMaxShots1819 <- plShots1819[1,]

plMinShots1819 <- plShots1819[20,]

plMaxShotsFaced1819 <- plShotsFaced1819[1,]

plMinShotsFaced1819 <- plShotsFaced1819[20,]

plHomeShots1819 <- plHomeStats1819 %>%
  dplyr::select(HomeTeam, HS) %>%
  dplyr::arrange(-HS)

plAwayShots1819 <- plAwayStats1819 %>%
  dplyr::select(AwayTeam, AS) %>%
  dplyr::arrange(-AS)

plMaxShotsTarget1819 <- plStats1819 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(-ST)

plMaxShotsTarget1819 <- plMaxShotsTarget1819[1,]

plMaxShotsTargetFaced1819 <- plStats1819 %>%
  dplyr::select(TEAM, STF) %>%
  dplyr::arrange(-STF)

plMaxShotsTargetFaced1819 <- plMaxShotsTargetFaced1819[1,]

plMinShots1819 <- plStats1819 %>%
  dplyr::select(TEAM, S) %>%
  dplyr::arrange(S)

plMinShots1819 <- plMinShots1819[1,]

plMinShotsTarget1819 <- plStats1819 %>%
  dplyr::select(TEAM, ST) %>%
  dplyr::arrange(ST)

plMinShotsTargetFaced1819 <- plStats1819 %>%
  dplyr::select(TEAM, STF) %>%
  dplyr::arrange(STF)

plMinShotsTarget1819 <- plMinShotsTarget1819[1,]

plMinShotsTargetFaced1819 <- plMinShotsTargetFaced1819[1,]

plGoalsScored1819 <- plStats1819 %>%
  dplyr::select(TEAM, FTG) %>%
  dplyr::arrange(-FTG)

plHomeGoalsScored1819 <- plHomeStats1819 %>%
  dplyr::select(HomeTeam, FTHG) %>%
  dplyr::arrange(-FTHG)

plAwayGoalsScored1819 <- plAwayStats1819 %>%
  dplyr::select(AwayTeam, FTAG) %>%
  dplyr::arrange(-FTAG)

plMaxGoalsScored1819 <- plGoalsScored1819[1,]
plMinGoalsScored1819 <- plGoalsScored1819[20,]


plGoalsConceeded1819 <- plStats1819 %>%
  dplyr::select(TEAM, FTGC) %>%
  dplyr::arrange(-FTGC)

plHomeGoalsConceeded1819 <- plHomeStats1819 %>%
  dplyr::select(HomeTeam, FTHGC) %>%
  dplyr::arrange(-FTHGC)

plAwayGoalsConceeded1819 <- plAwayStats1819 %>%
  dplyr::select(AwayTeam, FTAGC) %>%
  dplyr::arrange(-FTAGC)

plMaxGoalsConceeded1819 <- plGoalsConceeded1819[1,]
plMinGoalsConceeded1819 <- plGoalsConceeded1819[20,]

plCleanSheets1819 <- plStats1819 %>%
  dplyr::select(TEAM, CS) %>%
  dplyr::arrange(-CS)

plHomeCleanSheets1819 <- plHomeStats1819 %>%
  dplyr::select(HomeTeam, HCS) %>%
  dplyr::arrange(-HCS)

plAwayCleanSheets1819 <- plAwayStats1819 %>%
  dplyr::select(AwayTeam, ACS) %>%
  dplyr::arrange(-ACS)

plMaxCleanSheets1819 <- plCleanSheets1819[1,]
plMinCleanSheets1819 <- plCleanSheets1819[20,]

plYellowCards1819 <- plStats1819 %>%
  dplyr::select(TEAM, Y) %>%
  dplyr::arrange(-Y)

plHomeYellowCards1819 <- plHomeStats1819 %>%
  dplyr::select(HomeTeam, HY) %>%
  dplyr::arrange(-HY)

plAwayYellowCards1819 <- plAwayStats1819 %>%
  dplyr::select(AwayTeam, AY) %>%
  dplyr::arrange(-AY)

plMaxYellowCards1819 <- plYellowCards1819[1,]
plMinYellowCards1819 <- plYellowCards1819[20,]


plCHPoints <- pldata1819 %>%
  select(HomeTeam, Date, HOME_WIN, DRAW, FTHG, FTAG) %>%
  group_by(HomeTeam) %>%
  mutate(HG = FTHG, HGC = FTAG) %>%
  ungroup()

colnames(plCHPoints)[1] = "TEAM" 

plCAPoints <- pldata1819 %>%
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



plAChart <- plCPoints %>%
  select (TEAM, MD2, CPOINTS, RANKING)

plAChart0 <- plAChart %>%
  group_by(TEAM) %>%
  summarise(MD2 = 0, CPOINTS = 0) %>%
  mutate(RANKING = order(order(TEAM, decreasing=FALSE)))

plAChart <- union_all(plAChart0, plAChart)

plplayergoals1819 <- plplayergoals1819 %>%
  dplyr::mutate(VOLUME = 1) %>%
  dplyr::group_by(Team) %>%
  dplyr::mutate(Number_of_Players = sum(VOLUME), Team_Goals = sum(Goals), CGoals = cumsum(Goals)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Goal_Proportion = paste(round(Goals / Team_Goals * 100, 2), "%"),
                Team_Proportion = paste(round(CGoals / Team_Goals * 100, 2), "%")) %>%
  select(Player, Team, Nationality, Goals, Goal_Proportion, Team_Proportion)

plplayerassists1819 <- plplayerassists1819 %>%
  dplyr::mutate(VOLUME = 1) %>%
  dplyr::group_by(Team) %>%
  dplyr::mutate(Number_of_Players = sum(VOLUME), Team_Assists = sum(Assists), CAssists = cumsum(Assists)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Assist_Proportion = paste(round(Assists / Team_Assists * 100, 2), "%"),
                Team_Proportion = paste(round(CAssists / Team_Assists * 100, 2), "%")) %>%
  select(Player, Team, Nationality, Assists, Assist_Proportion, Team_Proportion)

plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Manchester City", "Man City")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Manchester United", "Man United")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Newcastle United", "Newcastle")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="AFC Bournemouth", "Bournemouth")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Brighton and Hove Albion", "Brighton")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Huddersfield Town", "Huddersfield")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Leicester City", "Leicester")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Tottenham Hotspur", "Tottenham")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Wolverhampton Wanderers", "Wolves")
plplayergoals1819$Team <- replace(plplayergoals1819$Team, plplayergoals1819$Team=="Cardiff City", "Cardiff")

plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Manchester City", "Man City")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Manchester United", "Man United")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Newcastle United", "Newcastle")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="AFC Bournemouth", "Bournemouth")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Brighton and Hove Albion", "Brighton")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Huddersfield Town", "Huddersfield")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Leicester City", "Leicester")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Tottenham Hotspur", "Tottenham")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Wolverhampton Wanderers", "Wolves")
plplayerassists1819$Team <- replace(plplayerassists1819$Team, plplayerassists1819$Team=="Cardiff City", "Cardiff")

