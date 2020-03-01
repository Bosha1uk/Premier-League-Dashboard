library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

pldata1920 <- read_csv("Premier League Table 1920.csv")
pldata1819 <- read_csv("Premier League Table 1819 2.csv")
pldata1718 <- read_csv("Premier League Table 1718.csv")
pldata1617 <- read_csv("Premier League Table 1617.csv")
pldata1516 <- read_csv("Premier League Table 1516.csv")

pldata1920 <- pldata1920 %>%
  select(-Time)

plDataAll <- union_all(pldata1920, pldata1819)
plDataAll <- union_all(plDataAll, pldata1718)
plDataAll <- union_all(plDataAll, pldata1617)
plDataAll <- union_all(plDataAll, pldata1516)

plDataAll <- plDataAll[rev(order(as.Date(plDataAll$Date, format="%d/%m/%Y"))),]

plDataAll <- plDataAll %>%
  select(-1) %>%
  select(1:22) %>%
  mutate(HOME_WIN = if_else(FTHG > FTAG, 1, 0),
         AWAY_WIN = if_else(FTHG < FTAG, 1, 0),
         DRAW = if_else(FTHG == FTAG, 1, 0),
         HOME_LOSS = if_else(FTHG < FTAG, 1, 0),
         AWAY_LOSS = if_else(FTHG > FTAG, 1, 0),
         HOME_CS = if_else(FTAG == 0, 1, 0),
         AWAY_CS = if_else(FTHG == 0, 1, 0),
         HT_HOME_WIN = if_else(HTHG > HTAG, 1, 0),
         HT_AWAY_WIN = if_else(HTHG < HTAG, 1, 0),
         HT_DRAW = if_else(HTHG == HTAG, 1, 0),
         HT_HOME_LOSS = if_else(HTHG < HTAG, 1, 0),
         HT_AWAY_LOSS = if_else(HTHG > HTAG, 1, 0),
         HTHG2 = FTHG - HTHG,
         HTAG2 = FTAG - HTAG,
         FT_HOME_WIN = if_else(HTHG2 > HTAG2, 1, 0),
         FT_AWAY_WIN = if_else(HTHG2 < HTAG2, 1, 0),
         FT_DRAW = if_else(HTHG2 == HTAG2, 1, 0),
         FT_HOME_LOSS = if_else(HTHG2 < HTAG2, 1, 0),
         FT_AWAY_LOSS = if_else(HTHG2 > HTAG2, 1, 0))

plDataAll2 <- plDataAll

plDataAll2 <- plDataAll2[order(plDataAll2$Date),]

plDataAll3 <- plDataAll %>%
  group_by(HomeTeam) %>%
  summarise(HOME_WIN = sum(HOME_WIN))

plDataAll3 <- plDataAll3[order(plDataAll3$HomeTeam),]

plDataResults <- plDataAll %>%
  select (1, 2, 4, 5, 3, 10)
  

