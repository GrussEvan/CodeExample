#Goal of code is to analyze play-by-play data to identify trends in successful outcomes throughout a game. 
#How does possession success change as a player accounts for more possessions throughout a game?
#Do certain players perform better in different quarters?

#install libraries
library(dplyr)
library(ggplot2)

# Read in 2020-2021 play by play data
SeasonTwoOne =read.csv("D:/NBA PBP/2021-combined-stats.csv",header=T,na.strings ="?")

#Identify & keep a subset of columns
keeps <- c("game_id","data_set", "date", "period", "away_score", "home_score", "remaining_time",
           "play_id", "team", "event_type", "assist", "player", "opponent", "points", "reason",
           "result", "type")
SeasonTwoOne = SeasonTwoOne[keeps]

#Filter on desired play types and remove team turnovers
SeasonTwoOne <-SeasonTwoOne[(SeasonTwoOne$event_type=="turnover" | 
                                SeasonTwoOne$reason =="s.foul" |
                               SeasonTwoOne$reason =="p.foul" |
                                SeasonTwoOne$event_type=="shot"
                                ),]

SeasonTwoOne <-SeasonTwoOne[!(SeasonTwoOne$type=="shot clock" | 
                                    SeasonTwoOne$type=="8-second violation" |
                                    SeasonTwoOne$type=="5-second violation"
                                    )
                              ,]

#Identify primary player for each type of play
SeasonTwoOne$FinalPlayer <- SeasonTwoOne$opponent

SeasonTwoOne$FinalPlayer <- ifelse(!(SeasonTwoOne$event_type == "foul"), 
                                   as.character(SeasonTwoOne$player), as.character(SeasonTwoOne$FinalPlayer))

  
SeasonTwoOne <- SeasonTwoOne[order(SeasonTwoOne$FinalPlayer, SeasonTwoOne$game_id, 
                                   SeasonTwoOne$play_id),]

SeasonTwoOne$AndOne <- ifelse(lag(SeasonTwoOne$result, 1L)=="made" 
                          & lag(SeasonTwoOne$play_id, 1L)==SeasonTwoOne$play_id - 1
                          & lag(SeasonTwoOne$game_id, 1L)==SeasonTwoOne$game_id,
                        1,0)

SeasonTwoOne <-SeasonTwoOne[!(SeasonTwoOne$AndOne==1 ),]


SeasonTwoOne$PosessionCount <- ifelse(lag(SeasonTwoOne$result, 1L)=="made" 
                              & lag(SeasonTwoOne$play_id, 1L)==SeasonTwoOne$play_id - 1
                              & lag(SeasonTwoOne$game_id, 1L)==SeasonTwoOne$game_id,
                              1,0)

Assisteds <- SeasonTwoOne[!(SeasonTwoOne$assist=="" ),]
Assisteds$FinalPlayer <- Assisteds$assist

SeasonTwoOneFinal <- rbind(SeasonTwoOne, Assisteds)

SeasonTwoOneFinal <- SeasonTwoOneFinal[order(SeasonTwoOneFinal$FinalPlayer, SeasonTwoOneFinal$game_id, 
                                   SeasonTwoOneFinal$play_id),]

# Count positive & negative outcomes
SeasonTwoOneFinal$Positive <- ifelse(SeasonTwoOneFinal$event_type == "foul" 
                                      | SeasonTwoOneFinal$result== "made",
                                      1,0)

SeasonTwoOneFinal$Negative <- ifelse(SeasonTwoOneFinal$Positive == 1,
                                     0,1)

SeasonTwoOneFinal$row_num <- seq.int(nrow(SeasonTwoOneFinal))

SeasonTwoOneFinal$GameCount <- ifelse(lag(SeasonTwoOneFinal$FinalPlayer, 1L)== SeasonTwoOneFinal$FinalPlayer
                                  & lag(SeasonTwoOneFinal$game_id, 1L)==SeasonTwoOneFinal$game_id,
                                  0,SeasonTwoOneFinal$row_num)
SeasonTwoOneFinal$GameCount <- ifelse(SeasonTwoOneFinal$row_num == 1, 1, SeasonTwoOneFinal$GameCount)

GamePlayer <- SeasonTwoOneFinal[!(SeasonTwoOneFinal$GameCount==0 ),]

TwoOneFinal <- merge(x=SeasonTwoOneFinal, y=GamePlayer, by = c("FinalPlayer" , "game_id"), all.x = TRUE )

TwoOneFinal$TotalCount <- TwoOneFinal$row_num.x - TwoOneFinal$GameCount.y + 1

keeps <- c("TotalCount","FinalPlayer", "Positive.x", "Negative.x", "event_type.x", "result.x", "period.x",
             "date.x", "reason.x", "type.x")

TwoOneFinal = TwoOneFinal[keeps]

Averages <- TwoOneFinal %>% group_by(FinalPlayer)  %>%
            summarise(total_Possessions = sum(Positive.x + Negative.x),
            total_positive = sum(Positive.x), 
            .groups = 'drop')

#Filter out players with <= 200 possessions
Averages <- Averages[(Averages$total_Possessions>200 ),]

Averages$PositiveRate <- Averages$total_positive/Averages$total_Possessions

#Summarize by player and posession count
PlayerByPossession <- TwoOneFinal %>% group_by(FinalPlayer, TotalCount)  %>%
                      summarise(total_Possessions = sum(Positive.x + Negative.x),
                       total_positive = sum(Positive.x), 
                       .groups = 'drop')

PlayerByPossession <- PlayerByPossession[(PlayerByPossession$total_Possessions>5 ),]

PlayerByPossession$PositiveRate <- PlayerByPossession$total_positive/PlayerByPossession$total_Possessions

#League outcome by possession count
ByPossession <- TwoOneFinal %>% group_by(TotalCount)  %>%
  summarise(total_Possessions = sum(Positive.x + Negative.x),
            total_positive = sum(Positive.x), 
            .groups = 'drop')

ByPossession <- ByPossession[(ByPossession$total_Possessions>5 ),]

ByPossession$PositiveRate <- ByPossession$total_positive/ByPossession$total_Possessions

#League outcomes by quarter
ByQuarter <- TwoOneFinal %>% group_by(period.x)  %>%
  summarise(total_Possessions = sum(Positive.x + Negative.x),
            total_positive = sum(Positive.x), 
            .groups = 'drop')

ByQuarter <- ByQuarter[(ByQuarter$total_Possessions>5 ),]

ByQuarter$PositiveRate <- ByQuarter$total_positive/ByQuarter$total_Possessions

#Player outcomes by quarter
PlayerByQuarter <- TwoOneFinal %>% group_by(FinalPlayer, period.x)  %>%
  summarise(total_Possessions = sum(Positive.x + Negative.x),
            total_positive = sum(Positive.x), 
            .groups = 'drop')

PlayerByQuarter <- PlayerByQuarter[(PlayerByQuarter$total_Possessions>50 ),]

PlayerByQuarter$PositiveRate <- PlayerByQuarter$total_positive/PlayerByQuarter$total_Possessions


PlayerByPossessionByQuarter <- TwoOneFinal %>% group_by(FinalPlayer, TotalCount, period.x)  %>%
  summarise(total_Possessions = sum(Positive.x + Negative.x),
            total_positive = sum(Positive.x), 
            .groups = 'drop')

PlayerByPossessionByQuarter <- PlayerByPossessionByQuarter[(PlayerByPossessionByQuarter$total_Possessions>5 ),]

PlayerByPossessionByQuarter$PositiveRate <- PlayerByPossessionByQuarter$total_positive/PlayerByPossessionByQuarter$total_Possessions

#band by 5 possessions
# Create ranges of 5
PlayerByPossession$Possession <- cut(PlayerByPossession$TotalCount, breaks = seq(1, 201, by = 5), labels = paste(seq(1, 196, by = 5)), include.lowest = TRUE, right = FALSE)

PlayerByPossessionRange <- PlayerByPossession %>% group_by(FinalPlayer, Possession)  %>%
  summarise(total_Possessions = sum(total_Possessions),
            total_positive = sum(total_positive), 
            .groups = 'drop')
PlayerByPossessionRange$PositiveRate <- PlayerByPossessionRange$total_positive/PlayerByPossessionRange$total_Possessions


#Graphs
Luka <- PlayerByPossessionRange[(PlayerByPossessionRange$FinalPlayer == "Luka Doncic"),]
LukaQ <-  PlayerByQuarter[(PlayerByQuarter$FinalPlayer == "Luka Doncic"),]
ggplot(Luka, aes(x=Possession, y=PositiveRate, group = FinalPlayer)) + geom_line()
ggplot(LukaQ, aes(x=period.x, y=PositiveRate)) + geom_line()


ggplot(subset(PlayerByQuarter, total_Possessions > 500 & period.x < 5), aes(x=period.x, y=PositiveRate, colour = FinalPlayer)) + geom_line()
ggplot(ByQuarter, aes(x=period.x, y=PositiveRate)) + geom_line()

# View By Quarter
ByQuarter$FinalPlayer <- "TotalLeague"
mylist <- list("TotalLeague", "James Harden", "Luka Doncic", "Giannis Antetokounmpo", 
               "Joel Embiid", "Jayson Tatum", "Stephen Curry", "Damian Lillard", "Trae Young")
shortlist <- filter(rbind(PlayerByQuarter, subset(ByQuarter, period.x < 5)), FinalPlayer %in% mylist)
ggplot(shortlist
       , aes(x=period.x, y=PositiveRate, colour = FinalPlayer)) + geom_line()

# View By Possession Range
mylist <- list("James Harden", "Luka Doncic", "Giannis Antetokounmpo", 
               "Joel Embiid", "Jayson Tatum", "Stephen Curry", "Damian Lillard", "Trae Young")
shortlist <- filter(PlayerByPossessionRange, FinalPlayer %in% mylist)
ggplot(shortlist
       , aes(x=Possession, y=PositiveRate, group = FinalPlayer, colour = FinalPlayer)) + geom_line()

#Output
write.csv(PlayerByPossession,"D:/NBA PBP/PlayerByPossession.csv", row.names = FALSE)
write.csv(PlayerByPossession,"D:/NBA PBP/PlayerByQuarter.csv", row.names = FALSE)
write.csv(PlayerByPossession,"D:/NBA PBP/ByQuarter.csv", row.names = FALSE)
write.csv(Averages,"D:/NBA PBP/Players.csv", row.names = FALSE)

 