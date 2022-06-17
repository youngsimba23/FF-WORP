#Install and load necessary packages
#install.packages("dplyr")
#install.packages("nflverse")
#install.packages("ffsimulator") # CRAN
#install.packages("ffsimulator", repos = "https://ffverse.r-universe.dev")
#install.packages("remotes")
#remotes::install_github("DynastyProcess/ffscrapr")

library(nflreadr)
library(dplyr)
library(ffscrapr)
library(ffsimulator)
library(ggplot2)
library(ggridges)
library(tidyr)

#SET WORKING DIRECTORY
setwd("C:/Users/Acer/Documents/Fantasy Football/WORP Test V1")

#Load Data from 2010-2021 and create database
playerstats<-load_player_stats(c(2010:2021),c("offense"))

#Bringing in positions
playerdata<-load_ff_playerids()
playerstats<-merge.data.frame(playerstats,playerdata,by.x = 'player_id',by.y = 'gsis_id',all.x = T)

#Delete non-offense positions (also happens to remove players without gsis ids)
playerstats<-playerstats %>% filter(position=='QB'|position=='WR'|position=='RB'|position=='TE')

#Remove unnecessary columns
playerstats<-playerstats %>% select(player_id,merge_name,position,team,season,week,season_type,
                                    completions,attempts,passing_yards,passing_tds,interceptions,sacks, sack_fumbles,sack_fumbles_lost,passing_2pt_conversions,
                                    carries,rushing_yards,rushing_tds,rushing_fumbles,rushing_fumbles_lost,rushing_first_downs,rushing_2pt_conversions,
                                    receptions,receiving_yards,receiving_tds,receiving_fumbles,receiving_fumbles_lost,receiving_first_downs,
                                    receiving_2pt_conversions)
playerstats<-playerstats %>% rename(player_name = merge_name)

#Filter for weeks 1-16
playerstats<-playerstats %>% filter(week<=16)

#Creating fumble and fumble lost
playerstats<-playerstats %>% mutate(fumbles=sack_fumbles+rushing_fumbles+receiving_fumbles)
playerstats<-playerstats %>% mutate(fumbles_lost=sack_fumbles_lost+rushing_fumbles_lost+receiving_fumbles_lost)

#Create position receiving scoring columns
playerstats$rec_wr<- ifelse(playerstats$position=='WR',playerstats$receptions,0)
playerstats$rec_wr <- `is.na<-`(playerstats$rec_wr,0)
playerstats$rec_rb<- ifelse(playerstats$position=='RB',playerstats$receptions,0)
playerstats$rec_rb <- `is.na<-`(playerstats$rec_rb,0)
playerstats$rec_te<- ifelse(playerstats$position=='TE',playerstats$receptions,0)
playerstats$rec_te <- `is.na<-`(playerstats$rec_te,0)
playerstats$rec_qb<- ifelse(playerstats$position=='QB',playerstats$receptions,0)
playerstats$rec_qb <- `is.na<-`(playerstats$rec_qb,0)

#Bring in league settings from Sleeper

leaguelist <- sleeper_userleagues("ENTER SLEEPER USERNAME HERE",2022)

#ABOVE ITEMS ONLY NEED TO BE RAN FIRST TIME SESSION IS STARTED

#START HERE TO RUN FOR NEW LEAGUE

LID <- leaguelist %>% filter(league_name == "ENTER SLEEPER LEAGUE NAME HERE") %>% pull(league_id)

query <- paste('league/',LID,sep = "")
leagueinfo <- sleeper_getendpoint(query)

leaguescoring <- leagueinfo$content$scoring_settings


#Where I start to copy Adeiko's code

#DEFINE LEAGUE SCORING
scoringvalues<-c("passing_yards"=leaguescoring$pass_yd,
                 "passing_tds"=leaguescoring$pass_td,
                 "interceptions"=leaguescoring$pass_int,
                 "sacks"=leaguescoring$pass_sack * -1,
                 "completions"=leaguescoring$pass_cmp,
                 "incompletions"=leaguescoring$pass_inc,
                 "passing_2pt_conversions"=leaguescoring$pass_2pt,
                 "carries"=leaguescoring$rush_att,
                 "rushing_yards"=leaguescoring$rush_yd,
                 "rushing_tds"=leaguescoring$rush_td,
                 "rushing_first_downs"=leaguescoring$rush_fd,
                 "rushing_2pt_conversions"=leaguescoring$rush_2pt,
                 "rec_qb"=sum(leaguescoring$rec,leaguescoring$bonus_rec_qb),
                 "rec_rb"=sum(leaguescoring$rec,leaguescoring$bonus_rec_rb),
                 "rec_wr"=sum(leaguescoring$rec,leaguescoring$bonus_rec_wr),
                 "rec_te"=sum(leaguescoring$rec,leaguescoring$bonus_rec_te),
                 "receiving_yards"=leaguescoring$rec_yd,
                 "receiving_tds"=leaguescoring$rec_td,
                 "receiving_first_downs"=leaguescoring$rec_fd,
                 "receiving_2pt_conversations"=leaguescoring$rec_2pt,
                 "fumbles"=leaguescoring$fum,
                 "fumbles_lost"=leaguescoring$fum_lost)

#DEFINE ROSTER SPOTS
RP<-c("TM"=leagueinfo$content$total_rosters,
      "QB"=sum(leagueinfo$content$roster_positions == 'QB') + sum(leagueinfo$content$roster_positions == 'SUPER_FLEX'),
      "RB"=sum(leagueinfo$content$roster_positions == 'RB'),
      "WR"=sum(leagueinfo$content$roster_positions == 'WR') + sum(leagueinfo$content$roster_positions == 'REC_FLEX'),
      "TE"=sum(leagueinfo$content$roster_positions == 'TE'),
      "FLEX"=sum(leagueinfo$content$roster_positions == 'FLEX'))

#Generate the scoring of each player per week based on ScoringValues
fp_scoring <- playerstats %>% dplyr::group_by(player_id,week,season) %>% dplyr::summarise(
  Player = dplyr::first(player_name),
  PaYardScore = sum(passing_yards*scoringvalues["passing_yards"],na.rm = T),
  PaTDScore = sum(passing_tds*scoringvalues["passing_tds"],na.rm = T),
  PaIntScore = sum(interceptions*scoringvalues["interceptions"],na.rm = T),
  PaSackScore = sum(sacks*scoringvalues["sacks"],na.rm = T),
  PaCompScore = sum(completions*scoringvalues["completions"],na.rm = T),
  PaIncScore = sum((attempts-completions)*scoringvalues["incompletions"],na.rm = T),
  PaTwopScore = sum(passing_2pt_conversions*scoringvalues["passing_2pt_conversions"],na.rm = T),
  RuAttemptScore = sum(carries*scoringvalues["carries"],na.rm = T),
  RuYardScore = sum(rushing_yards*scoringvalues["rushing_yards"],na.rm = T),
  RuTDScore = sum(rushing_tds*scoringvalues["rushing_tds"],na.rm = T),
  RuFDScore = sum(rushing_first_downs*scoringvalues["rushing_first_downs"],na.rm = T),
  RuTwopScore = sum(rushing_2pt_conversions*scoringvalues["rushing_2pt_converions"],na.rm = T),
  ReRecScore = sum((rec_qb*scoringvalues["rec_qb"]),(rec_wr*scoringvalues["rec_wr"]),(rec_rb*scoringvalues["rec_rb"]),(rec_te*scoringvalues["rec_te"]),na.rm = T),
  ReYardScore = sum(receiving_yards*scoringvalues["receiving_yards"],na.rm = T),
  ReTDScore = sum(receiving_tds*scoringvalues["receiving_tds"],na.rm = T),
  ReFDScore = sum(receiving_first_downs*scoringvalues["receiving_first_downs"],na.rm = T),
  ReTwopScore = sum(receiving_2pt_conversions*scoringvalues["receiving_2pt_conversions"],na.rm = T),
  FumbleScore = sum((fumbles*scoringvalues["fumbles"]),(fumbles_lost*scoringvalues["fumbles_lost"]),na.rm = T),
  PaFP = sum(PaYardScore,PaTDScore,PaIntScore,PaSackScore,PaCompScore,PaIncScore,PaTwopScore),
  RuFP = sum(RuAttemptScore,RuYardScore,RuTDScore,RuFDScore,RuTwopScore),
  ReFP = sum(ReRecScore,ReYardScore,ReTDScore,ReFDScore,ReTwopScore),
  MiscFP = sum(FumbleScore),
  CustomScoring = sum(PaFP,RuFP,ReFP,MiscFP)
)

# Generate a Rank and a PosRank for each week using the custom scoring.
StatCustom <- playerstats %>% 
  dplyr::select(player_name,player_id,week,season,position) %>% 
  dplyr::left_join((fp_scoring %>% select(player_id,week,season,CustomScoring)), by = c("player_id","week","season")) %>% arrange(position, CustomScoring) %>% 
  dplyr::group_by(season,week,position) %>% 
  dplyr::mutate(PosRank=rank(-CustomScoring,ties.method = "first")) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season,week) %>% 
  dplyr::mutate(Rank=rank(-CustomScoring,ties.method = "first"))

# Generate the Average of Scoring by regular positions by getting the average scoring of the (<PosSpot>*<Teams>).
Base_PlayersAvg = StatCustom %>% 
  dplyr::filter(!is.na(CustomScoring)) %>% 
  dplyr::filter(dplyr::case_when(position=="QB" ~ PosRank <= (RP["TM"]*RP["QB"]),position=="RB" ~ PosRank <= (RP["TM"]*RP["RB"]),position=="WR" ~ PosRank <= (RP["TM"]*RP["WR"]),position=="TE" ~ PosRank <= (RP["TM"]*RP["TE"]))) %>%
  dplyr::group_by(season,week) %>% 
  dplyr::summarise(
    AvgQB = mean(CustomScoring[position=='QB']),
    StdQB = sd(CustomScoring[position=='QB']),
    AvgRB = mean(CustomScoring[position=='RB']),
    StdRB = sd(CustomScoring[position=='RB']),
    AvgWR = mean(CustomScoring[position=='WR']),
    StdWR = sd(CustomScoring[position=='WR']),
    AvgTE = mean(CustomScoring[position=='TE']),
    StdTE = sd(CustomScoring[position=='TE']),
  )

# Generate the Average of Flex by getting the scoring of the (<FlexSpots>*<Teams>) by players that are not used in the other regular positions.
FlexData = StatCustom %>%
  dplyr::filter((!is.na(CustomScoring)) & (position=="RB"|position=="WR"|position=="TE")) %>%
  dplyr::filter(dplyr::case_when(position=="TE" ~ PosRank > (RP["TM"]*RP["TE"]),position=="RB" ~ PosRank > (RP["TM"]*RP["RB"]),position=="WR" ~ PosRank > (RP["TM"]*RP["WR"])))%>%
  dplyr::group_by(season,week) %>%
  dplyr::slice_max(order_by = CustomScoring, n = (RP["TM"]*RP["FLEX"])) %>%
  dplyr::summarise(
    AvgFLEX = mean(CustomScoring),
    StdFLEX = sd(CustomScoring)
  )  

# Generate the replacement level Player scoring
RPAvg = StatCustom %>%
  dplyr::filter(!is.na(CustomScoring))%>%
  dplyr::filter(dplyr::case_when(position=="QB" ~ PosRank == ((RP["TM"]*RP["QB"])+1),position=="RB" ~ PosRank == ((RP["TM"]*RP["RB"])+((RP["TM"]*RP["FLEX"])/2)+1),position=="WR" ~ PosRank == ((RP["TM"]*RP["WR"])+((RP["TM"]*RP["FLEX"])/2)+1),position=="TE" ~ PosRank == round((RP["TM"]*RP["TE"])+((RP["TM"]*RP["FLEX"])/3)+1)))%>%
  dplyr::group_by(season,week) %>%
  dplyr::summarise(
    RPQB = mean(CustomScoring[position=='QB']),
    RPRB = mean(CustomScoring[position=='RB']),
    RPWR = mean(CustomScoring[position=='WR']),
    RPTE = mean(CustomScoring[position=='TE']),
  )

# Generate the Replacement Player WAR
Base_PlayersAvg = Base_PlayersAvg %>%
  dplyr::left_join (FlexData, by = c("season","week"))%>%
  dplyr::left_join (RPAvg, by = c("season","week"))%>%
  dplyr::group_by(season,week) %>%
  dplyr::mutate(
    AvgTMScore = sum((AvgQB*RP["QB"]),(AvgRB*RP["RB"]),(AvgWR*RP["WR"]),(AvgTE*RP["TE"]),(AvgFLEX*RP["FLEX"])),
    StdTMScore = sum((StdQB*RP["QB"]),(StdRB*RP["RB"]),(StdWR*RP["WR"]),(StdTE*RP["TE"]),(StdFLEX*RP["FLEX"])),
    WinWQBRP = pnorm((RPQB-AvgQB)/StdTMScore),
    WinWRBRP = pnorm((RPRB-AvgRB)/StdTMScore),
    WinWWRRP = pnorm((RPWR-AvgWR)/StdTMScore),
    WinWTERP = pnorm((RPTE-AvgTE)/StdTMScore)
  )

# Generate the WAR substracting the Replacement player level of the position vs that player
StatCustom = StatCustom %>%
  dplyr::left_join(Base_PlayersAvg, by = c("season","week"))%>%
  dplyr::group_by(player_id,week,season) %>%
  dplyr::mutate(
    PoRP = dplyr::case_when(position=="QB" ~ CustomScoring-AvgQB,position=="RB" ~ CustomScoring-AvgRB,position=="WR" ~ CustomScoring-AvgWR,position=="TE" ~ CustomScoring-AvgTE),
    WinW = pnorm(PoRP/StdTMScore)
  )

# Generate the WAR subtracting the Replacement player level of the position vs that player
WarPlayerData = StatCustom %>%
  dplyr::group_by(season,player_id) %>%
  dplyr::summarize(
    FPSoRP = sum(PoRP),
    WinSoRP = round(dplyr::case_when(position=="QB" ~ sum(WinW)-sum(WinWQBRP),position=="RB" ~ sum(WinW)-sum(WinWRBRP),position=="WR" ~ sum(WinW)-sum(WinWWRRP),position=="TE" ~ sum(WinW)-sum(WinWTERP)),2)
  )

WORP = unique(playerstats[c("player_id","season","player_name","team","position")])%>%
  dplyr::left_join(dplyr::distinct(WarPlayerData,player_id, .keep_all = T))

#Start of visualization.

#install.packages('ggplot2')
#install.packages('ggthemes')
#install.packages('ggtext')

library(ggplot2)
library(ggthemes)
library(ggtext)

TopPlayers = 24
MinTopYear = 2014
MaxTopYear = 2021
SingleYear = 2021

WORP_df <- WORP %>%
  dplyr::filter(season>=MinTopYear,season<=MaxTopYear)%>%
  dplyr::group_by(season,position)%>%
  dplyr::slice_max(order_by = WinSoRP, n=TopPlayers, with_ties = FALSE)%>%
  dplyr::mutate(PosRank=rank(-WinSoRP,ties.method="first"))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(position,PosRank) %>%
  dplyr::select(season,position,PosRank,WinSoRP)%>%
  dplyr::mutate(
    aWORP = round(mean(WinSoRP),2)
  )

yrng <- range(WORP_df$aWORP)
xrng <- range(WORP_df$PosRank)
caption = c(paste0("~bold(Roster):~QB:",RP["QB"],"~RB:",RP["RB"],"~WR:",RP["WR"],"~TE:",RP["TE"],"~FLEX:",RP["FLEX"]),
            paste0("~bold(Pass~Scoring):~TD:",scoringvalues["passing_tds"],"~Int:",scoringvalues["interceptions"],"~Sack:",scoringvalues["sacks"]),
            paste0("~bold(Rec~Scoring):~RB:",scoringvalues["rec_rb"],"~WR:",scoringvalues["rec_wr"],"~TE:",scoringvalues["rec_te"]))

WORP_Pos_Graph <- ggplot(WORP_df,
                         ggplot2::aes(x = PosRank, y = aWORP, color = position))+
  ggplot2::scale_color_manual(values = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#ffe599","TE"="#9fc5e8"))+
  ggplot2::geom_line(size = 1.5) +
  ggplot2::geom_point(size = 2)+
  ggplot2::geom_hline(yintercept = 0, size = 1, color = "black") +
  ggplot2::scale_x_continuous(breaks = c(1,seq(from = 6, to = TopPlayers, by = 6),TopPlayers))+
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    text = ggplot2::element_text(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.direction = "vertical",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.14, 0.15),
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = 12),
    axis.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
  )+
  labs(x = "PosRank",
       y = "WORP",
       title = paste0("WORP Pos Data"),
       subtitle = paste0("Average Years ",MinTopYear,"-",MaxTopYear," Weeks 1-16"),
       caption = paste0("**Data:** nflverse | **Plot Designed by:** @Adeiko_ff<br>
                        **Roster:** QB:",RP["QB"]," RB:",RP["RB"]," WR:",RP["WR"]," TE:",RP["TE"]," FLEX:",RP["FLEX"],"<br>
                        **Pass Scoring:** TD:",scoringvalues["passing_tds"]," Int:",scoringvalues["interceptions"]," Sack: ",scoringvalues["sacks"],"<br>
                        **Rec Scoring:** RB:",scoringvalues["rec_rb"]," WR: ",scoringvalues["rec_wr"]," TE: ",scoringvalues["rec_te"]
       ))+
  annotate(geom = "text", x = xrng[2], y = c(yrng[2],yrng[2]*0.98,yrng[2]*0.96), label = caption, hjust = "inward", vjust ="inward", size = 4,color="black",parse = TRUE)

#UPDATE FILE PATH
ggsave(paste0("Images/WORP_Pos_Graph_",MinTopYear,"-",MaxTopYear,".jpg"), WORP_Pos_Graph,scale = 1.67,dpi = "retina")




# #WORP_Pos_Graph

WORP_Facet_Pos_Graph <- ggplot(WORP_df,ggplot2::aes(x = PosRank, y = WinSoRP, color = position))+
  ggplot2::scale_color_manual(values = c("QB"="#b6d7a8","RB"="#ea9999","WR"="#ffe599","TE"="#9fc5e8"))+
  ggplot2::geom_line(size = 1.5) +
  ggplot2::geom_hline(yintercept = 0, size = 1, color = "black") +
  ggplot2::scale_x_continuous(breaks = c(1,seq(from = 6, to = TopPlayers, by = 6),TopPlayers))+
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    text = ggplot2::element_text(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.caption = ggtext::element_markdown(size = 12),
    axis.text = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 12, face = "bold")
  )+
  labs(x = "PosRank",
       y = "WORP",
       title = paste0("WORP Pos Data"),
       subtitle = paste0("Weeks 1-16 Top",TopPlayers),
       caption = paste0("**Data:** nflverse | **Plot Designed by:** @Adeiko_ff<br>
                        **Roster:** QB:",RP["QB"]," RB:",RP["RB"]," WR:",RP["WR"]," TE:",RP["TE"]," FLEX:",RP["FLEX"],"<br>
                        **Pass Scoring:** TD:",scoringvalues["passing_tds"]," Int:",scoringvalues["interceptions"]," Sack: ",scoringvalues["sacks"],"<br>
                        **Rec Scoring:** RB:",scoringvalues["rec_rb"]," WR: ",scoringvalues["rec_wr"]," TE: ",scoringvalues["rec_te"]
       ))+
  facet_wrap(. ~ season, ncol = 3)+
  theme(strip.text = element_text(face="bold"))

#UPDATE FILE PATH
ggsave(paste0("C:/Users/acer/Documents/Fantasy Football/WORP Test V1/Images/WORP_Facet_Pos_Graph_",MinTopYear,"-",MaxTopYear,".jpg"), WORP_Facet_Pos_Graph, scale=1.67,  dpi="retina")
