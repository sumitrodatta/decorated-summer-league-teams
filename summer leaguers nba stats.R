library(tidyverse)

game_info=read_csv("Input Data/Game Info.csv")

sl_games=game_info %>% 
  #summer league games start with 15
  filter(str_starts(GAME_ID,'15')) %>%
  distinct(GAME_ID,GAME_DATE,SEASON_ID) %>% arrange(GAME_DATE) %>% 
  #season id to match player stats
  mutate(
    SEASON_ID=paste0(str_sub(SEASON_ID,start=2),"-",
                     str_sub(as.numeric(str_sub(SEASON_ID,start=2))+1,start=-2)),
    GAME_ID=as.numeric(GAME_ID)
  )

write_csv(sl_games,"Output Data/Summer League Games.csv")

sl_box=read_csv("Input Data/Summer League Box Scores.csv")

clean_sl_box=sl_box %>% 
  #comments on why player didn't play
  filter(is.na(comment)) %>%
  left_join(.,sl_games,by=join_by(gameId==GAME_ID))

# sl_player_career_stats=read_csv("Input Data/Summer League Player Stats.csv")
# 
# left_join(clean_sl_box,sl_player_career_stats,by=join_by(personId==PLAYER_ID)) %>%
#   #minimum games to be considered NBA-level talent
#   filter(GP>=20) %>%
#   group_by(gameId,teamTricode) %>%
#   reframe(players_w_gp=sum(!is.na(GP)), #how many players actually played NBA games
#           tot_gp=sum(GP,na.rm=TRUE),
#           tot_pts=sum(PTS,na.rm=TRUE)) %>%
#   mutate(pts_per_game=tot_pts/tot_gp) %>%
#   arrange(desc(players_w_gp))

sl_player_yoy_stats=read_csv("Input Data/Summer League Player YOY Stats.csv")

cumulative_player_stats=sl_player_yoy_stats %>% 
  #for players who played seasons with multiple teams, take only total row
  mutate(TEAM_ABBREVIATION=if_else(TEAM_ABBREVIATION=="TOT","1TOT",TEAM_ABBREVIATION)) %>%
  group_by(PLAYER_ID,SEASON_ID) %>% slice_min(TEAM_ABBREVIATION) %>% ungroup() %>%
  mutate(TEAM_ABBREVIATION=if_else(TEAM_ABBREVIATION=="1TOT","TOT",TEAM_ABBREVIATION)) %>%
  group_by(PLAYER_ID) %>% 
  #start from last season
  arrange(desc(SEASON_ID)) %>%
  mutate(across(c(GP:FGA,FG3M:FG3A,FTM:FTA,OREB:PTS),cumsum)) %>% ungroup() %>%
  mutate(FG_PCT=FGM/FGA,FG3_PCT=FG3M/FG3A,FT_PCT=FTM/FTA)

write_csv(cumulative_player_stats,"Output Data/Remaining Career Totals.csv")

team_level=
  left_join(clean_sl_box,cumulative_player_stats,by=join_by(personId==PLAYER_ID,SEASON_ID)) %>%
  #minimum games to be considered NBA-level talent
  filter(GP>=20) %>%
  group_by(gameId,GAME_DATE,teamTricode) %>%
  reframe(players_w_gp=sum(!is.na(GP)),
          players=paste(paste(firstName,familyName),collapse = ", "),
          across(c(GP:FGA,FG3M:FG3A,FTM:FTA,OREB:PTS),~sum(.,na.rm=TRUE))) %>%
  mutate(across(FGM:PTS,.fns =  ~./GP,.names = "{.col}_per_game"))

game_level=
  left_join(clean_sl_box,cumulative_player_stats,by=join_by(personId==PLAYER_ID,SEASON_ID)) %>%
  #minimum games to be considered NBA-level talent  
  filter(GP>=20) %>%
  group_by(gameId,GAME_DATE) %>%
  reframe(players_w_gp=sum(!is.na(GP)),
          players=paste(
            paste(
              firstName,familyName,paste0("(",teamTricode,")")
            ),
            collapse = ", "),
          across(c(GP:FGA,FG3M:FG3A,FTM:FTA,OREB:PTS),~sum(.,na.rm=TRUE))) %>%
  mutate(across(FGM:PTS,.fns =  ~./GP,.names = "{.col}_per_game"))

write_csv(team_level,"Output Data/Summer League Teams with NBA Players.csv")
write_csv(game_level,"Output Data/Summer League Games with NBA Players.csv")
