
### --- Notes: Playoff Matchups --- ###

setwd("~/Data Science/Basketball/Projects/Teams/BOS Interview")

# Load files downloaded from PBP stats
my_files <- list.files(pattern = "*.csv")

abrev_files <- gsub(".csv", "", my_files)

# file_list <- lapply(my_files, read.csv)

# names(file_list) <- paste0(basename(file_path_sans_ext(my_files)))

for(i in abrev_files){
  filepath <- file.path("/Users/alexandermerg/Data Science/Basketball/Projects/Teams/BOS Interview",paste(i,".csv",sep=""))
  assign(i, read.csv(filepath))
}

# Add season and regular season/playoffs distinctions to dataset
matchups_pi_21$type <- "playoffs"
matchups_pi_21$season <- "2021"
matchups_reg_18$type <- "reg"
matchups_reg_18$season <- "2018"
matchups_reg_19$type <- "reg"
matchups_reg_19$season <- "2019"
matchups_reg_20$type <- "reg"
matchups_reg_20$season <- "2020"
matchups_reg_21$type <- "reg"
matchups_reg_21$season <- "2021"
matchups_reg_22$type <- "reg"
matchups_reg_22$season <- "2022"
matchups_yoffs_18$type <- "playoffs"
matchups_yoffs_18$season <- "2018"
matchups_yoffs_19$type <- "playoffs"
matchups_yoffs_19$season <- "2019"
matchups_yoffs_20$type <- "playoffs"
matchups_yoffs_20$season <- "2020"
matchups_yoffs_21$type <- "playoffs"
matchups_yoffs_21$season <- "2021"

# Bind into a combined matchup dataset
matchups <- rbind(matchups_pi_21, matchups_reg_18, matchups_reg_19, matchups_reg_20, matchups_reg_21, matchups_reg_22,
                  matchups_yoffs_18, matchups_yoffs_19, matchups_yoffs_20, matchups_yoffs_21)


write.csv(matchups, file = paste0("matchups.csv"))


## Need player names. Scrape a datasets from PBP Stats to get Entity IDs with Player Names

pbp_scrape <- function(season){
  
  pbp_url <- paste0("https://api.pbpstats.com/get-totals/nba?Season=",season,"&SeasonType=Regular%2BSeason&Type=Player")
  
  json_data <- fromJSON(paste(readLines(pbp_url), collapse=""))
  pbp_shooting <- json_data[["multi_row_table_data"]]
  
  pbp_shooting <- pbp_shooting %>%
    select(`Name`,`EntityId`,  `TeamAbbreviation`, `TeamId`, `GamesPlayed`, `Minutes`) %>%
    clean_names() %>%
    mutate(season = season)
  
  pbp_shooting[is.na(pbp_shooting)] <- 0
  
  return(pbp_shooting)
  
}

seasons_input <- c("2021-22","2020-21", "2019-20", "2018-19", "2017-18")

pbp_data <- map_df(seasons_input, pbp_scrape)


## Create reference table of all unique player IDs to merge in names
name_ref <- pbp_data %>%
  group_by(entity_id, name) %>%
  summarise(min = mean(minutes)) %>%
  as.data.frame()

## Create reference table of all unique team IDs to merge in names
team_ref <- pbp_data %>%
  group_by(team_id, team_abbreviation) %>%
  summarise(min = mean(minutes)) %>%
  as.data.frame()

## Find and delete any duplicate names for each player ID
test_ref <- name_ref %>%
  group_by(entity_id) %>%
  summarise(unique = n())

#202683 (687)+
#1628408 (191)+
#1628995 (274)+
#1629651 (387)+
#1629718 (423)+
#202694 (699)+
#200782 (589)+

name_ref <- name_ref[-c(191, 274, 387, 423, 589, 687, 699),]

write.csv(name_ref, file = paste0("name_ref.csv"))



## Merge player and team names into dataset

matchups2 <- matchups %>%
  mutate(across(1:6, as.character)) %>%
  # Add defensive player names
  merge(name_ref %>% select(1:2),
        by.x = "def_player_id",
        by.y = "entity_id",
        all.x = T) %>%
  rename(def_player = name) %>%
  # Add offensive player names
  merge(name_ref %>% select(1:2),
        by.x = "off_player_id",
        by.y = "entity_id",
        all.x = T) %>%
  rename(off_player = name) %>%
  # Add offensive player team
  merge(team_ref %>% select(1:2),
        by.x = "off_team_id",
        by.y = "team_id",
        all.x = T) %>%
  rename(off_team = team_abbreviation) %>%
  # Add defensive player team
  merge(team_ref %>% select(1:2),
        by.x = "def_team_id",
        by.y = "team_id",
        all.x = T) %>%
  rename(def_team = team_abbreviation) %>%
  # Convert minute format to seconds and minutes
  mutate(matchup_sec = period_to_seconds(ms(matchup_min)),
         matchup_min = matchup_sec/60) %>%
  # Reorder columns
  select(26, 29, 27:28, 25, 24, 30, 7:23, 1:4, 6) %>%
  # Change numbers to numeric format
  mutate(across(matchup_sec:sfl, as.numeric))



write.csv(matchups2, file = paste0("matchups.csv"))

matchups <- read.csv("~/Data Science/Basketball/Projects/Playoff Matchups/playoff_matchups/matchups.csv")


## Create list of all unique players

# By season
player_list_by_season <- matchups %>%
  group_by(def_player, off_player, season) %>%
  summarize(matchup_min = sum(matchup_min),
            poss = sum(partial_poss)) %>%
  filter(poss > 20)

# Create list of offensive players
off_list_season <- as.list(unique(player_list_by_season$off_player))
# Create list of defensive players
def_list_season <- as.list(unique(player_list_by_season$def_player))

# Create list of all unique players
player_list <- matchups %>%
  group_by(def_player, off_player) %>%
  summarize(matchup_min = sum(matchup_min),
            poss = sum(partial_poss)) %>%
  filter(poss > 10)

# Create list of offensive players
off_player_list <- as.list(unique(player_list$off_player))
# Create list of defensive players
def_player_list <- as.list(unique(player_list$def_player))


# Create list of teams
team_list <- as.list(unique(matchups$def_team))
  

# Handle Zion, Simmons, and Kawhi





### --- COMBO #1: BY PLAYER --- ###

players <- c("Matisse Thybulle", "Khris Middleton", "OG Anunoby", "Kevin Durant")

# Retrieve offensive player averages for each metric from main matchups file
off_avg <- matchups %>%
  filter(season == "2022" | season == "2021" | season == "2020") %>%
  filter(off_player == "Jayson Tatum") %>%
  filter(type == "reg" | type == "playoffs") %>%
  group_by(off_player_id, off_player) %>%
  summarise(matchup_min = sum(matchup_min),
            poss = sum(partial_poss),
            points = sum(player_pts),
            ast = sum(matchup_ast),
            tov = sum(matchup_tov),
            blk = sum(matchup_blk),
            fgm = sum(matchup_fgm),
            fga = sum(matchup_fga),
            fg3m = sum(matchup_fg3m),
            fg3a = sum(matchup_fg3a),
            ftm = sum(matchup_ftm),
            fta = sum(matchup_fta),
            sfl = sum(sfl),
            off_avg_pts_per_100 = (points/poss)*100,
            off_avg_pts_per_shot = (points/fga),
            off_avg_ast_per_100 = (ast/poss)*100,
            off_avg_fta_per_100 = (fta/poss)*100,
            off_avg_sfl_per_shot = (sfl/fga),
            off_avg_shots_per_100 = (fga/poss)*100,
            off_avg_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            off_avg_fg3_pct = fg3m/fg3a,
            off_avg_ts_pct = points/(2*((.44 * fta) + fga)),
            off_avg_ast_to_ratio = ast/tov,
            off_avg_tov_per_100 = (tov/poss)*100,
            off_avg_blk_per_100 = (blk/poss)*100) %>%
  select(1:2, 16:27) %>%
  as.data.frame()


# Create data subset
by_player <- matchups %>%
  filter(season == "2022" | season == "2021" | season == "2020") %>%
  filter(type == "reg" | type == "playoffs") %>%
  filter(def_player %in% players) %>%
  filter(off_player %in% "Jayson Tatum") %>%
  group_by(def_player, off_player) %>%
  summarise(matchup_min = sum(matchup_min),
            poss = sum(partial_poss),
            points = sum(player_pts),
            pts_per_100 = (points/poss)*100,
            ast = sum(matchup_ast),
            tov = sum(matchup_tov),
            blk = sum(matchup_blk),
            fgm = sum(matchup_fgm),
            fga = sum(matchup_fga),
            fg3m = sum(matchup_fg3m),
            fg3a = sum(matchup_fg3a),
            ftm = sum(matchup_ftm),
            fta = sum(matchup_fta),
            sfl = sum(sfl),
            pts_per_shot = (points/fga),
            pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            shots_per_100 = (fga/poss)*100,
            sfl_per_shot = sfl/fga,
            fta_per_100 = (fta/poss)*100,
            ast_per_100 = (ast/poss)*100,
            tov_per_100 = (tov/poss)*100,
            ast_to_ratio = ast/tov,
            blk_per_100 = (blk/poss)*100,
            ts_pct = points/(2*((.44 * fta) + fga)),
            fg3_pct = fg3m/fg3a,
            fg3_luck = if(is.na(fg3_pct)){"Reasonable"}
            else if(fg3_pct < .2){"Lucky"}
            else if(fg3_pct > .5){"Unlucky"}
            else {"Reasonable"},
            pct_def_time = mean(pct_defender_total_time),
            pct_off_time = mean(pct_off_total_time),
            pct_time_both_on = mean(pct_total_time_both_on)) %>%
  #filter(poss > 15) %>%
  select(1:6, 17:31) %>%
  as.data.frame() %>%
  # Merge player averages into dataset
  merge(off_avg,
        by = "off_player",
        all.x = T) %>%
  select(-22) %>%
  rename(
    metric_pts_per_100 = pts_per_100,
    metric_pts_per_shot = pts_per_shot,
    metric_ast_per_100 = ast_per_100,
    metric_fta_per_100 = fta_per_100,
    metric_sfl_per_shot = sfl_per_shot,
    metric_shots_per_100 = shots_per_100,
    metric_pts_created_per_100 = pts_created_per_100,
    metric_ts_pct = ts_pct,
    metric_ast_to_ratio = ast_to_ratio,
    metric_tov_per_100 = tov_per_100,
    metric_blk_per_100 = blk_per_100
  ) %>%
  select(1:2, 4, 17, 6, 22) %>%
  rename(metric = starts_with("metric"),
         off_avg = starts_with("off_avg"))
  

off_player <- c("Jayson Tatum", "Jayson Tatum", "Jayson Tatum", "Jayson Tatum",
                "Jaylen Brown", "Jaylen Brown", "Jaylen Brown", "Jaylen Brown")
def_player <- c("Matisse Thybulle", "Khris Middleton", "OG Anunoby", "Kevin Durant",
                "Matisse Thybulle", "Khris Middleton", "OG Anunoby", "Kevin Durant")
poss <- c(10, 20, 30, 40, 50, 60, 70, 80)
pts_per_shot <- c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8)

df <- data.frame(off_player, def_player, poss, pts_per_shot)

# Filter by desired metric and prepare dataset for plotting
by_player <- by_player %>%
  select(1:2, 4, 19:21, 17:18, pts_created_per_100, off_avg_pts_created_per_100)

# Create plot

vline <- by_player[1,9]

  ggplot(data = by_player,
         aes(x = .data[[8]],
             y = fct_reorder(def_player, -pts_created_per_100),
             size = poss,
             fill = fg3_pct,
             color = after_scale(clr_darken(fill, 0.45)),
             stroke = .85)) +
    geom_point(shape = 21,
               alpha = .65) +
    geom_vline(aes(xintercept = off_avg_pts_created_per_100),
               color = "grey35", 
               linetype = "dashed", 
               size = .6) +
    geom_text(aes(label = "Average",
                  x = off_avg_pts_created_per_100,
                  y = 3.5),
              nudge_x = 1.2,
              family = "Avenir",
              color = "gray65",
              size = 3) +
    #geom_text(mapping = aes(label = "Avg against all matchups")) +
    #scale_x_continuous(breaks = seq(0, 160, 20)) +
    theme_personal() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "top",
          panel.border = element_blank(),
          plot.margin = margin(30, 35, 12.5, 38.5),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          plot.title = element_text(size = 26),
          legend.box = "horizontal",
          plot.subtitle = element_text(size = 14, color = "gray45")) +
    labs(y = "Defenders",
         x = "Points Created per 100 poss",
         title = "Jayson Tatum",
         subtitle = "Performance guarding offensive player",
         size = "Possessions") +
    scale_size_continuous(
      range = c(1, 8),
      limits = c(0, 200),
      breaks = c(50, 100, 150, 200),
      labels = c("50", "100", "150", "200+")
    ) +
    scale_fill_gradient2(
      limits = c(0,.8),
      breaks = c(.2, .4, .6),
      labels = function(x) paste0(round(as.numeric(x*100)), "%"),
      midpoint = .4,
      low = "dodgerblue1",
      mid = "snow1",
      high = "indianred2",
      guide = "colourbar") +
    guides(
      size = guide_legend(
        nrow = 1,
        title.position = 'top',
        title.hjust = .5,
        title.vjust = -.7,
        label.position = "bottom",
        barwidth = 12),
      fill = guide_colorbar(
        title = "3pt %",
        title.position = 'top',
        title.hjust = .5,
        title.vjust = -.3,
        nrow = 1,
        order = 1,
        ticks = F,
        barwidth = 10
        )
    )
  


## ------------------------------------------
## Table data
## ------------------------------------------



# Selected Player Table - Percent Rank
pct_rank <- matchups %>%
  filter(season == "2022" | season == "2021" | season == "2020") %>%
  filter(type == "reg" | type == "playoffs") %>%
  filter(off_player %in% "Jayson Tatum") %>%
  group_by(def_player_id, def_player) %>%
  summarise(matchup_min = sum(matchup_min),
            poss = sum(partial_poss),
            pct_defended = mean(pct_defender_total_time),
            points = sum(player_pts),
            ast = sum(matchup_ast),
            tov = sum(matchup_tov),
            blk = sum(matchup_blk),
            fgm = sum(matchup_fgm),
            fga = sum(matchup_fga),
            fg3m = sum(matchup_fg3m),
            fg3a = sum(matchup_fg3a),
            ftm = sum(matchup_ftm),
            fta = sum(matchup_fta),
            sfl = sum(sfl),
            pct_rank_pts_per_100 = (points/poss)*100,
            pct_rank_pts_per_shot = (points/fga)*100,
            pct_rank_ast_per_100 = (ast/poss)*100,
            pct_rank_fta_per_100 = (fta/poss)*100,
            pct_rank_sfl_per_shot = (sfl/fga),
            pct_rank_shots_per_100 = (fga/poss)*100,
            pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            pct_rank_fg3_pct = fg3m/fg3a,
            pct_rank_ts_pct = points/(2*((.44 * fta) + fga)),
            pct_rank_ast_to_ratio = ast/tov,
            pct_rank_tov_per_100 = (tov/poss)*100,
            pct_rank_blk_per_100 = (blk/poss)*100) %>%
  filter(poss > 10) %>%
  select(1:6, 17:28) %>%
  as.data.frame() %>%
  mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
  mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
  select(def_player_id, def_player, 13) %>%
  # filter(def_player %in% players) %>%
  arrange(desc(pct_rank_pts_created_per_100))


# Selected Player Table - Final
table_data_team <- matchups %>%
  filter(season == "2022" | season == "2021" | season == "2020") %>%
  filter(type == "reg" | type == "playoffs") %>%
  filter(off_player %in% "Jayson Tatum") %>%
  filter(current_team == "ATL") %>%
  group_by(def_player_id, def_player) %>%
  summarise(matchup_min = sum(matchup_min),
            poss = sum(partial_poss),
            pct_defended = mean(pct_defender_total_time),
            points = sum(player_pts),
            ast = sum(matchup_ast),
            tov = sum(matchup_tov),
            blk = sum(matchup_blk),
            fgm = sum(matchup_fgm),
            fga = sum(matchup_fga),
            fg3m = sum(matchup_fg3m),
            fg3a = sum(matchup_fg3a),
            ftm = sum(matchup_ftm),
            fta = sum(matchup_fta),
            sfl = sum(sfl),
            dummy1 = sum(sfl),
            dummy2 = sum(sfl),
            pts_per_100 = (points/poss)*100,
            pts_per_shot = (points/fga)*100,
            ast_per_100 = (ast/poss)*100,
            fta_per_100 = (fta/poss)*100,
            sfl_per_shot = (sfl/fga),
            shots_per_100 = (fga/poss)*100,
            pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            fg3_pct = fg3m/fg3a,
            ts_pct = points/(2*((.44 * fta) + fga)),
            ast_to_ratio = ast/tov,
            tov_per_100 = (tov/poss)*100,
            blk_per_100 = (blk/poss)*100) %>%
  mutate(ts_pct = percent(ts_pct)) %>%
  rename(
    `Player` = def_player,
    `Possessions` = poss,
    `% of Time Defended` = pct_defended,
    `Pts per 100` = pts_per_100,
    `Pts per shot` = pts_per_shot,
    `Ast per 100` = ast_per_100,
    `FT Att per 100` = fta_per_100,
    `Shooting Fouls per shot` = sfl_per_shot,
    `Shots per 100` = shots_per_100,
    `Pts Created per 100` = pts_created_per_100,
    `3pt %` = fg3_pct,
    `True Shooting %` = ts_pct,
    `Ast/TO` = ast_to_ratio,
    `Turnovers per 100` = tov_per_100,
    `Blocks per 100` = blk_per_100
  ) %>%
  select(1:2, 4:5, 26, 25) %>%
  merge(pct_rank %>% select(1,3),
        by = "def_player_id",
        all.x = T) %>%
  filter(Possessions > 10) %>%
  select(-1) %>%
  rename(`Percent Rank` = starts_with("pct_rank"))



# Top 5 Defenders - Performance
top_perf <- matchups %>%
  filter(season == "2022" | season == "2021" | season == "2020") %>%
  filter(type == "reg" | type == "playoffs") %>%
  filter(off_player %in% "Jayson Tatum") %>%
  group_by(def_player_id, def_player) %>%
  summarise(matchup_min = sum(matchup_min),
            poss = sum(partial_poss),
            pct_defended = mean(pct_defender_total_time),
            points = sum(player_pts),
            ast = sum(matchup_ast),
            tov = sum(matchup_tov),
            blk = sum(matchup_blk),
            fgm = sum(matchup_fgm),
            fga = sum(matchup_fga),
            fg3m = sum(matchup_fg3m),
            fg3a = sum(matchup_fg3a),
            ftm = sum(matchup_ftm),
            fta = sum(matchup_fta),
            sfl = sum(sfl),
            pct_rank_pts_per_100 = (points/poss)*100,
            pct_rank_pts_per_shot = (points/fga)*100,
            pct_rank_ast_per_100 = (ast/poss)*100,
            pct_rank_fta_per_100 = (fta/poss)*100,
            pct_rank_sfl_per_shot = (sfl/fga),
            pct_rank_shots_per_100 = (fga/poss)*100,
            pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            pct_rank_fg3_pct = fg3m/fg3a,
            pct_rank_ts_pct = points/(2*((.44 * fta) + fga)),
            pct_rank_ast_to_ratio = ast/tov,
            pct_rank_tov_per_100 = (tov/poss)*100,
            pct_rank_blk_per_100 = (blk/poss)*100,
            pts_per_100 = (points/poss)*100,
            pts_per_shot = (points/fga)*100,
            ast_per_100 = (ast/poss)*100,
            fta_per_100 = (fta/poss)*100,
            sfl_per_shot = (sfl/fga),
            shots_per_100 = (fga/poss)*100,
            pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            fg3_pct = fg3m/fg3a,
            ts_pct = points/(2*((.44 * fta) + fga)),
            ast_to_ratio = ast/tov,
            tov_per_100 = (tov/poss)*100,
            blk_per_100 = (blk/poss)*100) %>%
  filter(poss > 10) %>%
  select(1:6, 17:40) %>%
  as.data.frame() %>%
  mutate(across(points, as.numeric)) %>%
  mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
  mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
  select(def_player_id, def_player, poss, pct_defended, fg3_pct, pts_created_per_100, pct_rank_pts_created_per_100) %>%
  filter(poss > 50) %>%
  arrange(across(starts_with("pct_rank"), desc)) %>%
  slice(n = 1:5) %>%
  select(-1)
  

# Top 5 Defenders - Volume
top_vol <- matchups %>%
  filter(season == "2022" | season == "2021" | season == "2020") %>%
  filter(type == "reg" | type == "playoffs") %>%
  filter(off_player %in% "Jayson Tatum") %>%
  group_by(def_player_id, def_player) %>%
  summarise(matchup_min = sum(matchup_min),
            poss = sum(partial_poss),
            pct_defended = mean(pct_defender_total_time),
            points = sum(player_pts),
            ast = sum(matchup_ast),
            tov = sum(matchup_tov),
            blk = sum(matchup_blk),
            fgm = sum(matchup_fgm),
            fga = sum(matchup_fga),
            fg3m = sum(matchup_fg3m),
            fg3a = sum(matchup_fg3a),
            ftm = sum(matchup_ftm),
            fta = sum(matchup_fta),
            sfl = sum(sfl),
            pct_rank_pts_per_100 = (points/poss)*100,
            pct_rank_pts_per_shot = (points/fga)*100,
            pct_rank_ast_per_100 = (ast/poss)*100,
            pct_rank_fta_per_100 = (fta/poss)*100,
            pct_rank_sfl_per_shot = (sfl/fga),
            pct_rank_shots_per_100 = (fga/poss)*100,
            pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            pct_rank_fg3_pct = fg3m/fg3a,
            pct_rank_ts_pct = points/(2*((.44 * fta) + fga)),
            pct_rank_ast_to_ratio = ast/tov,
            pct_rank_tov_per_100 = (tov/poss)*100,
            pct_rank_blk_per_100 = (blk/poss)*100,
            pts_per_100 = (points/poss)*100,
            pts_per_shot = (points/fga)*100,
            ast_per_100 = (ast/poss)*100,
            fta_per_100 = (fta/poss)*100,
            sfl_per_shot = (sfl/fga),
            shots_per_100 = (fga/poss)*100,
            pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
            fg3_pct = fg3m/fg3a,
            ts_pct = points/(2*((.44 * fta) + fga)),
            ast_to_ratio = ast/tov,
            tov_per_100 = (tov/poss)*100,
            blk_per_100 = (blk/poss)*100) %>%
  filter(poss > 10) %>%
  select(1:6, 17:40) %>%
  as.data.frame() %>%
  mutate(across(points, as.numeric)) %>%
  mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
  mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
  select(def_player_id, def_player, poss, pct_defended, fg3_pct, 25, 13) %>%
  slice_max(poss, n = 5) %>%
  arrange(desc(poss)) %>%
  select(-1)




## ------------------------------------------
## DT Code
## ------------------------------------------





datatable(table_data_team,
          colnames = c(
            "Player" = "def_player",
            "Total Poss" = "poss",
            "% of Time Defended" = "pct_defended", 
            "3pt %" = "fg3_pct",
            "Pts Created per 100" = "pts_created_per_100",
            "Percent Rank"= "pct_rank_pts_created_per_100"),
          options = list(info = F,
                         paging = F,
                         searching = F,
                         stripeClasses = F,
                         lengthChange = F,
                         scrollY = '355px',
                         scrollX = T,
                         scrollCollapse = T,
                         order = list(5, 'desc'),
                         # autoWidth = T,
                         columnDefs = list(
                           list(width = '70px', targets = c(2,4,5)),
                           list(width = '100px', targets = c(0)),
                           list(className = "dt-left", targets = 0),
                           list(width = '20px', targets = c(1,3)))),
          rownames = F) %>%
  formatRound(
    columns = c(2,3,4,5,6),
    digits = c(1)) %>%
  formatPercentage(c(3,4), 1) %>%
  formatStyle(
    '3pt %',
    backgroundColor = styleInterval(c(.2, .4), c('red', 'white', 'green'))) %>%









## ------------------------------------------
## GT Code
## ------------------------------------------


selected_player_table <- table_data_player %>%
  arrange(-pct_rank_pts_created_per_100) %>%
  gt() %>%
  cols_label(def_player = "Player", 
             poss  = "Total Poss", 
             pct_defended  = "% of Time Defending", 
             fg3_pct  = "3pt %", 
             pts_created_per_100  = "Pts Created per 100", 
             pct_rank_pts_created_per_100  = "Percent Rank")  %>% 
  cols_width(def_player ~ 150, 
             poss ~ 90,
             pct_defended ~ 90,
             fg3_pct ~ 90,
             pts_created_per_100 ~ 90,
             pct_rank_pts_created_per_100 ~ 90) %>% 
  fmt_number(
    columns = c(poss, pct_defended, fg3_pct, pts_created_per_100, pct_rank_pts_created_per_100), 
             decimals = 0) %>%
  fmt_percent(
    columns = c(pct_defended,fg3_pct),
    decimals = 0
  ) %>%
  data_color(
    columns = pct_rank_pts_created_per_100,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = c(0, 100), 
      na.color = "#005C55FF"
    )) %>%
  tab_options(
    #column_labels.font.size = 12,
    column_labels.font.weight = 'bold',
    #table.font.size = 12,
    table.font.names = "Avenir", 
    data_row.padding = px(1),
    source_notes.font.size = 7
  )










