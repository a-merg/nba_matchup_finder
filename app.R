
### --- Matchup Finder --- ###

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(httr)
library(tools)
library(jsonlite)
library(janitor)
library(lubridate)
library(prismatic)
library(DT)
library(scales)

# Load data (sourced from PBPstats.com. Code used to clean and save dataset found in the Notes file)

# File contains matchup data for all NBA matchup combinations from 2017-18 thru 2021-22 season.
matchups <- read.csv("~/Data Science/Basketball/Projects/Matchup App/matchup_finder/matchups.csv")
pbp_data <- read.csv("~/Data Science/Basketball/Projects/Matchup App/matchup_finder/pbp_ref_data.csv")

# Load personal theme
theme_personal <- function (x) { 
  theme_minimal(base_size=12, base_family="Avenir") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      #panel.grid.major.x = element_blank(),
      panel.grid.major = element_line(color = 'gray91', size = .5),
      plot.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
      panel.border = element_rect(fill = NA, color = 'gray91', size = 1),
      axis.title.x = element_text(vjust = -1.5, size = 14),
      axis.title.y = element_text(vjust = 3, angle = 90, size = 14),
      plot.margin = margin(25, 25, 12.5, 25),
      plot.caption = element_text(vjust = -1.7, hjust = 1, size = 9, color = 'gray50'),
      plot.title = element_text(size = 20, hjust = 0, vjust = 4.3),
      plot.subtitle = element_text(size = 12, hjust = 0.0, vjust = 5)
    )
}

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

# Add column for 2022 team
team_ref <- pbp_data %>%
  filter(season == "2021-22" | name == "Zion Williamson" | name == "Ben Simmons") %>%
  group_by(entity_id, team_abbreviation) %>%
  summarise(min = mean(minutes)) %>%
  rename(current_team = team_abbreviation) %>%
  as.data.frame()

# Update reference dataset for Ben Simmons new team
team_ref[181,2] <- "BKN"
# Add row for Kawhi
team_ref[503,1] <- "202695"
team_ref[503,2] <- "LAC"
team_ref[503,3] <- 20

matchups <- matchups %>%
  merge(team_ref %>% select(1:2),
        by.x = "def_player_id",
        by.y = "entity_id",
        all.x = T) %>%
  select(-2)


## Create metric list
metric_choices <- c(
  "Points per 100 poss" = "pts_per_100",
  "Points per shot" = "pts_per_shot",
  "Points scored & ast'd on per 100" = "pts_created_per_100",
  # "Shots per 100" = "shots_per_100",
  "Shooting fouls per shot" = "sfl_per_shot",
  "FT attempts per 100" = "fta_per_100",
  "Assists per 100" = "ast_per_100",
  "Turnovers per 100" = "tov_per_100",
  "Assist/Turnover ratio" = "ast_to_ratio",
  "Blocks per 100" = "blk_per_100",
  "eFG %" = "efg_pct"
)



## Create reg/playoffs list
season_type_choices <- c(
  "Regular season" = "reg",
  "Playoffs" = "playoffs"
)



# New UI
ui <- navbarPage(
  title = "NBA Matchup Finder",
  inverse = T,
  tabPanel("Dashboard",
           style = "font-family: Avenir",
           fluidRow(
             column(6,
                    fluidRow(
                      column(12,
                             wellPanel(
                               style = "background-color: ghostwhite; border-color: #2c3e50; height: 360px;",
                               fluidRow(
                                 column(6,
                                        radioButtons(inputId = "radio",
                                                     label = "View",
                                                     choices = list(
                                                       "By player" = 1,
                                                       "By team" = 2)),
                                        selectInput(inputId = "off_player",
                                                    label = "Offensive player",
                                                    choices = c(off_player_list),
                                                    selectize = TRUE,
                                                    selected = c("Jayson Tatum")),
                                        conditionalPanel(
                                          condition = "input.radio == '1'",
                                          selectizeInput(
                                            inputId = "def_players",
                                            label = "Defensive players (8 max)",
                                            choices = c(def_player_list),
                                            multiple = TRUE,
                                            options = list(
                                              maxOptions = 844,
                                              maxItems = 8))),
                                        conditionalPanel(
                                          condition = "input.radio == '2'",
                                          selectInput(inputId = "def_team",
                                                      label = "Defensive team",
                                                      choices = c(team_list),
                                                      selectize = FALSE,
                                                      selected = NULL))),
                                 column(6,
                                        selectInput(inputId = "metrics",
                                                    label = "Select metric:",
                                                    choices = c(metric_choices),
                                                    selectize = FALSE,
                                                    selected = "pts_created_per_100"),
                                        checkboxGroupInput(
                                          inputId = "seasons",
                                          label = "Select season (will reset selections):",
                                          choices = c("2018", "2019", "2020", "2021", "2022"),
                                          selected = c("2020", "2021", "2022"),
                                          inline = TRUE),
                                        checkboxGroupInput(inputId = "season_type",
                                                           label = "Select type (will reset selections):",
                                                           choices = c(season_type_choices),
                                                           selected = c("playoffs", "reg"),
                                                           inline = TRUE),
                                        sliderInput(inputId = "poss",
                                                    label = "Minimum possessions:",
                                                    0, 160, 20, step = 20)))))),
                    fluidRow(
                      column(12,
                             wellPanel(
                               style = "background-color: ghostwhite; border-color: #2c3e50; height: 420px;",
                               tabsetPanel(type = "tabs",
                                           tabPanel("Selected", DTOutput("selected_table", width = 640)),
                                           tabPanel("Top 10 Defenders", DTOutput("top_perf", width = 640)),
                                           tabPanel("Bottom 10 Defenders", DTOutput("bot_perf", width = 640)),
                                           tabPanel("Most Frequent", DTOutput("top_vol", width = 640))))))),
             column(6,
                    wellPanel(
                      style = "background-color: ghostwhite; border-color: #2c3e50; height: 795px;",
                      fluidRow(
                        column(12,
                               mainPanel(
                                 plotOutput("plot1",
                                            height = 760,
                                            width = 620)))))))),
  tabPanel("About", icon = icon("bars"),
           fluidRow(
             column(12,
                    style = "background-color: #fff; border-color: #2c3e50;",
                    htmlOutput("about"))))
  
)



# 
# # Old UI
# ui <- fluidPage(
#   titlePanel("NBA Matchup Finder"),
#   style = "font-family: Avenir",
#   fluidRow(
#     style = "background-color: white",
#     column(6,
#            fluidRow(
#              column(12,
#                     wellPanel(
#                       style = "background-color: ghostwhite; border-color: #2c3e50; height: 360px;",
#                               fluidRow(
#                                        column(6,
#                                               radioButtons(inputId = "radio",
#                                                            label = "View",
#                                                            choices = list("By player" = 1,
#                                                                           "By team" = 2)),
#                                               selectInput(inputId = "off_player",
#                                                           label = "Offensive player",
#                                                           choices = c(off_player_list),
#                                                           selectize = TRUE,
#                                                           selected = c("Jayson Tatum")),
#                                               conditionalPanel(
#                                                 condition = "input.radio == '1'",
#                                                 selectizeInput(inputId = "def_players",
#                                                                label = "Defensive players (10 max)",
#                                                                choices = c(def_player_list),
#                                                                multiple = TRUE,
#                                                                options = list(maxOptions = 844,
#                                                                               maxItems = 10))),
#                                               conditionalPanel(
#                                                 condition = "input.radio == '2'",
#                                                 selectInput(inputId = "def_team",
#                                                             label = "Defensive team",
#                                                             choices = c(team_list),
#                                                             selectize = FALSE,
#                                                             selected = c("BOS")))),
#                                        column(6,
#                                               selectInput(inputId = "metrics",
#                                                           label = "Select metric:",
#                                                           choices = c(metric_choices),
#                                                           selectize = FALSE,
#                                                           selected = "pts_per_shot"),
#                                               checkboxGroupInput(inputId = "seasons",
#                                                                  label = "Select season:",
#                                                                  choices = c("2018", "2019", "2020", "2021", "2022"),
#                                                                  selected = c("2018", "2019", "2020", "2021", "2022"),
#                                                                  inline = TRUE),
#                                               checkboxGroupInput(inputId = "season_type",
#                                                                  label = "Select type:",
#                                                                  choices = c(season_type_choices),
#                                                                  selected = c("playoffs", "reg"),
#                                                                  inline = TRUE),
#                                               sliderInput(inputId = "poss",
#                                                           label = "Minimum possessions:",
#                                                           0, 160, 20, step = 20)
#                                               # ,actionButton("update" ,"Update",
#                                               #              class = "btn btn-primary")
#                                               ))))),
#            fluidRow(
#              column(12,
#                     wellPanel(
#                       style = "background-color: ghostwhite; border-color: #2c3e50; height: 420px;",
#                       tabsetPanel(type = "tabs",
#                                   tabPanel("Selected", DTOutput("selected_table", width =640)),
#                                   tabPanel("Top 10 Defenders", DTOutput("top_perf", width = 640)),
#                                   tabPanel("Bottom 10 Defenders", DTOutput("bot_perf", width = 640)),
#                                   tabPanel("Most Frequent", DTOutput("top_vol", width = 640))))))),
#     column(6,
#            wellPanel(
#              style = "background-color: ghostwhite; border-color: #2c3e50; height: 795px;",
#                      fluidRow(
#                        column(12,
#                               mainPanel(plotOutput("plot1", height = 760, width = 620))))))),
#     hr(style = "border-color: #cbcbcb;"),
#     fluidRow(
#       column(12,
#              p('Data used to generate this app was obtained from ', tags$a(href = "https://tracking.pbpstats.com/", 'PBP Stats Tracking', target = '_blank'),
#                'by way of the ', tags$a(href = "https://www.nba.com/stats/articles/nba-com-stats-unveils-improved-matchup-data-for-2019-20-season/", 'NBA matchup tracking data', '.', target = '_blank'), style = "font-size: 75%"),
#              p('App created by Alex Merg. Code can be found on my ', tags$a(href = "https://github.com/cjteeter/ShinyTeeter/tree/master/3_MastersGolf", 'GitHub', target = '_blank'), '.',  style = "font-size: 75%"),
#              p('Have a question? Spot an error? Send me an ', tags$a(href = "mailto:mergaj10@gmail.com", 'email', target = '_blank'), '.', style = "font-size: 75%"),
#              p(tags$em("Last updated: April 2022"), style = 'font-size:65%')))
#   )




addResourcePath("alexandermerg", getwd())
server <- function(input, output, session) {
  
  output$about <- renderUI({
    tags$iframe(seamless="seamless", 
                src= "alexandermerg/matchup_finder_about.html",
                width=800, 
                height=800)
  })
  
  
  # # Update UI based on radio button selection
  # output$player_team <- renderUI({
  #   
  #   if(input$radio == "1") {
  #     selectizeInput(inputId = "def_players",
  #                      label = "Defensive players",
  #                      choices = c(def_player_list),
  #                      multiple = TRUE,
  #                      options = list(maxOptions = 844,
  #                                     maxItems = 8))
  #   } else {
  #     selectInput(inputId = "def_team",
  #                   label = "Defensive team",
  #                   choices = c(team_list),
  #                   selectize = FALSE,
  #                   selected = c("BOS")) }
  # })
  
  
  # Update defensive players list based on offensive player choice
  observe({
    
    def_players <- matchups %>%
      filter(off_player %in% input$off_player) %>%
      filter(season %in% input$seasons) %>%
      filter(type %in% input$season_type) %>%
      select(def_player) %>%
      distinct() %>%
      arrange(def_player) %>%
      pull()
    
    updateSelectizeInput(session, inputId = "def_players", choices = def_players)
    
  })
  
  # Update defensive team list based on offensive player choice (in case a guy hasn't played against a team)
  observe({
    
    def_team <- matchups %>%
      filter(off_player %in% input$off_player) %>%
      filter(season %in% input$seasons) %>%
      filter(type %in% input$season_type) %>%
      select(def_team) %>%
      distinct() %>%
      arrange(def_team) %>%
      pull()
    
    updateSelectInput(session, inputId = "def_team", choices = def_team)
    
  })
  
  # To adjust plot name according to selections
  plot_label <- reactive({
    
    req(input$metrics)
    
    switch (input$metrics,
            "pts_per_100" = "Points per 100 possessions",
            "pts_per_shot" = "Points per shot",
            "pts_created_per_100" = "Points scored & assisted on per 100 possessions",
            "shots_per_100" = "Shots per 100 possessions",
            "sfl_per_shot" = "Shooting fouls per shot",
            "fta_per_100" = "Free throw attempts per 100 possessions",
            "ast_per_100" = "Assists per 100 possessions",
            "tov_per_100" = "Turnovers per 100 possessions",
            "ast_to_ratio" = "Assist/TO ratio",
            "blk_per_100" = "Blocks per 100 possessions",
            "efg_pct" = "eFG %"
    )
    
  })
  
  
  # To pass the metric inputs designated by the user by column reference
  metric_main <- reactive({
    
    req(input$metrics)
    
    switch (input$metrics,
            "pts_per_100" = 6,
            "pts_per_shot" = 7,
            "pts_created_per_100" = 8,
            "shots_per_100" = 9,
            "sfl_per_shot" = 10,
            "fta_per_100" = 11,
            "ast_per_100" = 12,
            "tov_per_100" = 13,
            "ast_to_ratio" = 14,
            "blk_per_100" = 15,
            "efg_pct" = 16,
    )
    
  })
  
  # To pass the pct_rank inputs designated by the user by column reference
  metric_off_avg <- reactive({
    
    req(input$metrics)
    
    switch (input$metrics,
            "pts_per_100" = 22,
            "pts_per_shot" = 23,
            "pts_created_per_100" = 28,
            "shots_per_100" = 27,
            "sfl_per_shot" = 26,
            "fta_per_100" = 25,
            "ast_per_100" = 24,
            "tov_per_100" = 32,
            "ast_to_ratio" = 31,
            "blk_per_100" = 33,
            "efg_pct" = 30,
    )
    
  })
  
  
  # To pass the pct_rank inputs designated by the user by name
  # metric_pct_rank_name <- reactive({
  #   
  #   req(input$metrics)
  #   
  #   switch (input$metrics,
  #           "pts_per_100" = pct_rank_pts_per_100, 
  #           "pts_per_shot" = pct_rank_pts_per_shot,
  #           "ast_per_100" = pct_rank_ast_per_100,
  #           "fta_per_100" = pct_rank_fta_per_100,
  #           "sfl_per_shot" = pct_rank_sfl_per_shot,
  #           "shots_per_100" = pct_rank_shots_per_100,
  #           "pts_created_per_100" = pct_rank_pts_created_per_100,
  #           "efg_pct" = pct_rank_efg_pct,
  #           "ast_to_ratio" = pct_rank_ast_to_ratio,
  #           "tov_per_100" = pct_rank_tov_per_100,
  #           "blk_per_100" = pct_rank_blk_per_100
  #   )
  #   
  # })
  
  
  # Offensive player average for given metric
  off_avg <- reactive({
    
    req(input$off_player)
    req(input$seasons)
    req(input$season_type)
    
    # Retrieve offensive player averages for each metric from main matchups file
    off_avg <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(off_player %in% input$off_player) %>%
      filter(type %in% input$season_type) %>%
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
                off_avg_efg = (fgm +(.5*fg3m))/fga,
                off_avg_ast_to_ratio = ast/tov,
                off_avg_tov_per_100 = (tov/poss)*100,
                off_avg_blk_per_100 = (blk/poss)*100) %>%
      select(1:2, 16:27) %>%
      as.data.frame()
    
  })
  
  
  # Final reactive element
  by_player <- reactive({
    
    req(input$off_player)
    req(input$def_players)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)
    
    # Create player data subset
    by_player <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(type %in% input$season_type) %>%
      filter(def_player %in% input$def_players) %>%
      filter(off_player %in% input$off_player) %>%
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
                efg_pct = (fgm +(.5*fg3m))/fga,
                fg3_pct = fg3m/fg3a,
                fg3_luck = if(is.na(fg3_pct)){"Reasonable"}
                else if(fg3_pct < .2){"Lucky"}
                else if(fg3_pct > .5){"Unlucky"}
                else {"Reasonable"},
                pct_def_time = mean(pct_defender_total_time),
                pct_off_time = mean(pct_off_total_time),
                pct_time_both_on = mean(pct_total_time_both_on)) %>%
      filter(poss > input$poss[1]) %>%
      select(1:6, 17:31) %>%
      as.data.frame() %>%
      # Merge player averages into dataset
      merge(off_avg(),
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
        metric_efg_pct = efg_pct,
        metric_ast_to_ratio = ast_to_ratio,
        metric_tov_per_100 = tov_per_100,
        metric_blk_per_100 = blk_per_100
      ) %>%
      select(1:2, 4, 17, metric_main(), metric_off_avg()) %>%
      rename(metric = starts_with("metric"),
             off_avg = starts_with("off_avg"))
    
  })
  
  by_team <- reactive({
    
    req(input$off_player)
    req(input$def_team)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)    
    
    # Create team data subset
    by_team <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(type %in% input$season_type) %>%
      filter(current_team %in% input$def_team) %>%
      filter(off_player %in% input$off_player) %>%
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
                efg_pct = (fgm +(.5*fg3m))/fga,
                fg3_pct = fg3m/fg3a,
                fg3_luck = if(is.na(fg3_pct)){"Reasonable"}
                else if(fg3_pct < .2){"Lucky"}
                else if(fg3_pct > .5){"Unlucky"}
                else {"Reasonable"},
                pct_def_time = mean(pct_defender_total_time),
                pct_off_time = mean(pct_off_total_time),
                pct_time_both_on = mean(pct_total_time_both_on)) %>%
      filter(poss > input$poss[1]) %>%
      select(1:6, 17:31) %>%
      as.data.frame() %>%
      # Merge player averages into dataset
      merge(off_avg(),
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
        metric_efg_pct = efg_pct,
        metric_ast_to_ratio = ast_to_ratio,
        metric_tov_per_100 = tov_per_100,
        metric_blk_per_100 = blk_per_100
      ) %>%
      select(1:2, 4, 17, metric_main(), metric_off_avg()) %>%
      rename(metric = starts_with("metric"),
             off_avg = starts_with("off_avg"))
    
  })
  
  
  
  output$plot1 <- renderPlot({
    
    input$metrics
    
    if(input$radio == "1"){
      
      vline <- by_player()[1,9]
      
      ggplot(data = by_player(),
             aes(x = metric,
                 y = fct_reorder(def_player, -metric))) +
        geom_vline(aes(xintercept = off_avg),
                   color = "grey35",
                   linetype = "dashed",
                   size = .7,
                   alpha = .9) +
        geom_point(aes(size = poss,
                       fill = fg3_pct,
                       color = after_scale(clr_darken(fill, 0.45))),
                   shape = 21,
                   alpha = 1.25) +
        theme_personal() +
        theme(panel.grid.major.x = element_blank(),
              legend.position = "top",
              panel.border = element_blank(),
              plot.margin = margin(30, 35, 12.5, 7.5),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(vjust = 1.5),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              plot.title = element_text(size = 26),
              plot.title.position = 'plot',
              legend.box = "horizontal",
              plot.subtitle = element_text(size = 14, color = "gray45")) +
        labs(y = "Defenders",
             x = plot_label(),
             title = input$off_player,
             subtitle = "Defender matchup performance compared to offensive player's metric average",
             size = "Possessions") +
        scale_size_continuous(
          range = c(1, 12.5),
          limits = if(max(by_player()$poss) > 400){c(0, 600)} 
          else if(max(by_player()$poss) > 300){c(0, 400)}
          else {c(0,300)},
          breaks = c(50, 100, 150, 200),
          labels = c("50", "100", "150", "200+")
        ) +
        scale_fill_gradient2(
          limits = c(0,1),
          breaks = c(.2, .4, .6, .8),
          labels = function(x) paste0(round(as.numeric(x*100)), "%"),
          midpoint = .4,
          low = "dodgerblue2",
          mid = "snow1",
          high = "indianred3",
          guide = "colourbar") +
        guides(
          size = guide_legend(
            nrow = 1,
            title.position = 'top',
            title.hjust = .5,
            title.vjust = -.7,
            # label.position = "bottom",
            order = 1),
          fill = guide_colorbar(
            title = "3pt %",
            title.position = 'top',
            title.hjust = .5,
            title.vjust = -.3,
            label.hjust = .2,
            nrow = 1,
            ticks = T,
            ticks.colour = "black",
            barwidth = 12
          )
        )
      
    } else {
      
      ggplot(data = by_team(), 
             aes(x = metric,
                 y = fct_reorder(def_player, -metric))) +
        geom_vline(aes(xintercept = off_avg),
                   color = "grey35",
                   linetype = "dashed",
                   size = .7,
                   alpha = .9) +
        geom_point(aes(size = poss,
                       fill = fg3_pct,
                       color = after_scale(clr_darken(fill, 0.45))),
                   shape = 21,
                   alpha = 1.25) +
        theme_personal() +
        theme(panel.grid.major.x = element_blank(),
              legend.position = "top",
              panel.border = element_blank(),
              plot.margin = margin(30, 35, 12.5, 7.5),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(vjust = 1.5),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              plot.title = element_text(size = 26),
              plot.title.position = 'plot',
              legend.box = "horizontal",
              plot.subtitle = element_text(size = 14, color = "gray45")) +
        labs(y = "Defenders",
             x = plot_label(),
             title = input$off_player,
             subtitle = "Defender matchup performance compared to offensive player's metric average",
             size = "Possessions") +
        scale_size_continuous(
          range = c(1, 12.5),
          limits = if(max(by_team()$poss) > 400){c(0, 600)} 
          else if(max(by_team()$poss) > 300){c(0, 400)}
          else {c(0,300)},
          breaks = c(50, 100, 150, 200),
          labels = c("50", "100", "150", "200+")
        ) +
        scale_fill_gradient2(
          limits = c(0,1),
          breaks = c(.2, .4, .6, .8),
          labels = function(x) paste0(round(as.numeric(x*100)), "%"),
          midpoint = .4,
          low = "dodgerblue2",
          mid = "snow1",
          high = "indianred3",
          guide = "colourbar") +
        guides(
          size = guide_legend(
            nrow = 1,
            title.position = 'top',
            title.hjust = .5,
            title.vjust = -.7,
            # label.position = "bottom",
            order = 1),
          fill = guide_colorbar(
            title = "3pt %",
            title.position = 'top',
            title.hjust = .5,
            title.vjust = -.3,
            label.hjust = .2,
            nrow = 1,
            ticks = T,
            ticks.colour = "black",
            barwidth = 12
          )
        )
    }
    
  })
  
  
  
  
  ## - - - - - - - - - - - - - - - - - - -
  ## Table section
  ## - - - - - - - - - - - - - - - - - - - 
  
  
  # To pass the metric inputs designated by the user by column reference
  metric <- reactive({
    
    req(input$metrics)
    
    switch (input$metrics,
            "pts_per_100" = 19,
            "pts_per_shot" = 20,
            "ast_per_100" = 21,
            "fta_per_100" = 22,
            "sfl_per_shot" = 23,
            "shots_per_100" = 24,
            "pts_created_per_100" = 25,
            "efg_pct" = 27,
            "ast_to_ratio" = 28,
            "tov_per_100" = 29,
            "blk_per_100" = 30,
    )
    
  })
  
  # To pass the pct_rank inputs designated by the user by column reference
  metric_pct_rank <- reactive({
    
    req(input$metrics)
    
    switch (input$metrics,
            "pts_per_100" = 7,
            "pts_per_shot" = 8,
            "ast_per_100" = 9,
            "fta_per_100" = 10,
            "sfl_per_shot" = 11,
            "shots_per_100" = 12,
            "pts_created_per_100" = 13,
            "efg_pct" = 15,
            "ast_to_ratio" = 16,
            "tov_per_100" = 17,
            "blk_per_100" = 18,
    )
    
  })
  
  
  # To pass the pct_rank inputs designated by the user by name
  metric_pct_rank_name <- reactive({
    
    req(input$metrics)
    
    switch (input$metrics,
            "pts_per_100" = pct_rank_pts_per_100, 
            "pts_per_shot" = pct_rank_pts_per_shot,
            "ast_per_100" = pct_rank_ast_per_100,
            "fta_per_100" = pct_rank_fta_per_100,
            "sfl_per_shot" = pct_rank_sfl_per_shot,
            "shots_per_100" = pct_rank_shots_per_100,
            "pts_created_per_100" = pct_rank_pts_created_per_100,
            "efg_pct" = pct_rank_efg_pct,
            "ast_to_ratio" = pct_rank_ast_to_ratio,
            "tov_per_100" = pct_rank_tov_per_100,
            "blk_per_100" = pct_rank_blk_per_100
    )
    
  })
  
  
  
  
  
  
  
  
  # Pct Rank Reactive
  pct_rank <- reactive({
    
    req(input$off_player)
    req(input$seasons)
    req(input$season_type)
    
    pct_rank <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(off_player %in% input$off_player) %>%
      filter(type %in% input$season_type) %>%
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
                pct_rank_pts_per_shot = (points/fga),
                pct_rank_ast_per_100 = (ast/poss)*100,
                pct_rank_fta_per_100 = (fta/poss)*100,
                pct_rank_sfl_per_shot = (sfl/fga),
                pct_rank_shots_per_100 = (fga/poss)*100,
                pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                pct_rank_fg3_pct = fg3m/fg3a,
                pct_rank_efg_pct = (fgm +(.5*fg3m))/fga,
                pct_rank_ast_to_ratio = ast/tov,
                pct_rank_tov_per_100 = (tov/poss)*100,
                pct_rank_blk_per_100 = (blk/poss)*100) %>%
      filter(poss > 10) %>%
      select(1:6, 17:28) %>%
      as.data.frame() %>%
      mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
      mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
      select(def_player_id, def_player, metric_pct_rank())
  })
  
  
  table_data_player <- reactive({
    
    req(input$off_player)
    req(input$def_players)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)
    
    # Selected Player Table - Final
    table_data_player <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(off_player %in% input$off_player) %>%
      filter(type %in% input$season_type) %>%
      filter(def_player %in% input$def_players) %>%
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
                pts_per_shot = (points/fga),
                ast_per_100 = (ast/poss)*100,
                fta_per_100 = (fta/poss)*100,
                sfl_per_shot = (sfl/fga),
                shots_per_100 = (fga/poss)*100,
                pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                fg3_pct = fg3m/fg3a,
                efg_pct = (fgm +(.5*fg3m))/fga,
                ast_to_ratio = ast/tov,
                tov_per_100 = (tov/poss)*100,
                blk_per_100 = (blk/poss)*100) %>%
      mutate(efg_pct = efg_pct*100) %>%
      rename(
        `Defender` = def_player,
        `Poss` = poss,
        `% of Time Defended` = pct_defended,
        `Pts per 100` = pts_per_100,
        `Pts per shot` = pts_per_shot,
        `Ast per 100` = ast_per_100,
        `FT Att per 100` = fta_per_100,
        `Shooting Fouls per shot` = sfl_per_shot,
        `Shots per 100` = shots_per_100,
        `Pts Scored/ Ast'd per 100` = pts_created_per_100,
        `3pt %` = fg3_pct,
        `eFG %` = efg_pct,
        `Ast/TO` = ast_to_ratio,
        `Turnovers per 100` = tov_per_100,
        `Blocks per 100` = blk_per_100
      ) %>%
      filter(Poss > input$poss[1]) %>%
      select(1:2, 4:5,`Shots per 100`, 26, metric()) %>%
      merge(pct_rank() %>% select(1,3),
            by = "def_player_id",
            all.x = T) %>%
      select(-1) %>%
      rename(`Percent Rank` = starts_with("pct_rank"))
  })
  
  # Selected Player Table - Final
  table_data_team <- reactive({
    
    req(input$off_player)
    req(input$def_team)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)
    
    table_data_team <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(type %in% input$season_type) %>%
      filter(current_team %in% input$def_team) %>%
      filter(off_player %in% input$off_player) %>%
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
                pts_per_shot = (points/fga),
                ast_per_100 = (ast/poss)*100,
                fta_per_100 = (fta/poss)*100,
                sfl_per_shot = (sfl/fga),
                shots_per_100 = (fga/poss)*100,
                pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                fg3_pct = fg3m/fg3a,
                efg_pct = (fgm +(.5*fg3m))/fga,
                ast_to_ratio = ast/tov,
                tov_per_100 = (tov/poss)*100,
                blk_per_100 = (blk/poss)*100) %>%
      mutate(efg_pct = efg_pct *100) %>%
      rename(
        `Defender` = def_player,
        `Poss` = poss,
        `% of Time Defended` = pct_defended,
        `Pts per 100` = pts_per_100,
        `Pts per shot` = pts_per_shot,
        `Ast per 100` = ast_per_100,
        `FT Att per 100` = fta_per_100,
        `Shooting Fouls per shot` = sfl_per_shot,
        `Shots per 100` = shots_per_100,
        `Pts Scored/ Ast'd per 100` = pts_created_per_100,
        `3pt %` = fg3_pct,
        `eFG %` = efg_pct,
        `Ast/TO` = ast_to_ratio,
        `Turnovers per 100` = tov_per_100,
        `Blocks per 100` = blk_per_100
      ) %>%
      filter(Poss > input$poss[1]) %>%
      select(1:2, 4:5,`Shots per 100`, `3pt %`, metric()) %>%
      merge(pct_rank() %>% select(1,3),
            by = "def_player_id",
            all.x = T) %>%
      select(-1) %>%
      rename(`Percent Rank` = starts_with("pct_rank"))
  })
  
  
  
  # Top 5 Defenders - Performance
  top_perf <- reactive({
    
    req(input$off_player)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)
    
    top_perf <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(off_player %in% input$off_player) %>%
      filter(type %in% input$season_type) %>%
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
                pct_rank_pts_per_shot = (points/fga),
                pct_rank_ast_per_100 = (ast/poss)*100,
                pct_rank_fta_per_100 = (fta/poss)*100,
                pct_rank_sfl_per_shot = (sfl/fga),
                pct_rank_shots_per_100 = (fga/poss)*100,
                pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                pct_rank_fg3_pct = fg3m/fg3a,
                pct_rank_efg_pct = (fgm +(.5*fg3m))/fga,
                pct_rank_ast_to_ratio = ast/tov,
                pct_rank_tov_per_100 = (tov/poss)*100,
                pct_rank_blk_per_100 = (blk/poss)*100,
                pts_per_100 = (points/poss)*100,
                pts_per_shot = (points/fga),
                ast_per_100 = (ast/poss)*100,
                fta_per_100 = (fta/poss)*100,
                sfl_per_shot = (sfl/fga),
                shots_per_100 = (fga/poss)*100,
                pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                fg3_pct = fg3m/fg3a,
                efg_pct = (fgm +(.5*fg3m))/fga,
                ast_to_ratio = ast/tov,
                tov_per_100 = (tov/poss)*100,
                blk_per_100 = (blk/poss)*100) %>%
      filter(poss > input$poss[1]) %>%
      mutate(efg_pct = efg_pct *100) %>%
      rename(
        `Defender` = def_player,
        `Poss` = poss,
        `% of Time Defended` = pct_defended,
        `Pts per 100` = pts_per_100,
        `Pts per shot` = pts_per_shot,
        `Ast per 100` = ast_per_100,
        `FT Att per 100` = fta_per_100,
        `Shooting Fouls per shot` = sfl_per_shot,
        `Shots per 100` = shots_per_100,
        `Pts Scored/ Ast'd per 100` = pts_created_per_100,
        `3pt %` = fg3_pct,
        `eFG %` = efg_pct,
        `Ast/TO` = ast_to_ratio,
        `Turnovers per 100` = tov_per_100,
        `Blocks per 100` = blk_per_100
      ) %>%
      select(1:6, 17:40) %>%
      as.data.frame() %>%
      mutate(across(points, as.numeric)) %>%
      mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
      mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
      select(2, 4:5,`Shots per 100`, 26, metric(), metric_pct_rank()) %>%
      arrange(across(starts_with("pct_rank"), desc)) %>%
      slice(n = 1:10) %>%
      rename(`Percent Rank` = starts_with("pct_rank"))
  })
  
  
  
  # Bottom 5 Defenders - Performance
  bot_perf <- reactive({
    
    req(input$off_player)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)
    
    top_perf <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(off_player %in% input$off_player) %>%
      filter(type %in% input$season_type) %>%
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
                pct_rank_pts_per_shot = (points/fga),
                pct_rank_ast_per_100 = (ast/poss)*100,
                pct_rank_fta_per_100 = (fta/poss)*100,
                pct_rank_sfl_per_shot = (sfl/fga),
                pct_rank_shots_per_100 = (fga/poss)*100,
                pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                pct_rank_fg3_pct = fg3m/fg3a,
                pct_rank_efg_pct = (fgm +(.5*fg3m))/fga,
                pct_rank_ast_to_ratio = ast/tov,
                pct_rank_tov_per_100 = (tov/poss)*100,
                pct_rank_blk_per_100 = (blk/poss)*100,
                pts_per_100 = (points/poss)*100,
                pts_per_shot = (points/fga),
                ast_per_100 = (ast/poss)*100,
                fta_per_100 = (fta/poss)*100,
                sfl_per_shot = (sfl/fga),
                shots_per_100 = (fga/poss)*100,
                pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                fg3_pct = fg3m/fg3a,
                efg_pct = (fgm +(.5*fg3m))/fga,
                ast_to_ratio = ast/tov,
                tov_per_100 = (tov/poss)*100,
                blk_per_100 = (blk/poss)*100) %>%
      filter(poss > input$poss[1]) %>%
      mutate(efg_pct = efg_pct *100) %>%
      rename(
        `Defender` = def_player,
        `Poss` = poss,
        `% of Time Defended` = pct_defended,
        `Pts per 100` = pts_per_100,
        `Pts per shot` = pts_per_shot,
        `Ast per 100` = ast_per_100,
        `FT Att per 100` = fta_per_100,
        `Shooting Fouls per shot` = sfl_per_shot,
        `Shots per 100` = shots_per_100,
        `Pts Scored/ Ast'd per 100` = pts_created_per_100,
        `3pt %` = fg3_pct,
        `eFG %` = efg_pct,
        `Ast/TO` = ast_to_ratio,
        `Turnovers per 100` = tov_per_100,
        `Blocks per 100` = blk_per_100
      ) %>%
      select(1:6, 17:40) %>%
      as.data.frame() %>%
      mutate(across(points, as.numeric)) %>%
      mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
      mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
      select(2, 4:5,`Shots per 100`, 26, metric(), metric_pct_rank()) %>%
      arrange(across(starts_with("pct_rank"))) %>%
      slice(n = 1:10) %>%
      rename(`Percent Rank` = starts_with("pct_rank"))
  })
  
  # Top 5 Defenders - Volume
  top_vol <- reactive({
    
    req(input$off_player)
    req(input$seasons)
    req(input$season_type)
    req(input$poss)
    
    top_vol <- matchups %>%
      filter(season %in% input$seasons) %>%
      filter(off_player %in% input$off_player) %>%
      filter(type %in% input$season_type) %>%
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
                pct_rank_pts_per_shot = (points/fga),
                pct_rank_ast_per_100 = (ast/poss)*100,
                pct_rank_fta_per_100 = (fta/poss)*100,
                pct_rank_sfl_per_shot = (sfl/fga),
                pct_rank_shots_per_100 = (fga/poss)*100,
                pct_rank_pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                pct_rank_fg3_pct = fg3m/fg3a,
                pct_rank_efg_pct = (fgm +(.5*fg3m))/fga,
                pct_rank_ast_to_ratio = ast/tov,
                pct_rank_tov_per_100 = (tov/poss)*100,
                pct_rank_blk_per_100 = (blk/poss)*100,
                pts_per_100 = (points/poss)*100,
                pts_per_shot = (points/fga),
                ast_per_100 = (ast/poss)*100,
                fta_per_100 = (fta/poss)*100,
                sfl_per_shot = (sfl/fga),
                shots_per_100 = (fga/poss)*100,
                pts_created_per_100 = (((points + (2.4*ast))/poss)*100),
                fg3_pct = fg3m/fg3a,
                efg_pct = (fgm +(.5*fg3m))/fga,
                ast_to_ratio = ast/tov,
                tov_per_100 = (tov/poss)*100,
                blk_per_100 = (blk/poss)*100) %>%
      filter(poss > input$poss[1]) %>%
      mutate(efg_pct = efg_pct *100) %>%
      rename(
        `Defender` = def_player,
        `Poss` = poss,
        `% of Time Defended` = pct_defended,
        `Pts per 100` = pts_per_100,
        `Pts per shot` = pts_per_shot,
        `Ast per 100` = ast_per_100,
        `FT Att per 100` = fta_per_100,
        `Shooting Fouls per shot` = sfl_per_shot,
        `Shots per 100` = shots_per_100,
        `Pts Scored/ Ast'd per 100` = pts_created_per_100,
        `3pt %` = fg3_pct,
        `eFG %` = efg_pct,
        `Ast/TO` = ast_to_ratio,
        `Turnovers per 100` = tov_per_100,
        `Blocks per 100` = blk_per_100
      ) %>%
      select(1:6, 17:40) %>%
      as.data.frame() %>%
      mutate(across(points, as.numeric)) %>%
      mutate(across(7:16, ~ ((1-percent_rank(.x))*100))) %>%
      mutate(across(17:18, ~ (percent_rank(.x)*100))) %>%
      select(2, 4:5,`Shots per 100`, 26, metric(), metric_pct_rank()) %>%
      slice_max(`Poss`, n = 10) %>%
      arrange(desc(`Poss`)) %>%
      rename(`Percent Rank` = starts_with("pct_rank"))
    
  })
  
  
  # Rename all unique metric column names to pass into data tables
  output$selected_table <- renderDT({
    
    if(input$radio == "1"){
      
      datatable(table_data_player(),
                options = list(info = F,
                               paging = F,
                               searching = F,
                               stripeClasses = F,
                               lengthChange = F,
                               scrollY = '255px',
                               scrollX = T,
                               scrollCollapse = T,
                               order = list(6, 'desc'),
                               # autoWidth = T,
                               columnDefs = list(
                                 list(width = '55px', targets = c(2,4,5)),
                                 list(width = '140px', targets = c(0)),
                                 list(className = "dt-left", targets = 0),
                                 list(className = "dt-right", targets = 4),
                                 list(width = '30px', targets = c(1,3)))),
                rownames = F) %>%
        formatRound(
          columns = c(2,3,4,5,6,7),
          digits = c(1,1,1,1,1,0)) %>%
        formatPercentage(c(3,5), 1) %>%
        formatStyle(names(table_data_player()), backgroundColor = "ghostwhite")
      
    }
    else {
      
      datatable(table_data_team(),
                options = list(info = F,
                               paging = F,
                               searching = F,
                               stripeClasses = F,
                               lengthChange = F,
                               scrollY = '240px',
                               scrollX = T,
                               scrollCollapse = T,
                               order = list(6, 'desc'),
                               # autoWidth = T,
                               columnDefs = list(
                                 list(width = '55px', targets = c(2,4,5)),
                                 list(width = '140px', targets = c(0)),
                                 list(className = "dt-left", targets = 0),
                                 list(className = "dt-right", targets = 4),
                                 list(width = '30px', targets = c(1,3)))),
                rownames = F) %>%
        formatRound(
          columns = c(2,3,4,5,6,7),
          digits = c(1,1,1,1,1,0)) %>%
        formatPercentage(c(3,5), 1) %>%
        formatStyle(names(table_data_team()), backgroundColor = "ghostwhite")
      
      
    }
    
    
    
    
  })
  
  
  output$top_perf <- renderDT({
    
    datatable(top_perf(), 
              options = list(info = F,
                             paging = F,
                             searching = F,
                             stripeClasses = F,
                             lengthChange = F,
                             scrollY = '240px',
                             scrollX = T,
                             scrollCollapse = T,
                             order = list(6, 'desc'),
                             # autoWidth = T,
                             columnDefs = list(
                               list(width = '55px', targets = c(2,4,5)),
                               list(width = '140px', targets = c(0)),
                               list(className = "dt-left", targets = 0),
                               list(className = "dt-right", targets = 4),
                               list(width = '30px', targets = c(1,3)))),
              rownames = F) %>%
      formatRound(
        columns = c(2,3,4,5,6,7),
        digits = c(1,1,1,1,1,0)) %>%
      formatPercentage(c(3,5), 1) %>%
      formatStyle(names(top_perf()), backgroundColor = "ghostwhite")
    
  })
  
  
  output$bot_perf <- renderDT({
    
    datatable(bot_perf(), 
              options = list(info = F,
                             paging = F,
                             searching = F,
                             stripeClasses = F,
                             lengthChange = F,
                             scrollY = '240px',
                             scrollX = T,
                             scrollCollapse = T,
                             order = list(6, 'asc'),
                             # autoWidth = T,
                             columnDefs = list(
                               list(width = '55px', targets = c(2,4,5)),
                               list(width = '140px', targets = c(0)),
                               list(className = "dt-left", targets = 0),
                               list(className = "dt-right", targets = 4),
                               list(width = '30px', targets = c(1,3)))),
              rownames = F) %>%
      formatRound(
        columns = c(2,3,4,5,6,7),
        digits = c(1,1,1,1,1,0)) %>%
      formatPercentage(c(3,5), 1) %>%
      formatStyle(names(bot_perf()), backgroundColor = "ghostwhite")
    
  })
  
  output$top_vol <- renderDT({
    
    datatable(top_vol(), 
              options = list(info = F,
                             paging = F,
                             searching = F,
                             stripeClasses = F,
                             lengthChange = F,
                             scrollY = '240px',
                             scrollX = T,
                             scrollCollapse = T,
                             order = list(6, 'desc'),
                             # autoWidth = T,
                             columnDefs = list(
                               list(width = '55px', targets = c(2,4,5)),
                               list(width = '140px', targets = c(0)),
                               list(className = "dt-left", targets = 0),
                               list(className = "dt-right", targets = 4),
                               list(width = '30px', targets = c(1,3)))),
              rownames = F) %>%
      formatRound(
        columns = c(2,3,4,5,6,7),
        digits = c(1,1,1,1,1,0)) %>%
      formatPercentage(c(3,5), 1) %>%
      formatStyle(names(top_vol()), backgroundColor = "ghostwhite")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
