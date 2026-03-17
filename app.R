library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(shinyWidgets)

# ============================================================================
# CARREGAR DADES (tots els grups, amb columna 'grup')
# ============================================================================
all_matches_events_all <- read.csv("consolidat_25_26_all_matches_events.csv", stringsAsFactors = FALSE)
all_matches_lineups_all <- read.csv("consolidat_25_26_all_matches_lineups.csv", stringsAsFactors = FALSE)
all_matches_info_all    <- read.csv("consolidat_25_26_all_matches_info.csv",    stringsAsFactors = FALSE)
player_match_stats_all  <- read.csv("consolidat_25_26_player_match_stats.csv",  stringsAsFactors = FALSE)
player_stats_all        <- read.csv("consolidat_25_26_player_stats.csv",        stringsAsFactors = FALSE)
standings_by_round_all  <- read.csv("consolidat_25_26_standings_by_round.csv",  stringsAsFactors = FALSE)
team_match_stats_all    <- read.csv("consolidat_25_26_team_match_stats.csv",    stringsAsFactors = FALSE)
matches_all             <- read.csv("consolidat_25_26_matches.csv",             stringsAsFactors = FALSE)

# Assegurem que el camp 'grup' existeix i normalitzem nom de columna local_team si cal
# (matches pot tenir local_team o home_team — usem local_team com a estàndard)
if ("home_team" %in% names(matches_all) && !"local_team" %in% names(matches_all)) {
  matches_all <- matches_all %>% rename(local_team = home_team)
}

# Índex de categories i grups disponibles
categories_disponibles <- sort(unique(all_matches_events_all$categoria))

# Label visual per categoria (TERCERA → "Tercera Catalana", etc.)
categoria_label <- function(cat) {
  switch(cat,
         "TERCERA"  = "Tercera Catalana",
         "SEGONA"   = "Segona Catalana",
         "PRIMERA"  = "Primera Catalana",
         cat   # fallback: el valor tal qual
  )
}
cat_choices <- setNames(categories_disponibles, sapply(categories_disponibles, categoria_label))

grups_disponibles <- sort(unique(all_matches_events_all$grup))

# ============================================================================
# FUNCIONS DE CÀLCUL (reben les dades ja filtrades per grup)
# ============================================================================

calculate_player_impact <- function(player_match_data, team_match_data) {
  team_match_w <- team_match_data %>%
    mutate(team_points = case_when(
      goals_for > goals_against ~ 3,
      goals_for == goals_against ~ 1,
      TRUE ~ 0
    ))
  # Usar jornada com a clau de join (evita problemes amb match_id inconsistents entre datasets)
  all_team_matches <- team_match_w %>%
    select(team, jornada, team_points) %>% distinct()
  all_players_teams <- player_match_data %>% select(player, team) %>% distinct()
  player_all_matches <- all_players_teams %>%
    left_join(all_team_matches, by = "team", relationship = "many-to-many")
  player_participation <- player_match_data %>% select(player, team, jornada, minutes_played) %>%
    group_by(player, team, jornada) %>% summarise(minutes_played = sum(minutes_played, na.rm=TRUE), .groups="drop")
  player_complete_record <- player_all_matches %>%
    left_join(player_participation, by = c("player", "team", "jornada")) %>%
    mutate(minutes_played = replace_na(minutes_played, 0), played = minutes_played > 0)
  points_when_playing <- player_complete_record %>%
    filter(played) %>%
    group_by(player, team) %>%
    summarise(avg_points_when_playing = mean(team_points, na.rm = TRUE),
              games_played = n(), .groups = "drop")
  points_when_not_playing <- player_complete_record %>%
    filter(!played) %>%
    group_by(player, team) %>%
    summarise(avg_points_when_not_playing = mean(team_points, na.rm = TRUE),
              games_not_played = n(), .groups = "drop")
  points_when_playing %>%
    left_join(points_when_not_playing, by = c("player", "team")) %>%
    mutate(
      games_not_played = replace_na(games_not_played, 0),
      impacte = if_else(games_not_played > 0,
                        avg_points_when_playing - avg_points_when_not_playing,
                        NA_real_)
    ) %>%
    select(player, team, avg_points_when_playing, avg_points_when_not_playing,
           impacte, games_played, games_not_played)
}

calculate_player_ratings <- function(player_match_data, player_impact_data) {
  if(nrow(player_match_data) == 0) return(data.frame(player=character(),team=character(),total_minutes=numeric(),goals_per_90=numeric(),impacte=numeric(),rating_global=numeric()))
  player_agg <- player_match_data %>%
    group_by(player, team) %>%
    summarise(
      total_minutes  = sum(minutes_played, na.rm = TRUE),
      total_goals    = sum(goals, na.rm = TRUE),
      total_yellows  = sum(yellow_cards, na.rm = TRUE),
      total_reds     = sum(red_cards, na.rm = TRUE),
      matches_played = n(), .groups = "drop"
    ) %>%
    mutate(
      goals_per_90   = ifelse(total_minutes > 0, (total_goals  / total_minutes) * 90, 0),
      yellows_per_90 = ifelse(total_minutes > 0, (total_yellows/ total_minutes) * 90, 0)
    ) %>%
    left_join(player_impact_data %>% select(player, team, impacte, games_not_played),
              by = c("player", "team")) %>%
    mutate(impacte = replace_na(impacte, 0)) %>%
    mutate(
      rating_off_raw    = 0.4 * goals_per_90 + 0.4 * pmax(-1, pmin(2, impacte)) +
        0.2 * (total_minutes / 900),
      rating_off        = (rating_off_raw - mean(rating_off_raw, na.rm = TRUE)) /
        (sd(rating_off_raw, na.rm = TRUE) + 0.01) * 0.3 + 1,
      rating_def_raw    = 1 - 0.15 * yellows_per_90 + 0.1 * pmax(0, impacte),
      rating_def        = pmax(0.5, pmin(1.5, rating_def_raw)),
      rating_global_raw = 0.65 * rating_off + 0.35 * rating_def,
      rating_global     = {
        rng_min <- min(rating_global_raw, na.rm = TRUE)
        rng_max <- max(rating_global_raw, na.rm = TRUE)
        if(is.finite(rng_min) && is.finite(rng_max) && rng_max > rng_min)
          round((rating_global_raw - rng_min) / (rng_max - rng_min) * 100, 0)
        else
          rep(50L, length(rating_global_raw))
      }
    )
  player_agg %>% select(player, team, total_minutes, goals_per_90, impacte, rating_global)
}

estimate_team_latents <- function(team_match_data) {
  if(nrow(team_match_data)==0) return(data.frame(team=character(),n_matches=integer(),attack=numeric(),defense=numeric(),avg_goals_for=numeric(),avg_goals_against=numeric(),rating=numeric()))
  global_avg_goals <- mean(c(team_match_data$goals_for, team_match_data$goals_against), na.rm=TRUE)
  res <- team_match_data %>%
    group_by(team) %>%
    summarise(n_matches = n(), avg_goals_for = mean(goals_for, na.rm=TRUE),
              avg_goals_against = mean(goals_against, na.rm=TRUE), .groups = "drop") %>%
    mutate(
      shrinkage = n_matches / (n_matches + 5),
      attack    = (log(avg_goals_for     + 0.1) - log(global_avg_goals + 0.1)) * shrinkage,
      defense   = -(log(avg_goals_against + 0.1) - log(global_avg_goals + 0.1)) * shrinkage,
      raw_score = (attack + defense) / 2
    )
  rng_min <- min(res$raw_score, na.rm=TRUE); rng_max <- max(res$raw_score, na.rm=TRUE)
  res %>% mutate(
    rating = if(is.finite(rng_min) && is.finite(rng_max) && rng_max > rng_min)
      round((raw_score - rng_min) / (rng_max - rng_min) * 100, 0)
    else rep(50L, n())
  ) %>%
    select(team, n_matches, attack, defense, avg_goals_for, avg_goals_against, rating)
}

calculate_team_radar <- function(team_match_data, all_events_data) {
  if(nrow(team_match_data)==0) return(data.frame(team=character(),radar_attack=numeric(),radar_defense=numeric(),radar_home=numeric(),radar_away=numeric(),radar_fairplay=numeric(),radar_first=numeric(),radar_second=numeric()))
  scale_0100 <- function(x) {
    x[!is.finite(x)] <- NA
    rng <- range(x, na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(50, length(x)))
    round((x - rng[1]) / (rng[2] - rng[1]) * 100, 1)
  }
  global_avg_goals <- mean(c(team_match_data$goals_for, team_match_data$goals_against), na.rm=TRUE)
  radar_raw <- team_match_data %>%
    group_by(team) %>%
    summarise(
      n_matches       = n(),
      avg_gf          = mean(goals_for, na.rm=TRUE),
      avg_ga          = mean(goals_against, na.rm=TRUE),
      avg_pts_home    = mean(team_points[home_away == "Home"], na.rm = TRUE),
      avg_pts_away    = mean(team_points[home_away == "Away"], na.rm = TRUE),
      cards_per_match = mean(yellow_cards + red_cards * 3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      shrinkage = n_matches / (n_matches + 5),
      attack    = (log(avg_gf + 0.1) - log(global_avg_goals + 0.1)) * shrinkage,
      defense   = -(log(avg_ga + 0.1) - log(global_avg_goals + 0.1)) * shrinkage
    )
  goals_by_half <- all_events_data %>%
    filter(event_type == "Gol", !is.na(minute)) %>%
    mutate(half = ifelse(minute <= 45, "goals_first", "goals_second")) %>%
    group_by(team, half) %>% summarise(goals = n(), .groups = "drop") %>%
    pivot_wider(names_from = half, values_from = goals, values_fill = 0)
  if (!"goals_first"  %in% names(goals_by_half)) goals_by_half$goals_first  <- 0
  if (!"goals_second" %in% names(goals_by_half)) goals_by_half$goals_second <- 0
  radar_raw <- radar_raw %>%
    left_join(goals_by_half, by = "team") %>%
    mutate(
      goals_first  = replace_na(goals_first,  0) / n_matches,
      goals_second = replace_na(goals_second, 0) / n_matches
    )
  radar_raw %>%
    mutate(
      radar_attack   = scale_0100(attack),
      radar_defense  = scale_0100(defense),
      radar_home     = scale_0100(avg_pts_home),
      radar_away     = scale_0100(avg_pts_away),
      radar_fairplay = scale_0100(-cards_per_match),
      radar_first    = scale_0100(goals_first),
      radar_second   = scale_0100(goals_second)
    ) %>%
    select(team, radar_attack, radar_defense, radar_home,
           radar_away, radar_fairplay, radar_first, radar_second)
}

calculate_player_radar <- function(player_match_data, player_impact_data, player_stats_data) {
  scale_0100 <- function(x) {
    x[!is.finite(x)] <- NA
    rng <- range(x, na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(50, length(x)))
    round(pmax(0, pmin(100, (x - rng[1]) / (rng[2] - rng[1]) * 100)), 1)
  }
  base <- player_match_data %>%
    group_by(player, team) %>%
    summarise(
      total_minutes  = sum(minutes_played, na.rm = TRUE),
      total_goals    = sum(goals, na.rm = TRUE),
      total_yellows  = sum(yellow_cards, na.rm = TRUE),
      total_reds     = sum(red_cards, na.rm = TRUE),
      matches_played = n(),
      starts         = sum(starter, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      goals_per_90   = ifelse(total_minutes > 0, (total_goals   / total_minutes) * 90, 0),
      yellows_per_90 = ifelse(total_minutes > 0, (total_yellows / total_minutes) * 90, 0),
      availability   = starts / pmax(matches_played, 1),
      participation  = total_minutes / pmax(matches_played * 90, 1)
    ) %>%
    left_join(player_impact_data %>% select(player, team, impacte), by = c("player","team")) %>%
    mutate(impacte = replace_na(impacte, 0))
  base %>%
    mutate(
      radar_gol         = scale_0100(goals_per_90),
      radar_impacte     = scale_0100(pmax(-2, pmin(3, impacte))),
      radar_titularitat = scale_0100(availability),
      radar_minuts      = scale_0100(participation),
      radar_fairplay    = scale_0100(-yellows_per_90 - 3 * (total_reds / pmax(total_minutes / 90, 1)))
    ) %>%
    select(player, team, radar_gol, radar_impacte, radar_titularitat, radar_minuts, radar_fairplay)
}

calculate_tilt <- function(team_match_data, team_ratings_data, n_recent = 5) {
  draw_param    <- 20 / 3
  rating_lookup <- team_ratings_data %>% select(team, rating)
  tilt_vals <- team_match_data %>%
    left_join(rating_lookup, by = "team") %>%
    left_join(rating_lookup %>% rename(rating_opp = rating, opponent = team), by = "opponent") %>%
    mutate(
      rating     = replace_na(rating, 50),
      rating_opp = replace_na(rating_opp, 50),
      s_i        = 10^(rating     / 50),
      s_j        = 10^(rating_opp / 50),
      denom      = s_i + s_j + draw_param,
      p_win        = s_i / denom,
      p_draw       = draw_param / denom,
      pts_expected = 3 * p_win + 1 * p_draw,
      surprise     = team_points - pts_expected
    ) %>%
    arrange(team, jornada)
  tilt_vals %>%
    group_by(team) %>%
    mutate(rank_recent = rank(jornada, ties.method = "last")) %>%
    filter(rank_recent > (max(rank_recent) - n_recent)) %>%
    mutate(weight = 2^(rank_recent - max(rank_recent) + n_recent - 1)) %>%
    summarise(
      tilt             = round(weighted.mean(surprise, weight, na.rm = TRUE), 2),
      n_recents        = n(),
      pts_real_avg     = round(mean(team_points,  na.rm = TRUE), 2),
      pts_expected_avg = round(mean(pts_expected, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# ============================================================================
# UI
# ============================================================================
ui <- dashboardPage(
  skin = "blue",
  title = "Futbol Català",
  
  dashboardHeader(
    title = "Futbol Català",
    # Selector de categoria + grup a la capçalera
    tags$li(class = "dropdown",
            style = "padding: 8px 10px;",
            div(style = "display:flex; align-items:center; gap:10px;",
                tags$span(style = "color:white; font-weight:bold; font-size:13px;", "Categoria:"),
                selectInput("cat_select", label = NULL,
                            choices  = cat_choices,
                            selected = categories_disponibles[1],
                            width    = "170px"),
                tags$span(style = "color:white; font-weight:bold; font-size:13px;", "Grup:"),
                uiOutput("grup_select_ui")
            )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Inici",         tabName = "inici",        icon = icon("home")),
                menuItem("Partits",       tabName = "partits",      icon = icon("calendar")),
                menuItem("Classificació", tabName = "classificacio", icon = icon("trophy")),
                menuItem("Equips",        tabName = "equips",        icon = icon("users")),
                menuItem("Jugadors",      tabName = "jugadors",      icon = icon("user")),
                menuItem("Estadístiques", tabName = "stats",         icon = icon("chart-bar")),
                menuItem("Simulador",     tabName = "simulador",     icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$meta(name = "google-site-verification",
                content = "Wp4ZYDVAi-_dmXURLT_hqTP0cRil4d1BvIckOsJrdO8"),
      tags$meta(name = "description",
                content = "Resultats, classificació i estadístiques avançades de la Tercera Catalana.")
    ),
    tags$head(tags$style(HTML("
      /* Selectors de categoria i grup a la capçalera */
      .main-header .navbar .dropdown { padding-top: 0 !important; }
      .main-header .navbar .dropdown select {
        height: 34px; border-radius: 4px; border: none;
        background: rgba(255,255,255,0.15); color: white;
        font-weight: bold; font-size: 13px; padding: 0 6px;
        cursor: pointer;
      }
      .main-header .navbar .dropdown select option { color: #333; background: white; }
      /* El grup_select_ui és un renderUI — assegurem estil consistent */
      .main-header .navbar .dropdown .shiny-input-container { margin: 0; }
      .main-header .navbar .dropdown .shiny-input-container select {
        height: 34px; border-radius: 4px; border: none;
        background: rgba(255,255,255,0.15); color: white;
        font-weight: bold; font-size: 13px; padding: 0 6px;
        cursor: pointer;
      }
      .content-wrapper { background-color: #f4f6f9; }
      .small-box .inner h3 { font-size: 28px; }
      .acta-box { background: white; border-radius: 8px; padding: 15px;
                  margin-bottom: 10px; box-shadow: 0 1px 4px rgba(0,0,0,.1); }
      .gol-event   { color: #27ae60; font-weight: bold; }
      .subst-event { color: #2980b9; }
      .card-event  { color: #e67e22; font-weight: bold; }
      #playerModal .modal-dialog { max-width: 900px; }
      #playerModal .modal-header { background: #1a6b3a; color: white; border-radius: 4px 4px 0 0; }
      #playerModal .modal-title  { font-size: 1.4em; font-weight: bold; }
      #playerModal .modal-body   { background: #f4f6f9; padding: 20px; }
      #partidModal .modal-dialog { max-width: 1100px; }
      #partidModal .modal-header { background: #1a3a6b; color: white; border-radius: 4px 4px 0 0; }
      #partidModal .modal-title  { font-size: 1.4em; font-weight: bold; }
      #partidModal .modal-body   { background: #f4f6f9; padding: 20px; min-height: 400px; }
      .player-rating-big {
        background: linear-gradient(135deg, #1a6b3a, #27ae60);
        color: white; border-radius: 12px; padding: 20px 10px;
        text-align: center; box-shadow: 0 4px 12px rgba(0,0,0,.2);
      }
      .player-rating-big .rating-num { font-size: 64px; font-weight: 900; line-height: 1; }
      .player-rating-big .rating-lbl { font-size: 13px; letter-spacing: 2px; opacity: .85; margin-top: 4px; }
      .vbox-mini { background: white; border-radius: 8px; padding: 10px 14px;
                   margin-bottom: 8px; box-shadow: 0 1px 4px rgba(0,0,0,.1);
                   display: flex; align-items: center; gap: 10px; }
      .vbox-mini .vbox-icon { font-size: 22px; width: 36px; text-align: center; }
      .vbox-mini .vbox-val  { font-size: 20px; font-weight: bold; }
      .vbox-mini .vbox-lbl  { font-size: 11px; color: #666; }
      .ultims-partits { background:white; border-radius:8px; padding:10px 12px;
                        box-shadow:0 1px 4px rgba(0,0,0,.1); }
      .ultims-partits h6 { font-size:12px; font-weight:700; color:#555;
                           text-transform:uppercase; letter-spacing:1px; margin-bottom:8px; }
      .ultims-partits table { width:100%; border-collapse:collapse; font-size:12px; }
      .ultims-partits table thead th { color:#888; font-weight:600; padding:3px 4px;
                                       border-bottom:1px solid #eee; }
      .ultims-partits table tbody td { padding:4px 4px; border-bottom:1px solid #f5f5f5;
                                       vertical-align:middle; }
      .ultims-partits table tbody tr:last-child td { border-bottom:none; }
      .evt-icon { font-size:13px; margin-right:2px; }
      /* === SIMULADOR === */
      .sim-scoreboard { background:linear-gradient(135deg,#1a3a6b,#2980b9); color:white;
                        border-radius:16px; padding:24px 20px; text-align:center;
                        box-shadow:0 6px 20px rgba(0,0,0,.25); margin-bottom:16px; }
      .sim-scoreboard .sim-teams { font-size:17px; font-weight:700; letter-spacing:.5px;
                                    opacity:.9; margin-bottom:8px; }
      .sim-scoreboard .sim-score { font-size:72px; font-weight:900; line-height:1;
                                   letter-spacing:8px; }
      .sim-scoreboard .sim-status { font-size:12px; opacity:.75; margin-top:6px;
                                     letter-spacing:2px; text-transform:uppercase; }
      .sim-event { display:flex; align-items:center; gap:10px; padding:7px 12px;
                   border-radius:8px; margin-bottom:5px; font-size:13px; }
      .sim-event.gol-h  { background:#d5f5e3; border-left:4px solid #27ae60; }
      .sim-event.gol-a  { background:#fadbd8; border-left:4px solid #e74c3c; }
      .sim-event.groga  { background:#fef9e7; border-left:4px solid #f39c12; }
      .sim-event.vermella { background:#fadbd8; border-left:4px solid #c0392b; }
      .sim-event.canvi  { background:#eaf4fb; border-left:4px solid #2980b9; }
      .sim-event .sim-min { font-weight:900; color:#555; min-width:36px; }
      .sim-prob-bar { height:32px; border-radius:6px; display:flex;
                      overflow:hidden; margin:8px 0; font-weight:bold; font-size:13px; }
      .sim-prob-h { background:#27ae60; display:flex; align-items:center;
                    justify-content:center; color:white; transition:width .5s; }
      .sim-prob-d { background:#f39c12; display:flex; align-items:center;
                    justify-content:center; color:white; transition:width .5s; }
      .sim-prob-a { background:#e74c3c; display:flex; align-items:center;
                    justify-content:center; color:white; transition:width .5s; }
      .sim-lineup-col { background:white; border-radius:8px; padding:10px 14px;
                        box-shadow:0 1px 4px rgba(0,0,0,.1); height:100%; }
      .sim-lineup-col h6 { font-size:11px; font-weight:700; color:#555;
                            text-transform:uppercase; letter-spacing:1px; margin-bottom:8px; }
      .sim-lineup-player { font-size:12px; padding:3px 0; border-bottom:1px solid #f5f5f5;
                            display:flex; justify-content:space-between; }
      .sim-lineup-player:last-child { border-bottom:none; }
    ")),
              tags$script(HTML("
      Shiny.addCustomMessageHandler('evalJS', function(msg) { eval(msg); });
      Shiny.addCustomMessageHandler('registerHandlers', function(msg) {});
    "))),
    
    # ---- MODAL FITXA JUGADOR ----
    tags$div(id = "playerModal", class = "modal fade", tabindex = "-1",
             tags$div(class = "modal-dialog modal-lg",
                      tags$div(class = "modal-content",
                               tags$div(class = "modal-header",
                                        tags$button(type = "button", class = "close",
                                                    `data-dismiss` = "modal",
                                                    tags$span(HTML("&times;"))),
                                        tags$h4(class = "modal-title", uiOutput("modal_player_title"))
                               ),
                               tags$div(class = "modal-body",
                                        fluidRow(
                                          column(4,
                                                 div(class = "player-rating-big",
                                                     div(class = "rating-num", textOutput("modal_rating", inline = TRUE)),
                                                     div(class = "rating-lbl", "RATING DEL JUGADOR")),
                                                 br(),
                                                 plotlyOutput("modal_radar", height = "280px"),
                                                 br(),
                                                 uiOutput("modal_ultims_partits")
                                          ),
                                          column(8,
                                                 fluidRow(
                                                   column(6, uiOutput("modal_vbox1")),
                                                   column(6, uiOutput("modal_vbox2")),
                                                   column(6, uiOutput("modal_vbox3")),
                                                   column(6, uiOutput("modal_vbox4")),
                                                   column(6, uiOutput("modal_vbox5")),
                                                   column(6, uiOutput("modal_vbox6"))
                                                 ),
                                                 hr(),
                                                 plotlyOutput("modal_gols_evolucio",  height = "180px"),
                                                 br(),
                                                 plotlyOutput("modal_minuts_jornada", height = "180px"),
                                                 br(),
                                                 plotlyOutput("modal_impacte_plot",   height = "180px")
                                          )
                                        )
                               ),
                               tags$div(class = "modal-footer",
                                        tags$button(type = "button", class = "btn btn-default",
                                                    `data-dismiss` = "modal", "Tancar"))
                      ))),
    
    # ---- MODAL ACTA / PRÈVIA PARTIT ----
    tags$div(id = "partidModal", class = "modal fade", tabindex = "-1",
             tags$div(class = "modal-dialog modal-lg",
                      tags$div(class = "modal-content",
                               tags$div(class = "modal-header",
                                        tags$button(type = "button", class = "close",
                                                    `data-dismiss` = "modal",
                                                    tags$span(HTML("&times;"))),
                                        tags$h4(class = "modal-title", uiOutput("modal_partit_title"))
                               ),
                               tags$div(class = "modal-body", uiOutput("modal_partit_body")),
                               tags$div(class = "modal-footer",
                                        tags$button(type = "button", class = "btn btn-default",
                                                    `data-dismiss` = "modal", "Tancar"))
                      ))),
    
    tabItems(
      
      # ====================================================================
      # TAB 1: INICI
      # ====================================================================
      tabItem(tabName = "inici",
              uiOutput("inici_title"),
              fluidRow(
                valueBoxOutput("vbox_equips",   width = 3),
                valueBoxOutput("vbox_jugadors", width = 3),
                valueBoxOutput("vbox_jornades", width = 3),
                valueBoxOutput("vbox_gols",     width = 3)
              ),
              fluidRow(
                valueBoxOutput("vbox_gols_local",       width = 3),
                valueBoxOutput("vbox_gols_visitant",    width = 3),
                valueBoxOutput("vbox_partits_local_win",width = 3),
                valueBoxOutput("vbox_partits_empat",    width = 3)
              ),
              fluidRow(
                box(width = 5, title = "🏆 Classificació Actual", solidHeader = TRUE, status = "primary",
                    DTOutput("inici_classificacio")),
                box(width = 4, title = "🥇 Top Golejadors", solidHeader = TRUE, status = "success",
                    DTOutput("inici_golejadors")),
                box(width = 3, title = "📅 Última Jornada", solidHeader = TRUE, status = "warning",
                    uiOutput("inici_ultima_jornada"))
              )
      ),
      
      # ====================================================================
      # TAB 2: PARTITS
      # ====================================================================
      tabItem(tabName = "partits",
              h2("📅 Partits"),
              fluidRow(
                column(3,
                       box(width = 12,
                           uiOutput("jornada_select_ui"))
                ),
                column(9,
                       box(width = 12, title = "Partits de la jornada",
                           solidHeader = TRUE, status = "primary",
                           p(style = "color:#888; font-size:12px; margin-bottom:8px;",
                             "Clica un partit per obrir l'acta o la prèvia"),
                           uiOutput("partits_cards"))
                )
              )
      ),
      
      # ====================================================================
      # TAB 3: CLASSIFICACIÓ
      # ====================================================================
      tabItem(tabName = "classificacio",
              h2("🏆 Classificació"),
              fluidRow(box(width = 12, title = "Classificació Actual", solidHeader = TRUE,
                           DTOutput("taula_classificacio"))),
              fluidRow(box(width = 12, title = "Evolució de Posicions", solidHeader = TRUE,
                           plotlyOutput("grafic_evolucio_posicions", height = 500))),
              fluidRow(box(width = 12, title = "Heatmap de Resultats", solidHeader = TRUE,
                           plotlyOutput("stats_heatmap", height = 500))),
              fluidRow(box(width = 12, title = "Matriu de Confrontacions Directes",
                           solidHeader = TRUE, status = "danger",
                           plotlyOutput("classif_head2head", height = 550)))
      ),
      
      # ====================================================================
      # TAB 4: EQUIPS
      # ====================================================================
      tabItem(tabName = "equips",
              h2("👥 Estadístiques per Equip"),
              fluidRow(box(width = 4,
                           uiOutput("equip_select_ui"))),
              tabBox(width = 12, id = "equip_subtab", title = NULL,
                     tabPanel("📋 Fitxa",
                              fluidRow(
                                column(4,
                                       div(style = "background:#1a6b8a; color:white; border-radius:8px;
                                           padding:20px 10px; text-align:center; margin-bottom:12px;
                                           box-shadow:0 4px 12px rgba(0,0,0,.2);",
                                           div(style = "font-size:72px; font-weight:900; line-height:1;",
                                               textOutput("equip_rating_display", inline = TRUE)),
                                           div(style = "font-size:13px; letter-spacing:2px; opacity:.85; margin-top:6px;",
                                               "RATING DE L'EQUIP")),
                                       uiOutput("equip_partits_recap")),
                                column(8,
                                       box(width = 12, title = "⬡ Diagrama de Radar",
                                           solidHeader = TRUE, status = "primary",
                                           plotlyOutput("equip_radar", height = 350)))
                              ),
                              fluidRow(
                                valueBoxOutput("equip_posicio",    width = 3),
                                valueBoxOutput("equip_punts",      width = 3),
                                valueBoxOutput("equip_gols_favor", width = 3),
                                valueBoxOutput("equip_victories",  width = 3)
                              ),
                              fluidRow(
                                valueBoxOutput("equip_vic_casa",    width = 3),
                                valueBoxOutput("equip_vic_fora",    width = 3),
                                valueBoxOutput("equip_gols_contra", width = 3),
                                valueBoxOutput("equip_targetes",    width = 3)
                              ),
                              fluidRow(box(width = 12, solidHeader = TRUE, status = "info",
                                           title = "⚡ Tilt — Momentum Recent",
                                           uiOutput("equip_tilt_box"))),
                              fluidRow(
                                box(width = 6, title = "Evolució de Punts", solidHeader = TRUE, status = "primary",
                                    plotlyOutput("equip_punts_evolucio")),
                                box(width = 6, title = "Gols per Jornada", solidHeader = TRUE, status = "success",
                                    plotlyOutput("equip_gols_jornada"))
                              ),
                              fluidRow(
                                box(width = 6, title = "Rendiment Casa vs Fora", solidHeader = TRUE, status = "warning",
                                    plotlyOutput("equip_casa_fora")),
                                box(width = 6, title = "Gols Marcats vs Rebuts per Període",
                                    solidHeader = TRUE, status = "info",
                                    plotlyOutput("equip_gols_periode"))
                              ),
                              fluidRow(
                                box(width = 6, title = "Efectivitat per Parts", solidHeader = TRUE, status = "danger",
                                    plotlyOutput("equip_efectivitat_parts")),
                                box(width = 6, title = "Càrrega de Treball dels Jugadors",
                                    solidHeader = TRUE, status = "primary",
                                    plotlyOutput("equip_carrega_treball"))
                              ),
                              fluidRow(
                                box(width = 6, title = "🏆 Rendiment vs Nivell de Rival",
                                    solidHeader = TRUE, status = "info",
                                    plotlyOutput("equip_vs_nivell", height = "320px")),
                                box(width = 6, title = "⭐ Dependència de Golejador",
                                    solidHeader = TRUE, status = "warning",
                                    plotlyOutput("equip_dependencia_gols", height = "320px"))
                              )
                     ),
                     tabPanel("📅 Calendari",
                              br(), uiOutput("equip_calendari_ui")),
                     tabPanel("👥 Plantilla",
                              br(),
                              fluidRow(box(width = 12,
                                           title = "Jugadors de l'Equip — clica un jugador per veure la fitxa",
                                           solidHeader = TRUE, status = "primary",
                                           DTOutput("equip_jugadors")))),
                     tabPanel("⚖️ Comparador",
                              br(),
                              fluidRow(
                                box(width = 4, uiOutput("comp_equip1_ui")),
                                box(width = 4, uiOutput("comp_equip2_ui"))
                              ),
                              fluidRow(box(width = 12, solidHeader = TRUE, status = "primary",
                                           title = "Resum comparatiu", uiOutput("comp_header"))),
                              fluidRow(
                                box(width = 4, title = "📊 Classificació", solidHeader = TRUE, status = "primary",
                                    uiOutput("comp_classif")),
                                box(width = 4, title = "🥅 Gols", solidHeader = TRUE, status = "success",
                                    uiOutput("comp_gols")),
                                box(width = 4, title = "🟨 Targetes & Fair-Play", solidHeader = TRUE, status = "warning",
                                    uiOutput("comp_targetes"))
                              ),
                              fluidRow(box(width = 12, solidHeader = TRUE, status = "info",
                                           title = "⚡ Tilt — Momentum Recent", uiOutput("comp_tilt"))),
                              fluidRow(
                                box(width = 6, title = "⬡ Radars comparats", solidHeader = TRUE, status = "info",
                                    plotlyOutput("comp_radar", height = "340px")),
                                box(width = 6, title = "📈 Evolució de Punts", solidHeader = TRUE, status = "primary",
                                    plotlyOutput("comp_punts_evolucio", height = "340px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "🏠 Casa vs Fora", solidHeader = TRUE, status = "warning",
                                    plotlyOutput("comp_casa_fora", height = "300px")),
                                box(width = 6, title = "⏱ Distribució de Gols per Minut",
                                    solidHeader = TRUE, status = "danger",
                                    plotlyOutput("comp_gols_minut", height = "300px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "⚔️ Confrontació Directa", solidHeader = TRUE, status = "danger",
                                    uiOutput("comp_h2h")),
                                box(width = 6, title = "⭐ Top Golejadors", solidHeader = TRUE, status = "success",
                                    uiOutput("comp_golejadors"))
                              )
                     )
              )
      ),
      
      # ====================================================================
      # TAB 5: JUGADORS
      # ====================================================================
      tabItem(tabName = "jugadors",
              h2("🔍 Jugadors"),
              tabBox(width = 12, id = "jugadors_subtab", title = NULL,
                     tabPanel("🔍 Buscador",
                              p("Clica qualsevol jugador per veure la seva fitxa completa."),
                              fluidRow(box(width = 12, DTOutput("taula_jugadors")))),
                     tabPanel("⚖️ Comparador",
                              br(),
                              fluidRow(
                                box(width = 4, uiOutput("comp_jug1_ui")),
                                box(width = 4, uiOutput("comp_jug2_ui"))
                              ),
                              fluidRow(box(width = 12, solidHeader = TRUE, status = "primary",
                                           title = "Resum comparatiu", uiOutput("compjug_header"))),
                              fluidRow(
                                box(width = 6, title = "⬡ Radars comparats", solidHeader = TRUE, status = "info",
                                    plotlyOutput("compjug_radar", height = "340px")),
                                box(width = 6, title = "📊 Estadístiques detallades", solidHeader = TRUE, status = "primary",
                                    uiOutput("compjug_stats_taula"))
                              ),
                              fluidRow(
                                box(width = 6, title = "⚽ Gols Acumulats", solidHeader = TRUE, status = "success",
                                    plotlyOutput("compjug_gols_evolucio", height = "280px")),
                                box(width = 6, title = "⏱ Minuts per Jornada", solidHeader = TRUE, status = "primary",
                                    plotlyOutput("compjug_minuts", height = "280px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "🎯 Timing de Gols", solidHeader = TRUE, status = "warning",
                                    plotlyOutput("compjug_timing", height = "280px")),
                                box(width = 6, title = "🏟️ Rendiment de l'Equip amb/sense cada Jugador",
                                    solidHeader = TRUE, status = "danger",
                                    plotlyOutput("compjug_impacte", height = "280px"))
                              )
                     )
              )
      ),
      
      # ====================================================================
      # TAB 6: ESTADÍSTIQUES
      # ====================================================================
      tabItem(tabName = "stats",
              h2("📊 Estadístiques de la Competició"),
              fluidRow(
                valueBoxOutput("stats_total_gols",    width = 3),
                valueBoxOutput("stats_avg_gols",      width = 3),
                valueBoxOutput("stats_max_gols",      width = 3),
                valueBoxOutput("stats_total_targetes",width = 3)
              ),
              tabBox(width = 12, id = "stats_subtab", title = NULL,
                     tabPanel("🏟️ Lliga",
                              br(),
                              fluidRow(
                                box(width = 6, title = "Distribució de Gols per Minut",
                                    solidHeader = TRUE, status = "info",
                                    plotlyOutput("stats_gols_minut", height = "300px")),
                                box(width = 6, title = "Gols per Jornada",
                                    solidHeader = TRUE, status = "success",
                                    plotlyOutput("stats_gols_jornada", height = "300px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "Targetes per Jornada",
                                    solidHeader = TRUE, status = "warning",
                                    plotlyOutput("stats_targetes_jornada", height = "300px")),
                                box(width = 6, title = "Resultats més Comuns",
                                    solidHeader = TRUE, status = "primary",
                                    plotlyOutput("stats_resultats_comuns", height = "300px"))
                              )
                     ),
                     tabPanel("🛡️ Equips",
                              br(),
                              fluidRow(
                                box(width = 6, title = "Gols a Favor vs en Contra per Equip",
                                    solidHeader = TRUE, status = "success",
                                    plotlyOutput("stats_gols_equip", height = "350px")),
                                box(width = 6, title = "Equips: Gols Casa vs Fora",
                                    solidHeader = TRUE, status = "info",
                                    plotlyOutput("stats_casa_fora", height = "350px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "Targetes per Equip",
                                    solidHeader = TRUE, status = "warning",
                                    plotlyOutput("stats_targetes", height = "320px")),
                                box(width = 6, title = "🎯 Targetes vs Punts",
                                    solidHeader = TRUE, status = "danger",
                                    plotlyOutput("stats_targetes_punts", height = "320px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "🥇 Ranking Millors Atacs",
                                    solidHeader = TRUE, status = "success",
                                    plotlyOutput("stats_ranking_atac", height = "320px")),
                                box(width = 6, title = "🛡️ Ranking Millors Defenses",
                                    solidHeader = TRUE, status = "primary",
                                    plotlyOutput("stats_ranking_defensa", height = "320px"))
                              ),
                              fluidRow(
                                box(width = 12, solidHeader = TRUE, status = "info",
                                    title = "⚡ Rating vs Tilt — Posicionament d'Equips",
                                    p(style = "color:#888; font-size:12px; margin-bottom:4px;",
                                      "Eix X: Tilt (momentum recent). Eix Y: Rating global de l'equip."),
                                    plotlyOutput("stats_rating_tilt", height = "460px"))
                              ),
                              fluidRow(
                                box(width = 12, solidHeader = TRUE, status = "primary",
                                    title = "⚔️ Latents d'Atac i Defensa",
                                    p(style = "color:#888; font-size:12px; margin-bottom:4px;",
                                      "Estimats amb model Poisson jeràrquic. Dreta = millor atac. Dalt = millor defensa."),
                                    plotlyOutput("stats_latents_atac_defensa", height = "480px"))
                              )
                     ),
                     tabPanel("👤 Jugadors",
                              br(),
                              fluidRow(
                                box(width = 6, title = "Evolució Top 10 Golejadors",
                                    solidHeader = TRUE, status = "success",
                                    plotlyOutput("stats_top_golejadors", height = "340px")),
                                box(width = 6, title = "Top 10 Jugadors amb més Targetes",
                                    solidHeader = TRUE, status = "warning",
                                    plotlyOutput("stats_top_targetes", height = "340px"))
                              ),
                              fluidRow(
                                box(width = 6, title = "⚽ Gols vs Minuts Jugats (min. 2 gols)",
                                    solidHeader = TRUE, status = "info",
                                    p(style="font-size:11px;color:#888;margin-bottom:4px;",
                                      "Cada punt és un jugador. La línia mostra la tendència esperada."),
                                    plotlyOutput("stats_gols_minuts_reg", height = "320px")),
                                box(width = 6, title = "🏃 Minuts per Gol — Eficiència Golejadora",
                                    solidHeader = TRUE, status = "success",
                                    p(style="font-size:11px;color:#888;margin-bottom:4px;",
                                      "Jugadors amb mínim 2 gols, ordenats per minuts necessaris per marcar."),
                                    plotlyOutput("stats_minuts_per_gol", height = "320px"))
                              )
                     )
              )
      ),
      
      # ====================================================================
      # TAB 7: SIMULADOR
      # ====================================================================
      tabItem(tabName = "simulador",
              h2("🔬 Simulador de Partits"),
              p(style="color:#666; font-size:13px; margin-bottom:16px;",
                "Simulació basada en un procés puntual Hawkes amb latents d'atac/defensa i player ratings. ",
                "Selecciona dos equips i el mode de simulació."),
              fluidRow(
                column(12,
                       box(width=12, title="⚙️ Configuració", solidHeader=TRUE, status="primary",
                           fluidRow(
                             column(3, uiOutput("sim_equip_home_ui")),
                             column(3, uiOutput("sim_equip_away_ui")),
                             column(3,
                                    sliderInput("sim_n_sims", "Simulacions Monte Carlo:",
                                                min=50, max=2000, value=500, step=50)
                             ),
                             column(3,
                                    br(),
                                    actionButton("sim_run_single", "⚡ Simular 1 Partit",
                                                 class="btn btn-warning btn-block",
                                                 style="font-weight:bold; font-size:14px; margin-bottom:8px;"),
                                    actionButton("sim_run", "🔄 Monte Carlo",
                                                 class="btn btn-success btn-block",
                                                 style="font-weight:bold; font-size:14px;")
                             )
                           ),
                           uiOutput("sim_running_msg")
                       )
                )
              ),
              # Resultats dinàmics: es mostren només quan hi ha dades
              uiOutput("sim_results_panel")
      )
    )
  )
)


# ============================================================================
# FUNCIONS DEL SIMULADOR HAWKES
# ============================================================================

sim_params <- list(
  mu_g              = -4.55,           # era -4.8 → +0.25 (≈+28% intensitat base gols)
  beta_pr           = 0.25,
  beta_score_trailing = 0.15,
  beta_score_leading  = -0.2,
  beta_fatigue      = -0.3,
  beta_home         = 0.12,
  mu_yellow         = -4.2,           # era -4.5 → +0.3 (més targetes grogues)
  beta_yellow_score = 0.08,
  beta_yellow_fatigue = 0.18,         # era 0.15 → lleugerament més al final
  mu_red            = -7.0,
  beta_red_yellows  = 0.4,
  alpha_gg          = 0.08,
  alpha_gy          = 0.05,
  alpha_yy          = 0.05,
  kappa             = 0.3,
  dt                = 1
)

agg_player_rating_fn <- function(onfield_ids, pr_df) {
  if (length(onfield_ids) == 0) return(1.0)
  r <- pr_df %>% filter(as.character(player) %in% as.character(onfield_ids)) %>% pull(rating_global)
  if (length(r) == 0 || all(is.na(r))) return(1.0)
  v <- mean(r, na.rm = TRUE)
  if (is.na(v) || is.nan(v)) return(1.0)
  return(v)
}

calc_fatigue_fn <- function(minutes_vec, current_minute) {
  if (current_minute < 60) return(0)
  avg_m <- mean(minutes_vec, na.rm = TRUE)
  if (is.na(avg_m)) return(0)
  fatigue <- (avg_m / 90) * ((current_minute - 60) / 30)
  return(pmin(fatigue, 1.5))
}

calc_hawkes_fn <- function(current_time, past_events, alpha, kappa) {
  if (length(past_events) == 0) return(0)
  recent <- past_events[past_events > (current_time - 20)]
  if (length(recent) == 0) return(0)
  v <- sum(alpha * exp(-kappa * (current_time - recent)))
  if (is.na(v) || is.nan(v)) return(0)
  return(v)
}

safe_p <- function(x) pmax(0, pmin(1, ifelse(is.na(x)|is.nan(x), 0, x)))

simulate_one_match <- function(home_team, away_team, lat_df, pr_df,
                               home_lineup, away_lineup, params,
                               verbose = FALSE) {
  lat_h <- lat_df %>% filter(team == home_team)
  lat_a <- lat_df %>% filter(team == away_team)
  if (nrow(lat_h) == 0 | nrow(lat_a) == 0) return(NULL)
  
  A_h <- lat_h$attack[1];  D_h <- lat_h$defense[1]
  A_a <- lat_a$attack[1];  D_a <- lat_a$defense[1]
  
  st <- list(
    score_h = 0L, score_a = 0L,
    on_h = home_lineup, on_a = away_lineup,
    min_h = rep(0, length(home_lineup)),
    min_a = rep(0, length(away_lineup)),
    yel_h = rep(0L, length(home_lineup)),
    yel_a = rep(0L, length(away_lineup)),
    ev_gh = numeric(), ev_ga = numeric(),
    ev_yh = numeric(), ev_ya = numeric(),
    ev_rh = numeric(), ev_ra = numeric()
  )
  
  all_h <- pr_df %>% filter(team == home_team, total_minutes >= 90) %>%
    arrange(desc(rating_global)) %>% pull(player) %>% as.character()
  all_a <- pr_df %>% filter(team == away_team, total_minutes >= 90) %>%
    arrange(desc(rating_global)) %>% pull(player) %>% as.character()
  bench_h <- setdiff(all_h, st$on_h); bench_a <- setdiff(all_a, st$on_a)
  subs_h <- 0L; subs_a <- 0L; max_subs <- 3L
  
  event_log <- list()
  
  for (t in seq(1, 90, by = params$dt)) {
    st$min_h <- st$min_h + params$dt
    st$min_a <- st$min_a + params$dt
    
    # --- substitucions ---
    if (t %in% c(60, 70, 80)) {
      # local
      if (subs_h < max_subs && length(bench_h) > 0 && length(st$on_h) == 11) {
        rat_h <- sapply(st$on_h, function(p) { r <- pr_df %>% filter(as.character(player)==p) %>% pull(rating_global); if(length(r)==0) 1.0 else r[1] })
        sub_pri <- st$min_h/90 - rat_h
        diff_h <- st$score_a - st$score_h
        do_sub <- (diff_h > 0 && t >= 60 && runif(1) < 0.78) ||
          (diff_h < 0 && t >= 70 && runif(1) < 0.58) ||
          (max(st$min_h) >= 80 && t >= 75 && runif(1) < 0.68)
        if (do_sub) {
          oi <- which.max(sub_pri); op <- st$on_h[oi]; ip <- bench_h[1]
          st$on_h[oi] <- ip; st$min_h[oi] <- 0
          bench_h <- c(setdiff(bench_h, ip), op)
          subs_h <- subs_h + 1L
          event_log <- append(event_log, list(list(minute=t, type="canvi_h", team=home_team, out=op, inn=ip)))
        }
      }
      # visitant
      if (subs_a < max_subs && length(bench_a) > 0 && length(st$on_a) == 11) {
        rat_a <- sapply(st$on_a, function(p) { r <- pr_df %>% filter(as.character(player)==p) %>% pull(rating_global); if(length(r)==0) 1.0 else r[1] })
        sub_pri_a <- st$min_a/90 - rat_a
        diff_a <- st$score_h - st$score_a
        do_sub_a <- (diff_a > 0 && t >= 60 && runif(1) < 0.78) ||
          (diff_a < 0 && t >= 70 && runif(1) < 0.58) ||
          (max(st$min_a) >= 80 && t >= 75 && runif(1) < 0.68)
        if (do_sub_a) {
          oi <- which.max(sub_pri_a); op <- st$on_a[oi]; ip <- bench_a[1]
          st$on_a[oi] <- ip; st$min_a[oi] <- 0
          bench_a <- c(setdiff(bench_a, ip), op)
          subs_a <- subs_a + 1L
          event_log <- append(event_log, list(list(minute=t, type="canvi_a", team=away_team, out=op, inn=ip)))
        }
      }
    }
    
    # --- intensitats ---
    PR_h <- agg_player_rating_fn(st$on_h, pr_df)
    PR_a <- agg_player_rating_fn(st$on_a, pr_df)
    sd_h <- st$score_a - st$score_h; sd_a <- st$score_h - st$score_a
    fat_h <- calc_fatigue_fn(st$min_h, t); fat_a <- calc_fatigue_fn(st$min_a, t)
    
    eta_gh <- params$mu_g + A_h - D_a + params$beta_pr*(PR_h-1) + params$beta_home +
      params$beta_score_trailing*max(0,sd_h) + params$beta_score_leading*max(0,-sd_h) -
      params$beta_fatigue*fat_h
    eta_ga <- params$mu_g + A_a - D_h + params$beta_pr*(PR_a-1) +
      params$beta_score_trailing*max(0,sd_a) + params$beta_score_leading*max(0,-sd_a) -
      params$beta_fatigue*fat_a
    
    lam_gh <- exp(eta_gh) + calc_hawkes_fn(t, st$ev_gh, params$alpha_gg, params$kappa)
    lam_ga <- exp(eta_ga) + calc_hawkes_fn(t, st$ev_ga, params$alpha_gg, params$kappa)
    
    eta_yh <- params$mu_yellow + params$beta_yellow_score*max(0,sd_h) + params$beta_yellow_fatigue*fat_h
    eta_ya <- params$mu_yellow + params$beta_yellow_score*max(0,sd_a) + params$beta_yellow_fatigue*fat_a
    lam_yh <- exp(eta_yh) + calc_hawkes_fn(t, st$ev_yh, params$alpha_yy, params$kappa)
    lam_ya <- exp(eta_ya) + calc_hawkes_fn(t, st$ev_ya, params$alpha_yy, params$kappa)
    
    tot_yh <- sum(st$yel_h > 0); tot_ya <- sum(st$yel_a > 0)
    lam_rh <- exp(params$mu_red) * (1 + params$beta_red_yellows * tot_yh)
    lam_ra <- exp(params$mu_red) * (1 + params$beta_red_yellows * tot_ya)
    
    p_gh <- safe_p(1 - exp(-lam_gh*params$dt))
    p_ga <- safe_p(1 - exp(-lam_ga*params$dt))
    p_yh <- safe_p(1 - exp(-lam_yh*params$dt))
    p_ya <- safe_p(1 - exp(-lam_ya*params$dt))
    p_rh <- safe_p(1 - exp(-lam_rh*params$dt))
    p_ra <- safe_p(1 - exp(-lam_ra*params$dt))
    
    # --- gol local ---
    if (runif(1) < p_gh && length(st$on_h) > 0) {
      w <- sapply(st$on_h, function(p) { r <- pr_df%>%filter(as.character(player)==p)%>%pull(rating_global); if(length(r)==0||is.na(r[1])) 1 else r[1] })
      w <- pmax(w, 0.01)
      si <- sample(length(st$on_h), 1, prob=w)
      sc <- st$on_h[si]
      st$score_h <- st$score_h + 1L
      st$ev_gh <- c(st$ev_gh, t)
      event_log <- append(event_log, list(list(minute=t, type="gol_h", team=home_team, player=sc, score=paste0(st$score_h,"-",st$score_a))))
    }
    # --- gol visitant ---
    if (runif(1) < p_ga && length(st$on_a) > 0) {
      w <- sapply(st$on_a, function(p) { r <- pr_df%>%filter(as.character(player)==p)%>%pull(rating_global); if(length(r)==0||is.na(r[1])) 1 else r[1] })
      w <- pmax(w, 0.01)
      si <- sample(length(st$on_a), 1, prob=w)
      sc <- st$on_a[si]
      st$score_a <- st$score_a + 1L
      st$ev_ga <- c(st$ev_ga, t)
      event_log <- append(event_log, list(list(minute=t, type="gol_a", team=away_team, player=sc, score=paste0(st$score_h,"-",st$score_a))))
    }
    # --- groga local ---
    if (runif(1) < p_yh && length(st$on_h) > 0) {
      pi <- sample(length(st$on_h), 1); pn <- st$on_h[pi]
      st$yel_h[pi] <- st$yel_h[pi] + 1L; st$ev_yh <- c(st$ev_yh, t)
      if (st$yel_h[pi] >= 2L) {
        st$on_h <- st$on_h[-pi]; st$min_h <- st$min_h[-pi]; st$yel_h <- st$yel_h[-pi]
        st$ev_rh <- c(st$ev_rh, t)
        event_log <- append(event_log, list(list(minute=t, type="vermella_h", team=home_team, player=pn)))
      } else {
        event_log <- append(event_log, list(list(minute=t, type="groga_h", team=home_team, player=pn)))
      }
    }
    # --- groga visitant ---
    if (runif(1) < p_ya && length(st$on_a) > 0) {
      pi <- sample(length(st$on_a), 1); pn <- st$on_a[pi]
      st$yel_a[pi] <- st$yel_a[pi] + 1L; st$ev_ya <- c(st$ev_ya, t)
      if (st$yel_a[pi] >= 2L) {
        st$on_a <- st$on_a[-pi]; st$min_a <- st$min_a[-pi]; st$yel_a <- st$yel_a[-pi]
        st$ev_ra <- c(st$ev_ra, t)
        event_log <- append(event_log, list(list(minute=t, type="vermella_a", team=away_team, player=pn)))
      } else {
        event_log <- append(event_log, list(list(minute=t, type="groga_a", team=away_team, player=pn)))
      }
    }
  }
  
  list(
    score_h = st$score_h, score_a = st$score_a,
    result = case_when(st$score_h > st$score_a ~ "H", st$score_h < st$score_a ~ "A", TRUE ~ "D"),
    n_goals_h = length(st$ev_gh), n_goals_a = length(st$ev_ga),
    n_yellows_h = length(st$ev_yh), n_yellows_a = length(st$ev_ya),
    n_reds_h = length(st$ev_rh), n_reds_a = length(st$ev_ra),
    events = event_log
  )
}

get_lineup_fn <- function(team_name, pr_df, n = 11) {
  pr_df %>% filter(team == team_name, total_minutes >= 90) %>%
    arrange(desc(total_minutes)) %>% head(n) %>% pull(player) %>% as.character()
}

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # ==========================================================================
  # SELECTOR DE GRUP DINÀMIC (depèn de la categoria seleccionada)
  # ==========================================================================
  output$grup_select_ui <- renderUI({
    req(input$cat_select)
    grups_cat <- sort(unique(
      all_matches_events_all$grup[all_matches_events_all$categoria == input$cat_select]
    ))
    selectInput("grup_select", label = NULL,
                choices  = setNames(grups_cat, paste0("Grup ", grups_cat)),
                selected = grups_cat[1],
                width    = "110px")
  })
  
  # ==========================================================================
  # DADES REACTIVES FILTRADES PER CATEGORIA + GRUP
  # ==========================================================================
  cat_sel <- reactive({ req(input$cat_select); input$cat_select })
  g       <- reactive({ req(input$grup_select); as.numeric(input$grup_select) })
  
  all_matches_events <- reactive({ all_matches_events_all %>% filter(categoria == cat_sel(), grup == g()) })
  all_matches_lineups<- reactive({ all_matches_lineups_all %>% filter(categoria == cat_sel(), grup == g()) })
  all_matches_info   <- reactive({ all_matches_info_all   %>% filter(categoria == cat_sel(), grup == g()) })
  player_match_stats <- reactive({ player_match_stats_all %>% filter(categoria == cat_sel(), grup == g()) })
  player_stats       <- reactive({ player_stats_all       %>% filter(categoria == cat_sel(), grup == g()) })
  standings_by_round <- reactive({ standings_by_round_all %>% filter(categoria == cat_sel(), grup == g()) })
  matches            <- reactive({ matches_all            %>% filter(categoria == cat_sel(), grup == g()) })
  
  team_match_stats <- reactive({
    team_match_stats_all %>%
      filter(categoria == cat_sel(), grup == g()) %>%
      mutate(team_points = case_when(
        goals_for > goals_against ~ 3,
        goals_for == goals_against ~ 1,
        TRUE ~ 0
      ))
  })
  
  # Càlculs derivats reactius
  player_impact <- reactive({
    calculate_player_impact(player_match_stats(), team_match_stats())
  })
  player_ratings <- reactive({
    calculate_player_ratings(player_match_stats(), player_impact())
  })
  team_ratings <- reactive({
    estimate_team_latents(team_match_stats())
  })
  team_radar_data <- reactive({
    calculate_team_radar(team_match_stats(), all_matches_events())
  })
  player_radar_data <- reactive({
    calculate_player_radar(player_match_stats(), player_impact(), player_stats())
  })
  tilt_data <- reactive({
    calculate_tilt(team_match_stats(), team_ratings())
  })
  tilt_min <- reactive({ round(min(tilt_data()$tilt, na.rm = TRUE), 2) })
  tilt_max <- reactive({ round(max(tilt_data()$tilt, na.rm = TRUE), 2) })
  
  current_standings <- reactive({
    standings_by_round() %>% filter(jornada == max(jornada)) %>% arrange(position)
  })
  all_teams   <- reactive({ sort(unique(team_match_stats()$team)) })
  all_players <- reactive({ sort(unique(player_stats()$player)) })
  
  # ==========================================================================
  # UI DINÀMICS (selectors que depenen del grup)
  # ==========================================================================
  output$inici_title <- renderUI({
    h2(paste0("⚽ ", categoria_label(cat_sel()), " 2025-2026 · Grup ", g()))
  })
  output$jornada_select_ui <- renderUI({
    req(nrow(matches()) > 0)
    selectInput("jornada_select", "Jornada:",
                choices = sort(unique(matches()$jornada)),
                selected = max(matches()$jornada))
  })
  output$equip_select_ui <- renderUI({
    selectInput("equip_select", "Selecciona Equip:", choices = all_teams())
  })
  output$comp_equip1_ui <- renderUI({
    teams <- all_teams()
    selectInput("comp_equip1", "Equip A:", choices = teams, selected = teams[1])
  })
  output$comp_equip2_ui <- renderUI({
    teams <- all_teams()
    selectInput("comp_equip2", "Equip B:", choices = teams, selected = teams[min(2, length(teams))])
  })
  output$comp_jug1_ui <- renderUI({
    players <- all_players()
    selectInput("comp_jug1", "Jugador A:", choices = players, selected = players[1])
  })
  output$comp_jug2_ui <- renderUI({
    players <- all_players()
    selectInput("comp_jug2", "Jugador B:", choices = players, selected = players[min(2, length(players))])
  })
  
  # Reiniciar selecció de partit quan canvia el grup o la jornada
  selected_partit <- reactiveVal(NULL)
  observeEvent(input$grup_select, { selected_partit(NULL) })
  observeEvent(input$jornada_select, { selected_partit(NULL) })
  
  # ==========================================================================
  # MODAL JUGADOR
  # ==========================================================================
  selected_player_modal <- reactiveVal(NULL)
  
  open_player_modal <- function(player_name) {
    selected_player_modal(player_name)
    session$sendCustomMessage("openPlayerModal", list())
  }
  observeEvent(input$selected_player_modal, {
    open_player_modal(input$selected_player_modal)
  })
  session$onFlushed(function() {
    session$sendCustomMessage("evalJS", "
      Shiny.addCustomMessageHandler('openPlayerModal', function(msg) {
        $('#playerModal').modal('show');
      });
      Shiny.addCustomMessageHandler('openPartitModal', function(msg) {
        $('#partidModal').modal('show');
      });
    ")
  }, once = TRUE)
  observe({ session$sendCustomMessage("registerHandlers", list()) })
  
  modal_player_data <- reactive({
    req(selected_player_modal())
    player_stats() %>% filter(player == selected_player_modal())
  })
  modal_match_data <- reactive({
    req(selected_player_modal())
    player_match_stats() %>% filter(player == selected_player_modal()) %>% arrange(jornada)
  })
  modal_impact_data <- reactive({
    req(selected_player_modal())
    player_impact() %>% filter(player == selected_player_modal())
  })
  modal_radar_row <- reactive({
    req(selected_player_modal())
    player_radar_data() %>% filter(player == selected_player_modal())
  })
  
  output$modal_player_title <- renderUI({
    req(selected_player_modal())
    pd <- modal_player_data()
    if (nrow(pd) == 0) return(NULL)
    tags$span(icon("user"), " ", selected_player_modal(),
              tags$small(style = "margin-left:12px; font-weight:normal; font-size:.8em;", pd$team))
  })
  output$modal_rating <- renderText({
    req(selected_player_modal())
    r <- player_ratings() %>% filter(player == selected_player_modal()) %>% pull(rating_global)
    if (length(r) == 0) "N/A" else as.character(r)
  })
  output$modal_radar <- renderPlotly({
    row <- modal_radar_row()
    if (nrow(row) == 0) return(plotly_empty())
    cats <- c("Gols/90","Impacte","Titularitat","Minuts","Fair-Play")
    vals <- c(row$radar_gol, row$radar_impacte, row$radar_titularitat,
              row$radar_minuts, row$radar_fairplay)
    plot_ly(type = "scatterpolar",
            r = c(vals, vals[1]), theta = c(cats, cats[1]),
            fill = "toself", fillcolor = "rgba(39,174,96,0.25)",
            line = list(color = "rgba(26,107,58,1)", width = 2),
            hovertemplate = "<b>%{theta}</b><br>%{r:.1f}<extra></extra>") %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0,100),
                                            tickvals = c(0,25,50,75,100), gridcolor = "lightgrey"),
                          angularaxis = list(tickfont = list(size = 11))),
             showlegend = FALSE, margin = list(t=10,b=10,l=30,r=30))
  })
  
  make_vbox_mini <- function(icon_name, value, label, color = "#1a6b3a") {
    div(class = "vbox-mini",
        div(class = "vbox-icon", style = paste0("color:", color), icon(icon_name)),
        div(div(class = "vbox-val", value), div(class = "vbox-lbl", label)))
  }
  output$modal_vbox1 <- renderUI({ pd <- modal_player_data(); req(nrow(pd)>0); make_vbox_mini("users", pd$team, "Equip", "#2980b9") })
  output$modal_vbox2 <- renderUI({ pd <- modal_player_data(); req(nrow(pd)>0); make_vbox_mini("futbol", pd$goals, "Gols", "#27ae60") })
  output$modal_vbox3 <- renderUI({ pd <- modal_player_data(); req(nrow(pd)>0); make_vbox_mini("clock", pd$total_minutes, "Minuts", "#2980b9") })
  output$modal_vbox4 <- renderUI({ pd <- modal_player_data(); req(nrow(pd)>0); make_vbox_mini("calendar", pd$matches_played, "Partits", "#e67e22") })
  output$modal_vbox5 <- renderUI({ pd <- modal_player_data(); req(nrow(pd)>0); make_vbox_mini("star", pd$starts, "Titularitats", "#8e44ad") })
  output$modal_vbox6 <- renderUI({
    req(selected_player_modal())
    imp_row <- modal_impact_data()
    if (nrow(imp_row) == 0 || is.na(imp_row$impacte)) return(make_vbox_mini("bolt", "N/A", "Impacte", "#7f8c8d"))
    val <- round(imp_row$impacte, 2)
    col <- if (val > 0) "#27ae60" else if (val < 0) "#e74c3c" else "#7f8c8d"
    make_vbox_mini("bolt", paste0(val, " pts"), "Impacte", col)
  })
  
  output$modal_gols_evolucio <- renderPlotly({
    data <- modal_match_data() %>% mutate(gols_acum = cumsum(goals), player_chr = as.character(selected_player_modal()))
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data, aes(x=jornada, y=gols_acum, text=paste("Jornada:",jornada,"<br>Gols acum.:",gols_acum))) +
      geom_line(color="darkgreen",linewidth=1.2) +
      geom_point(data=filter(data,goals>0),color="red",size=3,shape=17) +
      geom_point(color="darkgreen",size=1.5) +
      labs(x="Jornada",y="Gols acumulats",title="Gols Acumulats") + theme_minimal(base_size=11)
    ggplotly(p, tooltip="text") %>% layout(margin=list(t=30))
  })
  output$modal_minuts_jornada <- renderPlotly({
    data <- modal_match_data() %>% mutate(starter_lbl = factor(ifelse(starter==1,"Titular","Suplent"), levels=c("Titular","Suplent")))
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data, aes(x=jornada,y=minutes_played,fill=starter_lbl,
                          text=paste("Jornada:",jornada,"<br>Minuts:",minutes_played))) +
      geom_col() + scale_fill_manual(values=c("Suplent"="#e67e22","Titular"="steelblue")) +
      labs(x="Jornada",y="Minuts",fill="",title="Minuts per Jornada") + theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") %>% layout(margin=list(t=30))
  })
  output$modal_impacte_plot <- renderPlotly({
    imp <- modal_impact_data()
    if (nrow(imp)==0 || is.na(imp$impacte)) return(plotly_empty() %>% layout(title="Dades insuficients"))
    data <- data.frame(situacio=c("Quan juga","Quan NO juga"),
                       pts=c(imp$avg_points_when_playing,imp$avg_points_when_not_playing))
    p <- ggplot(data, aes(x=situacio,y=pts,fill=situacio,text=paste(situacio,"·",round(pts,2),"pts promig"))) +
      geom_col(show.legend=FALSE) + scale_fill_manual(values=c("Quan juga"="#27ae60","Quan NO juga"="#e74c3c")) +
      geom_text(aes(label=round(pts,2)),vjust=-0.4,size=3.5) +
      labs(x="",y="Punts Promig",title="Rendiment de l'Equip amb/sense el Jugador") + theme_minimal(base_size=11)
    ggplotly(p,tooltip="text") %>% layout(margin=list(t=35))
  })
  
  output$modal_ultims_partits <- renderUI({
    req(selected_player_modal())
    player_name <- selected_player_modal()
    matches_data <- modal_match_data() %>% arrange(desc(jornada)) %>% head(8)
    if (nrow(matches_data) == 0) return(NULL)
    evs <- all_matches_events() %>% filter(player == player_name) %>% select(jornada, event_type, detail)
    rows_html <- lapply(1:nrow(matches_data), function(i) {
      r <- matches_data[i,]; j <- r$jornada
      m_row <- matches() %>% filter(jornada == j, (local_team == r$team | away_team == r$team))
      if (nrow(m_row) > 0) {
        m_row <- m_row[1,]
        rival <- if (m_row$local_team == r$team) m_row$away_team else m_row$local_team
        loc   <- if (m_row$local_team == r$team) "C" else "F"
        res_lbl <- if (!is.na(m_row$goals_home)) paste0(m_row$goals_home,"-",m_row$goals_away) else ""
        partit_lbl <- paste0("J",j," ",loc," vs ",rival, if(res_lbl!="") paste0(" (",res_lbl,")") else "")
      } else { partit_lbl <- paste0("J",j) }
      evs_j <- evs %>% filter(jornada == j)
      icons <- character(0)
      n_gols <- sum(evs_j$event_type == "Gol")
      if (n_gols > 0) icons <- c(icons, rep("⚽", n_gols))
      if (sum(evs_j$event_type=="Groga")>0)   icons <- c(icons, rep("🟨", sum(evs_j$event_type=="Groga")))
      if (sum(evs_j$event_type=="Vermella")>0) icons <- c(icons, rep("🟥", sum(evs_j$event_type=="Vermella")))
      if (r$starter==0 && r$minutes_played>0) icons <- c(icons,"⬆️")
      else if (r$starter==1 && r$minutes_played<88) icons <- c(icons,"⬇️")
      tags$tr(
        tags$td(style="font-size:11px;color:#444;", partit_lbl),
        tags$td(style="text-align:center;font-weight:bold;color:#2980b9;", paste0(r$minutes_played,"'")),
        tags$td(style="text-align:center;font-size:14px;", HTML(paste(icons, collapse=" ")))
      )
    })
    div(class="ultims-partits",
        tags$h6("Últims partits"),
        tags$table(
          tags$thead(tags$tr(tags$th("Partit"),tags$th(style="text-align:center;","Min"),tags$th(style="text-align:center;","Evts"))),
          tags$tbody(rows_html)))
  })
  
  # ==========================================================================
  # FUNCIÓ LINK JUGADOR
  # ==========================================================================
  player_link_js <- function(player_name) {
    safe_name <- gsub("'", "\\\\'", player_name)
    sprintf('<a href="#" onclick="Shiny.setInputValue(\'selected_player_modal\',\'%s\',{priority:\'event\'});$(\'#playerModal\').modal(\'show\');return false;">%s</a>',
            safe_name, player_name)
  }
  
  # ==========================================================================
  # INICI
  # ==========================================================================
  output$vbox_equips   <- renderValueBox(valueBox(length(all_teams()), "Equips", icon("users"), color="blue"))
  output$vbox_jugadors <- renderValueBox(valueBox(nrow(player_stats()), "Jugadors", icon("user"), color="green"))
  output$vbox_jornades <- renderValueBox(valueBox(max(standings_by_round()$jornada), "Jornades Jugades", icon("calendar"), color="orange"))
  output$vbox_gols     <- renderValueBox(valueBox(sum(all_matches_events()$event_type=="Gol"), "Gols Totals", icon("futbol"), color="red"))
  output$vbox_gols_local <- renderValueBox({
    g_val <- team_match_stats() %>% filter(home_away=="Home") %>% summarise(g=sum(goals_for)) %>% pull(g)
    valueBox(g_val, "Gols Casa", icon("home"), color="green")
  })
  output$vbox_gols_visitant <- renderValueBox({
    g_val <- team_match_stats() %>% filter(home_away=="Away") %>% summarise(g=sum(goals_for)) %>% pull(g)
    valueBox(g_val, "Gols Fora", icon("plane"), color="purple")
  })
  output$vbox_partits_local_win <- renderValueBox({
    n <- team_match_stats() %>% filter(home_away=="Home", team_points==3) %>% nrow()
    valueBox(n, "Victòries Casa", icon("star"), color="yellow")
  })
  output$vbox_partits_empat <- renderValueBox({
    n <- team_match_stats() %>% filter(team_points==1) %>% nrow() / 2
    valueBox(n, "Empats Totals", icon("handshake"), color="light-blue")
  })
  output$inici_classificacio <- renderDT({
    current_standings() %>%
      select(Pos=position, Equip=team, PJ=played, Pts=points) %>%
      datatable(options=list(dom='t',pageLength=20,ordering=FALSE), rownames=FALSE)
  })
  output$inici_golejadors <- renderDT({
    player_stats() %>% filter(goals>0) %>% arrange(desc(goals)) %>% head(10) %>%
      select(Jugador=player, Equip=team, Gols=goals) %>%
      datatable(options=list(dom='t',pageLength=10,ordering=FALSE), rownames=FALSE, escape=FALSE)
  })
  output$inici_ultima_jornada <- renderUI({
    jmax <- max(matches()$jornada[!is.na(matches()$goals_home)])
    res  <- matches() %>% filter(jornada==jmax, !is.na(goals_home))
    div(h4(paste("Jornada", jmax)),
        lapply(1:nrow(res), function(i) {
          r <- res[i,]
          div(class="acta-box", tags$b(paste0(r$local_team,"  ",r$goals_home," - ",r$goals_away,"  ",r$away_team)))
        }))
  })
  
  # ==========================================================================
  # PARTITS
  # ==========================================================================
  observeEvent(input$clicked_partit, {
    req(input$clicked_partit)
    parts <- strsplit(input$clicked_partit,"|||",fixed=TRUE)[[1]]
    selected_partit(list(home=parts[1], away=parts[2]))
  })
  
  output$partits_cards <- renderUI({
    req(input$jornada_select)
    j <- as.numeric(input$jornada_select)
    m <- matches() %>% filter(jornada==j) %>% arrange(local_team)
    info <- all_matches_info() %>% filter(jornada==j) %>% select(home_team, away_team, date, time, referee)
    # info pot usar home_team
    if ("home_team" %in% names(info)) {
      m <- m %>% left_join(info, by=c("local_team"="home_team","away_team"="away_team"))
    }
    sel <- selected_partit()
    cards <- lapply(1:nrow(m), function(i) {
      r <- m[i,]
      jugat <- !is.na(r$goals_home)
      is_sel <- !is.null(sel) && sel$home==r$local_team && sel$away==r$away_team
      key <- paste0(r$local_team,"|||",r$away_team)
      safe_key <- gsub("'","\\\'",key)
      border_col <- if(is_sel) "#1a6b8a" else "#dee2e6"
      bg_col     <- if(is_sel) "#eaf4fb" else "white"
      centre_html <- if(jugat) {
        paste0('<div style="font-size:22px;font-weight:900;color:#1a6b8a;line-height:1;">',
               r$goals_home,' - ',r$goals_away,'</div>')
      } else {
        hora <- if(!is.na(r$time)&&r$time!="") r$time else "- - -"
        paste0('<div style="font-size:13px;color:#aaa;font-weight:600;">',hora,'</div>')
      }
      peu <- if(jugat && !is.na(r$date) && r$date!="") {
        paste0(r$date, if(!is.na(r$referee)&&r$referee!="") paste0(" · ",r$referee) else "")
      } else if(!is.null(r$venue) && !is.na(r$venue) && r$venue!="") { r$venue } else ""
      onclick_attr <- sprintf('onclick="Shiny.setInputValue(\'clicked_partit\',\'%s\',{priority:\'event\'});$(\'#partidModal\').modal(\'show\');"', safe_key)
      card_style <- paste0('style="background:',bg_col,';border:2px solid ',border_col,';border-radius:10px;padding:12px 14px;margin-bottom:8px;cursor:pointer;box-shadow:0 1px 4px rgba(0,0,0,.08);transition:all .15s;"')
      peu_html <- if(peu!="") paste0('<div style="font-size:10px;color:#aaa;text-align:center;margin-top:5px;">',peu,'</div>') else ""
      HTML(paste0('<div ',onclick_attr,' ',card_style,'>',
                  '<div style="display:flex;align-items:center;justify-content:space-between;">',
                  '<div style="flex:1;text-align:right;"><div style="font-size:13px;font-weight:700;color:#333;">',r$local_team,'</div><div style="font-size:10px;color:#999;">Casa</div></div>',
                  '<div style="flex:0 0 90px;text-align:center;padding:0 8px;">',centre_html,'</div>',
                  '<div style="flex:1;text-align:left;"><div style="font-size:13px;font-weight:700;color:#333;">',r$away_team,'</div><div style="font-size:10px;color:#999;">Fora</div></div>',
                  '</div>',peu_html,'</div>'))
    })
    tagList(cards)
  })
  
  output$modal_partit_title <- renderUI({
    sel <- selected_partit(); if(is.null(sel)) return(NULL)
    req(input$jornada_select)
    j <- as.numeric(input$jornada_select)
    m_row <- matches() %>% filter(jornada==j, local_team==sel$home, away_team==sel$away)
    jugat <- nrow(m_row)>0 && !is.na(m_row$goals_home[1])
    if(jugat) HTML(paste0("📋 Acta · J",j," · ",sel$home," <b>",m_row$goals_home[1]," - ",m_row$goals_away[1],"</b> ",sel$away))
    else HTML(paste0("⚡ Prèvia · J",j," · ",sel$home," vs ",sel$away))
  })
  
  output$modal_partit_body <- renderUI({
    sel <- selected_partit(); if(is.null(sel)) return(NULL)
    req(input$jornada_select)
    j <- as.numeric(input$jornada_select)
    m_row <- matches() %>% filter(jornada==j, local_team==sel$home, away_team==sel$away)
    jugat <- nrow(m_row)>0 && !is.na(m_row$goals_home[1])
    if(jugat) {
      tagList(
        fluidRow(
          box(width=6, title=paste0("Alineació · ",sel$home), solidHeader=TRUE, status="primary", uiOutput("lineup_home")),
          box(width=6, title=paste0("Alineació · ",sel$away), solidHeader=TRUE, status="danger",  uiOutput("lineup_away"))
        ),
        fluidRow(box(width=12, title="Esdeveniments", solidHeader=TRUE, status="info", uiOutput("acta_events")))
      )
    } else {
      tagList(
        fluidRow(box(width=12, solidHeader=TRUE, status="warning", uiOutput("previa_header"))),
        fluidRow(
          box(width=4, title="📊 Classificació", solidHeader=TRUE, status="primary", uiOutput("previa_classif")),
          box(width=4, title="🥅 Gols", solidHeader=TRUE, status="success", uiOutput("previa_gols")),
          box(width=4, title="🟨 Targetes", solidHeader=TRUE, status="warning", uiOutput("previa_targetes"))
        ),
        fluidRow(
          box(width=6, title="⭐ Top Golejadors", solidHeader=TRUE, status="success", uiOutput("previa_golejadors")),
          box(width=6, title="🏆 Millors Jugadors (Rating)", solidHeader=TRUE, status="primary", uiOutput("previa_ratings"))
        ),
        fluidRow(
          box(width=6, title="⬡ Radars comparats", solidHeader=TRUE, status="info", plotlyOutput("previa_radar", height="300px")),
          box(width=6, title="⏱ Quan es marquen els gols", solidHeader=TRUE, status="danger", plotlyOutput("previa_gols_minut", height="300px"))
        )
      )
    }
  })
  
  make_lineup_ui <- function(lineups) {
    if(nrow(lineups)==0) return(p("No disponible"))
    titulars <- filter(lineups, position=="Titular")
    suplents <- filter(lineups, position=="Suplent")
    mk_row <- function(r) div(class="acta-box", tags$b(paste0("#",r$shirt_number," ")),
                              HTML(player_link_js(r$player)),
                              if(!is.na(r$stats)) tags$span(style="color:gray;font-size:.85em;", paste0(" · ",r$stats)))
    div(h4("Titulars"), lapply(seq_len(nrow(titulars)), function(i) mk_row(titulars[i,])),
        h4("Suplents"), lapply(seq_len(nrow(suplents)), function(i) mk_row(suplents[i,])))
  }
  output$lineup_home <- renderUI({
    sel <- selected_partit(); req(sel, input$jornada_select)
    j <- as.numeric(input$jornada_select)
    lineups <- all_matches_lineups() %>%
      filter(home_team==sel$home, away_team==sel$away, jornada==j, team==sel$home) %>%
      arrange(position=="Suplent", as.numeric(shirt_number))
    make_lineup_ui(lineups)
  })
  output$lineup_away <- renderUI({
    sel <- selected_partit(); req(sel, input$jornada_select)
    j <- as.numeric(input$jornada_select)
    lineups <- all_matches_lineups() %>%
      filter(home_team==sel$home, away_team==sel$away, jornada==j, team==sel$away) %>%
      arrange(position=="Suplent", as.numeric(shirt_number))
    make_lineup_ui(lineups)
  })
  output$acta_events <- renderUI({
    sel <- selected_partit(); req(sel, input$jornada_select)
    j <- as.numeric(input$jornada_select)
    evs <- all_matches_events() %>% filter(home_team==sel$home, away_team==sel$away, jornada==j) %>% arrange(minute)
    if(nrow(evs)==0) return(p("No hi ha esdeveniments disponibles"))
    lapply(seq_len(nrow(evs)), function(i) {
      e <- evs[i,]
      ico <- switch(e$event_type, "Gol"="⚽","Substitució"="🔄","Groga"="🟨","Vermella"="🟥","❓")
      cls <- switch(e$event_type, "Gol"="gol-event","Substitució"="subst-event","card-event")
      div(class="acta-box",
          tags$span(class=cls, paste0(ico," min. ",e$minute,"' · "),
                    HTML(player_link_js(e$player)), paste0(" (",e$team,")"),
                    if(!is.na(e$detail)&&e$detail!="") paste0(" · ",e$detail)))
    })
  })
  
  previa_teams <- reactive({
    sel <- selected_partit(); req(sel)
    list(home=sel$home, away=sel$away)
  })
  
  output$previa_header <- renderUI({
    t <- previa_teams()
    st_h <- current_standings() %>% filter(team==t$home)
    st_a <- current_standings() %>% filter(team==t$away)
    div(style="display:flex;justify-content:space-around;align-items:center;padding:10px 0;",
        div(style="text-align:center;",
            div(style="font-size:28px;font-weight:900;color:#1a6b8a;",paste0("#",st_h$position)),
            div(style="font-size:12px;color:#666;",t$home),
            div(style="font-size:14px;font-weight:bold;",paste0(st_h$points," pts"))),
        div(style="font-size:22px;color:#ccc;font-weight:300;","VS"),
        div(style="text-align:center;",
            div(style="font-size:28px;font-weight:900;color:#c0392b;",paste0("#",st_a$position)),
            div(style="font-size:12px;color:#666;",t$away),
            div(style="font-size:14px;font-weight:bold;",paste0(st_a$points," pts"))))
  })
  
  make_comp_row_fn <- function(lbl, vh, va, hb=TRUE) {
    safe_num <- function(x) if(is.na(x)) "N/A" else x
    vh_s <- safe_num(vh); va_s <- safe_num(va)
    if(!is.na(vh)&&!is.na(va)) {
      col_h <- if(hb){if(vh>va)"#27ae60" else if(vh<va)"#e74c3c" else "#888"}else{if(vh<va)"#27ae60" else if(vh>va)"#e74c3c" else "#888"}
      col_a <- if(hb){if(va>vh)"#27ae60" else if(va<vh)"#e74c3c" else "#888"}else{if(va<vh)"#27ae60" else if(va>vh)"#e74c3c" else "#888"}
    } else { col_h <- "#888"; col_a <- "#888" }
    tags$tr(tags$td(style=paste0("text-align:right;font-weight:bold;color:",col_h,";padding:4px 8px;"),vh_s),
            tags$td(style="text-align:center;color:#888;font-size:11px;padding:4px;",lbl),
            tags$td(style=paste0("font-weight:bold;color:",col_a,";padding:4px 8px;"),va_s))
  }
  
  output$previa_classif <- renderUI({
    t <- previa_teams()
    st_h <- current_standings() %>% filter(team==t$home)
    st_a <- current_standings() %>% filter(team==t$away)
    tl_h <- tilt_data() %>% filter(team==t$home) %>% pull(tilt)
    tl_a <- tilt_data() %>% filter(team==t$away) %>% pull(tilt)
    tl_h <- if(length(tl_h)>0) round(tl_h,2) else NA
    tl_a <- if(length(tl_a)>0) round(tl_a,2) else NA
    fmt_tilt <- function(x) if(is.na(x)) "N/A" else sprintf("%+.2f",x)
    tilt_row <- tags$tr(
      tags$td(style=paste0("text-align:right;font-weight:bold;color:",
                           if(!is.na(tl_h)&&!is.na(tl_a)&&tl_h>tl_a)"#27ae60" else if(!is.na(tl_h)&&!is.na(tl_a)&&tl_h<tl_a)"#e74c3c" else "#888",
                           ";padding:5px 8px;"), fmt_tilt(tl_h)),
      tags$td(style="text-align:center;color:#aaa;font-size:11px;padding:5px 4px;","⚡ Tilt"),
      tags$td(style=paste0("font-weight:bold;color:",
                           if(!is.na(tl_h)&&!is.na(tl_a)&&tl_a>tl_h)"#27ae60" else if(!is.na(tl_h)&&!is.na(tl_a)&&tl_a<tl_h)"#e74c3c" else "#888",
                           ";padding:5px 8px;"), fmt_tilt(tl_a))
    )
    tags$table(style="width:100%;",
               tags$thead(tags$tr(tags$th(style="text-align:right;font-size:11px;color:#1a6b8a;padding:4px 8px;",t$home),tags$th(),tags$th(style="font-size:11px;color:#c0392b;padding:4px 8px;",t$away))),
               tags$tbody(
                 make_comp_row_fn("Posició",st_h$position,st_a$position,FALSE),
                 make_comp_row_fn("Punts",st_h$points,st_a$points),
                 make_comp_row_fn("PJ",st_h$played,st_a$played,FALSE),
                 make_comp_row_fn("Victòries",st_h$wins,st_a$wins),
                 make_comp_row_fn("Empats",st_h$draws,st_a$draws,FALSE),
                 make_comp_row_fn("Derrotes",st_h$losses,st_a$losses,FALSE),
                 tilt_row))
  })
  
  output$previa_gols <- renderUI({
    t <- previa_teams()
    gf <- function(tn) { d <- team_match_stats()%>%filter(team==tn); list(gf=sum(d$goals_for),gc=sum(d$goals_against),n=nrow(d)) }
    sh <- gf(t$home); sa <- gf(t$away)
    tags$table(style="width:100%;",
               tags$thead(tags$tr(tags$th(style="text-align:right;font-size:11px;color:#555;padding:4px 8px;",t$home),tags$th(),tags$th(style="font-size:11px;color:#555;padding:4px 8px;",t$away))),
               tags$tbody(
                 make_comp_row_fn("Gols marcats",sh$gf,sa$gf),
                 make_comp_row_fn("Gols/partit",round(sh$gf/sh$n,2),round(sa$gf/sa$n,2)),
                 make_comp_row_fn("Gols rebuts",sh$gc,sa$gc,FALSE),
                 make_comp_row_fn("Rebuts/partit",round(sh$gc/sh$n,2),round(sa$gc/sa$n,2),FALSE),
                 make_comp_row_fn("Diferència",sh$gf-sh$gc,sa$gf-sa$gc)))
  })
  
  output$previa_targetes <- renderUI({
    t <- previa_teams()
    gc <- function(tn) { d <- team_match_stats()%>%filter(team==tn); list(y=sum(d$yellow_cards),r=sum(d$red_cards),n=nrow(d)) }
    sh <- gc(t$home); sa <- gc(t$away)
    tags$table(style="width:100%;",
               tags$thead(tags$tr(tags$th(style="text-align:right;font-size:11px;color:#555;padding:4px 8px;",t$home),tags$th(),tags$th(style="font-size:11px;color:#555;padding:4px 8px;",t$away))),
               tags$tbody(
                 make_comp_row_fn("🟨 Grogues total",sh$y,sa$y,FALSE),
                 make_comp_row_fn("Grogues/partit",round(sh$y/sh$n,2),round(sa$y/sa$n,2),FALSE),
                 make_comp_row_fn("🟥 Vermelles",sh$r,sa$r,FALSE)))
  })
  
  output$previa_golejadors <- renderUI({
    t <- previa_teams()
    get_top <- function(tn) player_stats()%>%filter(team==tn,goals>0)%>%arrange(desc(goals))%>%head(5)
    th <- get_top(t$home); ta <- get_top(t$away)
    make_col <- function(df, tn, align="right") tagList(
      tags$b(style="font-size:12px;",tn),
      tags$table(style="width:100%;", lapply(seq_len(nrow(df)), function(i) {
        r <- df[i,]
        if(align=="right") tags$tr(tags$td(style="text-align:right;font-size:12px;padding:2px 4px;",HTML(player_link_js(r$player))),tags$td(style="font-size:13px;font-weight:bold;color:#27ae60;padding:2px 4px;",paste0("⚽ ",r$goals)))
        else tags$tr(tags$td(style="font-size:13px;font-weight:bold;color:#27ae60;padding:2px 4px;",paste0("⚽ ",r$goals)),tags$td(style="font-size:12px;padding:2px 4px;",HTML(player_link_js(r$player))))
      })))
    div(style="display:flex;gap:16px;",div(style="flex:1;",make_col(th,t$home,"right")),div(style="width:1px;background:#eee;"),div(style="flex:1;",make_col(ta,t$away,"left")))
  })
  
  output$previa_ratings <- renderUI({
    t <- previa_teams()
    get_top <- function(tn) player_ratings()%>%filter(team==tn)%>%arrange(desc(rating_global))%>%head(5)
    th <- get_top(t$home); ta <- get_top(t$away)
    make_col <- function(df, tn, align="right") tagList(
      tags$b(style="font-size:12px;",tn),
      tags$table(style="width:100%;", lapply(seq_len(nrow(df)), function(i) {
        r <- df[i,]
        if(align=="right") tags$tr(tags$td(style="text-align:right;font-size:12px;padding:2px 4px;",HTML(player_link_js(r$player))),tags$td(style="font-size:13px;font-weight:bold;color:#1a6b8a;padding:2px 4px;",paste0("★ ",r$rating_global)))
        else tags$tr(tags$td(style="font-size:13px;font-weight:bold;color:#1a6b8a;padding:2px 4px;",paste0("★ ",r$rating_global)),tags$td(style="font-size:12px;padding:2px 4px;",HTML(player_link_js(r$player))))
      })))
    div(style="display:flex;gap:16px;",div(style="flex:1;",make_col(th,t$home,"right")),div(style="width:1px;background:#eee;"),div(style="flex:1;",make_col(ta,t$away,"left")))
  })
  
  output$previa_radar <- renderPlotly({
    t <- previa_teams()
    cats <- c("Atac","Defensa","Força Casa","Força Fora","Fair-Play","1a Part","2a Part")
    gv <- function(tn) { r <- team_radar_data()%>%filter(team==tn); if(nrow(r)==0) return(rep(50,7)); c(r$radar_attack,r$radar_defense,r$radar_home,r$radar_away,r$radar_fairplay,r$radar_first,r$radar_second) }
    vh <- gv(t$home); va <- gv(t$away)
    plot_ly(type="scatterpolar") %>%
      add_trace(r=c(vh,vh[1]),theta=c(cats,cats[1]),fill="toself",name=t$home,fillcolor="rgba(26,107,138,0.25)",line=list(color="rgba(26,107,138,1)",width=2)) %>%
      add_trace(r=c(va,va[1]),theta=c(cats,cats[1]),fill="toself",name=t$away,fillcolor="rgba(192,57,43,0.2)",line=list(color="rgba(192,57,43,1)",width=2)) %>%
      layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,100),tickvals=c(0,25,50,75,100),gridcolor="lightgrey"),angularaxis=list(tickfont=list(size=11))),legend=list(orientation="h",y=-0.12),margin=list(t=20,b=40,l=40,r=40))
  })
  
  output$previa_gols_minut <- renderPlotly({
    t <- previa_teams()
    periodes <- c("0-15'","16-30'","31-45'","46-60'","61-75'","76-90'","90+'")
    br_v <- c(0,15,30,45,60,75,90,120)
    get_gols <- function(tn) {
      all_matches_events()%>%filter(event_type=="Gol",team==tn,!is.na(minute))%>%
        mutate(periode=cut(minute,breaks=br_v,labels=periodes,include.lowest=TRUE))%>%
        count(periode,.drop=FALSE)%>%complete(periode=factor(periodes,levels=periodes),fill=list(n=0))
    }
    g1 <- get_gols(t$home); g2 <- get_gols(t$away)
    max_val <- max(max(g1$n),max(g2$n),1)
    plot_ly()%>%
      add_bars(x=-g1$n,y=periodes,orientation="h",name=t$home,marker=list(color="rgba(26,107,138,0.8)"),customdata=g1$n,hovertemplate=paste0(t$home," - %{y}: %{customdata} gols<extra></extra>"))%>%
      add_bars(x=g2$n,y=periodes,orientation="h",name=t$away,marker=list(color="rgba(192,57,43,0.8)"),hovertemplate=paste0(t$away," - %{y}: %{x} gols<extra></extra>"))%>%
      layout(barmode="overlay",xaxis=list(tickvals=seq(-max_val,max_val,by=1),ticktext=as.character(abs(seq(-max_val,max_val,by=1))),title="Gols",zeroline=TRUE,zerolinecolor="#333",zerolinewidth=2),yaxis=list(title="",categoryorder="array",categoryarray=rev(periodes)),legend=list(orientation="h",y=-0.15),margin=list(t=10,b=50,l=60,r=20))
  })
  
  # ==========================================================================
  # CLASSIFICACIÓ
  # ==========================================================================
  output$taula_classificacio <- renderDT({
    current_standings()%>%select(Pos=position,Equip=team,PJ=played,G=wins,E=draws,P=losses,GF=goals_for,GC=goals_against,Pts=points)%>%
      datatable(options=list(dom='t',pageLength=20,ordering=FALSE),rownames=FALSE)
  })
  output$grafic_evolucio_posicions <- renderPlotly({
    data <- standings_by_round()%>%mutate(team=as.character(team))
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data,aes(x=jornada,y=position,color=team,group=team,text=paste("Equip:",team,"<br>Jornada:",jornada,"<br>Posició:",position)))+
      geom_line(linewidth=1)+geom_point(size=2)+scale_y_reverse(breaks=1:20)+labs(x="Jornada",y="Posició")+theme_minimal()
    ggplotly(p,tooltip="text")
  })
  output$stats_heatmap <- renderPlotly({
    data <- team_match_stats()%>%select(team,jornada,team_points)%>%mutate(res=case_when(team_points==3~"V",team_points==1~"E",TRUE~"D"))
    p <- ggplot(data,aes(x=jornada,y=team,fill=factor(team_points),text=paste("Equip:",team,"<br>Jornada:",jornada,"<br>Resultat:",res)))+
      geom_tile(color="white",size=0.5)+scale_fill_manual(values=c("0"="#e74c3c","1"="#f1c40f","3"="#2ecc71"),labels=c("Derrota","Empat","Victòria"))+
      labs(x="Jornada",y="",fill="Resultat")+theme_minimal()+theme(axis.text.y=element_text(size=8))
    ggplotly(p,tooltip="text")
  })
  output$classif_head2head <- renderPlotly({
    played <- matches()%>%filter(!is.na(goals_home))
    team_order <- current_standings()%>%arrange(position)%>%pull(team)
    h2h <- bind_rows(
      played%>%transmute(team=local_team,opponent=away_team,pts=case_when(goals_home>goals_away~3,goals_home==goals_away~1,TRUE~0)),
      played%>%transmute(team=away_team,opponent=local_team,pts=case_when(goals_away>goals_home~3,goals_away==goals_home~1,TRUE~0))
    )%>%group_by(team,opponent)%>%summarise(total_points=sum(pts),.groups="drop")%>%
      mutate(team=factor(team,levels=rev(team_order)),opponent=factor(opponent,levels=team_order))
    p <- ggplot(h2h,aes(x=opponent,y=team,fill=total_points,text=paste0(team," vs ",opponent,": ",total_points," pts")))+
      geom_tile(color="white",size=0.6)+geom_text(aes(label=total_points),color="white",fontface="bold",size=3.5)+
      scale_fill_gradient2(low="#e74c3c",mid="#f39c12",high="#27ae60",midpoint=3,na.value="grey90")+
      labs(x="Rival (columna)",y="Equip (fila)",fill="Pts")+theme_minimal()+
      theme(axis.text.x=element_text(angle=45,hjust=1,size=8),axis.text.y=element_text(size=8),panel.grid=element_blank())
    ggplotly(p,tooltip="text")
  })
  
  # ==========================================================================
  # EQUIPS
  # ==========================================================================
  equip_data <- reactive({
    req(input$equip_select)
    team_match_stats()%>%filter(team==input$equip_select)%>%arrange(jornada)
  })
  equip_standing <- reactive({
    req(input$equip_select)
    current_standings()%>%filter(team==input$equip_select)
  })
  output$equip_rating_display <- renderText({
    req(input$equip_select)
    r <- team_ratings()%>%filter(team==input$equip_select)%>%pull(rating)
    if(length(r)==0) "N/A" else as.character(r)
  })
  output$equip_partits_recap <- renderUI({
    eq <- req(input$equip_select)
    eq_matches <- matches()%>%filter(local_team==eq|away_team==eq)%>%arrange(jornada)%>%
      mutate(jugat=!is.na(goals_home),rival=ifelse(local_team==eq,away_team,local_team),
             loc=ifelse(local_team==eq,"Casa","Fora"),
             resultat=ifelse(jugat,ifelse(local_team==eq,paste0(goals_home," - ",goals_away),paste0(goals_away," - ",goals_home)),NA_character_))
    jugats <- eq_matches%>%filter(jugat); no_jugats <- eq_matches%>%filter(!jugat)
    match_card <- function(prefix_icon, prefix_txt, r, color_border) {
      if(is.null(r)||nrow(r)==0) return(NULL)
      div(style=paste0("background:white;border-left:4px solid ",color_border,";border-radius:6px;padding:10px 12px;margin-bottom:8px;box-shadow:0 1px 4px rgba(0,0,0,.1);"),
          div(style="font-size:11px;color:#888;margin-bottom:4px;",prefix_icon," ",prefix_txt,tags$span(style="float:right;",paste0("J",r$jornada," · ",r$loc))),
          div(style="font-size:15px;font-weight:bold;",r$rival),
          if(!is.na(r$resultat)) div(style=paste0("font-size:18px;font-weight:900;color:",color_border,";margin-top:2px;"),r$resultat))
    }
    if(nrow(no_jugats)==0) {
      ultims <- tail(jugats,2)
      tagList(h5(style="color:#555;margin-bottom:8px;","⏱ Últims 2 partits jugats"),
              match_card("🏁","Penúltim",ultims[1,],"#8e44ad"),match_card("🏁","Últim",ultims[2,],"#27ae60"))
    } else {
      ultim <- if(nrow(jugats)>0) tail(jugats,1) else NULL
      proxim <- if(nrow(no_jugats)>0) head(no_jugats,1) else NULL
      tagList(if(!is.null(ultim)) tagList(h5(style="color:#555;margin-bottom:6px;","✅ Últim partit"),match_card("✅","Jugat",ultim,"#27ae60")),
              if(!is.null(proxim)) tagList(h5(style="color:#555;margin:8px 0 6px;","⏭ Pròxim partit"),match_card("📅","Pendent",proxim,"#2980b9")))
    }
  })
  
  output$equip_calendari_ui <- renderUI({
    div(h4(style="margin-bottom:16px;",paste0("📅 Calendari de ",input$equip_select)),DTOutput("equip_calendari_dt"))
  })
  output$equip_calendari_dt <- renderDT({
    eq <- req(input$equip_select)
    cal <- matches()%>%filter(local_team==eq|away_team==eq)%>%arrange(jornada)%>%
      mutate(jugat=!is.na(goals_home),rival=ifelse(local_team==eq,away_team,local_team),
             loc_vis=ifelse(local_team==eq,"Casa","Fora"),
             gf=ifelse(jugat,ifelse(local_team==eq,goals_home,goals_away),NA_integer_),
             gc=ifelse(jugat,ifelse(local_team==eq,goals_away,goals_home),NA_integer_),
             Resultat=case_when(!jugat~"Pendent",gf>gc~paste0(gf," - ",gc),gf==gc~paste0(gf," - ",gc),TRUE~paste0(gf," - ",gc)),
             Punts=case_when(!jugat~NA_character_,gf>gc~"V",gf==gc~"E",TRUE~"D"))
    info_eq <- all_matches_info()%>%filter(home_team==eq|away_team==eq)%>%select(jornada,home_team,away_team,date,time,referee)
    cal_info <- cal%>%left_join(info_eq,by=c("jornada"="jornada","local_team"="home_team","away_team"="away_team"))%>%
      mutate(Data=ifelse(!is.na(date)&date!="",date,""),Hora=ifelse(!is.na(time)&time!="",time,""),Arbitre=ifelse(!is.na(referee)&referee!="",referee,""))%>%
      select(Jornada=jornada,`C/F`=loc_vis,Rival=rival,Resultat,V_E_D=Punts,Data,Hora,Arbitre)
    datatable(cal_info,rownames=FALSE,options=list(pageLength=30,dom='ft',ordering=FALSE,columnDefs=list(list(className='dt-center',targets=c(0,1,3,4,5,6)))))%>%
      formatStyle('V_E_D',backgroundColor=styleEqual(c("V","E","D"),c("#d5f5e3","#fef9e7","#fadbd8")),fontWeight="bold")
  })
  
  output$equip_radar <- renderPlotly({
    req(input$equip_select)
    row <- team_radar_data()%>%filter(team==input$equip_select)
    if(nrow(row)==0) return(plotly_empty())
    cats <- c("Atac","Defensa","Força Casa","Força Fora","Fair-Play","1a Part","2a Part")
    vals <- c(row$radar_attack,row$radar_defense,row$radar_home,row$radar_away,row$radar_fairplay,row$radar_first,row$radar_second)
    plot_ly(type="scatterpolar",r=c(vals,vals[1]),theta=c(cats,cats[1]),fill="toself",fillcolor="rgba(31,119,180,0.3)",line=list(color="rgba(31,119,180,1)",width=2),hovertemplate="<b>%{theta}</b><br>Valor: %{r:.1f}<extra></extra>")%>%
      layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,100),tickvals=c(0,25,50,75,100),gridcolor="lightgrey"),angularaxis=list(tickfont=list(size=12))),showlegend=FALSE,margin=list(t=20,b=20,l=40,r=40))
  })
  
  output$equip_posicio    <- renderValueBox(valueBox(equip_standing()$position,"Posició",icon("trophy"),color="blue"))
  output$equip_punts      <- renderValueBox(valueBox(equip_standing()$points,"Punts",icon("star"),color="green"))
  output$equip_gols_favor <- renderValueBox(valueBox(sum(equip_data()$goals_for),"Gols a Favor",icon("futbol"),color="green"))
  output$equip_gols_contra<- renderValueBox(valueBox(sum(equip_data()$goals_against),"Gols en Contra",icon("exclamation-triangle"),color="red"))
  output$equip_victories  <- renderValueBox(valueBox(equip_standing()$wins,"Victòries",icon("check"),color="green"))
  output$equip_vic_casa   <- renderValueBox({ n <- equip_data()%>%filter(home_away=="Home",team_points==3)%>%nrow(); valueBox(n,"Victòries Casa",icon("home"),color="purple") })
  output$equip_vic_fora   <- renderValueBox({ n <- equip_data()%>%filter(home_away=="Away",team_points==3)%>%nrow(); valueBox(n,"Victòries Fora",icon("plane"),color="light-blue") })
  output$equip_targetes   <- renderValueBox({ t <- sum(equip_data()$yellow_cards)+sum(equip_data()$red_cards); valueBox(t,"Targetes",icon("square"),color="yellow") })
  
  output$equip_punts_evolucio <- renderPlotly({
    data <- equip_data()%>%mutate(punts_acum=cumsum(team_points))
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data,aes(x=jornada,y=punts_acum,text=paste("Jornada:",jornada,"<br>Punts:",punts_acum)))+geom_line(color="steelblue",linewidth=1.5)+geom_point(size=3,color="darkblue")+labs(x="Jornada",y="Punts Acumulats")+theme_minimal()
    ggplotly(p,tooltip="text")
  })
  output$equip_gols_jornada <- renderPlotly({
    data <- equip_data()%>%select(jornada,goals_for,goals_against)%>%pivot_longer(c(goals_for,goals_against),names_to="tipus",values_to="gols")%>%mutate(tipus=ifelse(tipus=="goals_for","A favor","En contra"))
    p <- ggplot(data,aes(x=jornada,y=gols,fill=tipus,text=paste("Jornada:",jornada,"<br>",tipus,":",gols)))+geom_col(position="dodge")+scale_fill_manual(values=c("A favor"="green","En contra"="red"))+labs(x="Jornada",y="Gols",fill="")+theme_minimal()
    ggplotly(p,tooltip="text")
  })
  output$equip_casa_fora <- renderPlotly({
    data <- equip_data()%>%group_by(home_away)%>%summarise(avg_pts=mean(team_points),.groups="drop")%>%mutate(home_away=ifelse(home_away=="Home","Casa","Fora"))
    p <- ggplot(data,aes(x=home_away,y=avg_pts,fill=home_away,text=paste(home_away,"· Punts promig:",round(avg_pts,2))))+geom_col(show.legend=FALSE)+geom_text(aes(label=round(avg_pts,2)),vjust=-0.4)+scale_fill_manual(values=c("Casa"="darkgreen","Fora"="steelblue"))+labs(x="",y="Punts Promig")+theme_minimal()
    ggplotly(p,tooltip="text")
  })
  output$equip_gols_periode <- renderPlotly({
    eq <- req(input$equip_select)
    evts <- all_matches_events()
    data <- bind_rows(
      evts%>%filter(event_type=="Gol",team==eq)%>%mutate(type="Gols a favor"),
      evts%>%filter(event_type=="Gol",(home_team==eq&team!=eq)|(away_team==eq&team!=eq))%>%mutate(type="Gols en contra")
    )%>%mutate(period=case_when(minute<=15~"0-15'",minute<=30~"16-30'",minute<=45~"31-45'",minute<=60~"46-60'",minute<=75~"61-75'",minute<=90~"76-90'",TRUE~"90+'"))%>%mutate(period=factor(period,levels=c("0-15'","16-30'","31-45'","46-60'","61-75'","76-90'","90+'")))
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data,aes(x=period,fill=type))+geom_bar(position="dodge")+scale_fill_manual(values=c("Gols a favor"="green","Gols en contra"="red"))+labs(x="Període",y="Gols",fill="")+theme_minimal()
    ggplotly(p)
  })
  output$equip_efectivitat_parts <- renderPlotly({
    eq <- req(input$equip_select)
    evts <- all_matches_events()
    data <- bind_rows(
      evts%>%filter(event_type=="Gol",team==eq)%>%mutate(type="Gols marcats"),
      evts%>%filter(event_type=="Gol",(home_team==eq&team!=eq)|(away_team==eq&team!=eq))%>%mutate(type="Gols rebuts")
    )%>%mutate(half=if_else(minute<=45,"Primera part","Segona part"))%>%count(half,type)
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data,aes(x=half,y=n,fill=type))+geom_col(position="dodge")+geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.4)+scale_fill_manual(values=c("Gols marcats"="green","Gols rebuts"="red"))+labs(x="",y="Gols",fill="")+theme_minimal()
    ggplotly(p)
  })
  output$equip_carrega_treball <- renderPlotly({
    eq <- req(input$equip_select)
    data <- player_match_stats()%>%filter(team==eq)%>%group_by(player)%>%summarise(total_minutes=sum(minutes_played),avg_minutes=mean(minutes_played),matches=n(),.groups="drop")
    if(nrow(data)==0) return(plotly_empty())
    p <- ggplot(data,aes(x=avg_minutes,y=matches,size=total_minutes,color=total_minutes,text=paste(player,"<br>Total min:",total_minutes,"<br>Promig:",round(avg_minutes,1),"<br>Partits:",matches)))+geom_point(alpha=0.7)+scale_color_gradient(low="yellow",high="darkgreen")+labs(x="Minuts promig per partit",y="Partits jugats")+theme_minimal()+theme(legend.position="none")
    ggplotly(p,tooltip="text")
  })
  output$equip_jugadors <- renderDT({
    eq <- req(input$equip_select)
    data <- player_match_stats()%>%filter(team==eq)%>%group_by(player)%>%
      summarise(Partits=n(),Titularitats=sum(starter),Minuts=sum(minutes_played),Gols=sum(goals),Grogues=sum(yellow_cards),Vermelles=sum(red_cards),.groups="drop")%>%
      left_join(player_impact()%>%select(player,impacte),by="player")%>%
      left_join(player_ratings()%>%select(player,rating_global),by="player")%>%
      mutate(Impacte=round(impacte,2),Rating=rating_global)%>%arrange(desc(Minuts))%>%
      mutate(Jugador=sapply(player,player_link_js))%>%
      select(Jugador,Rating,Partits,Titularitats,Minuts,Gols,Grogues,Vermelles,Impacte)
    datatable(data,escape=FALSE,options=list(pageLength=20,ordering=TRUE),rownames=FALSE)
  })
  output$equip_vs_nivell <- renderPlotly({
    team_name <- req(input$equip_select)
    n_teams <- nrow(current_standings())
    top_n <- floor(n_teams/3); bot_n <- floor(n_teams/3)
    rank_lookup <- current_standings()%>%mutate(nivell=case_when(position<=top_n~"TOP",position>n_teams-bot_n~"CUA",TRUE~"MITJA"))%>%select(rival=team,nivell)
    data <- team_match_stats()%>%filter(team==team_name)%>%left_join(rank_lookup,by=c("opponent"="rival"))%>%mutate(nivell=replace_na(nivell,"MITJA"))%>%
      group_by(nivell)%>%summarise(PJ=n(),V=sum(team_points==3),E=sum(team_points==1),D=sum(team_points==0),pts_avg=round(mean(team_points),2),.groups="drop")%>%
      mutate(nivell=factor(nivell,levels=c("TOP","MITJA","CUA")),color=case_when(nivell=="TOP"~"#e74c3c",nivell=="MITJA"~"#f39c12",TRUE~"#27ae60"),label_hover=paste0("<b>vs ",nivell,"</b><br>",PJ," partits: ",V,"V ",E,"E ",D,"D<br>",pts_avg," pts/partit"))%>%arrange(nivell)
    plot_ly(data,x=~nivell,y=~pts_avg,type="bar",marker=list(color=~color,line=list(color="white",width=1)),text=~label_hover,hoverinfo="text",textposition="none",showlegend=FALSE)%>%
      layout(xaxis=list(title="Nivell del rival",tickfont=list(size=12)),yaxis=list(title="Pts/partit",range=c(0,3.2),zeroline=FALSE),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=40,l=50,r=10))
  })
  output$equip_dependencia_gols <- renderPlotly({
    team_name <- req(input$equip_select)
    validate(need(length(team_name)==1 && nchar(team_name)>0, "Selecciona un equip"))
    total_gols <- team_match_stats()%>%filter(team==team_name)%>%summarise(g=sum(goals_for,na.rm=TRUE))%>%pull(g)
    if(length(total_gols)==0) total_gols <- 0
    if(total_gols==0) return(plot_ly()%>%layout(title="Sense gols marcats"))
    data <- all_matches_events()%>%filter(team==team_name,event_type%in%c("Gol","Goal","goal"))%>%count(player,name="gols")%>%arrange(desc(gols))%>%
      mutate(pct=round(100*gols/total_gols,1),player_short=ifelse(nchar(player)>20,paste0(substr(player,1,18),"…"),player))%>%head(10)
    if(nrow(data)==0) return(plot_ly()%>%layout(title="Sense dades d'events"))
    data <- data%>%mutate(color=ifelse(row_number()==1,"#e74c3c","#3498db"),label_hover=paste0("<b>",player,"</b><br>",gols," gols (",pct,"% del total)"))
    plot_ly(data,x=~pct,y=~reorder(player_short,pct),type="bar",orientation="h",marker=list(color=~color,line=list(color="white",width=1)),text=~label_hover,hoverinfo="text",textposition="none",showlegend=FALSE)%>%
      layout(xaxis=list(title="% de gols de l'equip",zeroline=FALSE),yaxis=list(title="",tickfont=list(size=9)),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=40,l=160,r=40))
  })
  
  # Tilt helpers
  render_tilt_ui <- function(team_name, show_detail=TRUE) {
    row <- tilt_data()%>%filter(team==team_name)
    if(nrow(row)==0) return(div("Sense dades de tilt"))
    tilt_val <- row$tilt
    abs_max <- max(abs(c(tilt_min(),tilt_max())),0.01)
    pct <- (tilt_val-(-abs_max))/(2*abs_max)
    r_c <- round(255*(1-pct)); g_c <- round(255*pct); b_c <- 60L
    hex_col <- sprintf("#%02X%02X%02X",r_c,g_c,b_c)
    arrow <- if(tilt_val>0.1)"▲" else if(tilt_val<-0.1)"▼" else "▶"
    label <- if(tilt_val>0.5)"Molt bona dinàmica" else if(tilt_val>0.1)"Bona dinàmica" else if(tilt_val>-0.1)"Dinàmica neutra" else if(tilt_val>-0.5)"Mala dinàmica" else "Molt mala dinàmica"
    lliga_avg <- mean(tilt_data()$pts_real_avg,na.rm=TRUE)
    diff_lliga <- round(row$pts_real_avg-lliga_avg,2)
    diff_txt <- if(diff_lliga>0) paste0("+",diff_lliga," vs mitjana lliga") else if(diff_lliga<0) paste0(diff_lliga," vs mitjana lliga") else "igual que la mitjana de la lliga"
    div(div(style=paste0("display:inline-flex;align-items:center;gap:12px;background:",hex_col,"22;border:2px solid ",hex_col,";border-radius:12px;padding:10px 20px;"),
            div(style=paste0("font-size:36px;font-weight:900;color:",hex_col,";"),arrow," ",sprintf("%+.2f",tilt_val)),
            div(div(style="font-size:13px;font-weight:bold;",label),
                if(show_detail) div(style="font-size:11px;color:#666;margin-top:3px;",paste0(row$pts_real_avg," pts/partit en els últims ",row$n_recents," partits  ·  ",diff_txt)))))
  }
  output$equip_tilt_box <- renderUI({
    eq <- req(input$equip_select)
    validate(need(length(eq)==1 && nchar(eq)>0, ""))
    render_tilt_ui(eq, show_detail=TRUE)
  })
  
  # ==========================================================================
  # COMPARADOR D'EQUIPS
  # ==========================================================================
  make_comp_table <- function(t1,t2,rows_list) {
    tags$table(style="width:100%;",tags$thead(tags$tr(tags$th(style="text-align:right;font-size:12px;color:#1a6b8a;padding:4px 8px;",t1),tags$th(style="text-align:center;font-size:10px;color:#999;",""),tags$th(style="font-size:12px;color:#c0392b;padding:4px 8px;",t2))),
               tags$tbody(lapply(rows_list,function(row){
                 vh<-row$vh;va<-row$va;lbl<-row$lbl;hb<-if(is.null(row$hb))TRUE else row$hb
                 col_h<-if(hb){if(vh>va)"#27ae60" else if(vh<va)"#e74c3c" else "#888"}else{if(vh<va)"#27ae60" else if(vh>va)"#e74c3c" else "#888"}
                 col_a<-if(hb){if(va>vh)"#27ae60" else if(va<vh)"#e74c3c" else "#888"}else{if(va<vh)"#27ae60" else if(va>vh)"#e74c3c" else "#888"}
                 tags$tr(tags$td(style=paste0("text-align:right;font-weight:bold;color:",col_h,";padding:5px 8px;"),vh),tags$td(style="text-align:center;color:#aaa;font-size:11px;padding:5px 4px;",lbl),tags$td(style=paste0("font-weight:bold;color:",col_a,";padding:5px 8px;"),va))
               })))
  }
  
  output$comp_header <- renderUI({
    t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
    st1<-current_standings()%>%filter(team==t1);st2<-current_standings()%>%filter(team==t2)
    r1<-team_ratings()%>%filter(team==t1)%>%pull(rating);r2<-team_ratings()%>%filter(team==t2)%>%pull(rating)
    div(style="display:flex;justify-content:space-around;align-items:center;padding:10px 0;",
        div(style="text-align:center;",div(style="font-size:32px;font-weight:900;color:#1a6b8a;",paste0("#",st1$position)),div(style="font-size:16px;font-weight:bold;",t1),div(style="font-size:13px;color:#666;",paste0(st1$points," pts")),div(style="font-size:11px;background:#1a6b8a;color:white;border-radius:12px;padding:2px 10px;display:inline-block;margin-top:4px;",paste0("Rating: ",r1))),
        div(style="font-size:26px;color:#ccc;font-weight:300;","VS"),
        div(style="text-align:center;",div(style="font-size:32px;font-weight:900;color:#c0392b;",paste0("#",st2$position)),div(style="font-size:16px;font-weight:bold;",t2),div(style="font-size:13px;color:#666;",paste0(st2$points," pts")),div(style="font-size:11px;background:#c0392b;color:white;border-radius:12px;padding:2px 10px;display:inline-block;margin-top:4px;",paste0("Rating: ",r2))))
  
  })
output$comp_classif <- renderUI({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  s1<-current_standings()%>%filter(team==t1);s2<-current_standings()%>%filter(team==t2)
  make_comp_table(t1,t2,list(list(lbl="Posició",vh=s1$position,va=s2$position,hb=FALSE),list(lbl="Punts",vh=s1$points,va=s2$points),list(lbl="PJ",vh=s1$played,va=s2$played,hb=FALSE),list(lbl="Victòries",vh=s1$wins,va=s2$wins),list(lbl="Empats",vh=s1$draws,va=s2$draws,hb=FALSE),list(lbl="Derrotes",vh=s1$losses,va=s2$losses,hb=FALSE)))
})
output$comp_gols <- renderUI({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  gf<-function(tn){d<-team_match_stats()%>%filter(team==tn);list(gf=sum(d$goals_for),gc=sum(d$goals_against),n=nrow(d))}
  s1<-gf(t1);s2<-gf(t2)
  make_comp_table(t1,t2,list(list(lbl="Gols marcats",vh=s1$gf,va=s2$gf),list(lbl="Gols/partit",vh=round(s1$gf/s1$n,2),va=round(s2$gf/s2$n,2)),list(lbl="Gols rebuts",vh=s1$gc,va=s2$gc,hb=FALSE),list(lbl="Rebuts/partit",vh=round(s1$gc/s1$n,2),va=round(s2$gc/s2$n,2),hb=FALSE),list(lbl="Diferència",vh=s1$gf-s1$gc,va=s2$gf-s2$gc)))
})
output$comp_targetes <- renderUI({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  gc<-function(tn){d<-team_match_stats()%>%filter(team==tn);list(y=sum(d$yellow_cards),r=sum(d$red_cards),n=nrow(d))}
  s1<-gc(t1);s2<-gc(t2)
  make_comp_table(t1,t2,list(list(lbl="🟨 Grogues totals",vh=s1$y,va=s2$y,hb=FALSE),list(lbl="Grogues/partit",vh=round(s1$y/s1$n,2),va=round(s2$y/s2$n,2),hb=FALSE),list(lbl="🟥 Vermelles",vh=s1$r,va=s2$r,hb=FALSE)))
})
output$comp_tilt <- renderUI({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  r1<-tilt_data()%>%filter(team==t1)%>%pull(tilt);r2<-tilt_data()%>%filter(team==t2)%>%pull(tilt)
  v1<-if(length(r1)>0)r1 else NA;v2<-if(length(r2)>0)r2 else NA
  winner<-if(!is.na(v1)&&!is.na(v2)){if(v1>v2)paste0("Millor momentum: ",t1) else if(v2>v1)paste0("Millor momentum: ",t2) else "Momentum equivalent"} else ""
  tagList(div(style="display:flex;gap:32px;align-items:flex-start;flex-wrap:wrap;",
              div(tags$b(style="font-size:12px;color:#555;display:block;margin-bottom:6px;",t1),render_tilt_ui(t1,show_detail=TRUE)),
              div(tags$b(style="font-size:12px;color:#555;display:block;margin-bottom:6px;",t2),render_tilt_ui(t2,show_detail=TRUE))),
          if(winner!="") div(style="margin-top:10px;font-size:12px;color:#888;",paste0("⚡ ",winner)))
})
output$comp_radar <- renderPlotly({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  cats<-c("Atac","Defensa","Força Casa","Força Fora","Fair-Play","1a Part","2a Part")
  gv<-function(tn){r<-team_radar_data()%>%filter(team==tn);if(nrow(r)==0)return(rep(50,7));c(r$radar_attack,r$radar_defense,r$radar_home,r$radar_away,r$radar_fairplay,r$radar_first,r$radar_second)}
  v1<-gv(t1);v2<-gv(t2)
  plot_ly(type="scatterpolar")%>%add_trace(r=c(v1,v1[1]),theta=c(cats,cats[1]),fill="toself",name=t1,fillcolor="rgba(26,107,138,0.25)",line=list(color="rgba(26,107,138,1)",width=2))%>%add_trace(r=c(v2,v2[1]),theta=c(cats,cats[1]),fill="toself",name=t2,fillcolor="rgba(192,57,43,0.2)",line=list(color="rgba(192,57,43,1)",width=2))%>%layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,100),tickvals=c(0,25,50,75,100),gridcolor="lightgrey"),angularaxis=list(tickfont=list(size=11))),legend=list(orientation="h",y=-0.15),margin=list(t=20,b=40,l=40,r=40))
})
output$comp_punts_evolucio <- renderPlotly({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  d1<-team_match_stats()%>%filter(team==t1)%>%arrange(jornada)%>%mutate(punts_acum=cumsum(team_points),equip=t1)
  d2<-team_match_stats()%>%filter(team==t2)%>%arrange(jornada)%>%mutate(punts_acum=cumsum(team_points),equip=t2)
  data<-bind_rows(d1,d2)%>%mutate(equip=as.character(equip))
  if(nrow(data)==0) return(plotly_empty())
  p<-ggplot(data,aes(x=jornada,y=punts_acum,color=equip,group=equip,text=paste(equip,"· J",jornada,":",punts_acum,"pts")))+geom_line(linewidth=1.4)+geom_point(size=2)+scale_color_manual(values=setNames(c("#1a6b8a","#c0392b"),c(t1,t2)))+labs(x="Jornada",y="Punts Acumulats",color="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$comp_casa_fora <- renderPlotly({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  data<-bind_rows(team_match_stats()%>%filter(team==t1)%>%mutate(equip=t1),team_match_stats()%>%filter(team==t2)%>%mutate(equip=t2))%>%mutate(home_away=ifelse(home_away=="Home","Casa","Fora"))%>%group_by(equip,home_away)%>%summarise(avg_pts=mean(team_points),.groups="drop")
  p<-ggplot(data,aes(x=home_away,y=avg_pts,fill=equip,text=paste(equip,home_away,round(avg_pts,2),"pts")))+geom_col(position="dodge")+scale_fill_manual(values=setNames(c("#1a6b8a","#c0392b"),c(t1,t2)))+labs(x="",y="Punts Promig",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$comp_gols_minut <- renderPlotly({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  periodes<-c("0-15'","16-30'","31-45'","46-60'","61-75'","76-90'","90+'");br_v<-c(0,15,30,45,60,75,90,120)
  get_gols<-function(tn){all_matches_events()%>%filter(event_type=="Gol",team==tn,!is.na(minute))%>%mutate(periode=cut(minute,breaks=br_v,labels=periodes,include.lowest=TRUE))%>%count(periode,.drop=FALSE)%>%complete(periode=factor(periodes,levels=periodes),fill=list(n=0))}
  g1<-get_gols(t1);g2<-get_gols(t2);max_v<-max(max(g1$n),max(g2$n),1)
  plot_ly()%>%add_bars(x=-g1$n,y=periodes,orientation="h",name=t1,marker=list(color="rgba(26,107,138,0.8)"),customdata=g1$n,hovertemplate=paste0(t1," %{y}: %{customdata} gols<extra></extra>"))%>%add_bars(x=g2$n,y=periodes,orientation="h",name=t2,marker=list(color="rgba(192,57,43,0.8)"),hovertemplate=paste0(t2," %{y}: %{x} gols<extra></extra>"))%>%layout(barmode="overlay",xaxis=list(tickvals=seq(-max_v,max_v,by=1),ticktext=as.character(abs(seq(-max_v,max_v,by=1))),title="Gols",zeroline=TRUE,zerolinecolor="#333",zerolinewidth=2),yaxis=list(title="",categoryorder="array",categoryarray=rev(periodes)),legend=list(orientation="h",y=-0.15),margin=list(t=10,b=50,l=60,r=20))
})
output$comp_h2h <- renderUI({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  played<-matches()%>%filter(!is.na(goals_home))
  h2h<-played%>%filter((local_team==t1&away_team==t2)|(local_team==t2&away_team==t1))%>%
    mutate(gols_t1=ifelse(local_team==t1,goals_home,goals_away),gols_t2=ifelse(local_team==t1,goals_away,goals_home),loc_t1=ifelse(local_team==t1,"Casa","Fora"),resultat_t1=case_when(gols_t1>gols_t2~"V",gols_t1==gols_t2~"E",TRUE~"D"))%>%arrange(jornada)
  if(nrow(h2h)==0) return(div(style="padding:20px;color:#888;text-align:center;","Encara no s'han enfrontat en la competició actual"))
  wins_t1<-sum(h2h$resultat_t1=="V");draws<-sum(h2h$resultat_t1=="E");wins_t2<-sum(h2h$resultat_t1=="D")
  tagList(div(style="display:flex;justify-content:space-around;text-align:center;padding:12px 0;border-bottom:1px solid #eee;margin-bottom:10px;",
              div(div(style="font-size:28px;font-weight:900;color:#1a6b8a;",wins_t1),div(style="font-size:11px;color:#666;",t1)),
              div(div(style="font-size:28px;font-weight:900;color:#888;",draws),div(style="font-size:11px;color:#666;","Empats")),
              div(div(style="font-size:28px;font-weight:900;color:#c0392b;",wins_t2),div(style="font-size:11px;color:#666;",t2))),
          tags$table(style="width:100%;font-size:12px;",
                     tags$thead(tags$tr(tags$th("J"),tags$th("Loc"),tags$th(style="text-align:right;",t1),tags$th(""),tags$th(t2),tags$th("Res"))),
                     tags$tbody(lapply(seq_len(nrow(h2h)),function(i){
                       r<-h2h[i,];bg<-switch(r$resultat_t1,"V"="#f0fff4","E"="#fffde7","D"="#fff5f5")
                       tags$tr(style=paste0("background:",bg,";"),tags$td(r$jornada),tags$td(r$loc_t1),tags$td(style="font-weight:bold;text-align:right;",r$gols_t1),tags$td(style="text-align:center;color:#aaa;","-"),tags$td(style="font-weight:bold;",r$gols_t2),tags$td(style=paste0("font-weight:bold;color:",switch(r$resultat_t1,"V"="#27ae60","E"="#f39c12","D"="#e74c3c"),";"),r$resultat_t1))
                     }))))
})
output$comp_golejadors <- renderUI({
  t1<-req(input$comp_equip1);t2<-req(input$comp_equip2)
  get_top<-function(tn) player_stats()%>%filter(team==tn,goals>0)%>%arrange(desc(goals))%>%head(6)
  th<-get_top(t1);ta<-get_top(t2)
  make_col<-function(df,tn,align="right") tagList(tags$b(style="font-size:12px;",tn),if(nrow(df)==0) tags$p(style="color:#aaa;font-size:11px;","Sense gols") else tags$table(style="width:100%;",lapply(seq_len(nrow(df)),function(i){r<-df[i,];if(align=="right")tags$tr(tags$td(style="text-align:right;font-size:12px;padding:2px 4px;",HTML(player_link_js(r$player))),tags$td(style="font-size:13px;font-weight:bold;color:#27ae60;padding:2px 4px;",paste0("⚽ ",r$goals))) else tags$tr(tags$td(style="font-size:13px;font-weight:bold;color:#27ae60;padding:2px 4px;",paste0("⚽ ",r$goals)),tags$td(style="font-size:12px;padding:2px 4px;",HTML(player_link_js(r$player))))})))
  div(style="display:flex;gap:16px;",div(style="flex:1;",make_col(th,t1,"right")),div(style="width:1px;background:#eee;"),div(style="flex:1;",make_col(ta,t2,"left")))
})

# ==========================================================================
# JUGADORS
# ==========================================================================
output$taula_jugadors <- renderDT({
  data<-player_stats()%>%left_join(player_ratings()%>%select(player,team,rating_global),by=c("player","team"))%>%arrange(desc(rating_global))%>%mutate(Jugador=sapply(player,player_link_js))%>%select(Jugador,Equip=team,Rating=rating_global,Partits=matches_played,Titularitats=starts,Minuts=total_minutes,Gols=goals,`Gols/90`=goals_per_90,`Targetes/90`=cards_per_90)
  datatable(data,escape=FALSE,filter="top",rownames=FALSE,options=list(pageLength=20,scrollX=TRUE,order=list(list(2,'desc'))))
})

get_jug_data <- function(jname) list(
  stats=player_stats()%>%filter(player==jname),
  match=player_match_stats()%>%filter(player==jname)%>%arrange(jornada),
  events=all_matches_events()%>%filter(player==jname),
  impact=player_impact()%>%filter(player==jname),
  radar=player_radar_data()%>%filter(player==jname),
  rating=player_ratings()%>%filter(player==jname)%>%pull(rating_global)
)

output$compjug_header <- renderUI({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  d1<-get_jug_data(j1);d2<-get_jug_data(j2)
  s1<-d1$stats;s2<-d2$stats
  r1<-if(length(d1$rating)>0)d1$rating else "N/A";r2<-if(length(d2$rating)>0)d2$rating else "N/A"
  make_pill<-function(val,col) div(style=paste0("font-size:11px;background:",col,";color:white;border-radius:12px;padding:2px 10px;display:inline-block;margin-top:4px;"),paste0("Rating: ",val))
  div(style="display:flex;justify-content:space-around;align-items:center;padding:10px 0;",
      div(style="text-align:center;",div(style="font-size:20px;font-weight:bold;",j1),div(style="font-size:12px;color:#666;",if(nrow(s1)>0)s1$team else ""),make_pill(r1,"#1a6b8a")),
      div(style="font-size:24px;color:#ccc;font-weight:300;","VS"),
      div(style="text-align:center;",div(style="font-size:20px;font-weight:bold;",j2),div(style="font-size:12px;color:#666;",if(nrow(s2)>0)s2$team else ""),make_pill(r2,"#c0392b")))
})
output$compjug_radar <- renderPlotly({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  cats<-c("Gols/90","Impacte","Titularitat","Minuts","Fair-Play")
  gv<-function(jname){r<-player_radar_data()%>%filter(player==jname);if(nrow(r)==0)return(rep(50,5));c(r$radar_gol,r$radar_impacte,r$radar_titularitat,r$radar_minuts,r$radar_fairplay)}
  v1<-gv(j1);v2<-gv(j2)
  plot_ly(type="scatterpolar")%>%add_trace(r=c(v1,v1[1]),theta=c(cats,cats[1]),fill="toself",name=j1,fillcolor="rgba(26,107,138,0.25)",line=list(color="rgba(26,107,138,1)",width=2))%>%add_trace(r=c(v2,v2[1]),theta=c(cats,cats[1]),fill="toself",name=j2,fillcolor="rgba(192,57,43,0.2)",line=list(color="rgba(192,57,43,1)",width=2))%>%layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,100),tickvals=c(0,25,50,75,100),gridcolor="lightgrey"),angularaxis=list(tickfont=list(size=11))),legend=list(orientation="h",y=-0.15),margin=list(t=10,b=40,l=30,r=30))
})
output$compjug_stats_taula <- renderUI({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  d1<-get_jug_data(j1);d2<-get_jug_data(j2)
  s1<-d1$stats;s2<-d2$stats
  if(nrow(s1)==0||nrow(s2)==0) return(p("Dades no disponibles"))
  i1<-d1$impact;i2<-d2$impact
  imp1<-if(nrow(i1)>0&&!is.na(i1$impacte))round(i1$impacte,2) else NA
  imp2<-if(nrow(i2)>0&&!is.na(i2$impacte))round(i2$impacte,2) else NA
  rows<-list(list(lbl="Equip",vh=s1$team,va=s2$team,hb=NULL),list(lbl="Partits",vh=s1$matches_played,va=s2$matches_played),list(lbl="Titularitats",vh=s1$starts,va=s2$starts),list(lbl="Minuts totals",vh=s1$total_minutes,va=s2$total_minutes),list(lbl="Gols",vh=s1$goals,va=s2$goals),list(lbl="Gols/90'",vh=round(s1$goals_per_90,2),va=round(s2$goals_per_90,2)),list(lbl="Targetes/90'",vh=round(s1$cards_per_90,2),va=round(s2$cards_per_90,2),hb=FALSE),list(lbl="Impacte",vh=if(is.na(imp1))"N/A" else imp1,va=if(is.na(imp2))"N/A" else imp2))
  make_comp_table(j1,j2,rows)
})
output$compjug_gols_evolucio <- renderPlotly({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  d1<-player_match_stats()%>%filter(as.character(player)==j1)%>%arrange(jornada)%>%mutate(gols_acum=cumsum(goals),jugador=j1)
  d2<-player_match_stats()%>%filter(as.character(player)==j2)%>%arrange(jornada)%>%mutate(gols_acum=cumsum(goals),jugador=j2)
  data<-bind_rows(d1,d2)%>%mutate(jugador=as.character(jugador))
  if(nrow(data)==0) return(plotly_empty())
  p<-ggplot(data,aes(x=jornada,y=gols_acum,color=jugador,group=jugador,text=paste(jugador,"· J",jornada,":",gols_acum,"gols")))+geom_line(linewidth=1.3)+geom_point(size=2)+scale_color_manual(values=setNames(c("#1a6b8a","#c0392b"),c(j1,j2)))+labs(x="Jornada",y="Gols acumulats",color="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$compjug_minuts <- renderPlotly({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  d1<-player_match_stats()%>%filter(player==j1)%>%arrange(jornada)%>%mutate(jugador=j1)
  d2<-player_match_stats()%>%filter(player==j2)%>%arrange(jornada)%>%mutate(jugador=j2)
  data<-bind_rows(d1,d2); if(nrow(data)==0) return(plotly_empty())
  p<-ggplot(data,aes(x=jornada,y=minutes_played,fill=jugador,text=paste(jugador,"· J",jornada,":",minutes_played,"min")))+geom_col(position="dodge")+scale_fill_manual(values=setNames(c("#1a6b8a","#c0392b"),c(j1,j2)))+labs(x="Jornada",y="Minuts",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$compjug_timing <- renderPlotly({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  periodes<-c("0-15'","16-30'","31-45'","46-60'","61-75'","76-90'","90+'");br_v<-c(0,15,30,45,60,75,90,120)
  get_timing<-function(jname){all_matches_events()%>%filter(event_type=="Gol",player==jname,!is.na(minute))%>%mutate(periode=cut(minute,breaks=br_v,labels=periodes,include.lowest=TRUE))%>%count(periode,.drop=FALSE)%>%complete(periode=factor(periodes,levels=periodes),fill=list(n=0))%>%mutate(jugador=jname)}
  data<-bind_rows(get_timing(j1),get_timing(j2)); if(sum(data$n)==0) return(plotly_empty()%>%layout(title="Sense gols registrats"))
  p<-ggplot(data,aes(x=periode,y=n,fill=jugador,text=paste(jugador,"·",periode,":",n,"gols")))+geom_col(position="dodge")+scale_fill_manual(values=setNames(c("#1a6b8a","#c0392b"),c(j1,j2)))+labs(x="Període",y="Gols",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$compjug_impacte <- renderPlotly({
  j1<-req(input$comp_jug1);j2<-req(input$comp_jug2)
  get_imp<-function(jname){imp<-player_impact()%>%filter(player==jname);if(nrow(imp)==0||is.na(imp$impacte))return(NULL);data.frame(jugador=jname,situacio=c("Jugant","Sense jugar"),pts=c(imp$avg_points_when_playing,imp$avg_points_when_not_playing))}
  data<-bind_rows(get_imp(j1),get_imp(j2)); if(nrow(data)==0) return(plotly_empty()%>%layout(title="Dades insuficients"))
  p<-ggplot(data,aes(x=jugador,y=pts,fill=situacio,text=paste(jugador,"-",situacio,":",round(pts,2),"pts")))+geom_col(position="dodge")+scale_fill_manual(values=c("Jugant"="#27ae60","Sense jugar"="#e74c3c"))+geom_text(aes(label=round(pts,2)),position=position_dodge(width=0.9),vjust=-0.4,size=3)+labs(x="",y="Punts promig equip",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})

# ==========================================================================
# ESTADÍSTIQUES
# ==========================================================================
output$stats_total_gols <- renderValueBox(valueBox(sum(all_matches_events()$event_type=="Gol"),"Gols Totals",icon("futbol"),color="green"))
output$stats_avg_gols <- renderValueBox({
  n<-nrow(matches()%>%filter(!is.na(goals_home)))
  avg<-round(sum(all_matches_events()$event_type=="Gol")/n,2)
  valueBox(avg,"Gols per Partit",icon("chart-bar"),color="blue")
})
output$stats_max_gols <- renderValueBox({
  mg<-matches()%>%filter(!is.na(goals_home))%>%mutate(t=goals_home+goals_away)%>%summarise(m=max(t))%>%pull(m)
  valueBox(mg,"Màx. Gols en un Partit",icon("star"),color="orange")
})
output$stats_total_targetes <- renderValueBox({
  t<-sum(player_match_stats()$yellow_cards)+sum(player_match_stats()$red_cards)
  valueBox(t,"Targetes Totals",icon("square"),color="yellow")
})

output$stats_gols_minut <- renderPlotly({
  data <- all_matches_events()%>%filter(event_type=="Gol",!is.na(minute))
  if(nrow(data)==0) return(plotly_empty())
  p <- ggplot(data,aes(x=minute))+
    geom_histogram(binwidth=5,fill="steelblue",color="white")+
    geom_vline(xintercept=45,linetype="dashed",color="red")+
    scale_x_continuous(breaks=seq(0,120,by=15), limits=c(0,120))+
    labs(x="Minut",y="Gols")+theme_minimal()
  ggplotly(p)
})
output$stats_gols_jornada <- renderPlotly({
  data <- team_match_stats()%>%group_by(jornada)%>%summarise(gols=sum(goals_for,na.rm=TRUE),.groups="drop")%>%mutate(tooltip=paste0("J",jornada,": ",gols," gols"))
  if(nrow(data)==0) return(plotly_empty())
  avg <- round(mean(data$gols),1)
  n_jornades <- nrow(data)
  plot_ly(data,x=~jornada,y=~gols,type="bar",
          marker=list(color="#3498db",opacity=0.8),
          text=~tooltip,hoverinfo="text",textposition="none",showlegend=FALSE,
          width=NULL)%>%
    add_lines(x=~jornada,y=avg,line=list(color="#e74c3c",width=2,dash="dash"),
              name=paste0("Mitjana: ",avg),hoverinfo="none")%>%
    layout(xaxis=list(title="Jornada",tickmode="linear",tick0=1,dtick=max(1,floor(n_jornades/15))),
           yaxis=list(title="Gols totals",zeroline=FALSE),
           bargap=0.3,
           plot_bgcolor="#fafafa",paper_bgcolor="white",
           showlegend=TRUE,legend=list(x=0.75,y=1),
           margin=list(t=10,b=40,l=50,r=10))
})
output$stats_targetes_jornada <- renderPlotly({
  data <- team_match_stats()%>%group_by(jornada)%>%summarise(grogues=sum(yellow_cards,na.rm=TRUE),vermelles=sum(red_cards,na.rm=TRUE),.groups="drop")
  if(nrow(data)==0) return(plotly_empty())
  n_jornades <- nrow(data)
  plot_ly(data,x=~jornada)%>%
    add_bars(y=~grogues,name="Grogues",marker=list(color="#f39c12"),text=~paste0("J",jornada,": ",grogues," grogues"),hoverinfo="text",textposition="none")%>%
    add_bars(y=~vermelles,name="Vermelles",marker=list(color="#e74c3c"),text=~paste0("J",jornada,": ",vermelles," vermelles"),hoverinfo="text",textposition="none")%>%
    layout(barmode="stack",bargap=0.3,
           xaxis=list(title="Jornada",tickmode="linear",tick0=1,dtick=max(1,floor(n_jornades/15))),
           yaxis=list(title="Targetes",zeroline=FALSE),
           plot_bgcolor="#fafafa",paper_bgcolor="white",
           legend=list(x=0.75,y=1),margin=list(t=10,b=40,l=50,r=10))
})
output$stats_resultats_comuns <- renderPlotly({
  data<-matches()%>%filter(!is.na(goals_home))%>%mutate(resultat=paste0(goals_home,"-",goals_away))%>%count(resultat,name="freq")%>%arrange(desc(freq))%>%head(12)%>%
    mutate(color=case_when(as.integer(sub("-.*","",resultat))>as.integer(sub(".*-","",resultat))~"#27ae60",as.integer(sub("-.*","",resultat))<as.integer(sub(".*-","",resultat))~"#e74c3c",TRUE~"#95a5a6"),tooltip=paste0(resultat,": ",freq," vegades"))
  plot_ly(data,x=~reorder(resultat,freq),y=~freq,type="bar",marker=list(color=~color,line=list(color="white",width=1)),text=~tooltip,hoverinfo="text",textposition="none",showlegend=FALSE)%>%layout(xaxis=list(title="Resultat",tickfont=list(size=11)),yaxis=list(title="Freqüència",zeroline=FALSE),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=50,l=50,r=10))
})
output$stats_gols_equip <- renderPlotly({
  data<-team_match_stats()%>%group_by(team)%>%summarise(GF=sum(goals_for),GC=sum(goals_against),.groups="drop")%>%pivot_longer(c(GF,GC),names_to="tipus",values_to="gols")
  p<-ggplot(data,aes(x=reorder(team,gols),y=gols,fill=tipus,text=paste(team,"·",tipus,":",gols)))+geom_col(position="dodge")+scale_fill_manual(values=c("GF"="green","GC"="red"))+coord_flip()+labs(x="",y="Gols",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$stats_casa_fora <- renderPlotly({
  data<-team_match_stats()%>%mutate(home_away=ifelse(home_away=="Home","Casa","Fora"))%>%group_by(team,home_away)%>%summarise(avg_pts=mean(team_points),.groups="drop")
  p<-ggplot(data,aes(x=reorder(team,avg_pts),y=avg_pts,fill=home_away,text=paste(team,"·",home_away,":",round(avg_pts,2))))+geom_col(position="dodge")+scale_fill_manual(values=c("Casa"="darkgreen","Fora"="steelblue"))+coord_flip()+labs(x="",y="Punts Promig",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$stats_targetes <- renderPlotly({
  data<-team_match_stats()%>%group_by(team)%>%summarise(Grogues=sum(yellow_cards),Vermelles=sum(red_cards),.groups="drop")%>%pivot_longer(c(Grogues,Vermelles),names_to="tipus",values_to="n")
  p<-ggplot(data,aes(x=reorder(team,n),y=n,fill=tipus,text=paste(team,"·",tipus,":",n)))+geom_col(position="stack")+scale_fill_manual(values=c("Grogues"="#f39c12","Vermelles"="#e74c3c"))+coord_flip()+labs(x="",y="Targetes",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$stats_targetes_punts <- renderPlotly({
  data<-team_match_stats()%>%group_by(team)%>%summarise(targetes=sum(yellow_cards,na.rm=TRUE)+3*sum(red_cards,na.rm=TRUE),punts=sum(team_points,na.rm=TRUE),.groups="drop")%>%left_join(current_standings()%>%select(team,position),by="team")%>%mutate(color=colorRampPalette(c("#27ae60","#f39c12","#e74c3c"))(n())[rank(targetes)],tooltip=paste0("<b>",team,"</b><br>Targetes (pond.): ",targetes,"<br>Punts: ",punts))
  fit<-lm(punts~targetes,data=data);xr<-seq(min(data$targetes),max(data$targetes),length.out=50);yr<-predict(fit,newdata=data.frame(targetes=xr));trend_df<-data.frame(x=xr,y=yr)
  plot_ly()%>%add_markers(data=data,x=~targetes,y=~punts,text=~tooltip,hoverinfo="text",marker=list(size=14,color=~color,line=list(color="white",width=1.5)),showlegend=FALSE)%>%add_lines(data=trend_df,x=~x,y=~y,line=list(color="#aaa",width=1.5,dash="dot"),hoverinfo="none",showlegend=FALSE)%>%add_annotations(data=data,x=~targetes,y=~punts,text=~team,showarrow=FALSE,yshift=11,font=list(size=8,color="#555"),bgcolor="rgba(255,255,255,0.7)",borderpad=1)%>%layout(xaxis=list(title="Targetes (grogues + 3×vermelles)",zeroline=FALSE),yaxis=list(title="Punts totals",zeroline=FALSE),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=50,l=55,r=10))
})
output$stats_ranking_atac <- renderPlotly({
  data<-team_match_stats()%>%group_by(team)%>%summarise(gols_favor=sum(goals_for,na.rm=TRUE),gols_pg=round(mean(goals_for,na.rm=TRUE),2),.groups="drop")%>%arrange(desc(gols_favor))%>%mutate(color=colorRampPalette(c("#27ae60","#a8e6cf"))(n()),tooltip=paste0("<b>",team,"</b><br>",gols_favor," gols marcats<br>",gols_pg," gols/partit"))
  plot_ly(data,x=~gols_favor,y=~reorder(team,gols_favor),type="bar",orientation="h",marker=list(color=~color,line=list(color="white",width=0.5)),text=~tooltip,hoverinfo="text",textposition="none",showlegend=FALSE)%>%layout(xaxis=list(title="Gols marcats",zeroline=FALSE),yaxis=list(title="",tickfont=list(size=8)),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=40,l=170,r=40))
})
output$stats_ranking_defensa <- renderPlotly({
  data<-team_match_stats()%>%group_by(team)%>%summarise(gols_contra=sum(goals_against,na.rm=TRUE),gols_pg=round(mean(goals_against,na.rm=TRUE),2),.groups="drop")%>%arrange(gols_contra)%>%mutate(color=colorRampPalette(c("#2980b9","#aed6f1"))(n()),tooltip=paste0("<b>",team,"</b><br>",gols_contra," gols encaixats<br>",gols_pg," gols/partit"))
  plot_ly(data,x=~gols_contra,y=~reorder(team,-gols_contra),type="bar",orientation="h",marker=list(color=~color,line=list(color="white",width=0.5)),text=~tooltip,hoverinfo="text",textposition="none",showlegend=FALSE)%>%layout(xaxis=list(title="Gols encaixats",zeroline=FALSE),yaxis=list(title="",tickfont=list(size=8)),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=40,l=170,r=40))
})
output$stats_rating_tilt <- renderPlotly({
  data<-team_ratings()%>%left_join(tilt_data()%>%select(team,tilt,pts_real_avg,pts_expected_avg),by="team")%>%left_join(current_standings()%>%select(team,position,points),by="team")%>%
    mutate(tilt_label=sprintf("%+.2f",tilt),dinamica=case_when(tilt>0.5~"Molt bona dinàmica",tilt>0.1~"Bona dinàmica",tilt>-0.1~"Dinàmica neutra",tilt>-0.5~"Mala dinàmica",TRUE~"Molt mala dinàmica"),tooltip=paste0("<b>",team,"</b><br>Posició: #",position,"  ·  Punts: ",points,"<br>Rating: ",rating,"<br>Tilt: ",tilt_label,"  →  ",dinamica),quadrant=case_when(rating>=50&tilt>=0~"Alt Rating · Bon Momentum",rating>=50&tilt<0~"Alt Rating · Mal Momentum",rating<50&tilt>=0~"Baix Rating · Bon Momentum",TRUE~"Baix Rating · Mal Momentum"),color_q=case_when(quadrant=="Alt Rating · Bon Momentum"~"#27ae60",quadrant=="Alt Rating · Mal Momentum"~"#e67e22",quadrant=="Baix Rating · Bon Momentum"~"#3498db",TRUE~"#e74c3c"))
  x_min<-min(data$tilt,na.rm=TRUE)-0.15;x_max<-max(data$tilt,na.rm=TRUE)+0.15;y_min<-min(data$rating,na.rm=TRUE)-5;y_max<-max(data$rating,na.rm=TRUE)+5
  plot_ly(data,x=~tilt,y=~rating,text=~tooltip,hoverinfo="text",type="scatter",mode="markers",marker=list(size=20,color=~color_q,line=list(color="white",width=2),opacity=0.88),showlegend=FALSE)%>%add_annotations(x=~tilt,y=~rating,text=~team,showarrow=FALSE,yshift=14,font=list(size=9,color="#333"),bgcolor="rgba(255,255,255,0.75)",borderpad=2)%>%layout(shapes=list(list(type="line",x0=0,x1=0,y0=y_min,y1=y_max,line=list(color="#bbb",width=1,dash="dot")),list(type="line",x0=x_min,x1=x_max,y0=50,y1=50,line=list(color="#bbb",width=1,dash="dot"))),xaxis=list(title="Tilt (momentum recent)",zeroline=FALSE,tickformat="+.2f",range=c(x_min,x_max)),yaxis=list(title="Rating Global",zeroline=FALSE,range=c(y_min,y_max)),annotations=list(list(x=x_max,y=y_max,text="Favorits en forma",showarrow=FALSE,font=list(size=10,color="#27ae60"),xanchor="right",yanchor="top"),list(x=x_min,y=y_max,text="Favorits en crisi",showarrow=FALSE,font=list(size=10,color="#e67e22"),xanchor="left",yanchor="top"),list(x=x_max,y=y_min,text="Underdog en forma",showarrow=FALSE,font=list(size=10,color="#3498db"),xanchor="right",yanchor="bottom"),list(x=x_min,y=y_min,text="Cua en crisi",showarrow=FALSE,font=list(size=10,color="#e74c3c"),xanchor="left",yanchor="bottom")),margin=list(t=20,b=60,l=60,r=20),plot_bgcolor="#fafafa",paper_bgcolor="white")
})
output$stats_latents_atac_defensa <- renderPlotly({
  data<-team_ratings()%>%left_join(current_standings()%>%select(team,position,points),by="team")%>%
    mutate(quadrant=case_when(attack>=0&defense>=0~"Bons en atac i defensa",attack>=0&defense<0~"Bon atac, mala defensa",attack<0&defense>=0~"Mal atac, bona defensa",TRUE~"Febles en atac i defensa"),color_q=case_when(quadrant=="Bons en atac i defensa"~"#27ae60",quadrant=="Bon atac, mala defensa"~"#e67e22",quadrant=="Mal atac, bona defensa"~"#3498db",TRUE~"#e74c3c"),tooltip=paste0("<b>",team,"</b><br>Posició: #",position,"  ·  Punts: ",points,"<br>Atac: ",sprintf("%+.3f",attack),"<br>Defensa: ",sprintf("%+.3f",defense),"<br>Partits: ",n_matches,"<br>",quadrant))
  x_min<-min(data$attack,na.rm=TRUE)*1.15;x_max<-max(data$attack,na.rm=TRUE)*1.15;y_min<-min(data$defense,na.rm=TRUE)*1.15;y_max<-max(data$defense,na.rm=TRUE)*1.15
  plot_ly(data,x=~attack,y=~defense,text=~tooltip,hoverinfo="text",type="scatter",mode="markers",marker=list(size=~pmin(30,pmax(12,n_matches*1.5)),color=~color_q,line=list(color="white",width=2),opacity=0.85,sizemode="diameter"),showlegend=FALSE)%>%add_annotations(x=~attack,y=~defense,text=~team,showarrow=FALSE,yshift=14,font=list(size=9,color="#333"),bgcolor="rgba(255,255,255,0.75)",borderpad=2)%>%layout(shapes=list(list(type="line",x0=0,x1=0,y0=y_min,y1=y_max,line=list(color="#bbb",width=1,dash="dot")),list(type="line",x0=x_min,x1=x_max,y0=0,y1=0,line=list(color="#bbb",width=1,dash="dot"))),annotations=list(list(x=x_max,y=y_max,text="Millor de tot",showarrow=FALSE,font=list(size=10,color="#27ae60"),xanchor="right",yanchor="top"),list(x=x_max,y=y_min,text="Bon atac / mala def.",showarrow=FALSE,font=list(size=10,color="#e67e22"),xanchor="right",yanchor="bottom"),list(x=x_min,y=y_max,text="Mal atac / bona def.",showarrow=FALSE,font=list(size=10,color="#3498db"),xanchor="left",yanchor="top"),list(x=x_min,y=y_min,text="Pitjor de tot",showarrow=FALSE,font=list(size=10,color="#e74c3c"),xanchor="left",yanchor="bottom")),xaxis=list(title="Força d'Atac (positiu = millor que la mitjana)",zeroline=FALSE,tickformat="+.1f",dtick=0.2,range=c(x_min,x_max)),yaxis=list(title="Força de Defensa (positiu = menys gols rebuts)",zeroline=FALSE,tickformat="+.1f",dtick=0.2,range=c(y_min,y_max)),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=20,b=60,l=70,r=20))
})
output$stats_top_golejadors <- renderPlotly({
  top_scorers_total <- player_stats()%>%arrange(desc(goals))%>%head(10)
  if(nrow(top_scorers_total)==0) return(plotly_empty())
  top_names <- as.character(top_scorers_total$player)
  top_scorers_evolution <- player_match_stats()%>%
    filter(as.character(player)%in%top_names)%>%
    mutate(player=as.character(player))%>%
    arrange(player,jornada)%>%group_by(player)%>%
    mutate(goals_cumsum=cumsum(goals))%>%ungroup()
  if(nrow(top_scorers_evolution)==0) return(plotly_empty())
  p <- ggplot(top_scorers_evolution,aes(x=jornada,y=goals_cumsum,color=player,group=player,
                                        text=paste(player,"<br>Jornada:",jornada,"<br>Gols:",goals_cumsum)))+
    geom_line(linewidth=1.2)+geom_point(size=2)+
    labs(x="Jornada",y="Gols acumulats",color="Jugador")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$stats_top_targetes <- renderPlotly({
  data <- player_match_stats()%>%group_by(player,team)%>%
    summarise(Grogues=sum(yellow_cards),Vermelles=sum(red_cards),Total=Grogues+Vermelles*2,.groups="drop")%>%
    arrange(desc(Total))%>%head(10)%>%pivot_longer(c(Grogues,Vermelles),names_to="tipus",values_to="n")
  if(nrow(data)==0) return(plotly_empty())
  p <- ggplot(data,aes(x=reorder(player,Total),y=n,fill=tipus,text=paste(player,"·",tipus,":",n)))+
    geom_col(position="stack")+scale_fill_manual(values=c("Grogues"="#f39c12","Vermelles"="#e74c3c"))+
    coord_flip()+labs(x="",y="Targetes",fill="")+theme_minimal()
  ggplotly(p,tooltip="text")
})
output$stats_gols_minuts_reg <- renderPlotly({
  data<-player_stats()%>%filter(goals>=2,total_minutes>0)%>%left_join(player_ratings()%>%select(player,rating_global),by="player")%>%mutate(rating_global=replace_na(rating_global,50),color=colorRampPalette(c("#3498db","#e74c3c"))(100)[pmin(100,pmax(1,round(rating_global)))],tooltip=paste0("<b>",player,"</b><br>Equip: ",team,"<br>",goals," gols · ",total_minutes," min<br>",round(goals/total_minutes*90,2)," gols/90 min"))
  if(nrow(data)<2) return(plotly_empty())
  fit<-lm(goals~total_minutes,data=data);xr<-seq(min(data$total_minutes),max(data$total_minutes),length.out=60);yr<-predict(fit,newdata=data.frame(total_minutes=xr));trend_df<-data.frame(x=xr,y=yr)
  plot_ly()%>%add_markers(data=data,x=~total_minutes,y=~goals,text=~tooltip,hoverinfo="text",marker=list(size=12,color=~color,line=list(color="white",width=1.5)),showlegend=FALSE)%>%add_lines(data=trend_df,x=~x,y=~y,line=list(color="#e74c3c",width=2,dash="dash"),hoverinfo="none",showlegend=FALSE)%>%layout(xaxis=list(title="Minuts jugats",zeroline=FALSE),yaxis=list(title="Gols",zeroline=FALSE,dtick=1),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=50,l=50,r=10))
})
output$stats_minuts_per_gol <- renderPlotly({
  data<-player_stats()%>%filter(goals>=2,total_minutes>0)%>%mutate(min_per_gol=round(total_minutes/goals,1),tooltip=paste0("<b>",player,"</b><br>Equip: ",team,"<br>",goals," gols · ",total_minutes," min<br>1 gol cada ",round(total_minutes/goals,1)," min"))%>%arrange(min_per_gol)%>%head(15)
  plot_ly(data,x=~min_per_gol,y=~reorder(player,-min_per_gol),type="bar",orientation="h",marker=list(color=colorRampPalette(c("#27ae60","#f39c12","#e74c3c"))(nrow(data)),line=list(color="white",width=0.5)),text=~tooltip,hoverinfo="text",textposition="none",showlegend=FALSE)%>%layout(xaxis=list(title="Minuts per gol",zeroline=FALSE),yaxis=list(title="",tickfont=list(size=8)),plot_bgcolor="#fafafa",paper_bgcolor="white",margin=list(t=10,b=40,l=160,r=60))
})

# ==========================================================================
# SIMULADOR
# ==========================================================================

# Player ratings calculats pel simulador (de tot el grup, per fer lineup)
sim_pr <- reactive({
  pr <- player_ratings()
  if (nrow(pr) == 0) return(pr)
  # Normalitzar rating_global 0-100 → escala ~1 per al simulador
  rng_min <- min(pr$rating_global, na.rm=TRUE)
  rng_max <- max(pr$rating_global, na.rm=TRUE)
  pr %>% mutate(
    rating_global = if(is.finite(rng_min)&&is.finite(rng_max)&&rng_max>rng_min)
      (rating_global - rng_min)/(rng_max - rng_min) * 0.8 + 0.6
    else rep(1.0, n())
  )
})

sim_lat <- reactive({ team_ratings() })

output$sim_equip_home_ui <- renderUI({
  selectInput("sim_home", "🏠 Equip Local:", choices = all_teams(),
              selected = all_teams()[1])
})
output$sim_equip_away_ui <- renderUI({
  teams <- all_teams()
  selectInput("sim_away", "✈️ Equip Visitant:", choices = teams,
              selected = teams[min(2, length(teams))])
})

# Resultats de la simulació (reactiveVal per guardar)
sim_results_val    <- reactiveVal(NULL)
sim_single_val     <- reactiveVal(NULL)
sim_single_ind_val <- reactiveVal(NULL)
sim_running_val    <- reactiveVal(FALSE)
sim_mode_val       <- reactiveVal("none")   # "none" | "single" | "montecarlo"

# --- SIMULACIÓ INDIVIDUAL (1 PARTIT) ---
observeEvent(input$sim_run_single, {
  req(input$sim_home, input$sim_away)
  home <- input$sim_home; away <- input$sim_away
  pr   <- sim_pr(); lat  <- sim_lat()
  if (nrow(pr) == 0 || nrow(lat) == 0) return()
  lu_h <- get_lineup_fn(home, pr, 11)
  lu_a <- get_lineup_fn(away, pr, 11)
  if (length(lu_h) < 5 || length(lu_a) < 5) return()
  set.seed(as.integer(Sys.time()) %% 100000)
  single <- simulate_one_match(home, away, lat, pr, lu_h, lu_a, sim_params)
  single$home <- home; single$away <- away
  single$lu_h <- lu_h; single$lu_a <- lu_a
  sim_single_ind_val(single)
  sim_mode_val("single")
})

observeEvent(input$sim_run, {
  req(input$sim_home, input$sim_away)
  home <- input$sim_home; away <- input$sim_away
  n    <- input$sim_n_sims
  pr   <- sim_pr(); lat  <- sim_lat()
  
  if (nrow(pr) == 0 || nrow(lat) == 0) {
    sim_results_val(NULL); return()
  }
  
  # Obtenir lineups
  lu_h <- get_lineup_fn(home, pr, 11)
  lu_a <- get_lineup_fn(away, pr, 11)
  
  if (length(lu_h) < 5 || length(lu_a) < 5) {
    sim_results_val(NULL); return()
  }
  
  sim_running_val(TRUE)
  
  # Monte Carlo
  withProgress(message = paste0("Simulant ", n, " partits..."), value = 0, {
    mc_list <- lapply(seq_len(n), function(i) {
      if (i %% 50 == 0) incProgress(50/n)
      set.seed(i + sample(1e6, 1))
      r <- simulate_one_match(home, away, lat, pr, lu_h, lu_a, sim_params)
      if (is.null(r)) return(NULL)
      data.frame(score_h=r$score_h, score_a=r$score_a, result=r$result,
                 n_goals=r$n_goals_h+r$n_goals_a,
                 n_yellows=r$n_yellows_h+r$n_yellows_a,
                 n_reds=r$n_reds_h+r$n_reds_a, stringsAsFactors=FALSE)
    })
  })
  
  mc_df <- do.call(rbind, Filter(Negate(is.null), mc_list))
  sim_results_val(list(df=mc_df, home=home, away=away, lu_h=lu_h, lu_a=lu_a))
  
  # Un partit exemple per l'acta del MC
  set.seed(999)
  single <- simulate_one_match(home, away, lat, pr, lu_h, lu_a, sim_params, verbose=FALSE)
  sim_single_val(single)
  
  sim_running_val(FALSE)
  sim_mode_val("montecarlo")
})

output$sim_running_msg <- renderUI({
  if (sim_running_val()) {
    div(style="color:#e67e22;font-weight:bold;padding:6px 0;",
        tags$i(class="fa fa-spinner fa-spin"), " Simulant Monte Carlo, un moment...")
  } else if (sim_mode_val() == "none") {
    div(style="color:#aaa;font-size:12px;padding:6px 0;",
        "👆 Selecciona els equips i prem un botó per iniciar la simulació.")
  } else {
    NULL
  }
})

# ==========================================================================
# PANEL DE RESULTATS DINÀMIC (apareix/canvia segons el mode)
# ==========================================================================
output$sim_results_panel <- renderUI({
  mode <- sim_mode_val()
  if (mode == "none" || sim_running_val()) return(NULL)
  
  if (mode == "single") {
    single <- sim_single_ind_val()
    if (is.null(single)) return(NULL)
    pr <- sim_pr()
    lu_h <- single$lu_h; lu_a <- single$lu_a
    home <- single$home; away <- single$away
    
    # Scoreboard visual
    res_txt  <- if (single$score_h > single$score_a) paste0("Victòria ", home)
    else if (single$score_h < single$score_a) paste0("Victòria ", away)
    else "Empat"
    res_col  <- if (single$score_h > single$score_a) "#27ae60"
    else if (single$score_h < single$score_a) "#e74c3c"
    else "#f39c12"
    
    tagList(
      # Marcador
      fluidRow(
        column(12,
               div(class="sim-scoreboard",
                   div(class="sim-teams", paste(home, "vs", away)),
                   div(class="sim-score",
                       tags$span(style="color:rgba(255,255,255,.85);", single$score_h),
                       tags$span(style="font-size:40px;opacity:.5; margin:0 12px;", "-"),
                       tags$span(style="color:rgba(255,255,255,.85);", single$score_a)),
                   div(class="sim-status",
                       tags$span(style=paste0("color:",res_col,";font-weight:900;font-size:14px;"), res_txt),
                       tags$span(style="opacity:.6; margin: 0 10px;", "·"),
                       paste0("🟨 ", single$n_yellows_h, "-", single$n_yellows_a,
                              "  🟥 ", single$n_reds_h, "-", single$n_reds_a))
               )
        )
      ),
      # Alineacions + Acta
      fluidRow(
        column(4,
               box(width=12, title="📋 Alineacions", solidHeader=TRUE, status="info",
                   fluidRow(
                     column(6,
                            div(class="sim-lineup-col",
                                tags$h6(home),
                                lapply(lu_h, function(p) {
                                  r_val <- pr %>% filter(as.character(player)==p) %>% pull(rating_global)
                                  r_disp <- if(length(r_val)>0 && is.finite(r_val[1])) round(r_val[1]*100) else "—"
                                  div(class="sim-lineup-player",
                                      tags$span(style="font-size:11px;", p),
                                      tags$span(style="color:#1a6b8a;font-weight:bold;font-size:11px;", paste0("★",r_disp)))
                                })
                            )
                     ),
                     column(6,
                            div(class="sim-lineup-col",
                                tags$h6(away),
                                lapply(lu_a, function(p) {
                                  r_val <- pr %>% filter(as.character(player)==p) %>% pull(rating_global)
                                  r_disp <- if(length(r_val)>0 && is.finite(r_val[1])) round(r_val[1]*100) else "—"
                                  div(class="sim-lineup-player",
                                      tags$span(style="font-size:11px;", p),
                                      tags$span(style="color:#c0392b;font-weight:bold;font-size:11px;", paste0("★",r_disp)))
                                })
                            )
                     )
                   )
               )
        ),
        column(8,
               box(width=12, title="📜 Acta del Partit (minut a minut)",
                   solidHeader=TRUE, status="warning",
                   uiOutput("sim_single_acta_ui"))
        )
      )
    )
    
  } else if (mode == "montecarlo") {
    sr <- sim_results_val()
    if (is.null(sr)) return(NULL)
    
    tagList(
      # Scoreboard MC
      fluidRow(
        column(12, uiOutput("sim_scoreboard_ui"))
      ),
      # Pestanyes de resultats MC
      fluidRow(
        column(12,
               tabBox(width=12,
                      tabPanel("🎲 Probabilitats",
                               br(),
                               uiOutput("sim_prob_bar_ui"),
                               br(),
                               plotlyOutput("sim_prob_plot", height="240px")
                      ),
                      tabPanel("🗺️ Mapa de Marcadors",
                               br(),
                               plotlyOutput("sim_heatmap", height="380px")
                      ),
                      tabPanel("⚽ Gols",
                               br(),
                               plotlyOutput("sim_goals_dist", height="340px")
                      ),
                      tabPanel("📜 Acta exemple",
                               br(),
                               uiOutput("sim_acta_ui")
                      ),
                      tabPanel("📋 Alineacions",
                               br(),
                               uiOutput("sim_lineups_ui")
                      )
               )
        )
      )
    )
  }
})

output$sim_lineups_ui <- renderUI({
  req(input$sim_home, input$sim_away)
  pr <- sim_pr()
  lu_h <- get_lineup_fn(input$sim_home, pr, 11)
  lu_a <- get_lineup_fn(input$sim_away, pr, 11)
  make_player_row <- function(p, team_name) {
    r_val <- pr %>% filter(as.character(player)==p) %>% pull(rating_global)
    r_disp <- if(length(r_val)>0) round(r_val[1]*100) else "—"
    div(class="sim-lineup-player",
        tags$span(p),
        tags$span(style="color:#1a6b8a;font-weight:bold;", paste0("★",r_disp)))
  }
  fluidRow(
    column(6,
           div(class="sim-lineup-col",
               tags$h6(input$sim_home),
               if(length(lu_h)==0) p(style="color:#aaa;font-size:11px;","Sense dades")
               else lapply(lu_h, function(p) make_player_row(p, input$sim_home)))),
    column(6,
           div(class="sim-lineup-col",
               tags$h6(input$sim_away),
               if(length(lu_a)==0) p(style="color:#aaa;font-size:11px;","Sense dades")
               else lapply(lu_a, function(p) make_player_row(p, input$sim_away))))
  )
})

output$sim_scoreboard_ui <- renderUI({
  sr <- sim_results_val()
  if (is.null(sr)) {
    return(div(class="sim-scoreboard",
               div(class="sim-teams","Selecciona equips i simula"),
               div(class="sim-score","— : —"),
               div(class="sim-status","Resultat esperat (gols promig)")))
  }
  df <- sr$df
  avg_h <- round(mean(df$score_h), 2); avg_a <- round(mean(df$score_a), 2)
  # Resultat modal
  res_counts <- table(paste0(df$score_h,"-",df$score_a))
  modal_score <- names(which.max(res_counts))
  modal_freq  <- round(max(res_counts)/nrow(df)*100, 1)
  div(class="sim-scoreboard",
      div(class="sim-teams", paste(sr$home, "vs", sr$away)),
      div(class="sim-score", paste0(avg_h, " : ", avg_a)),
      div(class="sim-status",
          paste0("Marcador esperat (promig) · ",
                 "Resultat més probable: ", modal_score, " (", modal_freq, "%)"))
  )
})

output$sim_prob_bar_ui <- renderUI({
  sr <- sim_results_val()
  if (is.null(sr)) return(NULL)
  df <- sr$df; n <- nrow(df)
  ph <- round(sum(df$result=="H")/n*100, 1)
  pd <- round(sum(df$result=="D")/n*100, 1)
  pa <- round(sum(df$result=="A")/n*100, 1)
  div(
    div(style="display:flex;justify-content:space-between;font-size:12px;margin-bottom:4px;",
        tags$span(style="color:#27ae60;font-weight:bold;", sr$home),
        tags$span(style="color:#888;", "Empat"),
        tags$span(style="color:#e74c3c;font-weight:bold;", sr$away)),
    div(class="sim-prob-bar",
        div(class="sim-prob-h", style=paste0("width:",ph,"%;"), paste0(ph,"%")),
        div(class="sim-prob-d", style=paste0("width:",pd,"%;"), paste0(pd,"%")),
        div(class="sim-prob-a", style=paste0("width:",pa,"%;"), paste0(pa,"%")))
  )
})

output$sim_prob_plot <- renderPlotly({
  sr <- sim_results_val()
  if (is.null(sr)) return(plotly_empty())
  df <- sr$df; n <- nrow(df)
  res_df <- data.frame(
    Resultat = c(paste0("Victoria\n",sr$home), "Empat", paste0("Victoria\n",sr$away)),
    Prob     = c(sum(df$result=="H")/n, sum(df$result=="D")/n, sum(df$result=="A")/n),
    Color    = c("#27ae60","#f39c12","#e74c3c"),
    stringsAsFactors = FALSE
  )
  plot_ly(res_df, x=~Resultat, y=~Prob, type="bar",
          marker=list(color=~Color, line=list(color="white",width=1)),
          text=~paste0(round(Prob*100,1),"%"), textposition="outside",
          hovertemplate="%{x}: %{text}<extra></extra>",
          showlegend=FALSE) %>%
    layout(yaxis=list(title="Probabilitat", tickformat=".0%", range=c(0,1)),
           xaxis=list(title=""),
           plot_bgcolor="#fafafa", paper_bgcolor="white",
           margin=list(t=20,b=10,l=60,r=20))
})

output$sim_heatmap <- renderPlotly({
  sr <- sim_results_val()
  if (is.null(sr)) return(plotly_empty())
  df <- sr$df; n_total <- nrow(df)
  max_g <- min(max(max(df$score_h), max(df$score_a)), 6)
  counts_df <- df %>% count(score_h, score_a)   # columna 'n'
  score_mat <- expand.grid(score_h=0:max_g, score_a=0:max_g) %>%
    left_join(counts_df, by=c("score_h","score_a")) %>%
    mutate(n=replace_na(n, 0L),
           prob=n/n_total,
           label=paste0(round(prob*100,1),"%"))
  plot_ly(score_mat, x=~score_a, y=~score_h, z=~prob, type="heatmap",
          colorscale=list(c(0,"white"),c(1,"darkgreen")),
          text=~label, hovertemplate=paste0(sr$home," %{y} - %{x} ",sr$away,"<br>%{text}<extra></extra>"),
          showscale=FALSE) %>%
    add_annotations(x=~score_a, y=~score_h, text=~label,
                    showarrow=FALSE, font=list(size=10, color="#333")) %>%
    layout(xaxis=list(title=paste("Gols",sr$away), dtick=1),
           yaxis=list(title=paste("Gols",sr$home), dtick=1),
           plot_bgcolor="#fafafa", paper_bgcolor="white",
           margin=list(t=10,b=50,l=60,r=10))
})

output$sim_goals_dist <- renderPlotly({
  sr <- sim_results_val()
  if (is.null(sr)) return(plotly_empty())
  df <- sr$df
  avg_g <- round(mean(df$n_goals), 2)
  goal_counts <- df %>% count(n_goals) %>% mutate(prob=n/nrow(df))
  plot_ly(goal_counts, x=~n_goals, y=~prob, type="bar",
          marker=list(color="#3498db", opacity=0.85, line=list(color="white",width=1)),
          text=~paste0(round(prob*100,1),"%"), textposition="outside",
          hovertemplate="Gols %{x}: %{text}<extra></extra>",
          showlegend=FALSE) %>%
    add_lines(x=~n_goals, y=avg_g/max(goal_counts$n_goals+1),
              line=list(color="#e74c3c",width=2,dash="dash"),
              hoverinfo="none", showlegend=FALSE) %>%
    layout(xaxis=list(title="Gols totals al partit", dtick=1),
           yaxis=list(title="Probabilitat", tickformat=".0%"),
           annotations=list(list(x=avg_g, y=max(goal_counts$prob)*0.95,
                                 text=paste0("⌀ ",avg_g," gols"),
                                 showarrow=FALSE, font=list(size=11,color="#e74c3c"),
                                 bgcolor="rgba(255,255,255,0.8)")),
           plot_bgcolor="#fafafa", paper_bgcolor="white",
           margin=list(t=20,b=50,l=60,r=20))
})

output$sim_acta_ui <- renderUI({
  single <- sim_single_val()
  sr     <- sim_results_val()
  if (is.null(single) || is.null(sr)) {
    return(div(style="color:#aaa;padding:20px;text-align:center;","Executa la simulació per veure l'acta d'un partit exemple"))
  }
  evts <- single$events
  home <- sr$home; away <- sr$away
  
  # Capçalera del resultat
  header <- div(style="text-align:center;margin-bottom:12px;",
                div(style="font-size:24px;font-weight:900;color:#1a3a6b;",
                    paste0(home, "  ", single$score_h, " - ", single$score_a, "  ", away)),
                div(style="font-size:12px;color:#888;margin-top:4px;",
                    paste0("Targetes grogues: ",single$n_yellows_h," - ",single$n_yellows_a,
                           "  ·  Vermelles: ",single$n_reds_h," - ",single$n_reds_a)))
  
  # Esdeveniments ordenats per minut
  if (length(evts) == 0) {
    return(tagList(header, div(style="color:#aaa;text-align:center;","Sense esdeveniments")))
  }
  
  evt_items <- lapply(evts, function(e) {
    cls <- switch(e$type,
                  "gol_h"      = "gol-h",
                  "gol_a"      = "gol-a",
                  "groga_h"    = , "groga_a"    = "groga",
                  "vermella_h" = , "vermella_a" = "vermella",
                  "canvi_h"    = , "canvi_a"    = "canvi",
                  "canvi")
    
    icon_txt <- switch(e$type,
                       "gol_h"="⚽", "gol_a"="⚽",
                       "groga_h"="🟨", "groga_a"="🟨",
                       "vermella_h"="🟥", "vermella_a"="🟥",
                       "canvi_h"="🔄", "canvi_a"="🔄", "❓")
    
    team_lbl <- if (!is.null(e$team)) e$team else ""
    player_lbl <- if (!is.null(e$player)) e$player else ""
    score_lbl  <- if (!is.null(e$score))  paste0(" [",e$score,"]") else ""
    
    detail <- if (e$type %in% c("canvi_h","canvi_a"))
      paste0("⬆ ", e$inn, "  ⬇ ", e$out)
    else
      paste0(player_lbl, score_lbl)
    
    div(class=paste0("sim-event ", cls),
        tags$span(class="sim-min", paste0(e$minute,"'")),
        tags$span(icon_txt),
        tags$span(style="font-size:11px;color:#888;", team_lbl),
        tags$span(style="font-weight:600;", detail))
  })
  
  # Dividir entre primera i segona part
  is_first_half <- function(e) e$minute <= 45
  first_half  <- Filter(is_first_half, evts)
  second_half <- Filter(Negate(is_first_half), evts)
  
  first_items  <- lapply(first_half, function(e) {
    cls <- switch(e$type,"gol_h"="gol-h","gol_a"="gol-a","groga_h"=,"groga_a"="groga","vermella_h"=,"vermella_a"="vermella","canvi")
    icon_txt <- switch(e$type,"gol_h"="⚽","gol_a"="⚽","groga_h"="🟨","groga_a"="🟨","vermella_h"="🟥","vermella_a"="🟥","canvi_h"="🔄","canvi_a"="🔄","❓")
    team_lbl  <- if (!is.null(e$team)) e$team else ""
    detail    <- if (e$type %in% c("canvi_h","canvi_a")) paste0("⬆ ",e$inn,"  ⬇ ",e$out) else paste0(if(!is.null(e$player))e$player else "", if(!is.null(e$score))paste0(" [",e$score,"]") else "")
    div(class=paste0("sim-event ",cls),tags$span(class="sim-min",paste0(e$minute,"'")),tags$span(icon_txt),tags$span(style="font-size:11px;color:#888;",team_lbl),tags$span(style="font-weight:600;",detail))
  })
  second_items <- lapply(second_half, function(e) {
    cls <- switch(e$type,"gol_h"="gol-h","gol_a"="gol-a","groga_h"=,"groga_a"="groga","vermella_h"=,"vermella_a"="vermella","canvi")
    icon_txt <- switch(e$type,"gol_h"="⚽","gol_a"="⚽","groga_h"="🟨","groga_a"="🟨","vermella_h"="🟥","vermella_a"="🟥","canvi_h"="🔄","canvi_a"="🔄","❓")
    team_lbl  <- if (!is.null(e$team)) e$team else ""
    detail    <- if (e$type %in% c("canvi_h","canvi_a")) paste0("⬆ ",e$inn,"  ⬇ ",e$out) else paste0(if(!is.null(e$player))e$player else "", if(!is.null(e$score))paste0(" [",e$score,"]") else "")
    div(class=paste0("sim-event ",cls),tags$span(class="sim-min",paste0(e$minute,"'")),tags$span(icon_txt),tags$span(style="font-size:11px;color:#888;",team_lbl),tags$span(style="font-weight:600;",detail))
  })
  
  tagList(
    header,
    fluidRow(
      column(6,
             div(tags$b(style="font-size:12px;color:#555;display:block;margin-bottom:8px;","⏱ PRIMERA PART"),
                 if(length(first_items)==0) div(style="color:#aaa;font-size:12px;padding:8px;","Sense esdeveniments")
                 else first_items)),
      column(6,
             div(tags$b(style="font-size:12px;color:#555;display:block;margin-bottom:8px;","⏱ SEGONA PART"),
                 if(length(second_items)==0) div(style="color:#aaa;font-size:12px;padding:8px;","Sense esdeveniments")
                 else second_items))
    )
  )
})

# Helper per renderitzar acta d'un single match genèric
render_acta_from_single <- function(single, home, away) {
  if (is.null(single)) return(div(style="color:#aaa;padding:16px;text-align:center;","Prem ⚡ Simular 1 Partit per veure l'acta minut a minut"))
  evts <- single$events
  header <- div(style="text-align:center;margin-bottom:14px;",
                div(style="font-size:26px;font-weight:900;color:#1a3a6b;",
                    paste0(home,"  ",single$score_h," - ",single$score_a,"  ",away)),
                div(style="font-size:12px;color:#888;margin-top:4px;",
                    paste0("Grogues: ",single$n_yellows_h," - ",single$n_yellows_a,
                           "  ·  Vermelles: ",single$n_reds_h," - ",single$n_reds_a,
                           "  ·  Canvis: ",single$n_reds_h + sum(sapply(evts, function(e) e$type %in% c("canvi_h","canvi_a")))," - ",0)))
  make_evt_div <- function(e) {
    cls <- switch(e$type, "gol_h"="gol-h","gol_a"="gol-a",
                  "groga_h"=,"groga_a"="groga",
                  "vermella_h"=,"vermella_a"="vermella","canvi")
    icon_txt <- switch(e$type,"gol_h"="⚽","gol_a"="⚽",
                       "groga_h"="🟨","groga_a"="🟨",
                       "vermella_h"="🟥","vermella_a"="🟥",
                       "canvi_h"="🔄","canvi_a"="🔄","❓")
    team_lbl <- if (!is.null(e$team)) e$team else ""
    detail <- if (e$type %in% c("canvi_h","canvi_a"))
      paste0("⬆ ",e$inn,"  ⬇ ",e$out)
    else
      paste0(if(!is.null(e$player)) e$player else "",
             if(!is.null(e$score)) paste0("  [",e$score,"]") else "")
    div(class=paste0("sim-event ",cls),
        tags$span(class="sim-min", paste0(e$minute,"'")),
        tags$span(icon_txt),
        tags$span(style="font-size:11px;color:#888;min-width:120px;display:inline-block;", team_lbl),
        tags$span(style="font-weight:600;", detail))
  }
  if (length(evts) == 0) return(tagList(header, div(style="color:#aaa;text-align:center;padding:12px;","Sense esdeveniments destacats")))
  first_half  <- Filter(function(e) e$minute <= 45, evts)
  second_half <- Filter(function(e) e$minute > 45,  evts)
  tagList(
    header,
    fluidRow(
      column(6,
             tags$b(style="font-size:12px;color:#555;display:block;margin-bottom:8px;","⏱ PRIMERA PART (1'-45')"),
             if(length(first_half)==0) div(style="color:#aaa;font-size:12px;padding:8px;","Sense esdeveniments")
             else lapply(first_half, make_evt_div)),
      column(6,
             tags$b(style="font-size:12px;color:#555;display:block;margin-bottom:8px;","⏱ SEGONA PART (46'-90')"),
             if(length(second_half)==0) div(style="color:#aaa;font-size:12px;padding:8px;","Sense esdeveniments")
             else lapply(second_half, make_evt_div))
    )
  )
}

output$sim_single_acta_ui <- renderUI({
  single <- sim_single_ind_val()
  if (is.null(single)) {
    return(div(style="color:#aaa;padding:16px;text-align:center;",
               "Prem ⚡ Simular 1 Partit per veure l'acta minut a minut"))
  }
  render_acta_from_single(single, single$home, single$away)
})
}

shinyApp(ui = ui, server = server)