# Tennis Matchup Profit Model Shiny App
#
# RStudio instructions:
# 1) Save this file as app.R.
# 2) Install packages once if needed:
#    install.packages(c("shiny", "dplyr", "readr", "purrr", "tidyr", "lubridate", "ggplot2"))
# 3) Open app.R in RStudio and click Run App, or run: shiny::runApp()
#
# First run may take a while because the app downloads/cache-builds ATP match data
# and Match Charting profiles into the local outputs/ folder. Later runs use the cache.

suppressPackageStartupMessages({
  required_packages <- c("shiny", "dplyr", "readr", "purrr", "tidyr", "lubridate", "ggplot2")
})

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "Missing required packages: %s. Install them before running this script.",
      paste(missing_packages, collapse = ", ") 
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(purrr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
})

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(path)
}

clean_player_name <- function(x) {
  x <- gsub("_", " ", as.character(x), fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

as_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_rate <- function(successes, attempts, prior = 0.5, prior_n = 20) {
  successes <- ifelse(is.na(successes), 0, successes)
  attempts <- ifelse(is.na(attempts), 0, attempts)
  (successes + prior * prior_n) / (attempts + prior_n)
}

inv_logit <- function(x) {
  p <- 1 / (1 + exp(-x))
  pmin(pmax(p, 1e-6), 1 - 1e-6)
}

prob_to_american <- function(probability) {
  prob <- pmin(pmax(as.numeric(probability), 1e-6), 1 - 1e-6)

  ifelse(
    prob >= 0.5,
    round(-100 * prob / (1 - prob)),
    round(100 * (1 - prob) / prob)
  )
}

american_to_probability <- function(odds) {
  odds <- as_number(odds)

  ifelse(
    is.na(odds),
    NA_real_,
    ifelse(
      odds > 0,
      100 / (odds + 100),
      abs(odds) / (abs(odds) + 100)
    )
  )
}

odds_to_probability <- function(odds) {
  odds <- as_number(odds)
  is_decimal <- !is.na(odds) & odds > 1 & odds < 20

  ifelse(
    is.na(odds),
    NA_real_,
    ifelse(is_decimal, 1 / odds, american_to_probability(odds))
  )
}

profit_on_stake <- function(odds, won, stake = 100) {
  odds <- as_number(odds)
  is_decimal <- !is.na(odds) & odds > 1 & odds < 20

  win_profit <- ifelse(
    is_decimal,
    stake * (odds - 1),
    ifelse(odds > 0, stake * odds / 100, stake * 100 / abs(odds))
  )

  ifelse(is.na(odds), NA_real_, ifelse(won, win_profit, -stake))
}

coalesce_numeric_columns <- function(data, candidates) {
  values <- rep(NA_real_, nrow(data))

  for (candidate in candidates) {
    if (candidate %in% names(data)) {
      values <- dplyr::coalesce(values, as_number(data[[candidate]]))
    }
  }

  values
}

default_model_weights <- function() {
  c(
    overall_elo_diff = 1.20,
    surface_elo_diff = 1.35,
    serve_return_diff = 4.00,
    surface_form_diff = 1.25,
    recent_form_diff = 0.80,
    rank_strength_diff = 0.35,
    best_of5_elo_diff = 0.08,
    shot_quality_diff = 1.25,
    serve_pressure_diff = 1.00,
    return_pressure_diff = 1.00,
    second_serve_attack_diff = 0.80,
    archetype_delta_diff = 1.10,
    archetype_win_rate_diff = 0.70
  )
}

load_atp_matches <- function(
    start_year = 2015,
    end_year = 2025,
    cache_path = file.path("outputs", sprintf("tennis_atp_matches_%s_%s.rds", start_year, end_year)),
    refresh = FALSE) {
  ensure_directory(dirname(cache_path))

  if (file.exists(cache_path) && !isTRUE(refresh)) {
    return(readRDS(cache_path))
  }

  years <- start_year:end_year
  urls <- paste0(
    "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_",
    years,
    ".csv"
  )

  message(sprintf("Downloading ATP matches for %s-%s...", start_year, end_year))

  raw_matches <- purrr::map2_dfr(urls, years, function(url, year) {
    tryCatch(
      {
        data <- readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
        data$source_year <- year
        data
      },
      error = function(e) {
        warning(sprintf("Could not read %s: %s", url, conditionMessage(e)), call. = FALSE)
        tibble()
      }
    )
  })

  if (nrow(raw_matches) == 0) {
    stop("Could not load ATP match data. Check network access or rerun with a local cache.", call. = FALSE)
  }

  required_core <- c("tourney_date", "winner_name", "loser_name")
  missing_core <- setdiff(required_core, names(raw_matches))
  if (length(missing_core) > 0) {
    stop(
      sprintf("ATP data is missing required columns: %s", paste(missing_core, collapse = ", ")),
      call. = FALSE
    )
  }

  optional_columns <- c(
    "tourney_id", "tourney_name", "surface", "best_of", "round",
    "winner_rank", "loser_rank", "winner_rank_points", "loser_rank_points",
    "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon",
    "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon"
  )

  for (column in setdiff(optional_columns, names(raw_matches))) {
    raw_matches[[column]] <- NA
  }

  winner_odds <- coalesce_numeric_columns(
    raw_matches,
    c("winner_odds", "w_odds", "W_Odds", "B365W", "PSW", "MaxW", "AvgW")
  )
  loser_odds <- coalesce_numeric_columns(
    raw_matches,
    c("loser_odds", "l_odds", "L_Odds", "B365L", "PSL", "MaxL", "AvgL")
  )

  matches <- raw_matches |>
    mutate(
      tourney_date = lubridate::ymd(as.character(tourney_date)),
      winner_name = clean_player_name(winner_name),
      loser_name = clean_player_name(loser_name),
      surface = ifelse(is.na(surface) | surface == "", "Unknown", as.character(surface)),
      best_of = as.integer(best_of),
      best_of = ifelse(is.na(best_of), 3L, best_of),
      winner_rank = as_number(winner_rank),
      loser_rank = as_number(loser_rank),
      winner_market_odds = winner_odds,
      loser_market_odds = loser_odds
    ) |>
    filter(
      !is.na(tourney_date),
      !is.na(winner_name),
      !is.na(loser_name),
      winner_name != "",
      loser_name != "",
      winner_name != loser_name
    ) |>
    arrange(tourney_date, tourney_id, tourney_name, round, winner_name, loser_name) |>
    mutate(match_id = row_number()) |>
    select(
      match_id,
      source_year,
      tourney_id,
      tourney_name,
      tourney_date,
      surface,
      best_of,
      round,
      winner_name,
      loser_name,
      winner_rank,
      loser_rank,
      winner_rank_points,
      loser_rank_points,
      w_ace,
      w_df,
      w_svpt,
      w_1stIn,
      w_1stWon,
      w_2ndWon,
      l_ace,
      l_df,
      l_svpt,
      l_1stIn,
      l_1stWon,
      l_2ndWon,
      winner_market_odds,
      loser_market_odds
    )

  saveRDS(matches, cache_path)
  matches
}

read_charting_csv <- function(file_stub, gender = "m", cache_dir = file.path("outputs", "match_charting"), refresh = FALSE) {
  ensure_directory(cache_dir)

  file_name <- sprintf("charting-%s-stats-%s.csv", gender, file_stub)
  cache_path <- file.path(cache_dir, file_name)
  url <- paste0(
    "https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/",
    file_name
  )

  if (file.exists(cache_path) && !isTRUE(refresh)) {
    return(readr::read_csv(cache_path, show_col_types = FALSE, progress = FALSE))
  }

  message(sprintf("Downloading Match Charting %s...", file_stub))
  data <- tryCatch(
    readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      warning(
        sprintf("Could not read Match Charting %s: %s", file_stub, conditionMessage(e)),
        call. = FALSE
      )
      tibble()
    }
  )

  if (nrow(data) > 0) {
    readr::write_csv(data, cache_path)
  }

  data
}

add_missing_columns <- function(data, columns) {
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- NA_real_
  }

  data
}

parse_charting_date <- function(match_id) {
  lubridate::ymd(substr(as.character(match_id), 1, 8))
}

load_match_charting_profiles <- function(
    gender = "m",
    cache_dir = file.path("outputs", "match_charting"),
    refresh = FALSE) {
  overview <- read_charting_csv("Overview", gender, cache_dir, refresh)
  serve <- read_charting_csv("ServeBasics", gender, cache_dir, refresh)
  return_outcomes <- read_charting_csv("ReturnOutcomes", gender, cache_dir, refresh)
  return_depth <- read_charting_csv("ReturnDepth", gender, cache_dir, refresh)
  rally <- read_charting_csv("Rally", gender, cache_dir, refresh)
  shot_types <- read_charting_csv("ShotTypes", gender, cache_dir, refresh)
  shot_dirs <- read_charting_csv("ShotDirOutcomes", gender, cache_dir, refresh)

  if (nrow(overview) == 0 && nrow(shot_types) == 0) {
    warning("No Match Charting stats were loaded. Continuing with ATP match-stat proxies.", call. = FALSE)
    return(tibble())
  }

  overview <- add_missing_columns(
    overview,
    c(
      "match_id", "player", "set", "serve_pts", "aces", "dfs", "first_in", "first_won",
      "second_in", "second_won", "return_pts", "return_pts_won", "winners_fh",
      "winners_bh", "unforced_fh", "unforced_bh"
    )
  )
  overview_profile <- overview |>
    filter(.data$set == "Total") |>
    transmute(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      chart_serve_pts = as_number(serve_pts),
      chart_aces = as_number(aces),
      chart_dfs = as_number(dfs),
      chart_first_in = as_number(first_in),
      chart_first_won = as_number(first_won),
      chart_second_pts = as_number(second_in),
      chart_second_won = as_number(second_won),
      chart_return_pts = as_number(return_pts),
      chart_return_pts_won = as_number(return_pts_won),
      chart_overview_fh_winners = as_number(winners_fh),
      chart_overview_bh_winners = as_number(winners_bh),
      chart_overview_fh_unforced = as_number(unforced_fh),
      chart_overview_bh_unforced = as_number(unforced_bh)
    )

  serve <- add_missing_columns(
    serve,
    c("match_id", "player", "row", "pts", "pts_won", "aces", "unret", "forced_err", "pts_won_lte_3_shots")
  )
  serve_profile <- serve |>
    filter(row == "Total") |>
    transmute(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      chart_serve_basic_pts = as_number(pts),
      chart_serve_basic_won = as_number(pts_won),
      chart_unreturned_serves = as_number(unret),
      chart_serve_forced_errors = as_number(forced_err),
      chart_short_serve_points_won = as_number(pts_won_lte_3_shots)
    )

  return_outcomes <- add_missing_columns(
    return_outcomes,
    c("match_id", "player", "row", "pts", "pts_won", "returnable", "returnable_won", "in_play", "in_play_won", "winners", "total_shots")
  )
  return_total <- return_outcomes |>
    filter(row == "Total") |>
    transmute(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      chart_return_outcome_pts = as_number(pts),
      chart_return_outcome_won = as_number(pts_won),
      chart_returnable = as_number(returnable),
      chart_returnable_won = as_number(returnable_won),
      chart_returns_in_play = as_number(in_play),
      chart_returns_in_play_won = as_number(in_play_won),
      chart_return_winners = as_number(winners),
      chart_return_shots = as_number(total_shots)
    )
  return_v2 <- return_outcomes |>
    filter(row == "v2nd") |>
    transmute(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      chart_v2_returnable = as_number(returnable),
      chart_v2_returnable_won = as_number(returnable_won),
      chart_v2_return_winners = as_number(winners)
    )

  return_depth <- add_missing_columns(
    return_depth,
    c("match_id", "player", "row", "returnable", "shallow", "deep", "very_deep", "unforced")
  )
  return_depth_profile <- return_depth |>
    filter(row == "Total") |>
    transmute(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      chart_depth_returnable = as_number(returnable),
      chart_return_shallow = as_number(shallow),
      chart_return_deep = as_number(deep),
      chart_return_very_deep = as_number(very_deep),
      chart_return_unforced = as_number(unforced)
    )

  shot_types <- add_missing_columns(
    shot_types,
    c(
      "match_id", "player", "row", "shots", "pt_ending", "winners",
      "induced_forced", "unforced", "shots_in_pts_won", "shots_in_pts_lost"
    )
  )
  shot_profile <- shot_types |>
    mutate(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      wing = case_when(
        row %in% c("Fside", "Forehand", "FH") ~ "fh",
        row %in% c("Bside", "Backhand", "BH") ~ "bh",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(wing)) |>
    group_by(match_id, chart_date, player, wing) |>
    summarise(
      shots = sum(as_number(shots), na.rm = TRUE),
      pt_ending = sum(as_number(pt_ending), na.rm = TRUE),
      winners = sum(as_number(winners), na.rm = TRUE),
      induced_forced = sum(as_number(induced_forced), na.rm = TRUE),
      unforced = sum(as_number(unforced), na.rm = TRUE),
      shots_won = sum(as_number(shots_in_pts_won), na.rm = TRUE),
      shots_lost = sum(as_number(shots_in_pts_lost), na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = wing,
      values_from = c(shots, pt_ending, winners, induced_forced, unforced, shots_won, shots_lost),
      values_fill = 0,
      names_glue = "chart_{wing}_{.value}"
    )

  shot_dirs <- add_missing_columns(
    shot_dirs,
    c("match_id", "player", "row", "shots", "pt_ending", "winners", "induced_forced", "unforced")
  )
  shot_dir_profile <- shot_dirs |>
    mutate(
      match_id = as.character(match_id),
      chart_date = parse_charting_date(match_id),
      player = clean_player_name(player),
      wing = case_when(
        grepl("^F-", row) ~ "fh",
        grepl("^B-", row) ~ "bh",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(wing)) |>
    group_by(match_id, chart_date, player, wing) |>
    summarise(
      dir_shots = sum(as_number(shots), na.rm = TRUE),
      dir_pt_ending = sum(as_number(pt_ending), na.rm = TRUE),
      dir_winners = sum(as_number(winners), na.rm = TRUE),
      dir_induced_forced = sum(as_number(induced_forced), na.rm = TRUE),
      dir_unforced = sum(as_number(unforced), na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = wing,
      values_from = c(dir_shots, dir_pt_ending, dir_winners, dir_induced_forced, dir_unforced),
      values_fill = 0,
      names_glue = "chart_{wing}_{.value}"
    )

  rally_profile <- tibble()
  if (nrow(rally) > 0) {
    rally <- add_missing_columns(
      rally,
      c(
        "match_id", "server", "returner", "row", "pts", "pl1_won", "pl1_winners",
        "pl1_forced", "pl1_unforced", "pl2_won", "pl2_winners", "pl2_forced", "pl2_unforced"
      )
    )
    rally_server <- rally |>
      transmute(
        match_id = as.character(match_id),
        chart_date = parse_charting_date(match_id),
        player = clean_player_name(server),
        row = as.character(row),
        rally_pts = as_number(pts),
        rally_won = as_number(pl1_won),
        rally_winners = as_number(pl1_winners),
        rally_forced = as_number(pl1_forced),
        rally_unforced = as_number(pl1_unforced)
      )
    rally_returner <- rally |>
      transmute(
        match_id = as.character(match_id),
        chart_date = parse_charting_date(match_id),
        player = clean_player_name(returner),
        row = as.character(row),
        rally_pts = as_number(pts),
        rally_won = as_number(pl2_won),
        rally_winners = as_number(pl2_winners),
        rally_forced = as_number(pl2_forced),
        rally_unforced = as_number(pl2_unforced)
      )
    rally_profile <- bind_rows(rally_server, rally_returner) |>
      mutate(rally_bucket = case_when(
        row == "1-3" ~ "short",
        row %in% c("4-6", "7-9", "10+") ~ "long",
        TRUE ~ NA_character_
      )) |>
      filter(!is.na(rally_bucket)) |>
      group_by(match_id, chart_date, player, rally_bucket) |>
      summarise(
        rally_pts = sum(rally_pts, na.rm = TRUE),
        rally_won = sum(rally_won, na.rm = TRUE),
        rally_winners = sum(rally_winners, na.rm = TRUE),
        rally_forced = sum(rally_forced, na.rm = TRUE),
        rally_unforced = sum(rally_unforced, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = rally_bucket,
        values_from = c(rally_pts, rally_won, rally_winners, rally_forced, rally_unforced),
        values_fill = 0,
        names_glue = "chart_{rally_bucket}_{.value}"
      )
  }

  profile_parts <- list(
    overview_profile,
    serve_profile,
    return_total,
    return_v2,
    return_depth_profile,
    shot_profile,
    shot_dir_profile,
    rally_profile
  )
  profile_parts <- profile_parts[vapply(profile_parts, ncol, integer(1)) > 0]

  profiles <- profile_parts |>
    purrr::reduce(full_join, by = c("match_id", "chart_date", "player")) |>
    filter(!is.na(chart_date), !is.na(player), player != "") |>
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x))) |>
    arrange(chart_date, match_id, player)

  message(sprintf("Loaded %s Match Charting player-match profiles.", nrow(profiles)))
  profiles
}

surface_key <- function(player, surface) {
  paste(player, surface, sep = "\r")
}

archetype_key <- function(player, archetype) {
  paste(player, archetype, sep = "\r")
}

charting_counter_names <- function() {
  c(
    "chart_match_n",
    "chart_serve_pts", "chart_aces", "chart_dfs", "chart_first_in", "chart_first_won",
    "chart_second_pts", "chart_second_won", "chart_return_pts", "chart_return_pts_won",
    "chart_serve_basic_pts", "chart_serve_basic_won", "chart_unreturned_serves",
    "chart_serve_forced_errors", "chart_short_serve_points_won",
    "chart_return_outcome_pts", "chart_return_outcome_won", "chart_returnable",
    "chart_returnable_won", "chart_returns_in_play", "chart_returns_in_play_won",
    "chart_return_winners", "chart_return_shots", "chart_v2_returnable",
    "chart_v2_returnable_won", "chart_v2_return_winners", "chart_depth_returnable",
    "chart_return_shallow", "chart_return_deep", "chart_return_very_deep",
    "chart_return_unforced", "chart_fh_shots", "chart_fh_pt_ending",
    "chart_fh_winners", "chart_fh_induced_forced", "chart_fh_unforced",
    "chart_fh_shots_won", "chart_fh_shots_lost", "chart_bh_shots",
    "chart_bh_pt_ending", "chart_bh_winners", "chart_bh_induced_forced",
    "chart_bh_unforced", "chart_bh_shots_won", "chart_bh_shots_lost",
    "chart_fh_dir_shots", "chart_fh_dir_pt_ending", "chart_fh_dir_winners",
    "chart_fh_dir_induced_forced", "chart_fh_dir_unforced", "chart_bh_dir_shots",
    "chart_bh_dir_pt_ending", "chart_bh_dir_winners", "chart_bh_dir_induced_forced",
    "chart_bh_dir_unforced", "chart_short_rally_pts", "chart_short_rally_won",
    "chart_short_rally_winners", "chart_short_rally_forced", "chart_short_rally_unforced",
    "chart_long_rally_pts", "chart_long_rally_won", "chart_long_rally_winners",
    "chart_long_rally_forced", "chart_long_rally_unforced",
    "chart_overview_fh_winners", "chart_overview_bh_winners",
    "chart_overview_fh_unforced", "chart_overview_bh_unforced"
  )
}

named_value <- function(values, key, default = 0) {
  value <- unname(values[key])

  if (length(value) == 0 || is.na(value)) {
    return(default)
  }

  value
}

add_named_value <- function(values, key, increment) {
  if (!key %in% names(values)) {
    values[key] <- 0
  }

  values[key] <- values[key] + increment
  values
}

initialize_backtest_state <- function(matches) {
  players <- sort(unique(c(matches$winner_name, matches$loser_name)))
  surfaces <- sort(unique(matches$surface))

  surface_elo <- setNames(vector("list", length(surfaces)), surfaces)
  for (surface in surfaces) {
    surface_elo[[surface]] <- setNames(rep(1500, length(players)), players)
  }

  history_dates <- setNames(vector("list", length(players)), players)
  history_wins <- setNames(vector("list", length(players)), players)
  for (player in players) {
    history_dates[[player]] <- as.Date(character())
    history_wins[[player]] <- numeric()
  }

  state <- list(
    players = players,
    overall_elo = setNames(rep(1500, length(players)), players),
    surface_elo = surface_elo,
    match_n = setNames(rep(0, length(players)), players),
    win_n = setNames(rep(0, length(players)), players),
    ace = setNames(rep(0, length(players)), players),
    double_fault = setNames(rep(0, length(players)), players),
    first_in = setNames(rep(0, length(players)), players),
    first_won = setNames(rep(0, length(players)), players),
    second_points = setNames(rep(0, length(players)), players),
    second_won = setNames(rep(0, length(players)), players),
    serve_points = setNames(rep(0, length(players)), players),
    serve_points_won = setNames(rep(0, length(players)), players),
    return_points = setNames(rep(0, length(players)), players),
    return_points_won = setNames(rep(0, length(players)), players),
    return_first_points = setNames(rep(0, length(players)), players),
    return_first_won = setNames(rep(0, length(players)), players),
    return_second_points = setNames(rep(0, length(players)), players),
    return_second_won = setNames(rep(0, length(players)), players),
    surface_n = numeric(),
    surface_wins = numeric(),
    archetype_n = numeric(),
    archetype_wins = numeric(),
    history_dates = history_dates,
    history_wins = history_wins,
    league_ace = 0,
    league_double_fault = 0,
    league_first_in = 0,
    league_first_won = 0,
    league_second_points = 0,
    league_second_won = 0,
    league_serve_points = 0,
    league_serve_points_won = 0,
    league_return_points = 0,
    league_return_points_won = 0,
    league_return_first_points = 0,
    league_return_first_won = 0,
    league_return_second_points = 0,
    league_return_second_won = 0
  )

  for (counter in charting_counter_names()) {
    state[[counter]] <- setNames(rep(0, length(players)), players)
    state[[paste0("league_", counter)]] <- 0
  }

  state
}

league_rate <- function(state, type = c("serve", "return")) {
  type <- match.arg(type)

  if (type == "serve") {
    if (state$league_serve_points > 0) {
      return(state$league_serve_points_won / state$league_serve_points)
    }

    return(0.62)
  }

  if (state$league_return_points > 0) {
    return(state$league_return_points_won / state$league_return_points)
  }

  0.38
}

league_stat_rate <- function(state, numerator, denominator, fallback) {
  num <- state[[numerator]]
  den <- state[[denominator]]

  if (!is.null(num) && !is.null(den) && den > 0) {
    return(num / den)
  }

  fallback
}

get_surface_elo <- function(state, player, surface) {
  if (!surface %in% names(state$surface_elo)) {
    return(named_value(state$overall_elo, player, 1500))
  }

  named_value(state$surface_elo[[surface]], player, named_value(state$overall_elo, player, 1500))
}

sum_named_counters <- function(state, counters, player) {
  sum(vapply(
    counters,
    function(counter) named_value(state[[counter]], player, 0),
    numeric(1)
  ))
}

sum_league_counters <- function(state, counters) {
  sum(vapply(
    paste0("league_", counters),
    function(counter) {
      value <- state[[counter]]
      if (is.null(value) || is.na(value)) {
        return(0)
      }

      value
    },
    numeric(1)
  ))
}

chart_net_rate_diff <- function(
    state,
    player,
    positive_counters,
    negative_counters,
    denominator_counter,
    fallback_positive = 0.08,
    fallback_negative = 0.08,
    prior_n = 80) {
  denominator <- named_value(state[[denominator_counter]], player, 0)
  league_denominator <- state[[paste0("league_", denominator_counter)]]
  if (is.null(league_denominator) || is.na(league_denominator)) {
    league_denominator <- 0
  }

  league_positive_rate <- if (league_denominator > 0) {
    sum_league_counters(state, positive_counters) / league_denominator
  } else {
    fallback_positive
  }
  league_negative_rate <- if (league_denominator > 0 && length(negative_counters) > 0) {
    sum_league_counters(state, negative_counters) / league_denominator
  } else {
    fallback_negative
  }

  positive_rate <- safe_rate(
    sum_named_counters(state, positive_counters, player),
    denominator,
    prior = league_positive_rate,
    prior_n = prior_n
  )
  negative_rate <- if (length(negative_counters) > 0) {
    safe_rate(
      sum_named_counters(state, negative_counters, player),
      denominator,
      prior = league_negative_rate,
      prior_n = prior_n
    )
  } else {
    0
  }

  (positive_rate - negative_rate) - (league_positive_rate - league_negative_rate)
}

player_snapshot <- function(player, surface, date, state, recent_days = 365, min_charting_matches = 3) {
  match_n <- named_value(state$match_n, player, 0)
  win_n <- named_value(state$win_n, player, 0)
  overall_win_rate <- safe_rate(win_n, match_n, prior = 0.5, prior_n = 20)

  serve_rate <- safe_rate(
    named_value(state$serve_points_won, player, 0),
    named_value(state$serve_points, player, 0),
    prior = league_rate(state, "serve"),
    prior_n = 200
  )
  return_rate <- safe_rate(
    named_value(state$return_points_won, player, 0),
    named_value(state$return_points, player, 0),
    prior = league_rate(state, "return"),
    prior_n = 200
  )
  ace_rate <- safe_rate(
    named_value(state$ace, player, 0),
    named_value(state$serve_points, player, 0),
    prior = league_stat_rate(state, "league_ace", "league_serve_points", 0.06),
    prior_n = 200
  )
  df_rate <- safe_rate(
    named_value(state$double_fault, player, 0),
    named_value(state$serve_points, player, 0),
    prior = league_stat_rate(state, "league_double_fault", "league_serve_points", 0.035),
    prior_n = 200
  )
  first_in_rate <- safe_rate(
    named_value(state$first_in, player, 0),
    named_value(state$serve_points, player, 0),
    prior = league_stat_rate(state, "league_first_in", "league_serve_points", 0.62),
    prior_n = 200
  )
  first_serve_win_rate <- safe_rate(
    named_value(state$first_won, player, 0),
    named_value(state$first_in, player, 0),
    prior = league_stat_rate(state, "league_first_won", "league_first_in", 0.72),
    prior_n = 150
  )
  second_serve_win_rate <- safe_rate(
    named_value(state$second_won, player, 0),
    named_value(state$second_points, player, 0),
    prior = league_stat_rate(state, "league_second_won", "league_second_points", 0.50),
    prior_n = 150
  )
  return_first_win_rate <- safe_rate(
    named_value(state$return_first_won, player, 0),
    named_value(state$return_first_points, player, 0),
    prior = league_stat_rate(state, "league_return_first_won", "league_return_first_points", 0.28),
    prior_n = 150
  )
  return_second_win_rate <- safe_rate(
    named_value(state$return_second_won, player, 0),
    named_value(state$return_second_points, player, 0),
    prior = league_stat_rate(state, "league_return_second_won", "league_return_second_points", 0.50),
    prior_n = 150
  )

  league_serve <- league_rate(state, "serve")
  league_return <- league_rate(state, "return")
  serve_pressure <- (serve_rate - league_serve) + 0.35 * (ace_rate - league_stat_rate(state, "league_ace", "league_serve_points", 0.06)) -
    0.25 * (df_rate - league_stat_rate(state, "league_double_fault", "league_serve_points", 0.035))
  return_pressure <- return_rate - league_return
  serve_defense <- return_pressure
  second_serve_attack <- return_second_win_rate -
    league_stat_rate(state, "league_return_second_won", "league_return_second_points", 0.50)

  # Match-level ATP data does not contain true FH/BH shot direction. These are
  # latent placeholders so the architecture can use real charting data later.
  forehand_quality <- 0.55 * serve_pressure + 0.45 * second_serve_attack
  backhand_quality <- 0.60 * return_pressure + 0.40 * second_serve_attack
  shot_quality <- 0.40 * serve_pressure + 0.40 * return_pressure +
    0.10 * first_serve_win_rate + 0.10 * second_serve_win_rate
  serve_pressure_proxy <- serve_pressure
  return_pressure_proxy <- return_pressure
  forehand_quality_proxy <- forehand_quality
  backhand_quality_proxy <- backhand_quality
  shot_quality_proxy <- shot_quality
  charting_matches <- named_value(state$chart_match_n, player, 0)
  charting_available <- charting_matches >= min_charting_matches

  chart_serve_pressure <- NA_real_
  chart_return_pressure <- NA_real_
  chart_second_serve_attack <- NA_real_
  chart_forehand_quality <- NA_real_
  chart_backhand_quality <- NA_real_
  chart_short_rally_quality <- NA_real_
  chart_long_rally_quality <- NA_real_

  if (isTRUE(charting_available)) {
    chart_serve_pressure <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_aces", "chart_unreturned_serves", "chart_serve_forced_errors"),
      negative_counters = c("chart_dfs"),
      denominator_counter = "chart_serve_pts",
      fallback_positive = 0.16,
      fallback_negative = 0.04,
      prior_n = 120
    )
    chart_return_pressure <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_returnable_won", "chart_return_winners", "chart_return_deep", "chart_return_very_deep"),
      negative_counters = c("chart_return_unforced", "chart_return_shallow"),
      denominator_counter = "chart_returnable",
      fallback_positive = 0.65,
      fallback_negative = 0.30,
      prior_n = 120
    )
    chart_second_serve_attack <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_v2_returnable_won", "chart_v2_return_winners"),
      negative_counters = character(),
      denominator_counter = "chart_v2_returnable",
      fallback_positive = 0.50,
      fallback_negative = 0,
      prior_n = 80
    )
    chart_forehand_quality <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_fh_winners", "chart_fh_induced_forced", "chart_fh_dir_winners", "chart_fh_dir_induced_forced"),
      negative_counters = c("chart_fh_unforced", "chart_fh_dir_unforced"),
      denominator_counter = "chart_fh_shots",
      fallback_positive = 0.10,
      fallback_negative = 0.08,
      prior_n = 120
    )
    chart_backhand_quality <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_bh_winners", "chart_bh_induced_forced", "chart_bh_dir_winners", "chart_bh_dir_induced_forced"),
      negative_counters = c("chart_bh_unforced", "chart_bh_dir_unforced"),
      denominator_counter = "chart_bh_shots",
      fallback_positive = 0.08,
      fallback_negative = 0.08,
      prior_n = 120
    )
    chart_short_rally_quality <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_short_rally_won", "chart_short_rally_winners", "chart_short_rally_forced"),
      negative_counters = c("chart_short_rally_unforced"),
      denominator_counter = "chart_short_rally_pts",
      fallback_positive = 0.55,
      fallback_negative = 0.10,
      prior_n = 80
    )
    chart_long_rally_quality <- chart_net_rate_diff(
      state,
      player,
      positive_counters = c("chart_long_rally_won", "chart_long_rally_winners", "chart_long_rally_forced"),
      negative_counters = c("chart_long_rally_unforced"),
      denominator_counter = "chart_long_rally_pts",
      fallback_positive = 0.55,
      fallback_negative = 0.10,
      prior_n = 80
    )

    serve_pressure <- 0.55 * serve_pressure_proxy + 0.45 * chart_serve_pressure
    return_pressure <- 0.50 * return_pressure_proxy + 0.50 * chart_return_pressure
    second_serve_attack <- 0.45 * second_serve_attack + 0.55 * chart_second_serve_attack
    forehand_quality <- 0.35 * forehand_quality_proxy + 0.65 * chart_forehand_quality
    backhand_quality <- 0.35 * backhand_quality_proxy + 0.65 * chart_backhand_quality
    shot_quality <- 0.24 * serve_pressure + 0.24 * return_pressure +
      0.22 * forehand_quality + 0.22 * backhand_quality +
      0.04 * chart_short_rally_quality + 0.04 * chart_long_rally_quality
    serve_defense <- return_pressure
  }

  key <- surface_key(player, surface)
  surface_match_n <- named_value(state$surface_n, key, 0)
  surface_win_n <- named_value(state$surface_wins, key, 0)
  surface_win_rate <- safe_rate(surface_win_n, surface_match_n, prior = overall_win_rate, prior_n = 20)

  dates <- state$history_dates[[player]]
  wins <- state$history_wins[[player]]
  if (length(dates) > 0) {
    recent_idx <- dates >= (date - lubridate::days(recent_days)) & dates < date
    recent_match_n <- sum(recent_idx)
    recent_win_n <- sum(wins[recent_idx], na.rm = TRUE)
  } else {
    recent_match_n <- 0
    recent_win_n <- 0
  }
  recent_win_rate <- safe_rate(recent_win_n, recent_match_n, prior = overall_win_rate, prior_n = 8)

  list(
    prior_matches = match_n,
    overall_elo = named_value(state$overall_elo, player, 1500),
    surface_elo = get_surface_elo(state, player, surface),
    overall_win_rate = overall_win_rate,
    recent_win_rate = recent_win_rate,
    surface_matches = surface_match_n,
    surface_win_rate = surface_win_rate,
    serve_quality_raw = serve_rate,
    return_quality_raw = return_rate,
    ace_rate = ace_rate,
    df_rate = df_rate,
    first_in_rate = first_in_rate,
    first_serve_win_rate = first_serve_win_rate,
    second_serve_win_rate = second_serve_win_rate,
    return_first_win_rate = return_first_win_rate,
    return_second_win_rate = return_second_win_rate,
    serve_pressure = serve_pressure,
    return_pressure = return_pressure,
    serve_defense = serve_defense,
    second_serve_attack = second_serve_attack,
    forehand_quality = forehand_quality,
    backhand_quality = backhand_quality,
    shot_quality = shot_quality,
    charting_matches = charting_matches,
    charting_available = charting_available,
    serve_pressure_proxy = serve_pressure_proxy,
    return_pressure_proxy = return_pressure_proxy,
    forehand_quality_proxy = forehand_quality_proxy,
    backhand_quality_proxy = backhand_quality_proxy,
    shot_quality_proxy = shot_quality_proxy,
    chart_serve_pressure = chart_serve_pressure,
    chart_return_pressure = chart_return_pressure,
    chart_second_serve_attack = chart_second_serve_attack,
    chart_forehand_quality = chart_forehand_quality,
    chart_backhand_quality = chart_backhand_quality,
    chart_short_rally_quality = chart_short_rally_quality,
    chart_long_rally_quality = chart_long_rally_quality
  )
}

rank_strength_difference <- function(rank_a, rank_b) {
  rank_a <- as_number(rank_a)
  rank_b <- as_number(rank_b)

  if (is.na(rank_a) || is.na(rank_b) || rank_a <= 0 || rank_b <= 0) {
    return(0)
  }

  log(rank_b / rank_a)
}

assign_player_archetype <- function(snapshot, min_profile_matches = 10) {
  if (snapshot$prior_matches < min_profile_matches) {
    return("Low Sample")
  }

  if (snapshot$shot_quality >= 0.025 &&
      snapshot$serve_pressure >= 0.015 &&
      snapshot$return_pressure >= 0.015) {
    return("Balanced Elite")
  }

  if (snapshot$backhand_quality >= 0.012 &&
      snapshot$backhand_quality >= snapshot$forehand_quality + 0.004 &&
      snapshot$return_pressure >= 0) {
    return("Backhand Pressure")
  }

  if (snapshot$forehand_quality >= 0.012 &&
      snapshot$forehand_quality >= snapshot$backhand_quality + 0.004 &&
      snapshot$serve_pressure >= 0) {
    return("Forehand Attacker")
  }

  if (snapshot$serve_pressure >= 0.020 && snapshot$ace_rate >= 0.08) {
    return("Serve Monster")
  }

  if (snapshot$return_pressure >= 0.020 || snapshot$second_serve_attack >= 0.025) {
    return("Return Wall")
  }

  if (snapshot$return_pressure >= 0.005 && snapshot$serve_pressure < 0.005) {
    return("Baseline Grinder")
  }

  if (snapshot$serve_pressure >= 0.005 && snapshot$return_pressure < 0.005) {
    return("First-Strike Server")
  }

  "Balanced"
}

archetype_history_snapshot <- function(player, opponent_archetype, state, prior_win_rate, prior_n = 8) {
  key <- archetype_key(player, opponent_archetype)
  matches <- named_value(state$archetype_n, key, 0)
  wins <- named_value(state$archetype_wins, key, 0)
  win_rate <- safe_rate(wins, matches, prior = prior_win_rate, prior_n = prior_n)

  list(
    matches = matches,
    wins = wins,
    win_rate = win_rate,
    delta = win_rate - prior_win_rate
  )
}

compute_pair_features <- function(
    player_a,
    player_b,
    surface,
    best_of,
    date,
    rank_a,
    rank_b,
    state) {
  a <- player_snapshot(player_a, surface, date, state)
  b <- player_snapshot(player_b, surface, date, state)
  a_archetype <- assign_player_archetype(a)
  b_archetype <- assign_player_archetype(b)
  a_vs_b_archetype <- archetype_history_snapshot(
    player = player_a,
    opponent_archetype = b_archetype,
    state = state,
    prior_win_rate = a$overall_win_rate
  )
  b_vs_a_archetype <- archetype_history_snapshot(
    player = player_b,
    opponent_archetype = a_archetype,
    state = state,
    prior_win_rate = b$overall_win_rate
  )

  overall_elo_diff <- (a$overall_elo - b$overall_elo) / 400
  surface_elo_diff <- (a$surface_elo - b$surface_elo) / 400
  serve_return_diff <- (a$serve_quality_raw - b$return_quality_raw) -
    (b$serve_quality_raw - a$return_quality_raw)
  surface_form_diff <- a$surface_win_rate - b$surface_win_rate
  recent_form_diff <- a$recent_win_rate - b$recent_win_rate
  rank_diff <- rank_strength_difference(rank_a, rank_b)
  best_of5_elo_diff <- ifelse(as.integer(best_of) == 5L, overall_elo_diff + surface_elo_diff, 0)
  shot_quality_diff <- a$shot_quality - b$shot_quality
  serve_pressure_diff <- a$serve_pressure - b$serve_pressure
  return_pressure_diff <- a$return_pressure - b$return_pressure
  second_serve_attack_diff <- a$second_serve_attack - b$second_serve_attack
  archetype_delta_diff <- a_vs_b_archetype$delta - b_vs_a_archetype$delta
  archetype_win_rate_diff <- a_vs_b_archetype$win_rate - b_vs_a_archetype$win_rate

  tibble(
    a_prior_matches = a$prior_matches,
    b_prior_matches = b$prior_matches,
    a_surface_matches = a$surface_matches,
    b_surface_matches = b$surface_matches,
    a_archetype = a_archetype,
    b_archetype = b_archetype,
    a_charting_matches = a$charting_matches,
    b_charting_matches = b$charting_matches,
    a_charting_available = a$charting_available,
    b_charting_available = b$charting_available,
    a_shot_quality = a$shot_quality,
    b_shot_quality = b$shot_quality,
    a_shot_quality_proxy = a$shot_quality_proxy,
    b_shot_quality_proxy = b$shot_quality_proxy,
    a_serve_pressure = a$serve_pressure,
    b_serve_pressure = b$serve_pressure,
    a_chart_serve_pressure = a$chart_serve_pressure,
    b_chart_serve_pressure = b$chart_serve_pressure,
    a_return_pressure = a$return_pressure,
    b_return_pressure = b$return_pressure,
    a_chart_return_pressure = a$chart_return_pressure,
    b_chart_return_pressure = b$chart_return_pressure,
    a_serve_defense = a$serve_defense,
    b_serve_defense = b$serve_defense,
    a_second_serve_attack = a$second_serve_attack,
    b_second_serve_attack = b$second_serve_attack,
    a_chart_second_serve_attack = a$chart_second_serve_attack,
    b_chart_second_serve_attack = b$chart_second_serve_attack,
    a_forehand_quality = a$forehand_quality,
    b_forehand_quality = b$forehand_quality,
    a_chart_forehand_quality = a$chart_forehand_quality,
    b_chart_forehand_quality = b$chart_forehand_quality,
    a_backhand_quality = a$backhand_quality,
    b_backhand_quality = b$backhand_quality,
    a_chart_backhand_quality = a$chart_backhand_quality,
    b_chart_backhand_quality = b$chart_backhand_quality,
    a_chart_short_rally_quality = a$chart_short_rally_quality,
    b_chart_short_rally_quality = b$chart_short_rally_quality,
    a_chart_long_rally_quality = a$chart_long_rally_quality,
    b_chart_long_rally_quality = b$chart_long_rally_quality,
    a_ace_rate = a$ace_rate,
    b_ace_rate = b$ace_rate,
    a_df_rate = a$df_rate,
    b_df_rate = b$df_rate,
    a_first_serve_win_rate = a$first_serve_win_rate,
    b_first_serve_win_rate = b$first_serve_win_rate,
    a_second_serve_win_rate = a$second_serve_win_rate,
    b_second_serve_win_rate = b$second_serve_win_rate,
    a_vs_b_archetype_matches = a_vs_b_archetype$matches,
    b_vs_a_archetype_matches = b_vs_a_archetype$matches,
    a_vs_b_archetype_win_rate = a_vs_b_archetype$win_rate,
    b_vs_a_archetype_win_rate = b_vs_a_archetype$win_rate,
    a_vs_b_archetype_delta = a_vs_b_archetype$delta,
    b_vs_a_archetype_delta = b_vs_a_archetype$delta,
    overall_elo_diff = overall_elo_diff,
    surface_elo_diff = surface_elo_diff,
    serve_return_diff = serve_return_diff,
    surface_form_diff = surface_form_diff,
    recent_form_diff = recent_form_diff,
    rank_strength_diff = rank_diff,
    best_of5_elo_diff = best_of5_elo_diff,
    shot_quality_diff = shot_quality_diff,
    serve_pressure_diff = serve_pressure_diff,
    return_pressure_diff = return_pressure_diff,
    second_serve_attack_diff = second_serve_attack_diff,
    archetype_delta_diff = archetype_delta_diff,
    archetype_win_rate_diff = archetype_win_rate_diff
  )
}

prototype_probability <- function(feature_rows, weights = default_model_weights()) {
  linear <- rep(0, nrow(feature_rows))

  for (feature in names(weights)) {
    linear <- linear + weights[[feature]] * feature_rows[[feature]]
  }

  inv_logit(linear)
}


no_vig_market_probability <- function(odds_a, odds_b) {
  implied_a <- odds_to_probability(odds_a)
  implied_b <- odds_to_probability(odds_b)

  if (is.na(implied_a) || is.na(implied_b) || implied_a <= 0 || implied_b <= 0) {
    return(NA_real_)
  }

  implied_a / (implied_a + implied_b)
}

expected_value_per_100 <- function(model_probability, odds) {
  odds <- as_number(odds)
  p <- pmin(pmax(as.numeric(model_probability), 1e-6), 1 - 1e-6)

  if (is.na(odds)) {
    return(NA_real_)
  }

  win_profit <- ifelse(
    odds > 1 & odds < 20,
    100 * (odds - 1),
    ifelse(odds > 0, 100 * odds / 100, 100 * 100 / abs(odds))
  )

  p * win_profit - (1 - p) * 100
}

kelly_fraction <- function(model_probability, odds, fraction = 0.25) {
  odds <- as_number(odds)
  p <- pmin(pmax(as.numeric(model_probability), 1e-6), 1 - 1e-6)

  if (is.na(odds)) {
    return(NA_real_)
  }

  decimal_odds <- ifelse(
    odds > 1 & odds < 20,
    odds,
    ifelse(odds > 0, 1 + odds / 100, 1 + 100 / abs(odds))
  )
  b <- decimal_odds - 1
  full_kelly <- (b * p - (1 - p)) / b

  pmax(0, fraction * full_kelly)
}

shot_matchup_interaction_score <- function(features) {
  # Positive means the specific style matchup favors Player A.
  a_attacks_b_backhand <- features$a_backhand_quality - features$b_backhand_quality
  a_attacks_b_second_serve <- features$a_second_serve_attack - features$b_second_serve_win_rate
  a_handles_b_serve <- features$a_return_pressure - features$b_serve_pressure
  b_attacks_a_backhand <- features$b_backhand_quality - features$a_backhand_quality
  b_attacks_a_second_serve <- features$b_second_serve_attack - features$a_second_serve_win_rate
  b_handles_a_serve <- features$b_return_pressure - features$a_serve_pressure

  0.35 * (a_attacks_b_backhand - b_attacks_a_backhand) +
    0.35 * (a_attacks_b_second_serve - b_attacks_a_second_serve) +
    0.30 * (a_handles_b_serve - b_handles_a_serve)
}

ensemble_probability <- function(
    features,
    market_probability = NA_real_,
    prototype_weight = 0.55,
    market_weight = 0.25,
    matchup_weight = 0.20) {
  base_probability <- prototype_probability(features)
  matchup_probability <- inv_logit(qlogis(base_probability) + 3.0 * shot_matchup_interaction_score(features))

  if (is.na(market_probability)) {
    total <- prototype_weight + matchup_weight
    return((prototype_weight * base_probability + matchup_weight * matchup_probability) / total)
  }

  total <- prototype_weight + market_weight + matchup_weight
  (prototype_weight * base_probability +
     market_weight * market_probability +
     matchup_weight * matchup_probability) / total
}

extract_service_update <- function(match, side = c("winner", "loser")) {
  side <- match.arg(side)

  if (side == "winner") {
    serve_points <- as_number(match$w_svpt)
    ace <- as_number(match$w_ace)
    double_fault <- as_number(match$w_df)
    first_in <- as_number(match$w_1stIn)
    first_won <- as_number(match$w_1stWon)
    serve_points_won <- as_number(match$w_1stWon) + as_number(match$w_2ndWon)
    opp_serve_points <- as_number(match$l_svpt)
    opp_first_in <- as_number(match$l_1stIn)
    opp_first_won <- as_number(match$l_1stWon)
    opp_second_won <- as_number(match$l_2ndWon)
    return_points_won <- opp_serve_points - as_number(match$l_1stWon) - as_number(match$l_2ndWon)
  } else {
    serve_points <- as_number(match$l_svpt)
    ace <- as_number(match$l_ace)
    double_fault <- as_number(match$l_df)
    first_in <- as_number(match$l_1stIn)
    first_won <- as_number(match$l_1stWon)
    serve_points_won <- as_number(match$l_1stWon) + as_number(match$l_2ndWon)
    opp_serve_points <- as_number(match$w_svpt)
    opp_first_in <- as_number(match$w_1stIn)
    opp_first_won <- as_number(match$w_1stWon)
    opp_second_won <- as_number(match$w_2ndWon)
    return_points_won <- opp_serve_points - as_number(match$w_1stWon) - as_number(match$w_2ndWon)
  }

  second_points <- serve_points - first_in
  second_won <- serve_points_won - first_won
  return_first_points <- opp_first_in
  return_first_won <- opp_first_in - opp_first_won
  return_second_points <- opp_serve_points - opp_first_in
  return_second_won <- return_second_points - opp_second_won

  if (is.na(serve_points) || is.na(serve_points_won)) {
    serve_points <- 0
    serve_points_won <- 0
  }

  if (is.na(opp_serve_points) || is.na(return_points_won)) {
    opp_serve_points <- 0
    return_points_won <- 0
  }

  values <- list(
    ace = ace,
    double_fault = double_fault,
    first_in = first_in,
    first_won = first_won,
    second_points = second_points,
    second_won = second_won,
    return_first_points = return_first_points,
    return_first_won = return_first_won,
    return_second_points = return_second_points,
    return_second_won = return_second_won
  )
  values <- lapply(values, function(value) {
    if (length(value) == 0 || is.na(value) || value < 0) {
      return(0)
    }

    value
  })

  list(
    serve_points = serve_points,
    serve_points_won = serve_points_won,
    return_points = opp_serve_points,
    return_points_won = return_points_won,
    ace = values$ace,
    double_fault = values$double_fault,
    first_in = values$first_in,
    first_won = values$first_won,
    second_points = values$second_points,
    second_won = values$second_won,
    return_first_points = values$return_first_points,
    return_first_won = values$return_first_won,
    return_second_points = values$return_second_points,
    return_second_won = values$return_second_won
  )
}

update_player_results <- function(
    state,
    player,
    surface,
    date,
    won,
    service_update,
    opponent_archetype = NULL) {
  state$match_n[player] <- named_value(state$match_n, player, 0) + 1
  state$win_n[player] <- named_value(state$win_n, player, 0) + as.integer(won)

  state$ace[player] <- named_value(state$ace, player, 0) + service_update$ace
  state$double_fault[player] <- named_value(state$double_fault, player, 0) + service_update$double_fault
  state$first_in[player] <- named_value(state$first_in, player, 0) + service_update$first_in
  state$first_won[player] <- named_value(state$first_won, player, 0) + service_update$first_won
  state$second_points[player] <- named_value(state$second_points, player, 0) + service_update$second_points
  state$second_won[player] <- named_value(state$second_won, player, 0) + service_update$second_won
  state$serve_points[player] <- named_value(state$serve_points, player, 0) + service_update$serve_points
  state$serve_points_won[player] <- named_value(state$serve_points_won, player, 0) + service_update$serve_points_won
  state$return_points[player] <- named_value(state$return_points, player, 0) + service_update$return_points
  state$return_points_won[player] <- named_value(state$return_points_won, player, 0) + service_update$return_points_won
  state$return_first_points[player] <- named_value(state$return_first_points, player, 0) + service_update$return_first_points
  state$return_first_won[player] <- named_value(state$return_first_won, player, 0) + service_update$return_first_won
  state$return_second_points[player] <- named_value(state$return_second_points, player, 0) + service_update$return_second_points
  state$return_second_won[player] <- named_value(state$return_second_won, player, 0) + service_update$return_second_won

  key <- surface_key(player, surface)
  state$surface_n <- add_named_value(state$surface_n, key, 1)
  state$surface_wins <- add_named_value(state$surface_wins, key, as.integer(won))

  if (!is.null(opponent_archetype) && !is.na(opponent_archetype)) {
    archetype_match_key <- archetype_key(player, opponent_archetype)
    state$archetype_n <- add_named_value(state$archetype_n, archetype_match_key, 1)
    state$archetype_wins <- add_named_value(
      state$archetype_wins,
      archetype_match_key,
      as.integer(won)
    )
  }

  state$history_dates[[player]] <- c(state$history_dates[[player]], date)
  state$history_wins[[player]] <- c(state$history_wins[[player]], as.integer(won))

  state$league_ace <- state$league_ace + service_update$ace
  state$league_double_fault <- state$league_double_fault + service_update$double_fault
  state$league_first_in <- state$league_first_in + service_update$first_in
  state$league_first_won <- state$league_first_won + service_update$first_won
  state$league_second_points <- state$league_second_points + service_update$second_points
  state$league_second_won <- state$league_second_won + service_update$second_won
  state$league_serve_points <- state$league_serve_points + service_update$serve_points
  state$league_serve_points_won <- state$league_serve_points_won + service_update$serve_points_won
  state$league_return_points <- state$league_return_points + service_update$return_points
  state$league_return_points_won <- state$league_return_points_won + service_update$return_points_won
  state$league_return_first_points <- state$league_return_first_points + service_update$return_first_points
  state$league_return_first_won <- state$league_return_first_won + service_update$return_first_won
  state$league_return_second_points <- state$league_return_second_points + service_update$return_second_points
  state$league_return_second_won <- state$league_return_second_won + service_update$return_second_won

  state
}

update_charting_state <- function(state, charting_row) {
  player <- as.character(charting_row$player[[1]])
  if (is.na(player) || player == "") {
    return(state)
  }

  for (counter in charting_counter_names()) {
    increment <- if (counter == "chart_match_n") {
      1
    } else if (counter %in% names(charting_row)) {
      as_number(charting_row[[counter]][[1]])
    } else {
      0
    }

    if (length(increment) == 0 || is.na(increment)) {
      increment <- 0
    }

    state[[counter]][player] <- named_value(state[[counter]], player, 0) + increment
    state[[paste0("league_", counter)]] <- state[[paste0("league_", counter)]] + increment
  }

  state
}

advance_charting_state <- function(state, charting_profiles, charting_idx, current_date) {
  if (is.null(charting_profiles) || nrow(charting_profiles) == 0) {
    return(list(state = state, charting_idx = charting_idx))
  }

  while (charting_idx <= nrow(charting_profiles) &&
         charting_profiles$chart_date[[charting_idx]] < current_date) {
    state <- update_charting_state(state, charting_profiles[charting_idx, , drop = FALSE])
    charting_idx <- charting_idx + 1
  }

  list(state = state, charting_idx = charting_idx)
}

update_state_after_match <- function(
    state,
    match,
    winner_archetype = NULL,
    loser_archetype = NULL,
    k = 32,
    surface_k = 24) {
  winner <- match$winner_name[[1]]
  loser <- match$loser_name[[1]]
  surface <- match$surface[[1]]
  date <- match$tourney_date[[1]]

  winner_elo <- named_value(state$overall_elo, winner, 1500)
  loser_elo <- named_value(state$overall_elo, loser, 1500)
  winner_expected <- 1 / (1 + 10 ^ ((loser_elo - winner_elo) / 400))

  state$overall_elo[winner] <- winner_elo + k * (1 - winner_expected)
  state$overall_elo[loser] <- loser_elo + k * (0 - (1 - winner_expected))

  if (!surface %in% names(state$surface_elo)) {
    state$surface_elo[[surface]] <- state$overall_elo
  }

  winner_surface_elo <- named_value(state$surface_elo[[surface]], winner, winner_elo)
  loser_surface_elo <- named_value(state$surface_elo[[surface]], loser, loser_elo)
  winner_surface_expected <- 1 / (1 + 10 ^ ((loser_surface_elo - winner_surface_elo) / 400))

  state$surface_elo[[surface]][winner] <- winner_surface_elo + surface_k * (1 - winner_surface_expected)
  state$surface_elo[[surface]][loser] <- loser_surface_elo + surface_k * (0 - (1 - winner_surface_expected))

  state <- update_player_results(
    state,
    winner,
    surface,
    date,
    won = TRUE,
    service_update = extract_service_update(match, "winner"),
    opponent_archetype = loser_archetype
  )
  state <- update_player_results(
    state,
    loser,
    surface,
    date,
    won = FALSE,
    service_update = extract_service_update(match, "loser"),
    opponent_archetype = winner_archetype
  )

  state
}

generate_walk_forward_features <- function(
    matches,
    charting_profiles = tibble(),
    min_prior_matches = 10,
    progress_every = 1000) {
  matches <- matches |>
    arrange(tourney_date, match_id)
  if (!is.null(charting_profiles) && nrow(charting_profiles) > 0) {
    charting_profiles <- charting_profiles |>
      arrange(chart_date, match_id, player)
  } else {
    charting_profiles <- tibble()
  }

  state <- initialize_backtest_state(matches)
  charting_idx <- 1
  rows <- vector("list", nrow(matches))

  for (i in seq_len(nrow(matches))) {
    match <- matches[i, , drop = FALSE]
    charting_update <- advance_charting_state(
      state,
      charting_profiles,
      charting_idx,
      match$tourney_date[[1]]
    )
    state <- charting_update$state
    charting_idx <- charting_update$charting_idx

    if (progress_every > 0 && i %% progress_every == 0) {
      message(sprintf("Processed %s/%s matches...", i, nrow(matches)))
    }

    winner <- match$winner_name[[1]]
    loser <- match$loser_name[[1]]
    winner_is_a <- winner <= loser

    if (winner_is_a) {
      player_a <- winner
      player_b <- loser
      rank_a <- match$winner_rank[[1]]
      rank_b <- match$loser_rank[[1]]
      a_market_odds <- match$winner_market_odds[[1]]
      b_market_odds <- match$loser_market_odds[[1]]
      a_won <- 1L
    } else {
      player_a <- loser
      player_b <- winner
      rank_a <- match$loser_rank[[1]]
      rank_b <- match$winner_rank[[1]]
      a_market_odds <- match$loser_market_odds[[1]]
      b_market_odds <- match$winner_market_odds[[1]]
      a_won <- 0L
    }

    features <- compute_pair_features(
      player_a = player_a,
      player_b = player_b,
      surface = match$surface[[1]],
      best_of = match$best_of[[1]],
      date = match$tourney_date[[1]],
      rank_a = rank_a,
      rank_b = rank_b,
      state = state
    )

    rows[[i]] <- bind_cols(
      tibble(
        match_id = match$match_id[[1]],
        tourney_date = match$tourney_date[[1]],
        source_year = match$source_year[[1]],
        tourney_name = as.character(match$tourney_name[[1]]),
        surface = as.character(match$surface[[1]]),
        best_of = as.integer(match$best_of[[1]]),
        round = as.character(match$round[[1]]),
        player_a = player_a,
        player_b = player_b,
        a_won = a_won,
        winner_name = winner,
        loser_name = loser,
        rank_a = rank_a,
        rank_b = rank_b,
        a_market_odds = a_market_odds,
        b_market_odds = b_market_odds
      ),
      features
    ) |>
      mutate(
        eligible = a_prior_matches >= min_prior_matches &
          b_prior_matches >= min_prior_matches &
          surface != "Unknown"
      )

    if (winner_is_a) {
      winner_archetype <- features$a_archetype[[1]]
      loser_archetype <- features$b_archetype[[1]]
    } else {
      winner_archetype <- features$b_archetype[[1]]
      loser_archetype <- features$a_archetype[[1]]
    }

    state <- update_state_after_match(
      state,
      match,
      winner_archetype = winner_archetype,
      loser_archetype = loser_archetype
    )
  }

  bind_rows(rows)
}


build_current_model_state <- function(
    matches,
    charting_profiles = tibble(),
    progress_every = 1000) {
  matches <- matches |>
    arrange(tourney_date, match_id)

  if (!is.null(charting_profiles) && nrow(charting_profiles) > 0) {
    charting_profiles <- charting_profiles |>
      arrange(chart_date, match_id, player)
  } else {
    charting_profiles <- tibble()
  }

  state <- initialize_backtest_state(matches)
  charting_idx <- 1

  for (i in seq_len(nrow(matches))) {
    match <- matches[i, , drop = FALSE]

    charting_update <- advance_charting_state(
      state,
      charting_profiles,
      charting_idx,
      match$tourney_date[[1]]
    )
    state <- charting_update$state
    charting_idx <- charting_update$charting_idx

    if (progress_every > 0 && i %% progress_every == 0) {
      message(sprintf("Building current state: processed %s/%s matches...", i, nrow(matches)))
    }

    winner <- match$winner_name[[1]]
    loser <- match$loser_name[[1]]
    surface <- match$surface[[1]]
    date <- match$tourney_date[[1]]

    winner_features <- player_snapshot(winner, surface, date, state)
    loser_features <- player_snapshot(loser, surface, date, state)

    winner_archetype <- assign_player_archetype(winner_features)
    loser_archetype <- assign_player_archetype(loser_features)

    state <- update_state_after_match(
      state,
      match,
      winner_archetype = winner_archetype,
      loser_archetype = loser_archetype
    )
  }

  if (!is.null(charting_profiles) && nrow(charting_profiles) > 0) {
    while (charting_idx <= nrow(charting_profiles)) {
      state <- update_charting_state(state, charting_profiles[charting_idx, , drop = FALSE])
      charting_idx <- charting_idx + 1
    }
  }

  state
}

binary_log_loss <- function(y, p) {
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

auc_score <- function(y, p) {
  ok <- complete.cases(y, p)
  y <- y[ok]
  p <- p[ok]
  n_pos <- sum(y == 1)
  n_neg <- sum(y == 0)

  if (n_pos == 0 || n_neg == 0) {
    return(NA_real_)
  }

  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

model_summary <- function(results, probability_column, model_name) {
  p <- results[[probability_column]]
  y <- results$a_won
  predicted_a <- p >= 0.5

  tibble(
    model = model_name,
    matches = sum(!is.na(p)),
    accuracy = mean(predicted_a == as.logical(y), na.rm = TRUE),
    brier = mean((p - y) ^ 2, na.rm = TRUE),
    log_loss = binary_log_loss(y, p),
    auc = auc_score(y, p),
    avg_probability = mean(p, na.rm = TRUE),
    actual_a_win_rate = mean(y, na.rm = TRUE)
  )
}

calibration_table <- function(results, probability_column, model_name, bins = 10) {
  data <- results |>
    transmute(
      model = model_name,
      probability = .data[[probability_column]],
      actual = a_won
    ) |>
    filter(!is.na(probability), !is.na(actual)) |>
    mutate(
      bucket = cut(
        probability,
        breaks = seq(0, 1, length.out = bins + 1),
        include.lowest = TRUE
      )
    )

  data |>
    group_by(model, bucket) |>
    summarise(
      n = n(),
      mean_probability = mean(probability),
      actual_win_rate = mean(actual),
      .groups = "drop"
    )
}

score_bets <- function(results, probability_column, model_name, edge_threshold = 0.02, stake = 100) {
  betting_rows <- results |>
    mutate(
      model = model_name,
      p_a = .data[[probability_column]],
      p_b = 1 - p_a,
      a_implied_probability = odds_to_probability(a_market_odds),
      b_implied_probability = odds_to_probability(b_market_odds),
      a_edge = p_a - a_implied_probability,
      b_edge = p_b - b_implied_probability,
      bet_side = case_when(
        !is.na(a_edge) & a_edge >= edge_threshold & (is.na(b_edge) | a_edge >= b_edge) ~ "A",
        !is.na(b_edge) & b_edge >= edge_threshold ~ "B",
        TRUE ~ NA_character_
      ),
      bet_odds = case_when(
        bet_side == "A" ~ as_number(a_market_odds),
        bet_side == "B" ~ as_number(b_market_odds),
        TRUE ~ NA_real_
      ),
      bet_won = case_when(
        bet_side == "A" ~ a_won == 1,
        bet_side == "B" ~ a_won == 0,
        TRUE ~ NA
      ),
      profit = ifelse(is.na(bet_side), 0, profit_on_stake(bet_odds, bet_won, stake = stake)),
      stake = ifelse(is.na(bet_side), 0, stake),
      selected_edge = case_when(
        bet_side == "A" ~ a_edge,
        bet_side == "B" ~ b_edge,
        TRUE ~ NA_real_
      )
    )

  if (all(is.na(betting_rows$a_market_odds)) || all(is.na(betting_rows$b_market_odds))) {
    return(tibble())
  }

  betting_rows |>
    summarise(
      model = model_name,
      edge_threshold = edge_threshold,
      bets = sum(!is.na(bet_side)),
      wins = sum(bet_won, na.rm = TRUE),
      win_rate = ifelse(bets > 0, wins / bets, NA_real_),
      staked = sum(stake, na.rm = TRUE),
      profit = sum(profit, na.rm = TRUE),
      roi = ifelse(staked > 0, profit / staked, NA_real_),
      avg_selected_edge = mean(selected_edge, na.rm = TRUE)
    )
}

profile_rows_from_features <- function(features) {
  a_rows <- features |>
    transmute(
      tourney_date,
      match_id,
      player = player_a,
      archetype = a_archetype,
      prior_matches = a_prior_matches,
      surface_matches = a_surface_matches,
      charting_matches = a_charting_matches,
      charting_available = a_charting_available,
      shot_quality = a_shot_quality,
      shot_quality_proxy = a_shot_quality_proxy,
      serve_pressure = a_serve_pressure,
      chart_serve_pressure = a_chart_serve_pressure,
      return_pressure = a_return_pressure,
      chart_return_pressure = a_chart_return_pressure,
      serve_defense = a_serve_defense,
      second_serve_attack = a_second_serve_attack,
      chart_second_serve_attack = a_chart_second_serve_attack,
      forehand_quality = a_forehand_quality,
      chart_forehand_quality = a_chart_forehand_quality,
      backhand_quality = a_backhand_quality,
      chart_backhand_quality = a_chart_backhand_quality,
      chart_short_rally_quality = a_chart_short_rally_quality,
      chart_long_rally_quality = a_chart_long_rally_quality,
      ace_rate = a_ace_rate,
      df_rate = a_df_rate,
      first_serve_win_rate = a_first_serve_win_rate,
      second_serve_win_rate = a_second_serve_win_rate
    )

  b_rows <- features |>
    transmute(
      tourney_date,
      match_id,
      player = player_b,
      archetype = b_archetype,
      prior_matches = b_prior_matches,
      surface_matches = b_surface_matches,
      charting_matches = b_charting_matches,
      charting_available = b_charting_available,
      shot_quality = b_shot_quality,
      shot_quality_proxy = b_shot_quality_proxy,
      serve_pressure = b_serve_pressure,
      chart_serve_pressure = b_chart_serve_pressure,
      return_pressure = b_return_pressure,
      chart_return_pressure = b_chart_return_pressure,
      serve_defense = b_serve_defense,
      second_serve_attack = b_second_serve_attack,
      chart_second_serve_attack = b_chart_second_serve_attack,
      forehand_quality = b_forehand_quality,
      chart_forehand_quality = b_chart_forehand_quality,
      backhand_quality = b_backhand_quality,
      chart_backhand_quality = b_chart_backhand_quality,
      chart_short_rally_quality = b_chart_short_rally_quality,
      chart_long_rally_quality = b_chart_long_rally_quality,
      ace_rate = b_ace_rate,
      df_rate = b_df_rate,
      first_serve_win_rate = b_first_serve_win_rate,
      second_serve_win_rate = b_second_serve_win_rate
    )

  bind_rows(a_rows, b_rows)
}

latest_shot_profiles <- function(features, min_prior_matches = 10) {
  profile_rows_from_features(features) |>
    filter(prior_matches >= min_prior_matches) |>
    arrange(player, desc(tourney_date), desc(match_id)) |>
    group_by(player) |>
    slice(1) |>
    ungroup() |>
    arrange(desc(shot_quality), player)
}

player_archetype_report <- function(results, probability_column, min_matchups = 5) {
  a_rows <- results |>
    transmute(
      player = player_a,
      opponent = player_b,
      opponent_archetype = b_archetype,
      match_date = tourney_date,
      won = a_won == 1,
      model_probability = .data[[probability_column]],
      opponent_shot_quality = b_shot_quality,
      opponent_serve_pressure = b_serve_pressure,
      opponent_return_pressure = b_return_pressure,
      opponent_forehand_quality = b_forehand_quality,
      opponent_backhand_quality = b_backhand_quality
    )

  b_rows <- results |>
    transmute(
      player = player_b,
      opponent = player_a,
      opponent_archetype = a_archetype,
      match_date = tourney_date,
      won = a_won == 0,
      model_probability = 1 - .data[[probability_column]],
      opponent_shot_quality = a_shot_quality,
      opponent_serve_pressure = a_serve_pressure,
      opponent_return_pressure = a_return_pressure,
      opponent_forehand_quality = a_forehand_quality,
      opponent_backhand_quality = a_backhand_quality
    )

  bind_rows(a_rows, b_rows) |>
    filter(!is.na(model_probability), opponent_archetype != "Low Sample") |>
    group_by(player, opponent_archetype) |>
    summarise(
      matches = n(),
      wins = sum(won, na.rm = TRUE),
      win_rate = mean(won, na.rm = TRUE),
      expected_win_rate = mean(model_probability, na.rm = TRUE),
      actual_minus_expected = win_rate - expected_win_rate,
      avg_opponent_shot_quality = mean(opponent_shot_quality, na.rm = TRUE),
      avg_opponent_serve_pressure = mean(opponent_serve_pressure, na.rm = TRUE),
      avg_opponent_return_pressure = mean(opponent_return_pressure, na.rm = TRUE),
      avg_opponent_forehand_quality = mean(opponent_forehand_quality, na.rm = TRUE),
      avg_opponent_backhand_quality = mean(opponent_backhand_quality, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      matchup_read = case_when(
        matches < min_matchups ~ "Small sample",
        actual_minus_expected >= 0.08 ~ "Succeeds vs archetype",
        actual_minus_expected <= -0.08 ~ "Struggles vs archetype",
        TRUE ~ "Neutral"
      )
    ) |>
    arrange(player, desc(abs(actual_minus_expected)), desc(matches))
}

archetype_summary <- function(results, probability_column) {
  player_archetype_report(results, probability_column, min_matchups = 1) |>
    group_by(opponent_archetype) |>
    summarise(
      player_archetype_pairs = n(),
      total_matches = sum(matches),
      mean_win_rate = weighted.mean(win_rate, matches),
      mean_expected_win_rate = weighted.mean(expected_win_rate, matches),
      mean_actual_minus_expected = weighted.mean(actual_minus_expected, matches),
      .groups = "drop"
    ) |>
    arrange(desc(total_matches))
}

fit_trained_probability_model <- function(features, test_start_date) {
  model_terms <- c(
    "overall_elo_diff",
    "surface_elo_diff",
    "serve_return_diff",
    "surface_form_diff",
    "recent_form_diff",
    "rank_strength_diff",
    "best_of5_elo_diff",
    "shot_quality_diff",
    "serve_pressure_diff",
    "return_pressure_diff",
    "second_serve_attack_diff",
    "archetype_delta_diff",
    "archetype_win_rate_diff"
  )

  training_data <- features |>
    filter(eligible, tourney_date < test_start_date) |>
    select(a_won, all_of(model_terms)) |>
    filter(if_all(everything(), ~ !is.na(.x)))

  if (nrow(training_data) < 200 || length(unique(training_data$a_won)) < 2) {
    warning("Not enough pre-test data to fit trained model. Only prototype probabilities will be scored.", call. = FALSE)
    return(NULL)
  }

  formula <- as.formula(paste("a_won ~", paste(model_terms, collapse = " + ")))
  stats::glm(formula, data = training_data, family = stats::binomial())
}

run_tennis_backtest <- function(
    start_year = 2015,
    end_year = 2025,
    test_start_date = as.Date("2023-01-01"),
    min_prior_matches = 10,
    refresh_data = FALSE,
    use_match_charting = TRUE,
    refresh_charting = FALSE,
    output_dir = "outputs",
    edge_threshold = 0.02,
    stake = 100) {
  ensure_directory(output_dir)
  test_start_date <- as.Date(test_start_date)

  matches <- load_atp_matches(
    start_year = start_year,
    end_year = end_year,
    refresh = refresh_data
  )
  charting_profiles <- if (isTRUE(use_match_charting)) {
    load_match_charting_profiles(refresh = refresh_charting)
  } else {
    tibble()
  }

  message("Building leakage-free walk-forward features...")
  features <- generate_walk_forward_features(
    matches,
    charting_profiles = charting_profiles,
    min_prior_matches = min_prior_matches
  )

  features$prototype_p_a <- prototype_probability(features)
  features$prototype_fair_odds_a <- prob_to_american(features$prototype_p_a)
  features$prototype_fair_odds_b <- prob_to_american(1 - features$prototype_p_a)

  trained_model <- fit_trained_probability_model(features, test_start_date)
  if (!is.null(trained_model)) {
    features$trained_p_a <- as.numeric(stats::predict(trained_model, newdata = features, type = "response"))
    features$trained_fair_odds_a <- prob_to_american(features$trained_p_a)
    features$trained_fair_odds_b <- prob_to_american(1 - features$trained_p_a)
  } else {
    features$trained_p_a <- NA_real_
    features$trained_fair_odds_a <- NA_real_
    features$trained_fair_odds_b <- NA_real_
  }

  test_results <- features |>
    filter(eligible, tourney_date >= test_start_date)

  model_specs <- tibble(
    model = c("prototype_weights", "trained_glm"),
    probability_column = c("prototype_p_a", "trained_p_a")
  )
  model_specs <- model_specs[
    vapply(
      model_specs$probability_column,
      function(column) any(!is.na(test_results[[column]])),
      logical(1)
    ),
    ,
    drop = FALSE
  ]

  summaries <- purrr::pmap_dfr(
    model_specs,
    function(model, probability_column) {
      model_summary(test_results, probability_column, model)
    }
  )

  calibration <- purrr::pmap_dfr(
    model_specs,
    function(model, probability_column) {
      calibration_table(test_results, probability_column, model)
    }
  )

  betting <- purrr::pmap_dfr(
    model_specs,
    function(model, probability_column) {
      score_bets(
        test_results,
        probability_column,
        model,
        edge_threshold = edge_threshold,
        stake = stake
      )
    }
  )

  report_probability_column <- if ("trained_p_a" %in% model_specs$probability_column) {
    "trained_p_a"
  } else {
    "prototype_p_a"
  }
  profiles <- latest_shot_profiles(features, min_prior_matches = min_prior_matches)
  player_archetypes <- player_archetype_report(test_results, report_probability_column)
  archetypes <- archetype_summary(test_results, report_probability_column)

  prediction_path <- file.path(output_dir, "tennis_backtest_predictions.csv")
  summary_path <- file.path(output_dir, "tennis_backtest_summary.csv")
  calibration_path <- file.path(output_dir, "tennis_backtest_calibration.csv")
  betting_path <- file.path(output_dir, "tennis_backtest_betting.csv")
  profile_path <- file.path(output_dir, "tennis_latest_shot_profiles.csv")
  player_archetype_path <- file.path(output_dir, "tennis_player_archetype_report.csv")
  archetype_summary_path <- file.path(output_dir, "tennis_archetype_summary.csv")
  charting_profile_path <- file.path(output_dir, "tennis_match_charting_profiles.csv")
  plot_path <- file.path(output_dir, "tennis_backtest_calibration.png")

  readr::write_csv(test_results, prediction_path)
  readr::write_csv(summaries, summary_path)
  readr::write_csv(calibration, calibration_path)
  readr::write_csv(betting, betting_path)
  readr::write_csv(profiles, profile_path)
  readr::write_csv(player_archetypes, player_archetype_path)
  readr::write_csv(archetypes, archetype_summary_path)
  readr::write_csv(charting_profiles, charting_profile_path)

  if (nrow(calibration) > 0) {
    calibration_plot <- ggplot(
      calibration,
      aes(mean_probability, actual_win_rate, color = model)
    ) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#606060") +
      geom_point(aes(size = n), alpha = 0.85) +
      geom_line(aes(group = model), linewidth = 0.8) +
      scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), limits = c(0, 1)) +
      scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), limits = c(0, 1)) +
      labs(
        title = "Tennis Model Backtest Calibration",
        x = "Mean predicted probability",
        y = "Actual win rate",
        size = "Matches"
      ) +
      theme_minimal(base_size = 12)

    ggplot2::ggsave(plot_path, calibration_plot, width = 7.5, height = 5, dpi = 160)
  }

  message(sprintf("Saved predictions to %s", prediction_path))
  message(sprintf("Saved summary to %s", summary_path))
  message(sprintf("Saved calibration to %s", calibration_path))
  message(sprintf("Saved betting summary to %s", betting_path))
  message(sprintf("Saved latest shot profiles to %s", profile_path))
  message(sprintf("Saved player archetype report to %s", player_archetype_path))
  message(sprintf("Saved archetype summary to %s", archetype_summary_path))
  message(sprintf("Saved Match Charting profiles to %s", charting_profile_path))

  list(
    predictions = test_results,
    summary = summaries,
    calibration = calibration,
    betting = betting,
    profiles = profiles,
    player_archetypes = player_archetypes,
    archetypes = archetypes,
    charting_profiles = charting_profiles,
    trained_model = trained_model,
    files = list(
      predictions = prediction_path,
      summary = summary_path,
      calibration = calibration_path,
      betting = betting_path,
      profiles = profile_path,
      player_archetypes = player_archetype_path,
      archetypes = archetype_summary_path,
      charting_profiles = charting_profile_path,
      calibration_plot = plot_path
    )
  )
}


make_surface_profile_rows <- function(predictions, surface_choice = "Hard", min_prior_matches = 10) {
  data <- predictions |>
    filter(surface == surface_choice, eligible)

  if (nrow(data) == 0) {
    data <- predictions |>
      filter(eligible)
  }

  profile_rows_from_features(data) |>
    filter(prior_matches >= min_prior_matches) |>
    arrange(player, desc(tourney_date), desc(match_id)) |>
    group_by(player) |>
    slice(1) |>
    ungroup() |>
    arrange(player)
}

build_app_pair_features <- function(player_a, player_b, profile_data, best_of = 3) {
  a <- profile_data |>
    filter(player == player_a) |>
    slice(1)
  b <- profile_data |>
    filter(player == player_b) |>
    slice(1)

  if (nrow(a) == 0 || nrow(b) == 0 || player_a == player_b) {
    return(tibble())
  }

  serve_return_diff <- (a$serve_pressure - b$return_pressure) -
    (b$serve_pressure - a$return_pressure)

  tibble(
    player_a = player_a,
    player_b = player_b,
    best_of = as.integer(best_of),
    a_archetype = a$archetype,
    b_archetype = b$archetype,
    a_prior_matches = a$prior_matches,
    b_prior_matches = b$prior_matches,
    a_surface_matches = a$surface_matches,
    b_surface_matches = b$surface_matches,
    a_charting_matches = a$charting_matches,
    b_charting_matches = b$charting_matches,
    a_shot_quality = a$shot_quality,
    b_shot_quality = b$shot_quality,
    a_serve_pressure = a$serve_pressure,
    b_serve_pressure = b$serve_pressure,
    a_return_pressure = a$return_pressure,
    b_return_pressure = b$return_pressure,
    a_second_serve_attack = a$second_serve_attack,
    b_second_serve_attack = b$second_serve_attack,
    a_forehand_quality = a$forehand_quality,
    b_forehand_quality = b$forehand_quality,
    a_backhand_quality = a$backhand_quality,
    b_backhand_quality = b$backhand_quality,
    a_ace_rate = a$ace_rate,
    b_ace_rate = b$ace_rate,
    a_df_rate = a$df_rate,
    b_df_rate = b$df_rate,
    a_first_serve_win_rate = a$first_serve_win_rate,
    b_first_serve_win_rate = b$first_serve_win_rate,
    a_second_serve_win_rate = a$second_serve_win_rate,
    b_second_serve_win_rate = b$second_serve_win_rate,
    overall_elo_diff = 0,
    surface_elo_diff = 0,
    serve_return_diff = as.numeric(serve_return_diff),
    surface_form_diff = 0,
    recent_form_diff = 0,
    rank_strength_diff = 0,
    best_of5_elo_diff = 0,
    shot_quality_diff = as.numeric(a$shot_quality - b$shot_quality),
    serve_pressure_diff = as.numeric(a$serve_pressure - b$serve_pressure),
    return_pressure_diff = as.numeric(a$return_pressure - b$return_pressure),
    second_serve_attack_diff = as.numeric(a$second_serve_attack - b$second_serve_attack),
    archetype_delta_diff = 0,
    archetype_win_rate_diff = 0
  )
}

make_matchup_explanation <- function(pair_features) {
  if (nrow(pair_features) == 0) {
    return("Choose two different players with enough historical data.")
  }

  metrics <- tibble(
    metric = c(
      "overall shot quality",
      "serve pressure",
      "return pressure",
      "second-serve attack",
      "forehand quality",
      "backhand quality"
    ),
    diff = c(
      pair_features$shot_quality_diff,
      pair_features$serve_pressure_diff,
      pair_features$return_pressure_diff,
      pair_features$second_serve_attack_diff,
      pair_features$a_forehand_quality - pair_features$b_forehand_quality,
      pair_features$a_backhand_quality - pair_features$b_backhand_quality
    )
  ) |>
    arrange(desc(abs(diff)))

  top <- metrics |> slice(1)
  direction <- ifelse(top$diff >= 0, pair_features$player_a, pair_features$player_b)

  sprintf(
    "Biggest modeled edge: %s has the advantage in %s. Player archetypes: %s = %s, %s = %s.",
    direction,
    top$metric,
    pair_features$player_a,
    pair_features$a_archetype,
    pair_features$player_b,
    pair_features$b_archetype
  )
}

format_percent <- function(x) {
  sprintf("%.1f%%", 100 * as.numeric(x))
}

format_metric <- function(x) {
  sprintf("%.3f", as.numeric(x))
}

launch_tennis_shiny_app <- function(
    start_year = 2018,
    end_year = 2025,
    min_prior_matches = 5,
    refresh_data = FALSE,
    use_match_charting = TRUE,
    refresh_charting = FALSE,
    output_dir = "outputs") {
  ensure_directory(output_dir)

  message("Loading ATP matches...")
  matches <- load_atp_matches(
    start_year = start_year,
    end_year = end_year,
    refresh = refresh_data
  )

  charting_profiles <- if (isTRUE(use_match_charting)) {
    load_match_charting_profiles(refresh = refresh_charting)
  } else {
    tibble()
  }

  message("Building current prediction state...")
  model_state <- build_current_model_state(
    matches = matches,
    charting_profiles = charting_profiles,
    progress_every = 1000
  )

  player_counts <- tibble(
    player = names(model_state$match_n),
    matches = as.numeric(model_state$match_n)
  ) |>
    filter(matches >= min_prior_matches) |>
    arrange(player)

  if (nrow(player_counts) < 2) {
    stop("Not enough players with prior match history. Try lowering min_prior_matches.", call. = FALSE)
  }

  players <- player_counts$player
  surfaces <- matches |>
    filter(!is.na(surface), surface != "Unknown") |>
    distinct(surface) |>
    arrange(surface) |>
    pull(surface)

  default_surface <- ifelse("Hard" %in% surfaces, "Hard", surfaces[[1]])

  model_summary_data <- tibble(
    item = c("Historical matches loaded", "Players available", "Match Charting profiles loaded"),
    value = c(nrow(matches), length(players), nrow(charting_profiles))
  )

  ui <- fluidPage(
    titlePanel("Tennis Matchup Profit Model"),

    sidebarLayout(
      sidebarPanel(
        selectInput("player_a", "Player A", choices = players, selected = players[[1]]),
        selectInput("player_b", "Player B", choices = players, selected = players[[min(2, length(players))]]),
        selectInput("surface", "Surface", choices = surfaces, selected = default_surface),
        radioButtons("best_of", "Match Format", choices = c("Best of 3" = 3, "Best of 5" = 5), selected = 3),
        selectInput(
          "probability_mode",
          "Probability Mode",
          choices = c(
            "Model only" = "model",
            "Shot-matchup ensemble" = "ensemble_no_market",
            "Market-blended ensemble" = "ensemble_market"
          ),
          selected = "ensemble_no_market"
        ),
        numericInput("odds_a", "Player A Sportsbook Odds", value = NA, step = 5),
        numericInput("odds_b", "Player B Sportsbook Odds", value = NA, step = 5),
        numericInput("closing_odds_a", "Player A Closing Odds (optional)", value = NA, step = 5),
        numericInput("closing_odds_b", "Player B Closing Odds (optional)", value = NA, step = 5),
        helpText("This version uses the historical model state, not a 50/50 placeholder. Odds are used for market edge, EV, Kelly, and CLV checks.")
      ),

      mainPanel(
        h3(textOutput("matchup_title")),
        fluidRow(
          column(4, h4("Player A Win Probability"), h2(textOutput("prob_a"))),
          column(4, h4("Player B Win Probability"), h2(textOutput("prob_b"))),
          column(4, h4("Fair Odds"), h2(textOutput("fair_odds")))
        ),
        br(),
        strong(textOutput("matchup_explanation")),
        br(), br(),
        h4("Player Quality Comparison"),
        tableOutput("quality_table"),
        h4("Feature Differences Used by Model"),
        tableOutput("feature_table"),
        h4("Betting Edge / CLV Check"),
        tableOutput("betting_table"),
        h4("Shot-Matchup Weakness Read"),
        tableOutput("weakness_table"),
        h4("Loaded Data Summary"),
        tableOutput("model_summary")
      )
    )
  )

  server <- function(input, output, session) {
    pair_features <- reactive({
      req(input$player_a, input$player_b, input$surface)
      validate(need(input$player_a != input$player_b, "Choose two different players."))

      compute_pair_features(
        player_a = input$player_a,
        player_b = input$player_b,
        surface = input$surface,
        best_of = as.integer(input$best_of),
        date = Sys.Date(),
        rank_a = NA,
        rank_b = NA,
        state = model_state
      ) |>
        mutate(
          player_a = input$player_a,
          player_b = input$player_b
        )
    })

    market_probability_a <- reactive({
      no_vig_market_probability(input$odds_a, input$odds_b)
    })

    probability_a <- reactive({
      features <- pair_features()
      validate(need(nrow(features) > 0, "Choose two different players."))

      if (input$probability_mode == "model") {
        return(as.numeric(prototype_probability(features)))
      }

      if (input$probability_mode == "ensemble_market") {
        return(as.numeric(ensemble_probability(features, market_probability = market_probability_a())))
      }

      as.numeric(ensemble_probability(features, market_probability = NA_real_))
    })

    output$matchup_title <- renderText({
      paste(input$player_a, "vs", input$player_b, "on", input$surface)
    })

    output$prob_a <- renderText({
      format_percent(probability_a())
    })

    output$prob_b <- renderText({
      format_percent(1 - probability_a())
    })

    output$fair_odds <- renderText({
      p_a <- probability_a()
      paste0(input$player_a, ": ", prob_to_american(p_a), " / ", input$player_b, ": ", prob_to_american(1 - p_a))
    })

    output$matchup_explanation <- renderText({
      make_matchup_explanation(pair_features())
    })

    output$quality_table <- renderTable({
      features <- pair_features()

      tibble(
        Metric = c(
          "Archetype",
          "Prior matches",
          "Surface matches",
          "Match Charting matches",
          "Shot quality",
          "Serve pressure",
          "Return pressure",
          "Second-serve attack",
          "Forehand quality",
          "Backhand quality",
          "Ace rate",
          "Double-fault rate",
          "First-serve win rate",
          "Second-serve win rate"
        ),
        `Player A` = c(
          features$a_archetype,
          features$a_prior_matches,
          features$a_surface_matches,
          features$a_charting_matches,
          format_metric(features$a_shot_quality),
          format_metric(features$a_serve_pressure),
          format_metric(features$a_return_pressure),
          format_metric(features$a_second_serve_attack),
          format_metric(features$a_forehand_quality),
          format_metric(features$a_backhand_quality),
          format_percent(features$a_ace_rate),
          format_percent(features$a_df_rate),
          format_percent(features$a_first_serve_win_rate),
          format_percent(features$a_second_serve_win_rate)
        ),
        `Player B` = c(
          features$b_archetype,
          features$b_prior_matches,
          features$b_surface_matches,
          features$b_charting_matches,
          format_metric(features$b_shot_quality),
          format_metric(features$b_serve_pressure),
          format_metric(features$b_return_pressure),
          format_metric(features$b_second_serve_attack),
          format_metric(features$b_forehand_quality),
          format_metric(features$b_backhand_quality),
          format_percent(features$b_ace_rate),
          format_percent(features$b_df_rate),
          format_percent(features$b_first_serve_win_rate),
          format_percent(features$b_second_serve_win_rate)
        )
      )
    })

    output$feature_table <- renderTable({
      features <- pair_features()
      feature_names <- names(default_model_weights())

      tibble(
        Feature = feature_names,
        Difference = sprintf("%.4f", unlist(features[feature_names], use.names = FALSE)),
        Weight = sprintf("%.2f", default_model_weights()[feature_names])
      )
    })

    output$betting_table <- renderTable({
      p_a <- probability_a()
      p_b <- 1 - p_a
      implied_a <- odds_to_probability(input$odds_a)
      implied_b <- odds_to_probability(input$odds_b)
      market_a <- market_probability_a()
      edge_a <- p_a - implied_a
      edge_b <- p_b - implied_b
      ev_a <- expected_value_per_100(p_a, input$odds_a)
      ev_b <- expected_value_per_100(p_b, input$odds_b)
      kelly_a <- kelly_fraction(p_a, input$odds_a)
      kelly_b <- kelly_fraction(p_b, input$odds_b)
      closing_market_a <- no_vig_market_probability(input$closing_odds_a, input$closing_odds_b)
      clv_a <- ifelse(is.na(closing_market_a) || is.na(implied_a), NA_real_, closing_market_a - implied_a)
      clv_b <- ifelse(is.na(closing_market_a) || is.na(implied_b), NA_real_, (1 - closing_market_a) - implied_b)

      tibble(
        Metric = c(
          "No-vig market probability",
          "Model/ensemble probability",
          "Edge vs listed implied probability",
          "Expected value per $100",
          "Quarter-Kelly bankroll fraction",
          "CLV after close is entered"
        ),
        `Player A` = c(
          ifelse(is.na(market_a), "Enter both odds", format_percent(market_a)),
          format_percent(p_a),
          ifelse(is.na(edge_a), "Enter odds", format_percent(edge_a)),
          ifelse(is.na(ev_a), "Enter odds", sprintf("$%.2f", ev_a)),
          ifelse(is.na(kelly_a), "Enter odds", format_percent(kelly_a)),
          ifelse(is.na(clv_a), "Enter closing odds", format_percent(clv_a))
        ),
        `Player B` = c(
          ifelse(is.na(market_a), "Enter both odds", format_percent(1 - market_a)),
          format_percent(p_b),
          ifelse(is.na(edge_b), "Enter odds", format_percent(edge_b)),
          ifelse(is.na(ev_b), "Enter odds", sprintf("$%.2f", ev_b)),
          ifelse(is.na(kelly_b), "Enter odds", format_percent(kelly_b)),
          ifelse(is.na(clv_b), "Enter closing odds", format_percent(clv_b))
        )
      )
    })

    output$weakness_table <- renderTable({
      features <- pair_features()

      tibble(
        Read = c(
          paste0(features$player_a, " attacking ", features$player_b, " backhand"),
          paste0(features$player_b, " attacking ", features$player_a, " backhand"),
          paste0(features$player_a, " attacking second serve"),
          paste0(features$player_b, " attacking second serve"),
          paste0(features$player_a, " return pressure vs ", features$player_b, " serve"),
          paste0(features$player_b, " return pressure vs ", features$player_a, " serve")
        ),
        Score = sprintf(
          "%.4f",
          c(
            features$a_backhand_quality - features$b_backhand_quality,
            features$b_backhand_quality - features$a_backhand_quality,
            features$a_second_serve_attack - features$b_second_serve_win_rate,
            features$b_second_serve_attack - features$a_second_serve_win_rate,
            features$a_return_pressure - features$b_serve_pressure,
            features$b_return_pressure - features$a_serve_pressure
          )
        )
      )
    })

    output$model_summary <- renderTable({
      model_summary_data
    })
  }

  shinyApp(ui = ui, server = server)
}

app <- launch_tennis_shiny_app()
app

