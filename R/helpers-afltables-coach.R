get_all_coaches_table <- function() {
  url <- "https://afltables.com/afl/stats/coaches/coaches_idx.html"

  html <- rvest::read_html(url)

  tbl <- rvest::html_table(html) %>%
    purrr::pluck(1)

  names(tbl) <- c(
    "coach",
    "teams",
    "season",
    "home_away_wins",
    "home_away_draws",
    "home_away_losses",
    "home_away_matches",
    "home_away_win_pct",
    "finals_wins",
    "finals_draws",
    "finals_losses",
    "finals_matches",
    "finals_win_pct",
    "total_wins",
    "total_draws",
    "total_losses",
    "total_matches",
    "total_win_pct",
    "premierships",
    "grand_finals"
  )

  tbl
}

get_coaches_by_team_table <- function(team) {
  abr <- team_abr_afltables(team)
  if (is.null(abr) | abr == "") {
    rlang::abort(glue::glue("Team {team} is not valid."))
  }
  url <- glue::glue("https://afltables.com/afl/stats/coaches/{abr}.html")

  html <- rvest::read_html(url)

  tbl <- rvest::html_table(html) %>%
    purrr::pluck(1)

  names(tbl) <- c(
    "coach",
    "season",
    "home_away_wins",
    "home_away_draws",
    "home_away_losses",
    "home_away_matches",
    "home_away_win_pct",
    "finals_wins",
    "finals_draws",
    "finals_losses",
    "finals_matches",
    "finals_win_pct",
    "total_wins",
    "total_draws",
    "total_losses",
    "total_matches",
    "total_win_pct",
    "premierships",
    "grand_finals"
  )

  tbl
}

process_coach_detail_table <- function(coach_table) {
  coach_table <- coach_table[2:nrow(coach_table), ]

  # Exclude "*" rows
  coach_table <- coach_table[!grepl("^\\*", coach_table$coach), ]

  # Split name into first and last
  coach_table <- coach_table %>%
    tidyr::separate(
      .data$coach,
      into = c("last_name", "first_name"),
      sep = ","
    ) %>%
    dplyr::mutate(dplyr::across(c(.data$first_name, .data$last_name), trimws))

  # Split season into start and end
  coach_table <- coach_table %>%
    tidyr::separate(
      .data$season,
      into = c("season_start", "season_end"),
      sep = "-",
      fill = "right"
    ) %>%
    dplyr::mutate(
      season_end = dplyr::coalesce(.data$season_end, .data$season_start)
    )

  # Columns that contain N matches
  # All should be numeric and 0 if missing
  n_columns <- rlang::expr(c(
    tidyselect::ends_with("_wins"),
    tidyselect::ends_with("_draws"),
    tidyselect::ends_with("_losses"),
    tidyselect::ends_with("_matches"),
    .data$premierships,
    .data$grand_finals
  ))

  clean_number <- function(x) {
    x <- gsub("[^0-9]", "", x)
    as.numeric(ifelse(x == "", 0, x))
  }

  # Convert numeric columns to numeric
  coach_table <- coach_table %>%
    dplyr::mutate(dplyr::across(!!n_columns, clean_number))

  # Split teams into a list
  if ("teams" %in% names(coach_table)) {
      coach_table <- coach_table %>%
        dplyr::mutate(teams = purrr::map(strsplit(.data$teams, " "), get_full_team_afltables))
  }

  # Drop win percentage columns
  # There are different ways to calculate this, so leave up to user
  coach_table %>%
    dplyr::select(-tidyselect::ends_with("_win_pct"))
}