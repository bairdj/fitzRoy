#' Fetch coach details
#'
#' @description
#' `fetch_coach_details` returns coach details such as teams coached,
#' number of wins, losses and draws, premierships and grand finals.
#'
#' By default the source used will be AFL Tables.
#'
#' @param source The source to use. By default this is "afltables".
#' @return A tibble with coach details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' coaches <- fetch_coach_details()
#' }
#'
#' @family fetch coach functions
#' @seealso
#' * [fetch_coach_details_afltables] for AFL Tables data.
fetch_coach_details <- function(source = "afltables") {
  dat <- switch(source,
    "afltables" = fetch_coach_details_afltables(),
    NULL
  )

  if (is.null(dat)) {
    warning_message <- glue::glue(
      "The source \"{source}\" does not have coach details data. ",
      "Please use \"afltables\"."
    )
    rlang::warn(warning_message)
  }
  dat
}


fetch_coach_details_afltables <- function() {
  coach_url <- "https://afltables.com/afl/stats/coaches/coaches_idx.html"

  coach_xml <- rvest::read_html(coach_url)

  # Extract the record table
  # This has a two-row header, so skip these and manually set col names
  coach_table <- rvest::html_table(coach_xml) %>%
    purrr::pluck(1)

  names(coach_table) <- c(
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
  coach_table <- coach_table %>%
    dplyr::mutate(teams = purrr::map(strsplit(.data$teams, " "), get_full_team_afltables))

  # Drop win percentage columns
  # There are different ways to calculate this, so leave up to user
  coach_table %>%
    dplyr::select(-tidyselect::ends_with("_win_pct"))
}
