#' Gets player debut details from afltables
#'
#'
#' @param team Team played for
#'
#'
#' @keywords internal
#' @noRd
get_player_debut_afltables <- function(team = NULL) {
  if (!is.null(team)) valid_team <- team_check_afltables(team)

  column_names <- c(
    "num", "Player", "DOB", "debut_round", "Team", "v", "Oppo", "debut_date"
  )

  url <- "https://afltables.com/afl/stats/biglists/bg10.txt"

  df <- suppressMessages(
    readr::read_table(url,
      skip = 2,
      col_names = column_names
    )
  )

  df <- df %>%
    dplyr::mutate_at(c("DOB", "debut_date"), lubridate::dmy) %>%
    dplyr::mutate(debut_season = as.numeric(format(.data$debut_date, "%Y")))

  # Fix teams
  df <- df %>%
    dplyr::mutate(
      debut_team = get_full_team_afltables(.data$Team),
      debut_opposition = get_full_team_afltables(.data$Oppo)
    )


  # Filter out team
  if (!is.null(team)) {
    df <- df %>%
      dplyr::filter(.data$debut_team %in% team)
  }


  df %>%
    dplyr::select(
      "Player", 
      "DOB", 
      "debut_date", 
      "debut_season",
      "debut_round", 
      "debut_team", 
      "debut_opposition"
    )
}

get_player_details_afltables <- function(team) {
  cli_team <- cli::cli_process_start("Fetching player details for {team}")
  valid_team <- team_check_afltables(team)

  team_abr <- team_abr_afltables(team)

  url <- paste0("https://afltables.com/afl/stats/alltime/", team_abr, ".html")
  html <- rvest::read_html(url)

  df <- html %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    dplyr::mutate(Team = team) %>%
    dplyr::slice(1:dplyr::n() - 1) %>%
    tidyr::separate("Games (W-D-L)",
      into = c("Games", "Wins", "Draws", "Losses", "x"),
      fill = "right"
    ) %>%
    dplyr::select(-'x') %>%
    dplyr::mutate(date_accessed = Sys.Date()) %>%
    tidyr::separate("Player",
      into = c("surname", "firstname"),
      sep = ",", fill = "right"
    ) %>%
    dplyr::mutate(Player = paste0(
      trimws(.data$firstname),
      " ",
      trimws(.data$surname)
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::one_of(c(
        "Cap", "Games", "Wins", "Draws",
        "Losses", "Goals"
      )),
      as.numeric
    )) %>%
    dplyr::select(
      "Player", 
      "Team",
      dplyr::everything(),
      -"surname", 
      -"firstname", 
      -"DOB"
    ) %>%
    dplyr::arrange(.data$Cap)

  cli::cli_process_done(cli_team)
  return(df)
}
