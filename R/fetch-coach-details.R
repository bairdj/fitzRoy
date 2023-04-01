#' Fetch coach details
#'
#' @description
#' `fetch_coach_details` returns coach details such as teams coached,
#' number of wins, losses and draws, premierships and grand finals.
#'
#' By default the source used will be AFL Tables.
#'
#' @param team Only fetch details for coaches of this team. NULL returns all coaches.
#' @param source The source to use. By default this is "afltables".
#' @return A tibble with coach details.
#' 
#' @details
#' If the team argument is provided, the function will only include matches
#' where the specified team was coached by the coach in question. If the
#' coach has coached multiple teams, only matches where they coached
#' the specified team will be included.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' coaches <- fetch_coach_details()
#' }
#'
#' @family fetch coach functions
fetch_coach_details <- function(team = NULL, source = "afltables") {
  dat <- switch(source,
    "afltables" = fetch_coach_details_afltables(team),
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

#' Fetch coach details from AFL Tables
#' 
#' @describeIn fetch_coach_details Fetch coach details from AFL Tables.
fetch_coach_details_afltables <- function(team) {
  if (!is.null(team)) {
    team_check_afltables(team)
  }

  coach_table <- if(is.null(team)) {
    get_all_coaches_table()
  } else {
    get_coaches_by_team_table(team)
  }

  process_coach_detail_table(coach_table)
}
