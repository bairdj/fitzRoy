test_that("fetch_coach_details returns tibble with expected columns", {
    testthat::skip_if_offline()
    testthat::skip_on_cran()

    expected_columns <- c(
        "last_name",
        "first_name",
        "teams",
        "season_start",
        "season_end",
        "home_away_wins",
        "home_away_draws",
        "home_away_losses",
        "home_away_matches",
        "finals_wins",
        "finals_draws",
        "finals_losses",
        "finals_matches",
        "total_wins",
        "total_draws",
        "total_losses",
        "total_matches",
        "premierships",
        "grand_finals"
    )

    details <- fetch_coach_details()
    expect_s3_class(details, "tbl")
    expect_equal(names(details), expected_columns)
    
})

test_that("fetch_coach_details handles unimplemented source", {
    testthat::skip_if_offline()
    testthat::skip_on_cran()

    # Should only work with afltables
    expect_warning(fetch_coach_details(source = "AFL"))
    expect_warning(fetch_coach_details(source = "footywire"))
    expect_null(suppressWarnings(fetch_coach_details(source = "AFL")))
    expect_null(suppressWarnings(fetch_coach_details(source = "footywire")))
})

test_that("fetch_coach_details dispatches to source", {
    expect_equal(fetch_coach_details(source = "afltables"), fetch_coach_details_afltables())
})