# function: split values between isic code in isiccomb group
split_isiccomb <- function(threefour_df) {
    #' Helper function to split isiccomb values across isic codes
    #' @param threefour_df df with 3/4 digit values across isic & isiccomb

    # make list for interim tables
    interim <- list()

    # extract rows with isiccomb codes
    interim$isiccomb.rows <-
        threefour_df %>%
        filter(., str_detect(isiccomb, "[:alpha:]"))

    # test that we are not losing any data through spliting
    test_that("No `country,year` has more than one recorded `value` per `isiccomb` group", {
        rows_w_many_values_per_isiccomb <-
            interim$isiccomb.rows %>%
            group_by(country, year, isiccomb) %>%
            ## get  no of recorded (not NA) values for given `country, year, isiccomb`
            summarise(n_obs = sum(!is.na(value))) %>%
            filter(n_obs != 1) %>%
            nrow()
        expect_true(rows_w_many_values_per_isiccomb == 0)
    })

    # calculate average value over isiccomb group for each country, year
    interim$isiccomb.avg <-
        interim$isiccomb.rows %>%
        # group isiccomb rows, replace na with 0 for averaging
        group_by(country, year, isiccomb) %>%
        mutate(value = replace_na(value, 0)) %>%
        # split combination value over standard isic codes in isiccomb group
        summarise(
            avg.value = mean(value),
            ## checking variables
            n_isic = n_distinct(isic),
            n_rows = n()
        ) %>%
        mutate(row_check = (n_isic == n_rows))

    #  return(interim$isiccomb.avg)

    ## check n_isic == n_rows
    test_that("isiccomb split average is calculated with correct denominator", {
        expect_true(all(interim$isiccomb.avg$row_check))
    })

    # output processed data
    final <-
        left_join(threefour_df, interim$isiccomb.avg, by = c("country", "year", "isiccomb")) %>%
        rename(value.nosplit = value) %>%
        mutate(
            value = coalesce(avg.value, value.nosplit),
            split.isiccomb = !is.na(avg.value)
        ) %>%
        select(country, year, isic, isiccomb, value, value.nosplit, split.isiccomb) # not checking variables

    return(final)
}
