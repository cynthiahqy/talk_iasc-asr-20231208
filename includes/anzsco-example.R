## set-up data

anzsco_cw <- tibble::tribble(
    ~anzsco22, ~anzsco22_descr, ~isco8, ~partial, ~isco8_descr,
    "111111", "Chief Executive or Managing Director", "1112", "p", "Senior government officials",
    "111111", "Chief Executive or Managing Director", "1114", "p", "Senior officials of special-interest organizations",
    "111111", "Chief Executive or Managing Director", "1120", "p", "Managing directors and chief executives",
    "111211", "Corporate General Manager", "1112", "p", "Senior government officials",
    "111211", "Corporate General Manager", "1114", "p", "Senior officials of special-interest organizations",
    "111211", "Corporate General Manager", "1120", "p", "Managing directors and chief executives",
    "111212", "Defence Force Senior Officer", "0110", "p", "Commissioned armed forces officers",
    "111311", "Local Government Legislator", "1111", "p", "Legislators",
    "111312", "Member of Parliament", "1111", "p", "Legislators",
    "111399", "Legislators nec", "1111", "p", "Legislators"
)

## get code tables
table_anzsco <- anzsco_cw |>
    dplyr::distinct(anzsco22, anzsco22_descr)
table_isco8 <- anzsco_cw |>
    dplyr::distinct(isco8, isco8_descr)

## make xmap
anzsco_xmap_df <- anzsco_cw |>
    xmap::add_weights_equal(from = anzsco22, to = isco8) |>
    xmap::as_xmap_df(
        from = anzsco22,
        to = isco8,
        weights = weights,
        .drop_extra = TRUE
    )

## create xmap

anzsco_xmap <- anzsco_cw |>
    xmap::add_weights_equal(from = anzsco22, to = isco8) |>
    xmap::verify_links_as_xmap(
        from = anzsco22,
        to = isco8,
        weights = weights
    ) |>
    dplyr::select(anzsco22, isco8, weights)

## create anzsco mock data

anzsco22_stats <-
    tibble::tribble(
        ~anzsco22, ~count,
        111111, 1000,
        111211, 500,
        111212, 40,
        111311, 300,
        111312, 150,
        111399, 10
    ) |>
    dplyr::mutate(anzsco22 = as.character(anzsco22))

## mock apply_xmap function

apply_xmap <- function(.data, .xmap) {
    dplyr::left_join(
        x = .data,
        y = .xmap,
        by = "anzsco22"
    ) |>
        dplyr::mutate(part_count = count * weights) |>
        dplyr::group_by(isco8) |>
        dplyr::summarise(new_count = sum(part_count))
}
