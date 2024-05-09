library(xmap)
library(ggplot2)
library(ggraph)

abc_xmap <- tibble::tribble(
  ~from, ~to, ~weights,
  "a", "AA", 1,
  "b", "AA", 1,
  "c", "AA", 1,
  "d", "BB", 1,
  "e", "CC", 1,
  "f", "DD", 0.3,
  "f", "EE", 0.3,
  "f", "FF", 0.4
) |>
  as_xmap_df(from, to, weights)

abc_bigraph <- abc_xmap |> autoplot() +
  scale_fill_brewer(palette = "Greys") +
  scale_x_reverse()

# abc_print <- abc_xmap |> print()

plt_xmap_ggmatrix <- function(x, ...) {
  stopifnot(is_xmap_df(x))
  x_attrs <- attributes(x)
  edges_complete <- tibble::as_tibble(x) |>
    tidyr::complete(.data[[x_attrs$col_from]], .data[[x_attrs$col_to]])

  ## add link-out type
  gg_df <- edges_complete |>
    dplyr::mutate(out_case = dplyr::case_when(
      .data[[x_attrs$col_weights]] == 1 ~ "one-to-one",
      .data[[x_attrs$col_weights]] < 1 ~ "one-to-many",
      is.na(.data[[x_attrs$col_weights]]) ~ "no link"
    ))

  ## make plot
  gg_df |> ggplot(aes(
    x = .data[[x_attrs$col_to]],
    y = .data[[x_attrs$col_from]]
  )) +
    geom_tile(aes(fill = out_case), col = "grey") +
    scale_y_discrete(limits = rev) +
    scale_x_discrete(position = "top") +
    coord_fixed() +
    labs(x = x_attrs$col_to, y = x_attrs$col_from, fill = "Outgoing Relation Type") +
    theme_minimal() +
    geom_text(
      data = dplyr::filter(gg_df, !is.na(.data[[x_attrs$col_weights]])),
      mapping = aes(label = round(.data[[x_attrs$col_weights]], 2)),
      size = 5
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

abc_matrix <- plt_xmap_ggmatrix(abc_xmap) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_brewer(palette = "Blues")
