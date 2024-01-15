#' Network nodes and edges in Gephi friendly format
#'
#' @param pubs list of publications with one or two columns for node labels
#' @param pub_id the column name to use for publication id (default "UT")
#' @param label1 the column name to use for node labels (default "Name_eng")
#' @param label2 (optional) a second column name to use for node labels
#' @param indicator (optional) a column name to calculate averages per node for
#' @import dplyr
#' @export
network_gephi <- function(pubs,
                          pub_id = "UT",
                          label1 = "Name_eng",
                          label2 = NULL,
                          indicator = NULL) {
  data <- pubs |>
    rename(pub_id = !!pub_id,
           label1 = !!label1) |>
    mutate(label1 = as.character(label1)) |>
    select(!is.na(label1))

  if(!is.null(label2)){

    data <- data |>
      rename(label2 = !!label2) |>
      mutate(label2 = as.character(label2)) |>
      select(!is.na(label2))

    nodes1 <- data |>
      group_by(label1) |>
      summarise(p = n_distinct(pub_id),
                .groups = "drop") |>
      mutate(nodetype = 1) |>
      rename(label = label1)

    nodes2 <- data |>
      group_by(label2) |>
      summarise(p = n_distinct(pub_id),
                .groups = "drop") |>
      mutate(nodetype = 2) |>
      rename(label = label2)

    nodes <- nodes1 |>
      bind_rows(nodes2) |>
      mutate(id = row_number(),
             sqrt_p = sqrt(p)) |>
      relocate(id)

    edges <- nodes |>
      inner_join(data, by = c("label" = "label1"), relationship = "many-to-many") |>
      rename(source = id) |>
      select(source, pub_id, label2) |>
      inner_join(nodes, by = c("label2" = "label"), relationship = "many-to-many") |>
      rename(target = id) |>
      group_by(source, target) |>
      summarise(weight = n_distinct(pub_id),
                .groups = "drop")

    if(!is.null(indicator)) {

      avg1 <- data |>
        mutate(indicator = !!indicator) |>
        distinct(pub_id, label1, indicator) |>
        group_by(label1) |>
        summarise(avg = mean(indicator, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(nodetype = 1) |>
        rename(label = label1)

      avg2 <- data |>
        mutate(indicator = !!indicator) |>
        distinct(pub_id, label2, indicator) |>
        group_by(label2) |>
        summarise(avg = mean(indicator, na.rm = TRUE)) |>
        mutate(nodetype = 2) |>
        rename(label = label2)

      nodes <- nodes |> inner_join(bind_rows(avg1, avg2), by = c("label", "nodetype"))

    }

  } else {

    nodes <- data |>
      group_by(label1) |>
      summarise(p = n_distinct(pub_id),
                .groups = "drop") |>
      rename(label = label1) |>
      mutate(id = row_number(),
             sqrt_p = sqrt(p)) |>
      relocate(id)

    edges <- nodes |>
      inner_join(data, by = c("label" = "label1"), relationship = "many-to-many") |>
      rename(source = id) |>
      inner_join(data, by = "pub_id", relationship = "many-to-many") |>
      inner_join(nodes, by = c("label1" = "label"), relationship = "many-to-many") |>
      rename(target = id) |>
      filter(target > source) |>
      group_by(source, target) |>
      summarise(weight = n_distinct(pub_id),
                .groups = "drop")

    if(!is.null(indicator)) {

      avg <- data |>
        mutate(indicator = !!indicator) |>
        distinct(pub_id, label1, indicator) |>
        group_by(label1) |>
        summarise(avg = mean(indicator, na.rm = TRUE),
                  .groups = "drop") |>
        rename(label = label1)

      nodes <- nodes |> inner_join(bind_rows(avg1, avg2), by = "label")

    }
  }

  list(nodes = nodes, edges = edges)
}
