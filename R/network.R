#' Network nodes and edges in Gephi friendly format
#'
#' @param pubs list of publications with one or two columns for node labels
#' @param pub_id the column name to use for publication id (default "UT")
#' @param label1 the column name to use for node labels (default "Name_eng")
#' @param label2 (optional) a second column name to use for node labels
#' @param indicators (optional) a list of column names to calculate averages per node/edge for
#' @import dplyr
#' @export
network_gephi <- function(pubs,
                          pub_id = "UT",
                          label1 = "Name_eng",
                          label2 = NULL,
                          indicators = NULL) {
  data <- pubs |>
    rename(pub_id = !!pub_id,
           label1 = !!label1) |>
    mutate(label1 = as.character(label1)) |>
    filter(!is.na(label1))

  if(!is.null(label2)){

    data <- data |>
      rename(label2 = !!label2) |>
      mutate(label2 = as.character(label2)) |>
      filter(!is.na(label2))

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

    if(!is.null(indicators)) {

      node_avg1 <- data |>
        select(pub_id, label1, any_of(indicators)) |>
        distinct() |>
        group_by(label1) |>
        summarise_at(indicators, mean, na.rm = TRUE) |>
        mutate(nodetype = 1) |>
        rename(label = label1)

      node_avg2 <- data |>
        select(pub_id, label2, any_of(indicators)) |>
        distinct() |>
        group_by(label2) |>
        summarise_at(indicators, mean, na.rm = TRUE) |>
        mutate(nodetype = 2) |>
        rename(label = label2)

      nodes <- nodes |> inner_join(bind_rows(node_avg1, node_avg2), by = c("label", "nodetype"))

      edge_avg <- edges |>
        inner_join(nodes |> filter(nodetype == 1), by = c("source" = "id")) |>
        inner_join(nodes |> filter(nodetype == 2), by = c("target" = "id")) |>
        select(source, target, label.x, label.y) |>
        inner_join(data, by = c("label.x" = "label1", "label.y" = "label2")) |>
        select(source, target, pub_id, any_of(indicators)) |>
        distinct() |>
        group_by(source, target) |>
        summarise_at(indicators, mean, na.rm = TRUE)

      edges <- edges |> inner_join(edge_avg, by = c("source", "target"))

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

    if(!is.null(indicators)) {

      node_avg <- nodes |>
        inner_join(data, by = c("label" = "label1")) |>
        select(pub_id, id, any_of(indicators)) |>
        distinct() |>
        group_by(id) |>
        summarise_at(indicators, mean, na.rm = TRUE)

      nodes <- nodes |> inner_join(node_avg, by = "id")

      edge_avg <- edges |>
        inner_join(nodes, by = c("source" = "id")) |>
        inner_join(nodes, by = c("target" = "id")) |>
        inner_join(data, by = c("label.x" = "label1"), relationship = "many-to-many") |>
        select(source, target, pub_id, label.x, label.y) |>
        inner_join(data, by = c("pub_id", "label.y" = "label1"), relationship = "many-to-many") |>
        select(source, target, pub_id, any_of(indicators)) |>
        distinct() |>
        group_by(source, target) |>
        summarise_at(indicators, mean, na.rm = TRUE)

      edges <- edges |> inner_join(edge_avg, by = c("source", "target"))

    }
  }

  list(nodes = nodes, edges = edges)
}

#' Network nodes and edges with specific nodes handled separately
#'
#' The specified nodes will have no edge between them and
#' publications are only counted towards p if there is a link
#' to an "external" node (a label outside the separate group)
#'
#' @param pubs list of publications with one column for node labels
#' @param separate labels to treat separately
#' @param pub_id the column name to use for publication id (default "UT")
#' @param label the column name to use for node labels (default "Name_eng")
#' @param indicators (optional) a list of column names to calculate averages per node/edge for
#' @import dplyr
#' @export
network_gephi_separate <- function(pubs,
                                   separate,
                                   pub_id = "UT",
                                   label = "Name_eng",
                                   indicators = NULL) {
  data <- pubs |>
    rename(pub_id = !!pub_id,
           label = !!label) |>
    mutate(label = as.character(label)) |>
    filter(!is.na(label))


  nodes1 <- data |>
    filter(label %in% separate) |>
    select(label, pub_id) |>
    inner_join(data, by = "pub_id", relationship = "many-to-many") |>
    filter(!label.y %in% separate) |>
    group_by(label.x) |>
    summarise(p = n_distinct(pub_id),
              .groups = "drop") |>
    mutate(sqrt_p = sqrt(p),
           nodetype = 1) |>
    rename(label = label.x)


  nodes2 <- data |>
    filter(!label %in% separate) |>
    group_by(label) |>
    summarise(p = n_distinct(pub_id),
              .groups = "drop") |>
    mutate(sqrt_p = sqrt(p),
           nodetype = 2)

  nodes <- nodes1 |>
    bind_rows(nodes2) |>
    mutate(id = row_number()) |>
    relocate(id)

  edges <- nodes |>
    inner_join(data, by = "label", relationship = "many-to-many") |>
    rename(source = id) |>
    inner_join(data, by = "pub_id", relationship = "many-to-many") |>
    inner_join(nodes, by = c("label.y" = "label"), relationship = "many-to-many") |>
    rename(target = id) |>
    filter(target > source, !label.y %in% separate) |>
    group_by(source, target) |>
    summarise(weight = n_distinct(pub_id),
              .groups = "drop")

  if(!is.null(indicators)) {

    node_avg1 <- nodes1 |>
      inner_join(data, by = "label", relationship = "many-to-many") |>
      select(label, pub_id) |>
      inner_join(data, by = "pub_id", relationship = "many-to-many") |>
      inner_join(nodes, by = c("label.y" = "label"), relationship = "many-to-many") |>
      filter(!label.y %in% separate) |>
      select(pub_id, label.x, any_of(indicators)) |>
      distinct() |>
      group_by(label.x) |>
      summarise_at(indicators, mean, na.rm = TRUE) |>
      rename(label = label.x)

    node_avg2 <- nodes2 |>
      inner_join(data, by = "label", relationship = "many-to-many") |>
      select(pub_id, label, any_of(indicators)) |>
      distinct() |>
      group_by(label) |>
      summarise_at(indicators, mean, na.rm = TRUE)

    nodes <- nodes |> inner_join(bind_rows(node_avg1, node_avg2), by = "label")

    edge_avg <- edges |>
      inner_join(nodes, by = c("source" = "id")) |>
      inner_join(nodes, by = c("target" = "id")) |>
      inner_join(data, by = c("label.x" = "label"), relationship = "many-to-many") |>
      select(source, target, pub_id, label.x, label.y) |>
      inner_join(data, by = c("pub_id", "label.y" = "label"), relationship = "many-to-many") |>
      select(source, target, pub_id, any_of(indicators)) |>
      distinct() |>
      group_by(source, target) |>
      summarise_at(indicators, mean, na.rm = TRUE)

    edges <- edges |> inner_join(edge_avg, by = c("source", "target"))

  }

  list(nodes = nodes, edges = edges)
}

#' Add edge labels from node labels in Gephiesque network
#'
#' @param network a named list(nodes, edges)
#' @import dplyr
#' @returns a named list(nodes, edges)
#' @export
labelify_edges <- function(network) {

  nodes <- network$nodes
  edges <- network$edges

  edges |>
    inner_join(nodes |> select(id, label), by = c("source" = "id")) |>
    inner_join(nodes |> select(id, label), by = c("target" = "id")) |>
    relocate(source, target, label.x, label.y)

  list(nodes = nodes, edges = edges)
}
