#' Graph indicator by time
#'
#' @param df data frame with indicators by year
#' @param graphvar name of column with indicator to create graph for
#' @param timevar name of column with time, default Publication_Year
#' @param horizontal where to put a horizontal line, default NULL
#' @param perc set to TRUE for percent scale y axis, default FALSE
#' @import ktheme dplyr ggplot2 scales
#' @export
graph_by_year <- function(df, graphvar, timevar = "Publication_Year", horizontal = NULL, perc = FALSE) {


  kth_cols <- palette_kth(4)

  graphdf <- data.frame(df[, timevar], df[, graphvar]) |>
    rename(xvar = timevar, yvar = graphvar) |>
    filter(xvar != "Total", !is.na(yvar)) |>
    mutate(xvar = as.integer(xvar))

  if(perc){
    ymax <- max(0.2, ceiling(max(graphdf$yvar)*10)/10)
  } else {
    ymax <- max(2, ceiling(max(graphdf$yvar)))
  }

  breaks <- min(graphdf$xvar):max(graphdf$xvar)

  # Add missing years to df
  if(length(breaks) != nrow(graphdf)) {
    extrayears <- breaks[!breaks %in% graphdf$xvar]
    extrarows <- data.frame(xvar = extrayears)
    graphdf <- bind_rows(graphdf, extrarows) |> arrange(xvar)
  }

  gg <- ggplot(data = graphdf,
         aes(x = xvar, y = yvar, group = 1)) +
    geom_point() +
    geom_line(color = kth_cols["blue"], linetype = "dashed") +
    xlab(timevar) +
    ylab(graphvar) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(breaks = breaks, labels = breaks, minor_breaks = NULL)

  if(!is.null(horizontal)){
    gg <- gg + geom_hline(yintercept = horizontal, color = kth_cols["lightblue"])
  }
  if(perc){
    gg <- gg + scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax))
  } else {
    gg <- gg + ylim(0, ymax)
  }

  gg
}

#' Plot cf and jcf by site with circles proportional to number of publications
#'
#' @param df data frame with indicators by site (needs site, p, cf, jcf)
#' @param unfilled vector of sites to draw unfilled circle for (default empty)
#' @param xintercept where to put a vertical line, default 1
#' @param yintercept where to put a horizontal line, default 1
#' @param maxsize maximum size of circle (default 50)
#' @param pal colors to use
#' @param solid alpha value for circles, 1 = fully solid, 0 = fully transparent
#' @import ktheme dplyr ggplot2 scales
#' @export
cf_jcf_plot <- function(df, unfilled = c(), xintercept = 1, yintercept = 1, maxsize = 50, pal = palette_kth_neo(), solid = 0.9) {

  names(pal) <- NULL
  tmp_df <- df |>
    mutate(size = maxsize * sqrt(p/max(p)))

  minsize <- min(tmp_df$size)
  xmax <- ceiling(max(tmp_df$cf))
  ymax <- ceiling(max(tmp_df$jcf))

  shapevals <- if_else(tmp_df$site %in% unfilled, 1, 16) |> rev()

  ggplot(tmp_df) +
    geom_point(aes(x = cf, y = jcf, size = size, color = site, shape = site, stroke = 2, alpha = solid)) +
    scale_color_manual(values = pal) +
    scale_shape_manual(values = shapevals)  +
    scale_size_continuous(range = c(minsize, maxsize), guide = "none") +
    scale_alpha_identity(guide = "none") +
    geom_vline(xintercept = xintercept, color = "red", linewidth = 1) +
    geom_hline(yintercept = yintercept, color = "red", linewidth = 1) +
    scale_x_continuous(limits = c(0, xmax), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0,0)) +
    theme_light() +
    theme(legend.position = "right") +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    xlab("Cf value") +
    ylab("Jcf value")
}

#' Plot top10 and jcf by site with circles proportional to number of publications
#'
#' @param df data frame with indicators by site (needs site, p, top10, top20)
#' @param unfilled vector of sites to draw unfilled circle for (default empty)
#' @param xintercept where to put a vertical line, default 0.1
#' @param yintercept where to put a horizontal line, default 0.2
#' @param maxsize maximum size of circle (default 50)
#' @param pal colors to use
#' @param solid alpha value for circles, 1 = fully solid, 0 = fully transparent
#' @import ktheme dplyr ggplot2 scales
#' @export
top10_top20_plot <- function(df, unfilled = c(), xintercept = 0.1, yintercept = 0.2, maxsize = 50, pal = palette_kth_neo(), solid = 0.9) {

  names(pal) <- NULL
  tmp_df <- df |>
    mutate(size = maxsize * sqrt(p/max(p)))

  minsize <- min(tmp_df$size)
  xmax <- ceiling(10*max(tmp_df$top10))/10
  ymax <- ceiling(10*max(tmp_df$top20))/10
  xbreaks <- seq(0, xmax, 0.1)
  ybreaks <- seq(0, ymax, 0.1)

  shapevals <- if_else(tmp_df$site %in% unfilled, 1, 16) |> rev()

  ggplot(tmp_df) +
    geom_point(aes(x = top10, y = top20, size = size, color = site, shape = site, stroke = 2, alpha = solid)) +
    scale_color_manual(values = pal) +
    scale_shape_manual(values = shapevals)  +
    scale_size_continuous(range = c(minsize, maxsize), guide = "none") +
    scale_alpha_identity(guide = "none") +
    geom_vline(xintercept = xintercept, color = "red", linewidth = 1) +
    geom_hline(yintercept = yintercept, color = "red", linewidth = 1) +
    scale_x_continuous(limits = c(0, xmax), breaks = xbreaks, labels = percent_format(), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, ymax), breaks = ybreaks, labels = percent_format(), expand = c(0,0)) +
    theme_light() +
    theme(legend.position = "right") +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    xlab("Share Top10% publications") +
    ylab("Share publications in Top20% journals")
}
