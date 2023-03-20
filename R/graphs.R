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

  graphdf <- data.frame(df[, timevar], df[, graphvar]) %>%
    rename(xvar = timevar, yvar = graphvar) %>%
    filter(xvar != "Total", !is.na(yvar)) %>%
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
    graphdf <- bind_rows(graphdf, extrarows) %>% arrange(xvar)
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
