#' Graph indicator by time
#'
#' @param df data frame with indicators by year
#' @param graphvar name of column with indicator to create graph for
#' @param timevar name of column with time, default Publication_Year
#' @param horizontal where to put a horizontal line, default NULL
#' @import ktheme dplyr ggplot2
#' @export
graph_by_year <- function(df, graphvar, timevar = "Publication_Year", horizontal = NULL) {


  kth_cols <- palette_kth(4)

  graphdf <- data.frame(xvar = df[, timevar], yvar = df[, graphvar]) %>%
    filter(xvar != "Total") %>%
    mutate(xvar = as.integer(xvar))

  ymax <- max(2, ceiling(max(graphdf$yvar)))

  breaks <- min(graphdf$xvar):max(df$xvar)

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
    ylim(0, ymax) +
    theme_kth_osc() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(breaks = breaks, labels = breaks, minor_breaks = NULL)

  if(!is.null(horizontal)){
    gg <- gg + geom_hline(yintercept = 1.0, color = kth_cols["lightblue"])
  }

  gg
}
