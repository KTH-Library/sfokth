#' Graph indicator by time
#'
#' @param df data frame with indicators by year
#' @param graphvar name of column with indicator to create graph for
#' @param timevar name of column with time, default Publication_Year
#' @param horizontal where to put a horizontal line, default NULL
#' @param perc set to TRUE for percent scale y axis, default FALSE
#' @param graphcol color to use for curve
#' @param horizontal_col color to use for horizontal line
#' @import ktheme dplyr ggplot2 scales
#' @export
graph_by_year <- function(df,
                          graphvar,
                          timevar = "Publication_Year",
                          horizontal = NULL,
                          perc = FALSE,
                          graphcol = kth_colors("blue"),
                          horizontal_col = kth_colors("lightteal")) {

  graphdf <- data.frame(xvar = df[, timevar], yvar = df[, graphvar]) |>
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
    geom_point(color = graphcol) +
    geom_line(color = graphcol, linetype = "dashed") +
    xlab(timevar) +
    ylab(graphvar) +
    theme_kth_neo() +
    theme(axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(breaks = breaks, labels = breaks, minor_breaks = NULL)

  if(!is.null(horizontal)){
    gg <- gg + geom_hline(yintercept = horizontal, color = horizontal_col)
  }
  if(perc){
    gg <- gg + scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax))
  } else {
    gg <- gg + ylim(0, ymax)
  }

  gg
}

#' Plot indicators X and Y by site with circles proportional to the number of publications
#'
#' @param df data frame with indicators by site (needs site, p, cf, jcf)
#' @param xintercept where to put a vertical line
#' @param yintercept where to put a horizontal line
#' @param xvar the name of the variable to be plotted on the X axis
#' @param yvar the name of the variable to be plotted on the Y axis
#' @param xlab the label for the X axis (same as xvar is not given)
#' @param ylab the label for the Y axis (same as yvar is not given)
#' @param sitevar the name of the circle label variable, default "site"
#' @param sizevar the name of the size variable, defailt "p"
#' @param percentage set to TRUE for percentage scales
#' @param unfilled vector of sites to draw unfilled circle for, default empty
#' @param maxsize maximum size of circle (default 30)
#' @param pal colors to use for circles
#' @param solid alpha value for circles, 1 = fully solid, 0 = fully transparent
#' @param linecol color for horizontal/vertical lines
#' @import ktheme dplyr ggplot2 scales
#' @export
xy_plot <- function(df,
                    xintercept,
                    yintercept,
                    xvar,
                    yvar,
                    xlab,
                    ylab,
                    sitevar = "site",
                    sizevar = "p",
                    percentage = FALSE,
                    unfilled = c(),
                    maxsize = 30,
                    pal = palette_kth_neo(18),
                    solid = 0.9,
                    linecol = kth_colors("red")) {

  if(missing(xlab))
    xlab <- xvar
  if(missing(ylab))
    ylab <- yvar

  names(pal) <- NULL

  tmp_df <- df |>
    rename(x = !!xvar,
           y = !!yvar,
           site = !!sitevar,
           size = !!sizevar) |>
    mutate(size = maxsize * sqrt(size/max(size))) |>
    arrange(site)

  shapevals <- if_else(tmp_df$site %in% unfilled, 1, 16)

  minsize <- min(tmp_df$size)
  xmax <- max(2*xintercept, if_else(percentage, ceiling(10*max(tmp_df$x))/10, ceiling(max(tmp_df$x))))
  ymax <- max(2*yintercept, if_else(percentage, ceiling(10*max(tmp_df$y))/10, ceiling(max(tmp_df$y))))

  xbreaks <-  seq(0, xmax, ifelse(percentage, 0.1, 0.5))
  ybreaks <-  seq(0, ymax, ifelse(percentage, 0.1, 0.5))

  ggplot(tmp_df) +
    geom_vline(xintercept = xintercept, color = linecol, linewidth = 1) +
    geom_hline(yintercept = yintercept, color = linecol, linewidth = 1) +
    geom_point(aes(x = x, y = y, size = size, color = site, shape = site, stroke = 2, alpha = solid)) +
    scale_color_manual(values = pal) +
    scale_shape_manual(values = shapevals)  +
    scale_size_continuous(range = c(minsize, maxsize), guide = "none") +
    scale_alpha_identity(guide = "none") +
    scale_x_continuous(limits = c(0, xmax), breaks = xbreaks,
                       labels = ifelse(percentage, percent_format(), number_format()), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, ymax), breaks = ybreaks,
                       labels = ifelse(percentage, percent_format(), number_format()), expand = c(0,0)) +
    theme_light() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    xlab(xlab) +
    ylab(ylab)
}

#' Plot cf and jcf by site with circles proportional to number of publications
#'
#' @param df data frame with indicators by site (needs site, p, cf, jcf)
#' @param unfilled vector of sites to draw unfilled circle for (default empty)
#' @param xintercept where to put a vertical line, default 1
#' @param yintercept where to put a horizontal line, default 1
#' @param maxsize maximum size of circle (default 50)
#' @param pal colors to use for circles
#' @param solid alpha value for circles, 1 = fully solid, 0 = fully transparent
#' @param linecol color for horizontal/vertical lines
#' @import ktheme dplyr ggplot2 scales
#' @export
cf_jcf_plot <- function(df, unfilled = c(), xintercept = 1, yintercept = 1, maxsize = 50, pal = palette_kth_neo(), solid = 0.9, linecol = kth_colors("red")) {

  xy_plot(df,
          xintercept,
          yintercept,
          xvar = 'cf',
          yvar = 'jcf',
          xlab = 'Cf value',
          ylab = 'Jcf value',
          sitevar = 'site',
          sizevar = 'p',
          percentage = FALSE,
          unfilled,
          maxsize,
          pal,
          solid,
          linecol)
}

#' Plot two top indicators by site
#'
#' Plots one circle for each site, with size proportional to number of publications
#' and X/Y coordinates according to top indicators.
#'
#' Incoming data frame needs columns \code{site}, \code{p}, \code{topX} and \code{topY}
#' where \code{X = 100 * xintercept} and \code{Y = 100 * yintercept}.
#'
#' @param df data frame with indicators by site
#' @param unfilled vector of sites to draw unfilled circle for (default empty)
#' @param xintercept top percentage for publications, default 0.1
#' @param yintercept top percentage for journals, default 0.2
#' @param maxsize maximum size of circle (default 50)
#' @param pal colors to use for circles
#' @param solid alpha value for circles, 1 = fully solid, 0 = fully transparent
#' @param linecol color for horizontal/vertical lines
#' @import ktheme dplyr ggplot2 scales
#' @export
topXY_plot <- function(df, unfilled = c(), xintercept = 0.1, yintercept = 0.2, maxsize = 50, pal = palette_kth_neo(), solid = 0.9, linecol = kth_colors("red")) {

  xy_plot(df,
          xintercept,
          yintercept,
          xvar = paste0("top", round(100*xintercept), "share"),
          yvar = paste0("top", round(100*yintercept), "share"),
          xlab = paste0("Share Top", round(100*xintercept) , "% publications"),
          ylab = paste0("Share publications in Top", round(100*yintercept), "% journals"),
          sitevar = 'site',
          sizevar = 'p',
          percentage = TRUE,
          unfilled,
          maxsize,
          pal,
          solid,
          linecol)
}

#' Boxplot per year for some indicator with optional reference line
#'
#' @param data a data frame including columns for year and indicator value
#' @param year the column holding years, default Publication_Year
#' @param indicator the indicator to make boxplots for
#' @param ylabel a label for the y axis, default indicator
#' @param horizontal height of optional reference line
#' @param perc set to TRUE for percent y scale
#' @import dplyr ggplot2 scales
#' @export
years_boxplot <- function(data,
                          year = "Publication_Year",
                          indicator,
                          ylabel = NULL,
                          horizontal = NULL,
                          perc = FALSE) {

  if(is.null(ylabel))
    ylabel = indicator

  data <- data |>
    rename(year = !!year,
           value = !!indicator) |>
    filter(!is.na(value))

  ymax <- max(2 * coalesce(horizontal, 0),
              ceiling(10 * max(data$value)) / 10)

  gg <- ggplot(data,
         aes(x = year,
             y = value)) +
    geom_boxplot() +
    stat_summary(fun = mean,
                 geom = "point",
                 shape = 23,
                 size = 4) +
    theme_classic() +
    xlab("Publication Year") +
    ylab(ylabel)

  if(!is.null(horizontal))
    gg <- gg + geom_hline(yintercept = horizontal, color = "red")

  if(perc){
    gg <- gg + scale_y_continuous(labels = percent_format(accuracy = 5L), limits = c(0, ymax))
  } else {
    gg <- gg + ylim(0, ymax)
  }

  gg
}

#' Plot boxplots per year for bootstrap samples
#'
#' @param samples a matrix with samples, one named column for each year
#' @param ylabel a name for the indicator to use as label
#' @import tidyr dplyr
#' @export
bootstrap_graph <- function(samples, ylabel) {

  data <- samples |>
    as.data.frame() |>
    pivot_longer(cols = everything())

  years_boxplot(data,
                year = "name",
                indicator = "value",
                ylabel = ylabel,
                horizontal = 1,
                perc = FALSE)
}
