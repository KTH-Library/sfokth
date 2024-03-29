% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphs.R
\name{graph_by_year}
\alias{graph_by_year}
\title{Graph indicators by time}
\usage{
graph_by_year(
  df,
  graphvars,
  timevar = "Publication_Year",
  xlab = "Publication year",
  ylab = "Indicator value",
  graphtitle = NULL,
  horizontal = NULL,
  perc = FALSE,
  pal = palette_kth_neo(18),
  horizontal_col = kth_colors("lightteal")
)
}
\arguments{
\item{df}{data frame with indicators by year}

\item{graphvars}{names of columns with indicators to create graph for}

\item{timevar}{name of column with time, default Publication_Year}

\item{xlab}{label for X axis, default "Publication year"}

\item{ylab}{label for Y axis, default "Indicator value"}

\item{graphtitle}{main title for graph, default NULL}

\item{horizontal}{where to put a horizontal line, default NULL}

\item{perc}{set to TRUE for percent scale y axis, default FALSE}

\item{pal}{color palette to use for indicator}

\item{horizontal_col}{color to use for horizontal line}
}
\description{
Graph indicators by time
}
