\name{plot_horizon}
\alias{plot_horizon}
\title{Create a horizon graph}
\usage{
  plot_horizon(data,
    mapping = aes(x = x, y = y, group = group),
    num.bands = 2, smoothing = NULL, band.colors = NULL,
    calculate.diff = FALSE, reorder.by.change = TRUE,
    loess.span = 0.5, loess.interval = 1,
    spline.n = 3 * nrow(data), invert.bands = NULL)
}
\arguments{
  \item{data}{only supports \code{data.frame}. The order of
  the y-axis depends on the levels of the factor group.}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link[ggplot2]{aes}} or
  \code{\link[ggplot2]{aes_string}} . Requires at least x,y
  and group.}

  \item{num.bands}{number of bands}

  \item{smoothing}{character vector. Either "loess" or
  "splines".}

  \item{band.colors}{custom colours for the bands. Requires
  twice as many colours than num.bands. E.g. num.bands=2
  requires \code{c(DarkNegative, BrightNegative,
  DarkPositive, BrightPositive)}}

  \item{calculate.diff}{uses the percental difference
  between x and x+1 for the y-axis}

  \item{loess.span}{parameter span of
  \code{\link[stats]{loess}}. Only applicable if loess is
  used for smoothing}

  \item{loess.interval}{parameter interval of
  \code{\link[stats]{loess}}. Only applicable if loess is
  used for smoothing}

  \item{spline.n}{parameter n of
  \code{\link[stats]{spline}}. Only applicable if spline is
  used for smoothing}

  \item{reorder.by.change}{reorders the y-axis by the most
  change, determined by summing up the absolute values of
  positive and negative change}

  \item{invert.bands}{flips bands upside down. Values can
  either be "neg" or "pos".}
}
\value{
  Valid ggplot object. Any ggplot functions can be
  added/applied, including aesthetics but can be
  printed/saved without any changes.
}
\description{
  This function creates a ggplot object containing a
  horizon graph. Any ggplot manipulation is possible but
  might change the look of the graph (e.g. applying another
  \code{scale_x_*} or \code{scale_y_*}).
}
\examples{
\donttest{data(stocks)

plot_horizon(stocks,aes(x,y,group=group),num.bands=2,smoothing="loess",loess.span=0.2,
             loess.interval=0.5,calculate.diff=TRUE)

plot_horizon(stocks[stocks$group \%in\% c("CAC","DAX"),],aes(x,y,group=group),2,
             smoothing="spline", spline.n=40)

plot_horizon(stocks,aes(x,y,group=group),3)

plot_horizon(stocks,aes(x,y,group=group),3, smoothing="loess",invert.bands=TRUE)

plot_horizon(stocks,aes(x,y,group=group),2,
             smoothing="spline", spline.n=40)

plot_horizon(stocks,aes(x,y,group=group),2,
            band.colors=c("grey10","grey80","red1","red4"),calculate.diff=T,smoothing="loess")
}
}
\references{
  \url{http://vis.stanford.edu/files/2009-TimeSeries-CHI.pdf}

  \url{https://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=1532144}
}

