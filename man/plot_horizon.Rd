\name{plot_horizon}
\alias{plot_horizon}
\title{Create horizon graph}
\usage{
  plot_horizon(data,
    mapping = aes(x = x, y = y, group = group),
    num.bands = 2, smoothing = NULL, band.colors = NULL,
    calculate.diff = FALSE, reorder.by.change = TRUE,
    loess.span = 0.5, loess.interval = 1,
    spline.n = 3 * nrow(data))
}
\description{
  This function creates a ggplot object containing a
  horizon graph. Any ggplot manipulation is possible but
  might change the look of the graph (e.g. applying another
  scale_x_* or scale_y_*).
}
\examples{
\donttest{
data(stocks)
plot_horizon(stocks,aes(x,y,group=group),num.bands=2,smoothing="loess",loess.span=0.2,loess.interval=0.5,calculate.diff=TRUE)

df = data.frame(group="A",x=0:9,y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40)

df = data.frame(group=factor(c(rep("A",9), rep("B",8))), x=c(0:8,2:9),y=c(0.8,0.4627684,0.2072174,-1,-0.8324571,-1.0061331,-0.5056517,0.1,0.3085939,-0.9098858,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
plot_horizon(df,aes(x,y,group=group),3)

df = data.frame(group=factor(rep(c("A","B"),each=10),levels=c("B","A")), x=0:9,y=c(0.8,0.4627684,0.2072174,-1,-0.8324571,-1.0061331,-0.5056517,0.3085939,0.4383061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40)
}
}

