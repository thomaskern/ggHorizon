# Horizon Graph

Prototype of a horizon graph implementation based on ggplot2.

The idea came up by Saito and Heer. See 2009's CHI paper [`Sizing the Horizon: The Effects of Chart Size and Layering on the Graphical Perception of Time Series Visualizations`](http://vis.stanford.edu/files/2009-TimeSeries-CHI.pdf) by Heer, Kong and Agrawala as well as 2005's Infovis paper [`Two-Tone Pseudo-Coloring: Compact Visualization for One-Dimensional Data. Proc.`](https://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=1532144) by Saito et al.

A smoothed horizon graph of the first 200 days of the `EuStockMarkets` dataset:

![d](eu.png)

## Usage

For the `independent-geom-horizon.r` file, use a `data.frame` of your liking and call `plot_horizon`. The parameters are as followed:

* `data`: only supports `data.frame`'s. The order of the y-axis depends on the levels of the factor `group`. **Required**
* `mapping`: requires at least `x`,`y` and `group`. **Required**
* `num.bands`: number of bands
* `smoothing`: applies the smoothing function. Only valid values are `loess` and `splines` as characters
* `band.colors`: custom colours for the bands. Requires twice as many colours than `num.bands`. E.g. for num.bands=2 you would provide c(DarkNegative, BrightNegative, DarkPositive, BrightPositive)
* `calculate.diff`: uses the percental difference between x and x+1 for the y-axis
* `loess.span`: parameter `span`  of `loess()`. Only applicable if `loess` is used for smoothing
* `loess.interval`: parameter interval of `loess()`. Only applicable if `loess` is used for smoothing
* `spline.n`: parameter `n` of `spline()`. Only applicable if `spline` is used for smoothing

`plot_horizon` returns a ggplot plot.

## Examples

    df = data.frame(group="A", x=0:9, y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
    
    plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40)


    df = data.frame(group=factor(rep(c("A","B"),each=10)), x=0:9, y=c(0.8, 0.4627684, 0.2072174, -1, -0.8324571, -1.0061331, -0.5056517, 0.3085939, 0.4383061, -0.9098858, 0.3, 0.1627684, 0.3072174, -0.3, -1.8324571, -1.0061331, -0.5056517, 0.1085939, 0.6393061, -0.9098858))
    
    plot_horizon(df,aes(x,y,group=group),2, smoothing="spline", spline.n=40)


`eu()` is a function melting and adjusting the `EuStockMarkets` dataset to be used with ggplot2. Using it with `plot_horizon`:

    plot_horizon(with(eu(),eu()[x <= 200,]), aes(x,y,group=group), num.steps=2, smoothing="loess", loess.span=0.2, loess.interval=0.1, calculate.diff=TRUE)
    
    plot_horizon(with(eu(),eu()[x <= 200,]), aes(x,y,group=group), num.steps=2, smoothing="loess", loess.span=0.2, loess.interval=0.1)

## To be done

Making a custom `geom` out of the standalone function proves difficult since changing the y-axis labels from within a `geom` is not allowed. Script `geom-horizon.r` provides the first draft of the custom `geom`.