if(getRversion() >= "2.15.1") utils::globalVariables(c("y", "x", "group", "ymin", "ymax", "fill"))

steps = function(y,i,padding=1.00000000001){
  max(y)/i*padding
}

data.prep = function(df){
  if(!is.factor(df$group))
    df$group = factor(df$group)
  df$splitter = 1
  df2 = df
  df2$splitter = 2
  df[df$y > 0,"y"] = 0
  df2[df2$y < 0,"y"] = 0
  rbind(df,df2)
}

create.entry = function(mod,x,xs,pos){
  list(x[xs,"group"],
       get(mod)(pos,ifelse(mod=="-",interval.x.axis(x,xs,mod)*0.98,interval.x.axis(x,xs,mod))),
       0,
       x[xs,"splitter"])
}

interval.x.axis = function(x,xs,mod){
  abs(x[get(mod)(xs,1),"x"]-x[xs,"x"])/2
}

insert.zero = function(mod,x,xs,pos,nrows){
  if(x[get(mod)(xs,1),"y"] != 0 && 
     get(mod)(xs,1) < nrows && 
     get(mod)(xs,1) > 0 &&
     x[get(mod)(xs,1),"splitter"] == x[xs,"splitter"])
    x[nrow(x) + 1,c("group","x","y","splitter")] = create.entry(mod,x,xs,pos)
  x
}

padding = function(x,xs,nrows){
  pos = x[xs,]$x
  x = insert.zero("-",x,xs,pos,nrows) #before current position
  insert.zero("+",x,xs,pos,nrows) #after current position
}

counter = function(vals,start=0) start:(length(vals)-(1-start))

bands = function(df.all,counter,counter_labels,num.bands,grounds,fill.id,invert.bands){
  mapply(function(df,counter){
           lapply(seq(num.bands),function(xs){
                    add.band(adjust.band.data(df,
                                              steps(df.all$y,num.bands),
                                              grounds[counter+1],xs,
                                              invert.bands),
                             pick.colors(fill.id,xs))})}, 
         rev(split(df.all,df.all$group)),
         counter_labels) 
}

adjust.band.data = function(df,step,ground,i,invert.bands){
  df$y = adjust.y.values(df$y,step,i)
  df = Reduce(function(x,xs) padding(x,xs,nrow(df)),which(df$y == 0),df)
  df$ymin = ground
  df$ymax = df$y + ground
  if(!is.null(invert.bands)){
    idx = ifelse(invert.bands == "neg",2,1)
    df$ymin[df$splitter == idx] = ground + step - abs(df$ymax[df$splitter == idx] - ground)
    df$ymax[df$splitter == idx] = ground + step 
  }
  df
}

pick.colors = function(colors,i){
  list(colors[length(colors)/2+1-i],
       colors[length(colors)+1-i])
}

add.band = function(df,colors){
  list(add.area(df,colors[[1]],colors[[2]])) 
}

plot.bands = function(df.all,num.bands,user.colors,invert.bands){
  fill.id = LETTERS[1:(num.bands*2)]
  y_labels = rev(levels(df.all$group))
  grounds = steps(df.all$y,num.bands) * 0:(length(y_labels)-1)

  create.blank.ggplot() +
      add.fill(get.colors(user.colors,num.bands),
               fill.id,
               labels(df.all$y,c(-1,1),num.bands)) +
      bands(df.all,counter,counter(y_labels),num.bands,grounds,fill.id,invert.bands) + 
      add.lines.and.labels(grounds,y_labels,steps(df.all$y,num.bands))
}

add.lines.and.labels = function(grounds,y_labels,step){
  list(geom_hline(yintercept=c(grounds,grounds[length(grounds)]+step)), 
       scale_y_continuous(expand=c(0,0),breaks=grounds+step*1.05/2,labels=y_labels))
}

adjust.y.values = function(y,step,i){
  y = abs(y)
  current_bottom =  step * (i-1)
  current_middle =  step * (i)
  current_top = step * (i+1)
  y[y < current_bottom] = 0
  y[y >= current_bottom & y < current_middle] = y[y >= current_bottom & y < current_middle] - current_bottom
  y[y > current_middle] = step
  y
}

add.area = function(df,c1,c2){
  df$fill = ifelse(df$splitter==1,c1,c2)
  geom_ribbon(aes(x,ymin=ymin,ymax=ymax,fill=fill),df)
}

prepare.data = function(df,step,i,ground){
  df$y = adjust.y.values(df$y,step,i)
  df = Reduce(function(x,xs) padding(x,xs,nrow(df)),which(df$y == 0),df)
  df$ymin = ground
  df$ymax = df$y + ground
  df
}

calc.diff = function(tmp){
  tmp = transform(tmp,diff=c(diff(y),0))
  transform(tmp,diff_perc=diff/y*100)
}

calculate.diff = function(df,to.calculate){
  if(to.calculate){
    df = ddply(df,.(group),calc.diff)
    df$y = df$diff_perc
  }
  df
}

re.order = function(df,should.reorder){
  if(should.reorder) {
    ordered = ddply(df,.(group), function(x) sum(abs(x$y)))
    ordered = ordered[order(ordered$V1,decreasing=T),]
    df$group = factor(df$group,levels=ordered$group)
  }
  df
}

reverse.mapping = function(mapping){
  mapping2 = names(mapping)
  names(mapping2) = lapply(names(mapping),function(x) mapping[[x]])  
  mapping2
}

#' Create a horizon graph
#' 
#' This function creates a ggplot object containing a horizon graph. Any ggplot manipulation is possible but might change the look of the graph (e.g. applying another \code{scale_x_*} or \code{scale_y_*}).

#' @references
#' \url{http://vis.stanford.edu/files/2009-TimeSeries-CHI.pdf}
#'
#' \url{https://ieeexplore.ieee.org/xpl/login.jsp?tp=&arnumber=1532144}
#'
#' @return Valid ggplot object. Any ggplot functions can be added/applied, including aesthetics but can be printed/saved without any changes.
#' @param data only supports \code{data.frame}. The order of the y-axis depends on the levels of the factor group.
#' @param mapping The aesthetic mapping, usually constructed with \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_string}} . Requires at least x,y and group.
#' @param num.bands number of bands
#' @param smoothing character vector. Either "loess" or "splines".
#' @param band.colors custom colours for the bands. Requires twice as many colours than num.bands. E.g. num.bands=2 requires \code{c(DarkNegative, BrightNegative, DarkPositive, BrightPositive)}
#' @param calculate.diff uses the percental difference between x and x+1 for the y-axis
#' @param loess.span parameter span of \code{\link[stats]{loess}}. Only applicable if loess is used for smoothing
#' @param loess.interval parameter interval of \code{\link[stats]{loess}}. Only applicable if loess is used for smoothing
#' @param spline.n parameter n of \code{\link[stats]{spline}}. Only applicable if spline is used for smoothing
#' @param reorder.by.change reorders the y-axis by the most change, determined by summing up the absolute values of positive and negative change
#' @param invert.bands flips bands upside down. Values can either be "neg" or "pos".
#' @export
#' @examples
#' \donttest{data(stocks)
#'
#' plot_horizon(stocks,aes(x,y,group=group),num.bands=2,smoothing="loess",loess.span=0.2,
#'              loess.interval=0.5,calculate.diff=TRUE)
#'
#' plot_horizon(stocks[stocks$group %in% c("CAC","DAX"),],aes(x,y,group=group),2,
#'              smoothing="spline", spline.n=40) 
#'
#' plot_horizon(stocks,aes(x,y,group=group),3)
#'
#' plot_horizon(stocks,aes(x,y,group=group),3, smoothing="loess",invert.bands="neg")
#' 
#' plot_horizon(stocks,aes(x,y,group=group),2,
#'              smoothing="spline", spline.n=40)
#'
#'plot_horizon(stocks,aes(x,y,group=group),2,
#'             band.colors=c("grey10","grey80","red1","red4"),calculate.diff=T,smoothing="loess")
#' }
plot_horizon = function(data,mapping=aes(x=x,y=y,group=group),num.bands=2,smoothing=NULL,band.colors=NULL,
                        calculate.diff=FALSE,
                        reorder.by.change=TRUE,
                        loess.span=0.5,loess.interval=1,spline.n=3*nrow(data),
                        invert.bands=NULL){
  plot.bands(ddply(smooth.data(re.order(calculate.diff(rename(data,reverse.mapping(mapping)),
                                              calculate.diff),
                                        reorder.by.change),
                               smoothing,
                               loess.span,
                               loess.interval,
                               spline.n),
                   .(group),
                   data.prep),
             num.bands,
             band.colors,
             invert.bands)
}
