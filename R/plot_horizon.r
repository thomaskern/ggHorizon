steps = function(y,i,padding=1.00000000001){
  max(y)/i*padding
}

data.prep = function(df,num.bands){
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
  ret = abs(x[get(mod)(xs,1),"x"]-x[xs,"x"])/2
  ret
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

bands = function(df.all,counter,y_labels,num.bands,grounds,fill.id){
    mapply(function(df,counter){
             lapply(seq(num.bands),function(xs){
                      add.band(adjust.band.data(df,
                                               steps(df.all$y,num.bands),
                                               grounds[counter+1],xs),
                              pick.colors(fill.id,xs))})}, 
           rev(split(df.all,df.all$group)),
           counter(y_labels)) 
}

adjust.band.data = function(df,step,ground,i){
  df$y = adjust.y.values(df$y,step,i)
  df = Reduce(function(x,xs) padding(x,xs,nrow(df)),which(df$y == 0),df)
  df$ymin = ground
  df$ymax = df$y + ground
  df
}

pick.colors = function(colors,i){
  list(colors[length(colors)/2+1-i],
       colors[length(colors)+1-i])
}

add.band = function(df,colors){
  list(add.area(df,colors[[1]],colors[[2]])) 
}

plot.bands = function(df.all,num.bands,user.colors){
  fill.id = LETTERS[1:(num.bands*2)]
  y_labels = rev(levels(df.all$group))
  grounds = steps(df.all$y,num.bands) * 0:(length(y_labels)-1)

  create.blank.ggplot() +
      add.fill(get.colors(user.colors,num.bands),
               fill.id,
               labels(df.all$y,c(-1,1),num.bands)) +
      bands(df.all,counter,y_labels,num.bands,grounds,fill.id) + 
      add.lines.and.labels(grounds,y_labels,steps(df.all$y,num.bands))
}

add.lines.and.labels = function(grounds,y_labels,step){
  list(geom_hline(yintercept=c(grounds,grounds[length(grounds)]+step)), 
       scale_y_continuous(expand=c(0,0),breaks=grounds+step*1.05/2,labels=y_labels),
       scale_x_continuous(expand=c(0,0)))
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

#' Create horizon graph
#' 
#' This function creates a ggplot object containing a horizon graph. Any ggplot manipulation is possible but might change the look of the graph (e.g. applying another scale_x_* or scale_y_*).
#' @export
#' @examples
#' \donttest{
#' data(stocks)
#' plot_horizon(stocks,aes(x,y,group=group),num.bands=2,smoothing="loess",loess.span=0.2,loess.interval=0.5,calculate.diff=TRUE)
#'   
#' df = data.frame(group="A",x=0:9,y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
#' plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40) 
 #'  
#' df = data.frame(group=factor(c(rep("A",9), rep("B",8))), x=c(0:8,2:9),y=c(0.8,0.4627684,0.2072174,-1,-0.8324571,-1.0061331,-0.5056517,0.1,0.3085939,-0.9098858,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
#' plot_horizon(df,aes(x,y,group=group),3)
#' 
#' df = data.frame(group=factor(rep(c("A","B"),each=10),levels=c("B","A")), x=0:9,y=c(0.8,0.4627684,0.2072174,-1,-0.8324571,-1.0061331,-0.5056517,0.3085939,0.4383061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
#' plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40)
#' }
plot_horizon = function(data,mapping=aes(x=x,y=y,group=group),num.bands=2,smoothing=NULL,band.colors=NULL,
                        calculate.diff=FALSE,
                        reorder.by.change=TRUE,
                        loess.span=0.5,loess.interval=1,spline.n=3*nrow(data)){
  plot.bands(ddply(smooth.data(re.order(calculate.diff(rename(data,reverse.mapping(mapping)),
                                              calculate.diff),
                                        reorder.by.change),
                               smoothing,
                               loess.span,
                               loess.interval,
                               spline.n),
                   .(group),
                   data.prep,
                   num.bands=num.bands),
             num.bands,
             band.colors)
}
