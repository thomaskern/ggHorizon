smooth.loess = function(df,span=0.4,interval=1,...){
  l = loess("y ~ x",data.frame(x=df$x,y=df$y),span=span)
  newx = seq(range(df$x)[1],range(df$x)[2],interval)
  create.df(df,predict(l,newdata=data.frame(x=newx)),newx)
}

smooth.spline = function(df,n,...){
  l = spline(df$x,df$y,n=n)
  create.df(df,l$y,l$x)
}

smooth.data <- function(df,smoothing,loess.span,loess.interval,spline.n){
  if(!is.null(smoothing) && exists(smoothing)){
    df = ddply(df,.(group),get(paste0("smooth.",smoothing)),span=loess.span,interval=loess.interval,n=spline.n)
  }
  df
}

create.df = function(df,newy,newx){
  data.frame(x=newx,y=newy,group=df$group[1])
}


