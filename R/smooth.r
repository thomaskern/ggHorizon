smooth.loess = function(df,span=0.4,interval=1,...){
  l = loess("y ~ x",data.frame(x=df$x,y=df$y),span=span)
  newx = seq(range(df$x)[1],range(df$x)[2],interval)
  create.df(df,predict(l,newdata=data.frame(x=newx)),newx)
}

smooth.spline = function(df,n,...){
  l = spline(df$x,df$y,n=n)
  create.df(df,l$y,l$x)
}

