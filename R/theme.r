set.color <- function(num.bands, user.colors, band.colors){
  if(!is.null(user.colors)){
    if(length(user.colors) == 2*num.bands)
      user.colors
    else
      stop(paste("You have provided the wrong number of colors. You need to provide exactly ",2*num.bands,"colors, not ",length(colors)))
  }else{
    if(length(band.colors) < num.bands){
      c(rev(brewer.pal(num.bands,"Reds")),rev(brewer.pal(num.bands,"Blues")))
    }else
      band.colors[[num.bands]]
  }
}

default.band.colors = function(){
  list(c("#590000","#003BF7"),
       c("#B11019","#F5AA9C","#1F61B2","#A2C8DB"),
       c("#B11019","#DE2F35","#F5AA9C","#1F61B2","#4E8AC6","#A2C8DB"))
}

get.colors = function(user.colors,num.bands){
  set.color(num.bands,user.colors,default.band.colors())
}

default_theme = function(){
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.position="None")
}
labels = function(vals,sides,num.bands){
  sapply(sides,function(side){
         if(num.bands == 1){
           ifelse(side == 1, paste0("(0,",max(abs(vals)),"]"),paste0("[-",max(abs(vals)),",0]"))
         }else
           levels(cut(side*abs(vals),num.bands))
        })
}

add.fill = function(colors,fill.id,labels){
  scale_fill_manual("Ranges",values=colors,breaks=fill.id,
                    labels=labels) 
}

create.blank.ggplot = function(){
  ggplot() + default_theme() + scale_x_continuous(expand=c(0,0))
}

