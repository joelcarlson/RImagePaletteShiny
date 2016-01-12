show_colors <- function(pal, labels=FALSE){

  n<-length(pal)
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  indices <- expand.grid("nrow"=1:nrow, "ncol"=1:ncol)
  
  #add NAs to fill out grid 
  pal <- c(sort(pal), rep(NA, nrow * ncol - length(pal)))
  pal <- data.frame(indices, pal, stringsAsFactors=FALSE)
  
  if(labels){
    labs <- pal$pal
  } else {
    labs <- NA
  }
  
  ggplot(data=pal, aes(x=nrow, y=rev(ncol), label=labs) ) + 
    geom_tile(aes(fill=pal)) + 
    geom_text(na.rm=TRUE)+
    guides(fill=FALSE)+
    scale_fill_manual(values=pal$pal) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_blank())
}



