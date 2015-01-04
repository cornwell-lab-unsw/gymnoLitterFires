
read.phylo.data<-function(tree.file){
  library(ape)
  phy<-read.tree(tree.file)
  return(phy)
}

combine.trait.data<-function(trait.file,tree){
  temp<-list()
  temp$traits<-trait.file
  temp$phy<-tree
  return(temp)
}
  
  
  
phylo_fig<-function(phy2,out.df){
  library(ape)
  library(fields)
  #combine.trait.data(across.species.data,phy)
  #phy2<-aa$phy
  #out.df<-aa$traits
  
  # matching data with the tips of the phylogeny
  massLoss<-out.df$massLoss[match(phy2$tip.label,sub(" ","_",out.df$gs))]/100
  maxTemp<-out.df$maxTemp[match(phy2$tip.label,sub(" ","_",out.df$gs))]
  
  # making a color scheme
  color <- tim.colors(round(max(massLoss*100), 0))
  
  #pdf("phylo_fig.pdf")
  layout(matrix(1:3, nrow = 1), widths = c(6, rep(1,2)), heights = c(rep(1,3)))
  par(mar = c(1, 0, 1, 0))
  plot(phy2, cex = 0.7, show.node.label = FALSE, show.tip.label = TRUE, adj = 0.5,no.margin=F)
  par(mar = c(1, 0, 1, 2), tcl = -0.1)
  barplot(unname(massLoss), xlim = c(-0.08, 1), horiz = T, 
          xaxt = "n", space = 2, col = color[massLoss*100])
  axis(1,cex.axis = 0.5,mgp = c(0.5, 0.2, 0),line=-0.8)
  title("Proportion \n of sample burned",line=-1,cex.main=0.8)
  #par(mar = c(1, 0, 1, 2))
  barplot(unname(maxTemp), horiz = T, 
          xaxt = "n", space = 2, col = color[maxTemp*100/max(maxTemp)])
  #par(cex = 0.4, mgp = c(0.5, 0.2, 0), tcl = -0.1)
  axis(1,cex.axis = 0.5,mgp = c(0.5, 0.2, 0),line=-0.8)
  title("Maximum \n temperature (C)",line=-1,cex.main=0.8)
  #dev.off()
}
