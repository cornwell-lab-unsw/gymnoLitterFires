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

process.data.manipulation<-function(aa){
  aa<-read.delim(aa)
  aa$percent.sample.burned[is.na(aa$percent.sample.burned)]<-100
  # get correct units for speed, changing from sec to cm / min
  aa$speed.to.sensors<-6.25*60/aa$speed.to.sensors
  return(aa)
}

read.across.spp.data<-function(aa="across_species_fire_data.csv"){
  fss<-read.csv(aa)
  return(fss)
}

md2html <- function(filename) {
  library(markdown)
  dest <- paste0(tools::file_path_sans_ext(filename), ".html")
  opts <- setdiff(markdownHTMLOptions(TRUE), 'base64_images')
  markdownToHTML(filename, dest, options=opts)
}
