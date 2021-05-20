library(shiny)
library(r3dmol)
library(bio3d)
library(colourpicker)
library(stringr)
library(shinyWidgets)
library(shinyjs)


#kinases_list <- list.files(path='data',
#           pattern=glob2rx("*tgt*"))
files <- list.files(path = 'data')
#poses <- test[test %>% str_detect("tgt.pdb")]
homologs <- str_subset(files, "tgt.pdb")
homologs <- homologs %>% str_sub(1,-9)
#names(homologs)<-seq_len(length(homologs))

poses <- str_subset(files, "lgd.pdb")

#(groups <- mapply(function(h) str_subset(poses, h), homologs))
(groups <- lapply(homologs, function(h) {h = str_subset(poses, h)}))
names(groups)<- homologs

#kinases_list <- list.files(path='data',
#                           pattern=paste(poses, collapse = "|"))

compound_list <- list.files(path='data',
                            pattern=glob2rx("*PAK*lgd*"))
get_choices <- function(){
  
}
  
  
bio3d_docked<- function(path2m1, path2m2){
  print(path2m1)
  print(path2m2)
  pdb1 <- read.pdb(path2m1)
  pdb2 <- read.pdb(path2m2)
  docked <- cat.pdb(pdb1, pdb2, pdb1, rechain=FALSE, renumber=FALSE)
  return(docked)
}
# display_multiple<- function(path2m1, path2m2){
#   #print(path2m1)
#   m1<- r3dmol(
#     cartoonQuality = 10,
#     lowerZoomLimit = 50,
#     upperZoomLimit = 350,
#     backgroundColor = "#FFFFFF"
#   ) %>%
#     m_add_models(data = path2m1
#                  
#                  , format = "pdb") %>%
#     m_set_style(style = m_style_cartoon())%>%
#     
#     m_zoom_to()
#   
#   m2<- r3dmol(
#     cartoonQuality = 10,
#     lowerZoomLimit = 50,
#     upperZoomLimit = 350,
#     backgroundColor = "#FFFFFF"
#   ) %>%
#     m_add_models(data = path2m2
#                  
#                  , format = "pdb") %>%
#     m_set_style(style = m_style_stick()) %>%
#     
#     m_zoom_to()
#   
#   grid<-m_grid(viewer = list(m1, m2), control_all = TRUE,
#     viewer_config = m_viewer_spec(backgroundColor = "white")
#   )
#   return(grid)
# }