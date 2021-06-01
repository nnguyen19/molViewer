library(shiny)
library(r3dmol)
library(bio3d)
library(colourpicker)
library(stringr)
library(shinyWidgets)



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



bio3d_docked<- function(homolog, poses){
  print(homolog)
  print(poses)
  #pdb1 <- read.pdb(path2m1)
  #pdb2 <- read.pdb(path2m2)
  hom<- read.pdb(homolog)
  ps <- lapply(poses, function(x) read.pdb(x))
  
  docked <- cat.pdb(hom, ps[[1]], hom, rechain=FALSE, renumber=FALSE)
  if (length(ps)>1){
    
    
    for (i in 2:length(ps)) {
      
      docked <- cat.pdb(docked, ps[[i]], hom, rechain=FALSE, renumber=FALSE)
    }
  }
  return(docked)
}


render_homolog<- function(homolog, pose){ 
  
  docked_model <- bio3d_docked(paste(c("data/", homolog, "_tgt.pdb"), collapse = ""),
                               lapply(pose, function(x) paste("data", x, sep="/")))
  renderR3dmol({
    return(
      r3dmol(
        cartoonQuality = 20,
        #lowerZoomLimit = 50,
        #upperZoomLimit = 350,
        backgroundColor = "#FFFFFF"
      ) %>%
        m_add_model(data = m_bio3d(docked_model)
                    , format = "pdb") %>%
        #m_center() %>%
        m_zoom_to() %>%
        m_set_style( 
          style = m_style_cartoon( color = "#00cc96")) %>%
        m_set_style(
          sel = m_sel(ss = "s"),
          style = m_style_cartoon(color = "#636efa", arrows = TRUE)
        ) %>%
        # Style the alpha helix
        m_set_style(
          sel = m_sel( ss = "h"), # Style alpha helix
          style = m_style_cartoon(color = "#ff7f0e")) %>%
        m_set_style(
          sel = m_sel(resn = "LIG"),
          style = m_style_stick()
        ) 
    )
  })
}




