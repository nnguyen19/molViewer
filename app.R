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



##########################################



#UI elements
outerUI<-function(id){ #This is main UI
  
  
  ns <- NS(id)
  
  fluidPage(
  fluidRow(column(3,
          actionButton(inputId=ns("addItem"), "Add New Homolog")),
          column(3,
           actionButton(inputId=ns("removeItem"), "Remove Homolog"))
    
  ),
  fluidRow(column(3,
                  div(id = ns('innerModulePlaceholder_odd'))
                  ),
           column(2),
           column(3,
                  div(id = ns('innerModulePlaceholder_even'))))
  )
}



#####sever code inner UI

innerUiTemplate<-function(id, data){
  
  ns=NS(id)
  
  
  
  
  fluidRow(
    
    
    pickerInput(  inputId=ns("homologSelector"),
                  label = "Select Homolog",
                  choices=colnames(data),
                  selected = NULL,
                  multiple = FALSE 
                  
    ),
    br(),
    
    selectizeInput(  inputId=ns("poseSelector"),
                  label = "Select Multiple Pose(s)",
                  choices= NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    plugins = list("remove_button")
                  )
    ),
    br(),
    r3dmolOutput(outputId = ns("model"))#, height = "500px", width = "400px")
  )
  
  
  
  
}

#updates
innerServer<-function(id, data, var){
  render_homolog<- function(homolog, pose){ 
    
    # if (length(pose) ==1){
    #   docked_model <- bio3d_docked(
    #     paste(c("data/", homolog, "_tgt.pdb"), collapse = ""),
    #     paste("data", pose, sep="/"))
    # }else{
    #   docked_model<- lapply(list, function)
    # }
    
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
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(eventExpr = input$homologSelector, handlerExpr = {
        #if (!is.null(input$columnSelector)) mychoices$col <- data[[input$columnSelector]]
        updateSelectizeInput(
          session,
          inputId="poseSelector",
          choices = data[[input$homologSelector]]
        )
        
      })
      
      observeEvent(input$poseSelector, {
        session_ns <- session$ns('tmp')
        mod_id <- substr(session_ns, 1, nchar(session_ns)-4)
        
        # eval(parse(text=paste0("output$",mod_id,
        #                        "<- render_homolog(input$homologSelector, input$poseSelector)"
        # )))
        output$model <- render_homolog(input$homologSelector, input$poseSelector)  
      })
    }
  )
}





##########server code - outer UI
# Initialize empty vector
inserted<- c()

outerServer<-  function(id,data){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      counter<-reactiveValues()
      
      counter$count=0
      
      ns <-session$ns
      
      
      
      
      observeEvent(input$addItem, {
        
        counter$count=counter$count+1
        inserted <<- c(counter$count,inserted)
        if (counter$count %%2 ==1){
        insertUI(selector=paste0("#",ns("innerModulePlaceholder_odd")),where="beforeEnd",
                 ui = tags$div(id = counter$count,
                   innerUiTemplate(id=ns(paste0("innerModule", counter$count )), data))
        )}
        else{
          insertUI(selector=paste0("#",ns("innerModulePlaceholder_even")),where="beforeEnd",
                   ui = tags$div(id = counter$count,
                                 innerUiTemplate(id=ns(paste0("innerModule", counter$count )), data))
        )}
        innerServer(id=paste0("innerModule", counter$count ), data )
        
      }
      )
      
      observeEvent(input$removeItem, {
        
        removeUI(selector = paste0("#",inserted[length(inserted)]))
        inserted <<- inserted[-length(inserted)]
      }
      )
      
      
    }
    
  )
}




#mainUI

ui <- fluidPage(
  uiOutput("Module")
)

# main server
server <- function(input, output, session) {
  
  
  data <- reactive({
    return(data.frame(groups))
  })
  
  output$Module <-renderUI({
    outerUI(id="firstTime" ) 
    
  })
  outerServer(id="firstTime", data() )
}

# run app
shinyApp(ui, server)
