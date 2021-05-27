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



bio3d_docked<- function(path2m1, path2m2){
  print(path2m1)
  print(path2m2)
  pdb1 <- read.pdb(path2m1)
  pdb2 <- read.pdb(path2m2)
  docked <- cat.pdb(pdb1, pdb2, pdb1, rechain=FALSE, renumber=FALSE)
  return(docked)
}



##########################################



#UI elements
outerUI<-function(id){ #This is main UI
  
  
  ns <- NS(id)
  
  fluidRow(column(3,
          actionButton(inputId=ns("addItem"), "Add New Homolog"),
          
           actionButton(inputId=ns("removeItem"), "Remove Homolog"),
          div(id = ns('innerModulePlaceholder'))),
    
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
    
    pickerInput(  inputId=ns("poseSelector"),
                  label = "Select Pose",
                  choices= NULL,
                  selected = NULL,
                  multiple = FALSE
    ),
    br(),
    r3dmolOutput(outputId = ns("model"))#, height = "500px", width = "400px")
  )
  
  
  
  
}

#updates
innerServer<-function(id,data, var){
  render_homolog<- function(homolog, pose){ 
    renderR3dmol({
      return(
        r3dmol(
          cartoonQuality = 20,
          #lowerZoomLimit = 50,
          #upperZoomLimit = 350,
          backgroundColor = "#FFFFFF"
        ) %>%
          m_add_model(data = m_bio3d(bio3d_docked(
            paste(c("data/", homolog, "_tgt.pdb"), collapse = ""),
            paste("data", pose, sep="/")))
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
        updatePickerInput(
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
        insertUI(selector=paste0("#",ns("innerModulePlaceholder")),where="afterEnd",
                 ui = tags$div(id = counter$count,
                   innerUiTemplate(id=ns(paste0("innerModule", counter$count )), data))
                 )
        innerServer(id=paste0("innerModule", counter$count ), data )
      }
      )
      
      observeEvent(input$removeItem, {
        #removeUI(selector = paste0('#', ns(inserted[length(inserted)])))
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
  
  # data<-reactive({
  #   
  #   column1<-c(1,2,3,4,5)
  #   column2<-c("a","d","e","g","k")
  #   data<-data.frame(column1, column2)
  #   
  #   return(data)
  # })
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
