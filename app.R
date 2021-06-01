source("molViewer.R")

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
