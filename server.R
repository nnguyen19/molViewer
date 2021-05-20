
module_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    
    tags$div(
  selectizeInput(
    inputId = ns("select_pose"),
    label = "Poses",
    #choices = groups[input$select_homolog]
    choices = c()
  ),
  r3dmolOutput(outputId = ns("homolog"), height = "500px", width = "400px")
  ))
}
module_server <- function(input, output, session, homolog_selector, 
                          set_background_color, spin, clear, zoom_in,
                          zoom_out, set_style,set_slab, set_projection,
                          set_perceived_distance){
  
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
  
  ns <- session$ns
  #print(input$select_homolog)
  #print(input$select_pose)
  #print(selector())
  observeEvent(homolog_selector(), {
    updateSelectInput(session, "select_pose", choices=groups[[homolog_selector()]])
    #eval(parse(text=paste0("output$",id,"<- render_homolog(input$select_homolog)")))
    #print(input$select_pose)
    #print(parse(text=paste0("input$",ns("select_pose"))))
    #pose <- eval(parse(text=paste0("input$",ns("select_pose"))))
    
  })
  
  observeEvent(input$select_pose,{
    #output$construct <- render_homolog(selector(), input$select_pose)
    output$homolog <- renderR3dmol({
      return(
        r3dmol(
          cartoonQuality = 20,
          #lowerZoomLimit = 50,
          #upperZoomLimit = 350,
          backgroundColor = "#FFFFFF"
        ) %>%
          m_add_model(data = m_bio3d(bio3d_docked(
            paste(c("data/", homolog_selector(), "_tgt.pdb"), collapse = ""),
            paste("data", input$select_pose, sep="/")))
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
    })
  
  

# observeEvent(reactive({bg_color()}), {
#   updateColourInput(session, "set_background_color", label = "Set background color",
#                     closeOnClick = TRUE,
#                     value = "#FFFFFF")
#   print("UPDATE")
#   
# })

observeEvent(set_background_color(),{
  m_set_background_color(
    id = "homolog",
    hex = set_background_color()
  )
})



observeEvent(spin(), {
  
    m_spin(id = "homolog")
  
})

observeEvent(zoom_out(), {
  
    m_zoom(
      id = "homolog",
      factor = 0.7,
      animationDuration = 100)
    
}) 

observeEvent(zoom_in(), {
  
    m_zoom(
      id = "homolog",
      factor = 1.3,
      animationDuration = 100)
    
})

# observeEvent(input$set_style, {
#   style <- switch(
#     input$set_style,
#     "Line" = list(line = list()),
#     "Cartoon" = list(cartoon = list()),
#     "Stick" = list(stick = list()),
#     "Cross" = list(cross = list()),
#     "Sphere" = list(sphere = list())
#   )
# 
#   m_set_style(id = "r3dmol", style = c(style, list(color = "#00cc96"))) %>%
#   m_set_style(
#     sel = m_sel(ss = "s"),
#     style = c(style, list(color = "#636efa", arrows = TRUE))) %>%
#     # Style the alpha helix
#     m_set_style(
#       sel = m_sel(ss = "h"), # Style alpha helix
#       style = c(style, list(color = "#ff7f0e"))) %>%
#     m_set_style(
#       sel = m_sel(resn = "LIG"),
#       style = m_style_stick()
#     ) 
# 
#     #Style the alpha helix
#     # m_set_style(
#     #   id = "r3dmol",
#     #   sel = m_sel(ss = "h"), # Style alpha helix
#     #   style = list(style = style, color = "#636efa"))
# }, ignoreInit = TRUE)

observeEvent(set_projection(), {
  
    m_set_projection(id = "homolog", scheme = set_projection())
})

observeEvent(clear(), {
    m_clear(id = "homolog")
})

observeEvent(set_slab(), {
    m_set_slab(
      id = "homolog",
      near = set_slab()[1],
      far = set_slab()[2]
    )
})

observeEvent(set_perceived_distance(), {

    m_set_preceived_distance(id = "homolog", dist = set_perceived_distance())
})

#returnValue <- reactive({
#  input$select_pose
#})

#print(returnValue)
#return(returnValue)
}


server <- function(input, output, session) {
  

  
  #inserted <- c()
  observeEvent(input$add, {
    #btn <- input$insertBtn
    counter <- input$add
    #id <- paste0('txt', btn)
    input_name<- paste0('construct_', counter)
    #id <- session$ns("select_homolog")
    if (counter%%2 == 1){
      place_holder = "#place_odd"
    } else {
      place_holder = "#place_even"
    }
    ns <- session$ns
    insertUI(
      selector = place_holder,
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        wellPanel(
          
          colourInput(
            inputId = ns("set_background_color"),
            label = "Set background color",
            closeOnClick = TRUE,
            value = "#FFFFFF"
          ),
          uiOutput(outputId = "select_panel"),
          actionButton(
            inputId = ns("zoom_in"),
            label = "Zoom in",
            icon = icon("plus")
          ),
          actionButton(
            inputId = ns("zoom_out"),
            label = "Zoom out",
            icon = icon("minus")
          ),
          actionButton(
            inputId = ns("spin"),
            label = "Spin",
            icon = icon("sync-alt")
          ),
          actionButton(
            inputId = ns("clear"),
            label = "Clear",
            icon = icon("trash-alt")
          )
        ),
        selectizeInput(
          inputId = ns("select_homolog"),#session$ns(paste0("select_homolog_", counter)),
          label = "Choose homolog",
          choices = names(groups)
        ),selectInput(
          inputId = ns("set_style"),
          label = "Set Style",
          choices = c("Stick","Line", "Cross", "Sphere", "Cartoon"),
          selected = "Cartoon"
        ),
        sliderInput(
          inputId = ns("set_slab"),
          label = "Set slab of view",
          min = -150,
          value = c(-50, 50),
          animate = TRUE,
          step = 5,
          max = 150,
          dragRange = TRUE
        ),
        radioButtons(
          inputId = ns("set_projection"),
          label = "Set view projection scheme",
          choices = c("perspective", "orthographic"),
          inline = TRUE
        ),
        sliderInput(
          inputId = ns("set_perceived_distance"),
          label = "Set perceived distance",
          min = 0,
          max = 500,
          value = 300
        ),
        module_ui(ns(input_name)),
        br(),
        br()
      )
    )
     callModule(module_server, input_name, reactive({input$select_homolog}), 
                reactive({input$set_background_color}), reactive({input$spin}),
                reactive({input$clear}), reactive({input$zoom_in}), 
                reactive({input$zoom_out}), reactive({input$set_style}),
                reactive({input$set_slab}), reactive({input$set_projection}),
                reactive({input$set_perceived_distance})
                )
    
     
    #output$id<- callModule(module_server, id, reactive({eval(parse(text="input$select_homolog_",counter))}))
    
    #inserted <<- c(id, inserted)

    #eval(parse(text=paste0("output$",id,"<- render_homolog(input$select_homolog)")))
     
     
  })
  
  observeEvent(input$remove, {
    removeTab(inputId = "tabs", target = "Homolog")
  })
  
  
  
  # observeEvent(input$select_single_model, {
  #   output$select_panel <- renderUI({
  #     #animate_panel <- input$select_single_model
  #     # switch(
  #     #   input$select_single_model,
  #     #   "animate_sample" = radioButtons(
  #     #     inputId = "animate",
  #     #     label = "Animate",
  #     #     choices = c(FALSE, TRUE),
  #     #     inline = TRUE
  #     #   )
  #     # )
  #     
  #     tagList(
  #       selectInput(
  #         inputId = "set_style",
  #         label = "Set Style",
  #         choices = c("Stick","Line", "Cross", "Sphere", "Cartoon"),
  #         selected = "Cartoon"
  #       ),
  #       sliderInput(
  #         inputId = "set_slab",
  #         label = "Set slab of view",
  #         min = -150,
  #         value = c(-50, 50),
  #         animate = TRUE,
  #         step = 5,
  #         max = 150,
  #         dragRange = TRUE
  #       ),
  #       radioButtons(
  #         inputId = "set_projection",
  #         label = "Set view projection scheme",
  #         choices = c("perspective", "orthographic"),
  #         inline = TRUE
  #       ),
  #       sliderInput(
  #         inputId = "set_perceived_distance",
  #         label = "Set perceived distance",
  #         min = 0,
  #         max = 500,
  #         value = 300
  #       )
  #     )
  #   })
  # })
  

  
}

