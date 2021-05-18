
module_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
  selectizeInput(
    inputId = ns("select_pose"),
    label = "Poses",
    #choices = groups[input$select_homolog]
    choices = c()
  )
  r3dmolOutput(outputId = id, height = "500px", width = "400px")
  )
}
module_server <- function(input, output, session, selector){
  
  render_homolog<- function(homolog, pose){ renderR3dmol({
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
  observeEvent(selector(), {
    updateSelectInput(session, "select_pose", choices=groups[[selector()]]),
    eval(parse(text=paste0("output$",id,"<- render_homolog(input$select_homolog)")))
  })
  
  
  
observeEvent(input$set_background_color, {
  for (h in c("construct_1", "construct_2")){ 
    m_set_background_color(
      id = h,
      hex = input$set_background_color
    )}
})

observeEvent(input$spin, {
  for (h in c("construct_1", "construct_2")){ 
    m_spin(id = h)
  }
})

observeEvent(input$zoom_out, {
  for (h in c("construct_1", "construct_2")){ 
    m_zoom(
      id = h,
      factor = 0.7,
      animationDuration = 100
    )}
}) 

observeEvent(input$zoom_in, {
  for (h in c("construct_1", "construct_2")){ 
    m_zoom(
      id = h,
      factor = 1.3,
      animationDuration = 100
    )}
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

observeEvent(input$set_projection, {
  for (h in c("construct_1", "construct_2")){ 
    m_set_projection(id = h, scheme = input$set_projection)}
})

observeEvent(input$clear, {
  for (h in c("construct_1", "construct_2")){ 
    m_clear(id = h)}
})

observeEvent(input$set_slab, {
  for (h in c("construct_1", "construct_2")){ 
    m_set_slab(
      id = h,
      near = input$set_slab[1],
      far = input$set_slab[2]
    )}
})

observeEvent(input$set_perceived_distance, {
  for (h in c("construct_1", "construct_2")){ 
    m_set_preceived_distance(id = "r3dmol", dist = input$set_perceived_distance)}
})

returnValue <- reactive({
  input$selectHere
})

#print(returnValue)
return(returnValue)
}


server <- function(input, output, session) {
  

  
  #inserted <- c()
  observeEvent(input$add, {
    #btn <- input$insertBtn
    counter <- input$add
    #id <- paste0('txt', btn)
    id<- paste0('construct_', counter)
    if (counter%%2 == 1){
      place_holder = "#place_odd"
    } else {
      place_holder = "#place_even"
    }
    insertUI(
      selector = place_holder,
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        selectizeInput(
          inputId = "select_homolog",
          label = "Choose homolog",
          choices = names(groups)
        ),
        module_ui(id),
        br(),
        br()
      )
    )
    mod1<- callModule(module_server, id, reactive({input$select_homolog}))
    
    
    #inserted <<- c(id, inserted)

    #eval(parse(text=paste0("output$",id,"<- render_homolog(input$select_homolog)")))
  })
  
  observeEvent(input$remove, {
    removeTab(inputId = "tabs", target = "Homolog")
  })
  
  observeEvent(input$select_single_model, {
    output$select_panel <- renderUI({
      #animate_panel <- input$select_single_model
      # switch(
      #   input$select_single_model,
      #   "animate_sample" = radioButtons(
      #     inputId = "animate",
      #     label = "Animate",
      #     choices = c(FALSE, TRUE),
      #     inline = TRUE
      #   )
      # )
      
      tagList(
        selectInput(
          inputId = "set_style",
          label = "Set Style",
          choices = c("Stick","Line", "Cross", "Sphere", "Cartoon"),
          selected = "Cartoon"
        ),
        sliderInput(
          inputId = "set_slab",
          label = "Set slab of view",
          min = -150,
          value = c(-50, 50),
          animate = TRUE,
          step = 5,
          max = 150,
          dragRange = TRUE
        ),
        radioButtons(
          inputId = "set_projection",
          label = "Set view projection scheme",
          choices = c("perspective", "orthographic"),
          inline = TRUE
        ),
        sliderInput(
          inputId = "set_perceived_distance",
          label = "Set perceived distance",
          min = 0,
          max = 500,
          value = 300
        )
      )
    })
  })
  

  
}

