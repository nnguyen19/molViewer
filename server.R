constructUI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 4, column(
        width = 9,
  selectizeInput(
          inputId = ns("select_homolog"),#session$ns(paste0("select_homolog_", counter)),
          label = "Choose homolog",
          choices = names(groups)
        ),
  selectizeInput(
    inputId = ns("select_pose"),
    label = "Poses",
    choices = groups[ns("select_homolog")]#c()
  ),
  
  r3dmolOutput(outputId = ns("construct"), height = "500px", width = "400px")
      )
    )
  )
}

constructServer <- function(input, output, session){
  ns <- session$ns
  
  output$construct <- renderR3dmol({
    return(
      r3dmol(
        cartoonQuality = 20,
        #lowerZoomLimit = 50,
        #upperZoomLimit = 350,
        backgroundColor = "#FFFFFF"
      ) %>%
        m_add_model(data = m_bio3d(bio3d_docked(
          paste(c("data/", input$select_homolog, "_tgt.pdb"), collapse = ""),
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
  
  observeEvent(input$set_background_color, {
      m_set_background_color(
        id = "construct",
        hex = input$set_background_color
      )
  })
  
  observeEvent(input$spin, {
      m_spin(id = "construct")
    
  })
  
  observeEvent(input$zoom_out, {
      m_zoom(
        id = "construct",
        factor = 0.7,
        animationDuration = 100
      )
  }) 
  
  observeEvent(input$zoom_in, {
      m_zoom(
        id = "construct",
        factor = 1.3,
        animationDuration = 100
      )
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
      m_set_projection(id = "construct", scheme = input$set_projection)
  })
  
  observeEvent(input$clear, {
      m_clear(id = "construct")
  })
  
  observeEvent(input$set_slab, {
      m_set_slab(
        id = "construct",
        near = input$set_slab[1],
        far = input$set_slab[2]
      )
  })
  
  observeEvent(input$set_perceived_distance, {
    
      m_set_preceived_distance(id = "construct", dist = input$set_perceived_distance)
  })
  
  observeEvent(input$select_pose, {
    output$select_panel <- renderUI({

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

server <- function(input, output, session) {
  #ns <- session$ns
  
   observeEvent(input$add, {
     
  #   noh <- input$num_hom
  # output$display_structure <- renderUI({
  #   lapply(1:noh, function(i) {
  #   input_name <- paste0("homolog_", i)
  #   #print(paste0("input name: ",input_name))
  #   
  #   callModule(constructServer, input_name)
  #   constructUI(id = input_name)
  #   })
  # })
  
  counter <- input$add
  #id <- paste0('txt', btn)
  input_name<- paste0('homolog_', counter)
  #id <- session$ns("select_homolog")
  if (counter%%2 == 1){
    place_holder = "#place_odd"
  } else {
    place_holder = "#place_even"
  }
  insertUI(
    selector = place_holder,
    ## wrap element in a div with id for ease of removal
    ui = tags$div(
      constructUI(input_name),
      br(),
      br()
    ),
    callModule(constructServer, input_name)
  )
  #output$id<- 
})
  
}












