
ui <- fluidPage(
  fluidRow(column(
    width = 12,
    titlePanel("Kinase Docking Models")
  )),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        
        colourInput(
          inputId = "set_background_color",
          label = "Set background color",
          closeOnClick = TRUE,
          value = "#FFFFFF"
        ),
        uiOutput(outputId = "select_panel"),
        actionButton(
          inputId = "zoom_in",
          label = "Zoom in",
          icon = icon("plus")
        ),
        actionButton(
          inputId = "zoom_out",
          label = "Zoom out",
          icon = icon("minus")
        ),
        actionButton(
          inputId = "spin",
          label = "Spin",
          icon = icon("sync-alt")
        ),
        actionButton(
          inputId = "clear",
          label = "Clear",
          icon = icon("trash-alt")
        )
      )
    ),
    # column(
    #        width = 6,
    #       selectizeInput(
    #         inputId = "select_homolog",
    #         label = "Choose homolog to start",
    #       choices = names(groups)
    #       )
    # ),
    column(
      width = 6,
      #tabsetPanel(id = "tabs",
                  #tabPanel("dummy", actionButton("add", label = "More", icon = icon("plus")),
                  #         actionButton("remove", label = "Less" ,icon = icon("minus")))
                  #tabPanel("Number of Homologs to Be Displayed",""),
                   fluidRow(actionButton("add", label = "", icon = icon("plus")),
                            actionButton("remove", label = "" ,icon = icon("minus")))
                  #fluidRow(numericInput("num_hom", "Number of Homologs to Be Displayed", 2, min = 2, max = 100))
    ),
    br(),
    # column(
    #   width = 4,
    #   # column(
    #   #   width = 9,
    #     selectizeInput(
    #       inputId = "select_single_model",
    #       #label = "PAK1_HUMAN_D0_4DAW_A_tgt.pdb",
    #       label = "Homolog",
    #       choices = list("drug option" = compound_list)
    #       # choices = list(
    #       #   "6ZSL: Crystal structure of the SARS-CoV-2 helicase" = "6zsl",
    #       #   "Animate Sample" = "animate_sample"
    #       # ),
    #     #)
    #   ),
    #   
    #               
    #               
    #     # tabPanel(title = "More",
    #     #          icon = icon("plus"),
    #     #          fluidRow()),
    #     # tabPanel(title = "Less",
    #     #          icon = icon("minus"),
    #     #          fluidRow())
    #   
    #   #),
    #   #uiOutput("r3dmolPlots")
    #   br(),
    #   br(),
    #   r3dmolOutput(outputId = "construct_1", height = "500px", width = "400px")
    # ),
    ######################## adding module to place holder
     column(
       width = 4,
       tags$div(id = 'place_odd')),
     column(
       width = 5,
       tags$div(id = 'place_even'))
    
    #uiOutput(outputId = "display_structure")
    #########################
    
    #column(
     # width = 4,
      # column(
        # width = 9,
    #tags$div(id = 'place_odd')),
    #column(
     # width = 5,
      # column(
      # width = 9,
    #  tags$div(id = 'place_even'))
    # column(
    #   width = 4,
    #   column(
    #     width = 9,
    #     selectizeInput(
    #       inputId = "select_single_model",
    #       label = "Homolog",
    #       choices = list("drug option" = compound_list)
    #     )
    #   ),
    #   
    #   br(),
    #   br(),
    #   r3dmolOutput(outputId = "construct_2", height = "500px", width = "400px")
    # )
   
  )
)