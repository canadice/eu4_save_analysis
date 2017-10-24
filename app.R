#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 60*1024^2)
# Sys.setlocale("LC_ALL", 'English')
# options(encoding = "UTF-8")

library(shiny, quietly = TRUE)
library(stringr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(stringr, quietly = TRUE)
require(parallel, quietly = TRUE)
source("save_scraper.R")
source("value_scraper.R")

# Define UI for application
ui <- fluidPage(title = "Alliance comparisons in EU4",
  
  titlePanel(fluidRow(
    column(2),
    column(8,"Alliance comparisons in EU4"))
  ),
  
  
  fluidRow(
    column(width = 2,
           wellPanel(
             h4("The latest RotR save is already loaded."),
             actionButton(inputId = "data_choice", label = "I want to upload my own save!"
             ),
             br(),
             br(),
             
             uiOutput("upload"),
             
             selectInput(inputId = "continent_choice", label = "Select continent(s) where nations are to be selected:", 
                         choices = c("Europe", "Asia", "Africa", "America"), 
                         multiple = TRUE,
                         selectize = TRUE,
                         width = NULL, 
                         size = NULL
             ),
             
             checkboxGroupInput(inputId = "grouping_choice", label = "Select pre-defined groups you want to visualize:", 
                                choices = c("Religion", "HRE", "Own selection")
             ),
             uiOutput("religion_selector"),
             
             uiOutput("own_team1"),
             uiOutput("attacking"),
             
             uiOutput("own_team2"),
             uiOutput("defending")
           )
    ),
    
    column(width = 10,
           plotOutput("circle_facet"),
           plotOutput("circle_facet_religion"),
           plotOutput("circle_facet_HRE")
    )
  )
  
  # fluidRow(
  #   column(2),
  #   column(4, )
  # )
)

# Define server logic 
server <- function(input, output) {
  getData <- reactive({
    
    inFile <- input$data$datapath
    
    if (is.null(input$data)){
      # save <- readLines(con = "MP_RotR_latest.eu4", encoding = "UTF-8", warn = FALSE)
      # game_data <- save_processing(save)
      # save(game_data, file = "latest_rotr.RData")
      
      load("latest_rotr.RData")
    } else {
      save <- readLines(con = inFile, encoding = "UTF-8", warn = FALSE)  
      
      game_data <- save_processing(save)
    }

    if(!is.null(input$continent_choice)){
      continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
      
      game_data <- game_data[game_data$continent %in% continents,]
    }
        
    return(game_data)
  })
  
  aggData_reactive <- reactive({
    if(is.null(getData())){
      return(NULL)
    }
    
    game_data <- getData()
    

    
    # Inputs the teams and summarizes data
    game_data$Alliance <- rep(NA, nrow(game_data))
    game_data$Alliance[which(game_data$Name %in% input$axis)] <- input$team_1
    game_data$Alliance[which(game_data$Name %in% input$allies)] <- input$team_2
    
    game_data_table <- group_by(.data = game_data[!is.na(game_data$Alliance),], Alliance) %>% 
      summarize_at(.vars = 4:10, .funs = sum, na.rm = TRUE) %>%
      mutate_at(.vars = 2:8, .funs = prop.table)
      
      
    colnames(game_data_table) <- c("Alliance", "Development", 
                                   "Great Power Score", "Treasury", 
                                   "Monthly Income", "Military Strength", 
                                   "Manpower", "Army Size")
    
    
    game_data_table <- gather(game_data_table, Variable, Values, 2:8)
    
    
    return(game_data_table)
  })
  
  ############# Plot Forcelimit distribution
  output$circle_facet <- renderPlot({
    

    if(!is.null(input$axis) | !is.null(input$allies)){
      # Creates plot over the distribution
      ggplot(data = aggData_reactive()) + aes(x = "", y = Values, fill = Alliance) + 
        geom_bar(stat = "identity", width = 1, color = "black") + 
        scale_fill_manual(values = c("#c0c0c0", "#E69F00")) + theme_bw() + 
        scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0,0), breaks = NULL) +
        coord_polar(theta = "y", start = 0) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme(panel.grid = element_blank(), panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
              legend.title = element_text(face = "bold")) +
        facet_grid(facets = ~ Variable) + 
        theme(strip.background = element_blank(), strip.text = )
    }
    
    
  })
  
  ############################# Religion
  
  current_selection_rel <- reactiveVal(NULL)
  observeEvent(input$religion_choice, {
    current_selection_rel(input$religion_choice)
  })
  
  output$religion_selector <- renderUI({
    if("Religion" %in% input$grouping_choice){
      # What religions are to be chosen
      checkboxGroupInput(inputId = "religion_choice", label = "Select religons(s) to be compared:", 
                        choices = sort(c("Catholic", "Protestant", "Reformed", "Orthodox", "Sunni")), 
                        selected = current_selection_rel()
                        )
    } else {
      return(NULL)
    }
  })
  
  
  
  output$circle_facet_religion <- renderPlot({
    if(is.null(getData()) | is.null(input$religion_choice)){
      return(NULL)
    }
    
    if("Religion" %in% input$grouping_choice){
      
      game_data <- getData()
      
      game_data_table <- group_by(.data = game_data[game_data$religion %in% str_to_lower(input$religion_choice),], religion) %>% 
        summarize_at(.vars = 4:10, .funs = sum, na.rm = TRUE) %>%
        mutate_at(.vars = 2:8, .funs = prop.table)
      
      
      colnames(game_data_table) <- c("Religion", "Development", 
                                     "Great Power Score", "Treasury", 
                                     "Monthly Income", "Military Strength", 
                                     "Manpower", "Army Size")
      
      
      game_data_table <- gather(game_data_table, Variable, Values, 2:8)
      
      # Creates plot over the distribution
      ggplot(data = game_data_table) + aes(x = "", y = Values, fill = Religion) + 
        geom_bar(stat = "identity", width = 1, color = "black") + 
        scale_fill_manual(values = c("#c0c0c0", "#E69F00", "#e69138", "#000000", "#a4c2f4")[1:length(input$religion_choice)],
                          labels = input$religion_choice) + 
        theme_bw() + 
        scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0,0), breaks = NULL) +
        coord_polar(theta = "y", start = 0) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme(panel.grid = element_blank(), panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
              legend.title = element_text(face = "bold")) +
        facet_grid(facets = ~ Variable) + 
        theme(strip.background = element_blank(), strip.text = )
    }
    
    
  })
  
  output$circle_facet_HRE <- renderPlot({
    if(is.null(getData())){
      return(NULL)
    }
    
    if("HRE" %in% input$grouping_choice){
      game_data <- getData()
      
      game_data_table <- group_by(.data = game_data[!is.na(game_data$hre),], hre) %>% 
        summarize_at(.vars = 4:10, .funs = sum, na.rm = TRUE) %>%
        mutate_at(.vars = 2:8, .funs = prop.table)
      
      
      colnames(game_data_table) <- c("HRE", "Development", 
                                     "Great Power Score", "Treasury", 
                                     "Monthly Income", "Military Strength", 
                                     "Manpower", "Army Size")
      
      
      game_data_table <- gather(game_data_table, Variable, Values, 2:8)
      
      # Creates plot over the distribution
      ggplot(data = game_data_table) + aes(x = "", y = Values, fill = HRE) +
        geom_bar(stat = "identity", width = 1, color = "black") + 
        scale_fill_manual(values = c("#c0c0c0", "#E69F00"), labels = c("non-HRE", "HRE")) + 
        theme_bw() + 
        scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0,0), breaks = NULL) +
        coord_polar(theta = "y", start = 0) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme(panel.grid = element_blank(), panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
              legend.title = element_text(face = "bold")) +
        facet_grid(facets = ~ Variable) + 
        theme(strip.background = element_blank(), strip.text = )
    }
    
    
  })
  # ############# Plot Development distribution
  # output$bar_dev <- renderPlot({
  #   
  #   if(any(!is.null(input$axis), !is.null(input$allies))){
  #     # Creates plot over the distribution
  #     ggplot(data = aggData()) + aes(x = "", y = development, fill = Alliance) + 
  #       geom_bar(stat = "identity", width = 1, color = "black") + 
  #       scale_fill_manual(values = c("#c0c0c0", "#E69F00")) + theme_bw() + 
  #       scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0,0), breaks = NULL) +
  #       coord_polar(theta = "y", start = 0) +
  #       labs(x = NULL, y = NULL, title = "Development") +
  #       theme(panel.grid = element_blank(), panel.border = element_blank(),
  #             plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
  #             legend.title = element_text(face = "bold"))   
  #   }
  #   
  # })
  # 
  # ############# Plot Development distribution
  # output$bar_income <- renderPlot({
  #   
  #   if(any(!is.null(input$axis), !is.null(input$allies))){
  #     # Creates plot over the distribution
  #     ggplot(data = aggData()) + aes(x = "", y = est_month_income, fill = Alliance) + 
  #       geom_bar(stat = "identity", width = 1, color = "black") + 
  #       scale_fill_manual(values = c("#c0c0c0", "#E69F00")) + theme_bw() + 
  #       scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0,0), breaks = NULL) +
  #       coord_polar(theta = "y", start = 0) +
  #       labs(x = NULL, y = NULL, title = "Monthly Income") +
  #       theme(panel.grid = element_blank(), panel.border = element_blank(),
  #             plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
  #             legend.title = element_text(face = "bold"))   
  #   }
  #   
  # })
  # 
  # ############# Plot Development distribution
  # output$bar_army_size <- renderPlot({
  #   
  #   if(any(!is.null(input$axis), !is.null(input$allies))){
  #     # Creates plot over the distribution
  #     ggplot(data = aggData()) + aes(x = "", y = cur_army_size, fill = Alliance) + 
  #       geom_bar(stat = "identity", width = 1, color = "black") + 
  #       scale_fill_manual(values = c("#c0c0c0", "#E69F00")) + theme_bw() + 
  #       scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0,0), breaks = NULL) +
  #       coord_polar(theta = "y", start = 0) +
  #       labs(x = NULL, y = NULL, title = "Standing Army") +
  #       theme(panel.grid = element_blank(), panel.border = element_blank(),
  #             plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
  #             legend.title = element_text(face = "bold"))   
  #   }
  #   
  # })
  
  
  ################ OWN STUFF
  ############# Selection to upload own save file
  output$upload <- renderUI({
    if(input$data_choice == 0){
      return(NULL)
    }
    fileInput(inputId = "data", label = "Upload an uncompressed .eu4-file", 
              multiple = FALSE, buttonLabel = "Browse", placeholder = "MP_RotR_latest.eu4 is loaded")
  })
  
  
  ############ Selection of own groupings
  output$own_team1 <- renderUI({
    if("Own selection" %in% input$grouping_choice){
      # What one side will be called
      textInput(inputId = "team_1", label = "What do you want to call the attacking alliance?", 
                value = "Axis", 
                width = NULL, 
                placeholder = NULL
      )
    } else {
      return(NULL)
    }
  })
  
  output$own_team2 <- renderUI({
    if("Own selection" %in% input$grouping_choice){
      # What the other side will be called
      textInput(inputId = "team_2", label = "What do you want to call the defending alliance?", 
                value = "Allies", 
                width = NULL, 
                placeholder = NULL
      )
    } else {
      return(NULL)
    }
  })
  
  ############# Selection of Axis
  # Saves current selection of Axis
  current_selection_ax <- reactiveVal(NULL)
  observeEvent(input$axis, {
    current_selection_ax(input$axis)
  })
  
  output$attacking <- renderUI({
    if("Own selection" %in% input$grouping_choice){
      game_data <- getData()
      
      # Marking nations fighting on the other side of the war
      selectInput(inputId = "axis", label = paste("Select nations on the", input$team_1 ,"side:"), 
                  choices = sort(game_data$Name[!(game_data$Name %in% input$allies)]), 
                  selected = current_selection_ax(), 
                  multiple = TRUE,
                  selectize = TRUE,
                  width = NULL, 
                  size = NULL
      )
    } else {
      return(NULL)
    }
  })
  
  ############# Selection of Allies
  # Saves current selection of Allies
  current_selection_al <- reactiveVal(NULL)
  observeEvent(input$allies, {
    current_selection_al(input$allies)
  })
  
  output$defending <- renderUI({
    if("Own selection" %in% input$grouping_choice){
      game_data <- getData()
      
      # Marking nations fighting on the other side of the war
      selectInput(inputId = "allies", label = paste("Select nations on the", input$team_2 ,"side:"), 
                  choices = sort(game_data$Name[!(game_data$Name %in% input$axis)]), 
                  selected = current_selection_al(), 
                  multiple = TRUE,
                  selectize = TRUE,
                  width = NULL, 
                  size = NULL
      )
    } else {
      return(NULL)
    }
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
