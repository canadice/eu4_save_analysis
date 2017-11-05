#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 60*1024^2)
# setwd("F:/GitHubs/eu4_save_analysis")

library(shiny, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(stringr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(stringr, quietly = TRUE)
require(parallel, quietly = TRUE)
require(shinycssloaders, quietly = TRUE)
source("save_scraper.R")
source("data_compiler.R")


# Define UI for application
ui <- fluidPage(theme = shinytheme("united"), title = "Save analyzer for Europa Universalis 4",
  
  uiOutput(outputId = "title"),
  
  navbarPage("Functions",
    tabPanel("Import",
             fluidRow(
               column(width = 2,
                      wellPanel(
                        actionButton(inputId = "data_choice", 
                                     label = "Upload uncompressed .eu4"),
                        br(), 
                        br(),
                        withSpinner(uiOutput("upload")) 
                      )
               )
             )
    ),
    tabPanel("Religion comparison",
             fluidRow(
               column(width = 2,
                      wellPanel(
                        withSpinner(uiOutput("continents")),
                        checkboxGroupInput(inputId = "religion_choice", label = "Select religons(s) to be compared:", 
                                           choices = sort(c("Catholic", "Protestant", "Reformed", "Orthodox", "Sunni"))
                        )
                      )
               ),
               column(width = 10,
                      withSpinner(plotOutput("circle_facet_religion"))
                      )
               )
    ),
    tabPanel("HRE comparison",
             fluidRow(
               column(width = 2,
                      wellPanel(
                        withSpinner(uiOutput("continents2"))
                      )
               ),
               column(width = 10,
                      withSpinner(plotOutput("circle_facet_HRE"))
               )
             )
    ),
    tabPanel("Own groupings Comparison",
             fluidRow(
               column(width = 2,
                      wellPanel(
                        withSpinner(uiOutput("continents3")),
                        withSpinner(uiOutput("own_team1")),
                        withSpinner(uiOutput("attacking")),
                        withSpinner(uiOutput("own_team2")),
                        withSpinner(uiOutput("defending"))
                      )
               ),
               column(width = 10,
                      withSpinner(plotOutput("circle_facet"))
               )
             )
    ),
    tabPanel("Data tables & Exports",
             fluidRow(
               column(width = 2,
                      wellPanel(
                        radioButtons(inputId = "export_choice", label = "Select data to visualize.", 
                                     choices = c("None selected", "Province", "Country")
                        ),
                        uiOutput("export_province_vars"),
                        uiOutput("export_country_vars")
                      )
               ),
               column(width = 10,
                      fluidRow(
                        column(width = 12,
                               dataTableOutput("province_data"),
                               dataTableOutput("country_data")
                        )
                      ),
                      fluidRow(
                        column(width = 2,
                               withSpinner(uiOutput("export_data_button"))
                        ),
                        column(width = 2,
                               withSpinner(uiOutput("export_data_button_view"))
                        )
                      )
               )
             )
    )
  ) # End navbarPage
) # End fluidPage

# Define server logic 
server <- function(input, output) {
  #################################################
  ### title
  #################################################
  output$title <- renderUI({
    meta_data <- getData()$meta
    
    titlePanel(
      fluidRow(column(width = 1),
               column(width = 5,
                      h1("Save analyzer for Europa Universalis 4"),
                      h4(paste("Current date of the loaded save", meta_data$save_game, "is", meta_data$date))
               )
      )
    )
  })
  
  #################################################
  ### Parsing data
  #################################################
  getData <- reactive({
    
    if (is.null(input$data)){
      # save <- readLines(con = "C:/Users/Canadice/Documents/Paradox Interactive/Europa Universalis IV/save games/MP_RotR_latest.eu4", encoding = "ANSI", warn = FALSE)
      # game_data <- save_processing(save)
      # save(game_data, file = "latest_rotr.RData")
      
      load("latest_rotr.RData")
    } else {
      inFile <- input$data$datapath
      
      save <- readLines(con = inFile, encoding = "ANSI", warn = FALSE)  
      
      game_data <- save_processing(save)
    }

    return(game_data)
  })
  
  #################################################
  ### Comparison visualization
  #################################################
  
  # Religion
  output$circle_facet_religion <- renderPlot({
    if(is.null(getData()) | is.null(input$religion_choice)){
      return(NULL)
    }
    game_data <- getData()$country
    
    # Subsets continents
    if(!is.null(input$continent_choice)){
      continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
      
      game_data <- game_data[game_data$continent %in% continents,]
    }
    
    game_data_table <- group_by(.data = game_data[game_data$religion %in% str_to_lower(input$religion_choice),], religion) %>% 
      summarize_at(.vars = 5:11, .funs = sum, na.rm = TRUE) %>%
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
  })
  
  # HRE
  output$circle_facet_HRE <- renderPlot({
    if(is.null(getData())){
      return(NULL)
    }
    
    game_data <- getData()$country
    
    # Subsets continents
    if(!is.null(input$continent_choice)){
      continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
      
      game_data <- game_data[game_data$continent %in% continents,]
    }
    
    game_data_table <- group_by(.data = game_data[!is.na(game_data$hre),], hre) %>% 
      summarize_at(.vars = 5:11, .funs = sum, na.rm = TRUE) %>%
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
  })
  
  
  #################################################
  ### Tables of data 
  #################################################
  output$export_province_vars <- renderUI({
    if(input$export_choice != "Province"){
      return(NULL)
    } else {
      checkboxGroupInput(inputId = "export_province_choice", label = "Select variables to show:", 
                  choices = colnames(getData()$province)[colnames(getData()$province) != "name"]
      )
    }
  })
  
  output$province_data <- renderDataTable({
    if(input$export_choice != "Province"){
      return(NULL)
    } else {
      getData()$province[, c("name", input$export_province_choice)]    
    }
  }, options = list(orderClasses = TRUE,pageLength = 10)
  )
  
  output$export_country_vars <- renderUI({
    if(input$export_choice != "Country"){
      return(NULL)
    } else {
      
      game_data <- getData()$country
      # Subsets continents
      if(!is.null(input$continent_choice)){
        continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
        
        game_data <- game_data[game_data$continent %in% continents,]
      }
      
      checkboxGroupInput(inputId = "export_country_choice", label = "Select variables to show:", 
                         choices = colnames(game_data)[colnames(game_data) != "Name"]
      )
    }
  })
  
  output$country_data <- renderDataTable({
    if(input$export_choice != "Country"){
      return(NULL)
    }  else {
      
      game_data <- getData()$country
      # Subsets continents
      if(!is.null(input$continent_choice)){
        continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
        
        game_data <- game_data[game_data$continent %in% continents,]
      }
      
      game_data[, c("Name", input$export_country_choice)]    
    }
  }, options = list(orderClasses = TRUE,pageLength = 10)
  )
  
  # Download buttons
  output$export_data_button <- renderUI({
    if(input$export_choice == "Country"){
      downloadButton(outputId = "export_country", 
                     label = "Download entire data table")
        
    } else if(input$export_choice == "Province"){
      downloadButton(outputId = "export_province", 
                     label = "Download entire data table")
    } else {
      return(NULL)
    }
  })
  
  output$export_data_button_view <- renderUI({
    if(input$export_choice == "Country"){
      downloadButton(outputId = "export_country_view", 
                     label = "Download current selection")
      
    } else if(input$export_choice == "Province"){
      downloadButton(outputId = "export_province_view", 
                     label = "Download current selection")
    } else {
      return(NULL)
    }
  })
  
  # Export functions
  output$export_country <- downloadHandler(
    filename = function(){
      meta_data <- getData()$meta
      
      paste(meta_data$save_game, meta_data$date, "country_data.csv", sep = "_")
    },
    content = function(file){
      game_data <- getData()$country
      # Subsets continents
      if(!is.null(input$continent_choice)){
        continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
        
        game_data <- game_data[game_data$continent %in% continents,]
      }
      
      write.csv(game_data, file, row.names = FALSE)
    }
  )
  
  output$export_province <- downloadHandler(
    filename = function(){
      meta_data <- getData()$meta
      
      paste(meta_data$save_game, meta_data$date, "province_data.csv", sep = "_")
    }, 
    content = function(file){
      write.csv(getData()$province, file, row.names = FALSE)
    }
  )
  
  output$export_country_view <- downloadHandler(
    filename = "country_data_view.csv", 
    content = function(file){
      game_data <- getData()$country
      # Subsets continents
      if(!is.null(input$continent_choice)){
        continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
        
        game_data <- game_data[game_data$continent %in% continents,]
      }
      
      write.csv(game_data[, c("Name", input$export_country_choice)], file, row.names = FALSE)
    }
  )
  
  output$export_province_view <- downloadHandler(
    filename = "province_data_view.csv", 
    content = function(file){
      write.csv(getData()$province[, c("name", input$export_province_choice)], file, row.names = FALSE)
    }
  )
  
  #################################################
  ### OWN STUFF
  #################################################
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
    # What one side will be called
    textInput(inputId = "team_1", label = "What do you want to call the attacking alliance?", 
              value = "Axis", 
              width = NULL, 
              placeholder = NULL
    )
  })
  
  output$own_team2 <- renderUI({
    # What the other side will be called
    textInput(inputId = "team_2", label = "What do you want to call the defending alliance?", 
              value = "Allies", 
              width = NULL, 
              placeholder = NULL
    )
  })
  
  
  ############ Continents
  # current_selection_con <- reactiveVal(NULL)
  # observeEvent(input$continent_choice, {
  #   current_selection_con(input$continent_choice)
  # })
  
  output$continents <- output$continents2 <- output$continents3 <- renderUI({
    selectInput(inputId = "continent_choice", 
                label = "Select continent(s) where nations are to be selected:", 
                choices = c("Europe", "Asia", "Africa", "America"),
                #selected = current_selection_con(),
                multiple = TRUE,
                selectize = TRUE,
                width = NULL, 
                size = NULL
                )
  })
  
  
  ############# Selection of Axis
  # Saves current selection of Axis
  current_selection_ax <- reactiveVal(NULL)
  observeEvent(input$axis, {
    current_selection_ax(input$axis)
  })
  
  output$attacking <- renderUI({
    game_data <- getData()$country
    # Subsets continents
    if(!is.null(input$continent_choice)){
      continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
      
      game_data <- game_data[game_data$continent %in% continents,]
    }
    
    # Marking nations fighting on the other side of the war
    selectInput(inputId = "axis", label = paste("Select nations on the", input$team_1 ,"side:"), 
                choices = sort(game_data$Name[!(game_data$Name %in% input$allies)]), 
                selected = current_selection_ax(), 
                multiple = TRUE,
                selectize = TRUE,
                width = NULL, 
                size = NULL
    )
  })
  
  ############# Selection of Allies
  # Saves current selection of Allies
  current_selection_al <- reactiveVal(NULL)
  observeEvent(input$allies, {
    current_selection_al(input$allies)
  })
  
  output$defending <- renderUI({
    game_data <- getData()$country
    # Subsets continents
    if(!is.null(input$continent_choice)){
      continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
      
      game_data <- game_data[game_data$continent %in% continents,]
    }
    
    # Marking nations fighting on the other side of the war
    selectInput(inputId = "allies", label = paste("Select nations on the", input$team_2 ,"side:"), 
                choices = sort(game_data$Name[!(game_data$Name %in% input$axis)]), 
                selected = current_selection_al(), 
                multiple = TRUE,
                selectize = TRUE,
                width = NULL, 
                size = NULL
    )
  })
  
  # Aggregates the data according to the two alliances
  aggData_reactive <- reactive({
    if(is.null(getData())){
      return(NULL)
    }
    
    game_data <- getData()$country
    # Subsets continents
    if(!is.null(input$continent_choice)){
      continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))  
      
      game_data <- game_data[game_data$continent %in% continents,]
    }
    
    
    
    # Inputs the teams and summarizes data
    game_data$Alliance <- rep(NA, nrow(game_data))
    game_data$Alliance[which(game_data$Name %in% input$axis)] <- input$team_1
    game_data$Alliance[which(game_data$Name %in% input$allies)] <- input$team_2
    
    game_data_table <- group_by(.data = game_data[!is.na(game_data$Alliance),], Alliance) %>% 
      summarize_at(.vars = 5:11, .funs = sum, na.rm = TRUE) %>%
      mutate_at(.vars = 2:8, .funs = prop.table)
    
    
    colnames(game_data_table) <- c("Alliance", "Development", 
                                   "Great Power Score", "Treasury", 
                                   "Monthly Income", "Military Strength", 
                                   "Manpower", "Army Size")
    
    
    game_data_table <- gather(game_data_table, Variable, Values, 2:8)
    
    
    return(game_data_table)
  })
  
  ## Plots output of own grouping
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
}


# Run the application 
shinyApp(ui = ui, server = server)

