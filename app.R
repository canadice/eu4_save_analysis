############################################################
###
### Europa Universalis Save Analysis App
###
###
############################################################

############################################################
### Basic options
############################################################
## Increasing the possible allowed size for uploaded save-files.
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

# Sourcing the scraper and compiler scripts
# source("save_scraper.R")
# source("data_compiler.R")
source("functions.R")

## Loading the required packages
require(shiny, quietly = TRUE)
require(shinythemes, quietly = TRUE)
require(stringr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(RColorBrewer, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(stringr, quietly = TRUE)
require(parallel, quietly = TRUE)
require(shinycssloaders, quietly = TRUE)
require(DT, quietly = TRUE)
require(plotly, quietly = TRUE)

############################################################
### User Interface of the application
############################################################
# Define UI for application
ui <-
  fluidPage(
    theme = shinytheme("united"),
    title = "Save analysis for Europa Universalis 4",
    
    ## Adds a title to the page containing the meta-information about the currently loaded save
    uiOutput(outputId = "header"),
    
    ## The panel of options present in the app
    navbarPage(
      "Functions",
      #### Adds a tab containing Import options for new saves ####
      tabPanel("Import New Save",
               fluidRow(column(
                 width = 2,
                 wellPanel(
                   actionButton(inputId = "data_choice",
                                label = "Upload uncompressed .eu4"),
                   br(),
                   br(),
                   withSpinner(uiOutput("upload"))
                 )
               ))),
      
      #### Adds a tab containing pie-chart comparisons of two groups of nations ####
      tabPanel("Group Comparisons",
               fluidRow(
                 column(
                   width = 2,
                   wellPanel(
                     ## Choice of names and nations for grouping
                     withSpinner(uiOutput("own_team_name1")),
                     withSpinner(uiOutput("attacking")),
                     withSpinner(uiOutput("own_team_name2")),
                     withSpinner(uiOutput("defending"))
                   )
                 ),
                 column(width = 10,
                        withSpinner(
                          ## Visualization
                          plotOutput("pie_chart_facet_comparison")
                        ))
               )),
      
      #### Adds a tab giving table visualizations of province data ####
      tabPanel(
        "Province Information",
        column(width = 2,
               wellPanel(uiOutput(
                 "data_province_vars"
               ))),
        column(width = 10,
               fluidRow(column(
                 width = 12,
                 withSpinner(dataTableOutput("province_data"))
               )))
      ),
      
      #### Adds a tab giving table visualizations of nation data ####
      tabPanel("Nation Information",
               fluidRow(
                 column(width = 2,
                        wellPanel(
                          ## Allows for selection of specific nations
                          withSpinner(uiOutput("nations")),
                          withSpinner(uiOutput("human_players")),
                          uiOutput("data_nation_vars")
                        )),
                 column(width = 10,
                        fluidRow(column(
                          width = 12,
                          withSpinner(dataTableOutput("country_data"))
                        )))
               )),
      
      #### Scoring tab
      tabPanel("Campaign Scoring",
               fluidRow(
                 column(width = 12,
                        dataTableOutput("scoring_data")
                        )
               )),
      #### Time-Series tab
      tabPanel("Time series",
               fluidRow(
                 column(width = 2,
                        wellPanel(
                          radioButtons(inputId = "ts_variable",
                                       label = "What to show?",
                                       choiceNames = c("Income", 
                                                       "Inflation",
                                                       "Score",
                                                       "Size of Nation"),
                                       choiceValues = c("income", 
                                                        "inflation",
                                                        "score",
                                                        "nation_size")),
                          uiOutput("ts_nations")
                        )),
                 column(width = 10,
                        withSpinner(plotlyOutput("time_series"))
                 )
               ))
      
      #### End of Panels ####
    )
  )

# Define server logic
server <- function(input, output) {
  #######################################
  ##### Import and scraping of data
  #######################################
  #### Reading the name and date from loaded file and adds it in header ####
  output$header <- renderUI({
    ## Acquires meta data from the save file
    meta_data <- getData()$meta
    
    titlePanel(fluidRow(column(width = 1),
                        column(
                          width = 5,
                          h1("Save Analysis App for Europa Universalis 4"),
                          h4(paste("Current date of the loaded save", 
                                   meta_data$save_game,
                                   "is",
                                   meta_data$date
                                   )
                             ),
                          h5("App updated for the", meta_data$name, "patch.", 
                             tagList("Source code via", 
                                     a("GitHub", 
                                       href= "https://github.com/canadice/eu4_save_analysis", 
                                       target = "_blank")))
                        )))
  })
  
  
  #### Parsing data from loaded/imported save file ####
  getData <- reactive({
    if (is.null(input$data)) {
      load("latest.RData")
    } else {
      inFile <- input$data$datapath
      
      save <-
        readLines(con = inFile,
                  encoding = "ANSI",
                  warn = FALSE)
      
      game_data <- save_processing(save)
    }
    
    return(game_data)
  })
  
  
  
  #### Uploading own save file ####
  output$upload <- renderUI({
    if (input$data_choice == 0) {
      return(NULL)
    }
    fileInput(
      inputId = "data",
      label = "Upload an *uncompressed* .eu4-file",
      multiple = FALSE,
      buttonLabel = "Browse",
      placeholder = "latest.eu4 is loaded"
    )
  })
  #######################################
  ##### Tables of data
  #######################################
  #### Selection of specific nations ####
  current_selection_nat <- reactiveVal(NULL)
  observeEvent(input$nations, {
    current_selection_nat(input$nations)
  })
  
  output$nations <- renderUI({
    game_data <- getData()$country
    
    if(is.null(input$only_human)){
      
    } else {
      if(input$only_human == "No"){
        # Selecting the nations to present in the table
        selectInput(
          inputId = "nations",
          label = paste("Select nations to show in the table."),
          choices = sort(game_data$Name),
          selected = current_selection_nat(),
          multiple = TRUE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        )  
      } else {
        selectInput(
          inputId = "nations",
          label = paste("Select nations to show in the table."),
          choices = sort(game_data$Name),
          selected = game_data$Name[game_data$human == "yes" | game_data$was_player == "yes"],
          multiple = TRUE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        )
      }  
    }
    
  })
  
  #### Output to select only human players to show in a multiplayer game ####
  output$human_players <- renderUI({
    game_data <- getData()$country
    
    multiplayer <- getData()$meta$multi_player
    
    if(multiplayer != "yes"){
      NULL
    } else {
      radioButtons(inputId = "only_human",
                   label = paste("Do you want to select all human players?"),
                   choices = c("Yes", "No"),
                   selected = "No",
                   width = NULL)  
    }
    
  })
  
  
  
  
  #### Province data functions ####
  current_selection_prov_var <- reactiveVal(NULL)
  observeEvent(input$selected_province_vars, {
    current_selection_prov_var(input$selected_province_vars)
  })
  
  output$data_province_vars <- renderUI({
    selectInput(
      inputId = "selected_province_vars",
      label = "Select variables to show:",
      choices = colnames(getData()$province)[colnames(getData()$province) != "name"],
      selected = current_selection_prov_var(),
      multiple = TRUE
    )
  })
  output$province_data <- DT::renderDataTable({
    if(length(input$selected_province_vars) == 0){
      return(NULL)
    }
    getData()$province[, c("name", input$selected_province_vars)]
  }, rownames = FALSE, 
  class = 'compact cell-border stripe',
  options = list(orderClasses = TRUE, autoWidth = TRUE, pageLength = 20))
  
  
  
  #### Nation data functions ####
  current_selection_nat_var <- reactiveVal(NULL)
  observeEvent(input$selected_nation_vars, {
    current_selection_nat_var(input$selected_nation_vars)
  })
  
  output$data_nation_vars <- renderUI({
    game_data <- getData()$country
    
    selectInput(
      inputId = "selected_nation_vars",
      label = "Select variables to show:",
      choices = sort(colnames(game_data)[colnames(game_data) != "Name"]),
      selected = current_selection_nat_var(),
      multiple = TRUE
    )
    
  })
  output$country_data <- DT::renderDataTable({
    if(length(input$selected_nation_vars) == 0){
      return(NULL)
    }
    game_data <- getData()$country
    
    ## Presents all nations if none specific has been selected
    if (is.null(input$nations)) {
      game_data[, c("Name", input$selected_nation_vars)]
    } else {
      game_data[(game_data$Name %in% input$nations), c("Name", input$selected_nation_vars)]
    }
    
    
  }, rownames = FALSE,
  class = 'compact cell-border stripe',
  options = list(orderClasses = TRUE, autoWidth = TRUE, pageLength = 20))
  #### Scoring for multiplayer sessions ####
  output$scoring_data <- DT::renderDataTable({
    game_data <- getData()
    
    country <- game_data$country[which(game_data$country$was_player == "yes"),
                                 c("Name",
                                   "tag",
                                   "score_place",
                                   "prestige",
                                   
                                   "capped_development",
                                   "raw_development",
                                   "treasury",
                                   "estimated_monthly_income",
                                   "base_tax",
                                   "loan_size", 
                                   "estimated_loan",
                                   "mercantilism",
                                   "inflation",
                                   
                                   "army_tradition",
                                   "army_professionalism",
                                   "max_historic_army_professionalism",
                                   "total_war_worth",
                                   "army_size",
                                   "manpower",
                                   "max_manpower",
                                   "sailors",
                                   "max_sailors",
                                   
                                   "num_uncontested_cores",
                                   "num_owned_home_cores",
                                   "num_of_core_ports",
                                   "num_of_total_ports",
                                   "num_of_cities",
                                   "num_of_cardinals",
                                   "num_of_allies",
                                   "num_of_royal_marriages",
                                   "num_of_subjects",
                                   
                                   "innovativeness",
                                   "meritocracy",
                                   "republican_tradition",
                                   "average_unrest",
                                   "average_autonomy",
                                   "great_power_score",
                                   "card_score",
                                   "is_elector")]
    
    human_provinces <- game_data$province[game_data$province$controller %in% country$tag,
                                          c("name",
                                            "owner", 
                                            "controller",
                                            "base_tax",
                                            "base_production",
                                            "base_manpower",
                                            "trade_goods",
                                            "trade_power",
                                            "improve_count",
                                            "center_of_trade",
                                            "center_of_religion")]
    
    contested_provinces_name <- c(
      # Konigsberg
      "Königsberg", "Królewiec", 
      # Prague
      "Praha", "Prague", 
      # Wien
      "Wien", "Vienna",
      # Regensburg
      "Regensburg", 
      # Paris
      "Paris", 
      # London
      "London", "Middlesex",
      # Madrid
      "Madrid", 
      # Rome + Vatican
      "Roma", "Rome", "Vaticana", "Vatican",
      # Copenhagen
      "Copenhagen", "København",
      # Constantinople
      "Constantinople", "Istanbul", "Konstantinoupolis")
    
    contested_provinces <- game_data$province[which(game_data$province$name %in% contested_provinces_name),]
    
    if(nrow(contested_provinces) > 0){
      for(i in contested_provinces$name){
        var_name <- paste("province", i, sep = "_")
        
        country <- country %>%
          mutate(!!var_name := case_when(contested_provinces$controller[contested_provinces$name == i] == tag ~ TRUE,
                                  TRUE ~ FALSE))
      }  
    }
    
    country
    
  }, rownames = FALSE,
  class = 'compact cell-border stripe',
  options = list(orderClasses = TRUE, autoWidth = TRUE, pageLength = 40))
  
  #######################################
  ##### Selection functions
  #######################################
  #### Selection of names for own groupings ####
  output$own_team_name1 <- renderUI({
    # What one side will be called
    textInput(
      inputId = "team_1",
      label = "What do you want to call the attacking alliance?",
      value = "Offense",
      width = NULL,
      placeholder = NULL
    )
  })
  
  output$own_team_name2 <- renderUI({
    # What the other side will be called
    textInput(
      inputId = "team_2",
      label = "What do you want to call the defending alliance?",
      value = "Defense",
      width = NULL,
      placeholder = NULL
    )
  })
  
  #### Selection of nations for own groupings ####
  ## Saves current selection of Offense
  current_selection_off <- reactiveVal(NULL)
  observeEvent(input$offense, {
    current_selection_off(input$offense)
  })
  
  output$attacking <- renderUI({
    game_data <- getData()$country
    # Marking nations fighting on the other side of the war
    selectInput(
      inputId = "offense",
      label = paste("Select nations on the", input$team_1 , "side:"),
      choices = sort(game_data$Name[!(game_data$Name %in% input$defense)]),
      selected = current_selection_off(),
      multiple = TRUE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  ## Saves current selection of Defense
  current_selection_def <- reactiveVal(NULL)
  observeEvent(input$defense, {
    current_selection_def(input$defense)
  })
  
  output$defending <- renderUI({
    game_data <- getData()$country
    
    # Marking nations fighting on the other side of the war
    selectInput(
      inputId = "defense",
      label = paste("Select nations on the", input$team_2 , "side:"),
      choices = sort(game_data$Name[!(game_data$Name %in% input$offense)]),
      selected = current_selection_def(),
      multiple = TRUE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  #### Aggregation of data according to selected groupings ####
  aggData_reactive <- reactive({
    if (is.null(getData())) {
      return(NULL)
    }
    
    game_data <- getData()$country
    # Inputs the teams and summarizes data
    game_data$Alliance <- rep(NA, nrow(game_data))
    game_data$Alliance[which(game_data$Name %in% input$offense)] <-
      input$team_1
    game_data$Alliance[which(game_data$Name %in% input$defense)] <-
      input$team_2
    
    ## Selects variables to be aggreagated and presented
    plot_vars <-
      c(
        "development",
        "raw_development",
        "treasury",
        "estimated_monthly_income",
        "total_war_worth",
        "manpower",
        "num_of_regulars"
      )
    
    
    game_data_table <-
      group_by(.data = game_data[!is.na(game_data$Alliance),], Alliance) %>%
      summarize_at(.vars = plot_vars,
                   .funs = sum,
                   na.rm = TRUE) %>%
      mutate_at(.vars = 2:8, .funs = prop.table)
    
    
    colnames(game_data_table) <- c(
      "Alliance",
      "Autonomy adj. \n Development",
      "Raw Development",
      "Treasury",
      "Monthly income",
      "Military Strength",
      "Manpower",
      "Army Size"
    )
    
    
    game_data_table <-
      gather(game_data_table, Variable, Values, 2:8)
    
    
    return(game_data_table)
  })
  
  
  #######################################
  ##### Visualizations
  #######################################
  #### Visualization of Time-Series data ####
  current_selection_ts_nat <- reactiveVal(NULL)
  observeEvent(input$ts_nations, {
    current_selection_ts_nat(input$ts_nations)
  })
  
  output$ts_nations <- renderUI({
    game_data <- getData()
    game_data <- game_data$country$Name[which(game_data$country$was_player == "yes")]
    
    selectInput(
      inputId = "ts_nations",
      label = HTML("Select nations to highlight in the table. <br/>
                    Maximum of 7 nations can be selected"),
      choices = sort(game_data),
      selected = sort(game_data)[1:4],
      multiple = TRUE,
      selectize = TRUE,
      width = NULL,
      size = NULL
        )
  })
  
  output$time_series <- renderPlotly({
    game_data <- getData()
    
    human_country <- game_data$country[which(game_data$country$was_player == "yes"), c("tag", "Name")]
    
    data <- lapply(seq_along(game_data$time_series), FUN = function(x, name, i){
      x_reduced <- x[[i]]
      
      x_reduced <- x_reduced[x_reduced$tag %in% human_country$tag,]
      
      x_reduced$var <- name[[i]]
      
      x_reduced
    }, name = names(game_data$time_series), x = game_data$time_series)
    
    data <- do.call(rbind, data)
    
    data <- data %>% 
      left_join(human_country, by = c("tag" = "tag"))
    
    data$groups <- factor(data$Name, levels = c(input$ts_nations, "Other"))
    data$groups[is.na(data$groups)] <- "Other"
    
    p <- ggplot(data[data$var == input$ts_variable,]) + 
      aes(x = year, y = value, group = Name, color = groups, size = groups) +
      geom_line() + theme_bw() + 
      theme(text = element_text(size = 16),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            plot.title = element_text(hjust = 0.5)) +
      scale_colour_manual(values = c(brewer.pal(n = max(3,length(input$ts_nations)),
                                              name = "Paired"), "grey"),
                          guide = guide_legend(title = "Country")) +
      scale_size_manual(values = c(rep(1, length(input$ts_nations)), 0.5),
                        guide = guide_legend(title = "Country")) + 
      labs(x = "Year", y = "Values", title = str_to_title(input$ts_variable))
    
    ggplotly(p, tooltip = c("Name", "y", "x"), height = 600)
    
  })  
  
  #### Pie-chart visualization of own groupings ####
  output$pie_chart_facet_comparison <- renderPlot({
    if (!is.null(input$offense) | !is.null(input$defense)) {
      # Creates plot over the distribution
      ggplot(data = aggData_reactive()) + aes(x = "", y = Values, fill = Alliance) +
        geom_bar(stat = "identity",
                 width = 1,
                 color = "black") +
        scale_fill_manual(values = c("#c0c0c0", "#E69F00")) + theme_bw() +
        scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0, 0), breaks = NULL) +
        coord_polar(theta = "y", start = 0) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(
            hjust = 0.5,
            size = 15,
            face = "bold"
          ),
          legend.title = element_text(face = "bold")
        ) +
        facet_grid(facets = ~ Variable) +
        theme(strip.background = element_blank(), strip.text = )
    }
  })
  
  #### End of server functions ####
}


# Run the application
shinyApp(ui = ui, server = server)


############################################################
### Deprecated functionality
############################################################

#
# ############ Continents
# # current_selection_con <- reactiveVal(NULL)
# # observeEvent(input$continent_choice, {
# #   current_selection_con(input$continent_choice)
# # })
#
# output$continents <-
#   output$continents2 <- output$continents3 <- renderUI({
#     NULL
#     # selectInput(inputId = "continent_choice",
#     #             label = "Select continent(s) where nations are to be selected:",
#     #             choices = c("Europe", "Asia", "Africa", "America"),
#     #             #selected = current_selection_con(),
#     #             multiple = TRUE,
#     #             selectize = TRUE,
#     #             width = NULL,
#     #             size = NULL
#     #             )
#   })


# ## Download buttons
# output$export_data_button <- renderUI({
#   if (input$export_choice == "Country") {
#     downloadButton(outputId = "export_country",
#                    label = "Download entire data table")
#
#   } else if (input$export_choice == "Province") {
#     downloadButton(outputId = "export_province",
#                    label = "Download entire data table")
#   } else {
#     return(NULL)
#   }
# })
#
# output$export_data_button_view <- renderUI({
#   if (input$export_choice == "Country") {
#     downloadButton(outputId = "export_country_view",
#                    label = "Download current selection")
#
#   } else if (input$export_choice == "Province") {
#     downloadButton(outputId = "export_province_view",
#                    label = "Download current selection")
#   } else {
#     return(NULL)
#   }
# })
#
# # Export functions
# output$export_country <- downloadHandler(
#   filename = function() {
#     meta_data <- getData()$meta
#
#     paste(meta_data$save_game,
#           meta_data$date,
#           "country_data.csv",
#           sep = "_")
#   },
#   content = function(file) {
#     game_data <- getData()$country
#     # Subsets continents
#     if (!is.null(input$continent_choice)) {
#       continents <-
#         as.numeric(factor(
#           input$continent_choice,
#           levels = c("Europe", "Asia", "Africa", "America")
#         ))
#
#       game_data <-
#         game_data[game_data$continent %in% continents,]
#     }
#
#     write.csv(game_data, file, row.names = FALSE)
#   }
# )
#
# output$export_province <- downloadHandler(
#   filename = function() {
#     meta_data <- getData()$meta
#
#     paste(meta_data$save_game,
#           meta_data$date,
#           "province_data.csv",
#           sep = "_")
#   },
#   content = function(file) {
#     write.csv(getData()$province, file, row.names = FALSE)
#   }
# )
#
# output$export_country_view <- downloadHandler(
#   filename = "country_data_view.csv",
#   content = function(file) {
#     game_data <- getData()$country
#     # Subsets continents
#     if (!is.null(input$continent_choice)) {
#       continents <-
#         as.numeric(factor(
#           input$continent_choice,
#           levels = c("Europe", "Asia", "Africa", "America")
#         ))
#
#       game_data <-
#         game_data[game_data$continent %in% continents,]
#     }
#
#     write.csv(game_data[, c("Name", input$selected_nation_vars)], file, row.names = FALSE)
#   }
# )
#
# output$export_province_view <- downloadHandler(
#   filename = "province_data_view.csv",
#   content = function(file) {
#     write.csv(getData()$province[, c("name", input$selected_province_vars)], file, row.names = FALSE)
#   }
# )



## Subsetting continents in game_data scraping
# # Subsets continents
# if (!is.null(input$continent_choice)) {
#   continents <-
#     as.numeric(factor(
#       input$continent_choice,
#       levels = c("Europe", "Asia", "Africa", "America")
#     ))
#
#   game_data <-
#     game_data[game_data$continent %in% continents, ]
# }

## Religion and HRE comparisons
# navbarMenu("Categories",
#   tabPanel("Religion comparison",
#            fluidRow(
#              column(width = 2,
#                     wellPanel(
#                       withSpinner(uiOutput("continents")),
#                       checkboxGroupInput(inputId = "religion_choice", label = "Select religons(s) to be compared:",
#                                          choices = sort(c("Catholic", "Protestant", "Reformed", "Orthodox",
#                                                           "Sunni", "Shiite", "Coptic",
#                                                           "Nahuatl", "Totemism", "Inti", "Shamanism", "Buddhism",
#                                                           "Hinduism", "Mahayana", "Shinto", "Tengri" = "tengri_pagan_reformed",
#                                                           "Confucianism", "Animism", "Vajrayana",
#                                                           "Mesoamerican" = "mesoamerican_religion"))
#                       )
#                     )
#              ),
#              column(width = 10,
#                     withSpinner(plotOutput("circle_facet_religion"))
#                     )
#              )
#   ),
#   tabPanel("HRE comparison",
#            fluidRow(
#              column(width = 2,
#                     wellPanel(
#                       withSpinner(uiOutput("continents2"))
#                     )
#              ),
#              column(width = 10,
#                     withSpinner(plotOutput("circle_facet_HRE"))
#              )
#            )
#   )
# ),

# # Religion
# output$circle_facet_religion <- renderPlot({
#   if (is.null(getData()) | is.null(input$religion_choice)) {
#     return(NULL)
#   }
#   game_data <- getData()$country
#
#   # # Subsets continents
#   # if(!is.null(input$continent_choice)){
#   #   continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))
#   #
#   #   game_data <- game_data[game_data$continent %in% continents,]
#   # }
#
#   plot_vars <-
#     c(
#       "development",
#       "raw_development",
#       "treasury",
#       "estimated_monthly_income",
#       "total_war_worth",
#       "manpower",
#       "num_of_regulars"
#     )
#
#   game_data_table <-
#     group_by(.data = game_data[game_data$religion %in% str_to_lower(input$religion_choice), ], religion) %>%
#     summarize_at(.vars = plot_vars,
#                  .funs = sum,
#                  na.rm = TRUE) %>%
#     mutate_at(.vars = 2:8, .funs = prop.table)
#
#
#   colnames(game_data_table) <- c(
#     "Alliance",
#     "Autonomy adj. \n Development",
#     "Raw Development",
#     "Treasury",
#     "Monthly income",
#     "Military Strength",
#     "Manpower",
#     "Army Size"
#   )
#
#
#   game_data_table <-
#     gather(game_data_table, Variable, Values, 2:8)
#
#   # Creates plot over the distribution
#   ggplot(data = game_data_table) + aes(x = "", y = Values, fill = Religion) +
#     geom_bar(stat = "identity",
#              width = 1,
#              color = "black") +
#     scale_fill_manual(
#       values = c("#c0c0c0", "#E69F00", "#e69138", "#000000", "#a4c2f4")[1:length(input$religion_choice)],
#       labels = input$religion_choice
#     ) +
#     theme_bw() +
#     scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0, 0), breaks = NULL) +
#     coord_polar(theta = "y", start = 0) +
#     labs(x = NULL, y = NULL, title = NULL) +
#     theme(
#       panel.grid = element_blank(),
#       panel.border = element_blank(),
#       plot.title = element_text(
#         hjust = 0.5,
#         size = 15,
#         face = "bold"
#       ),
#       legend.title = element_text(face = "bold")
#     ) +
#     facet_grid(facets = ~ Variable) +
#     theme(strip.background = element_blank(), strip.text =)
# })
#
# # HRE
# output$circle_facet_HRE <- renderPlot({
#   if (is.null(getData())) {
#     return(NULL)
#   }
#
#   game_data <- getData()$country
#
#   # # Subsets continents
#   # if(!is.null(input$continent_choice)){
#   #   continents <- as.numeric(factor(input$continent_choice, levels = c("Europe", "Asia", "Africa", "America")))
#   #
#   #   game_data <- game_data[game_data$continent %in% continents,]
#   # }
#
#   plot_vars <-
#     c(
#       "development",
#       "raw_development",
#       "treasury",
#       "estimated_monthly_income",
#       "total_war_worth",
#       "manpower",
#       "num_of_regulars"
#     )
#
#
#   game_data_table <-
#     group_by(.data = game_data[!is.na(game_data$hre), ], hre) %>%
#     summarize_at(.vars = plot_vars,
#                  .funs = sum,
#                  na.rm = TRUE) %>%
#     mutate_at(.vars = 2:8, .funs = prop.table)
#
#
#   colnames(game_data_table) <- c(
#     "Alliance",
#     "Autonomy adj. \n Development",
#     "Raw Development",
#     "Treasury",
#     "Monthly income",
#     "Military Strength",
#     "Manpower",
#     "Army Size"
#   )
#
#
#   game_data_table <-
#     gather(game_data_table, Variable, Values, 2:8)
#
#   # Creates plot over the distribution
#   ggplot(data = game_data_table) + aes(x = "", y = Values, fill = HRE) +
#     geom_bar(stat = "identity",
#              width = 1,
#              color = "black") +
#     scale_fill_manual(values = c("#c0c0c0", "#E69F00"),
#                       labels = c("non-HRE", "HRE")) +
#     theme_bw() +
#     scale_x_discrete(breaks = NULL) + scale_y_continuous(expand = c(0, 0), breaks = NULL) +
#     coord_polar(theta = "y", start = 0) +
#     labs(x = NULL, y = NULL, title = NULL) +
#     theme(
#       panel.grid = element_blank(),
#       panel.border = element_blank(),
#       plot.title = element_text(
#         hjust = 0.5,
#         size = 15,
#         face = "bold"
#       ),
#       legend.title = element_text(face = "bold")
#     ) +
#     facet_grid(facets = ~ Variable) +
#     theme(strip.background = element_blank(), strip.text =)
# })
