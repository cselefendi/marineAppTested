# setup ----

# libraries ====

library(shiny)
library(leaflet)
library(shiny.semantic)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)

options(semantic.themes = TRUE)

# loading the preprocessed data file ====
load("data/ships.Rdata")

# loading module for dropdown selector ====
source('R/selector_module.R')

# variable config ====

please_select_type <- "Please select a vessel type"
please_select_name_1 <- "Please select a vessel type first"
please_select_name_2 <- "Please select a vessel name"
ship_icon <- "ship_icon.png"

shipIcons <- iconList(
  from = makeIcon(ship_icon,
                  ship_icon,
                  18, 18, 
                  className = "from_icon"),
  to = makeIcon(ship_icon,
                ship_icon,
                24, 24,
                className = "to_icon")
)


####### UI side for marineApp -----

ui <- semanticPage(
  
  # app title ====
  title = "Marine app",
  
  # external css ====
  tags$head(
    shinyjs::useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "marineApp.css")
  ),
  
  # card for title ====
  card(
    class = "box_head",
    style = "width: 100%; background-color: darkslateblue; color: white; padding: 15px;",
    h1(class = "header", "Marine app"),
    div(
      icon("lightbulb", style = "color: white;"),
      "Find the largest distance for a ship between two observations.")
  ),
  
  # sidebar-like card for selectors ====
  card(
    class = "box_selection",
    style = "height: auto; width: 30%; display: inline-block; vertical-align: top; margin: 0px; background-color: cornsilk;",
    div(class = "content",
        div(class = "select", 
            
            br(),
            
            # type dropdown selector from module
            selectorUI('typeselector',
                       explainer = "Vessel type:",
                       all_choices = NULL,
                       selected_value = NULL,
                       please_select = please_select_type),
            
            br(),
            
            # additional info for user
            icon('info', style = "color: dodgerblue;"),
            textOutput("num_of_vessels"),
            
            shinyjs::hidden(
              actionLink('feeling_lucky', label = "I'm feeling lucky")
            ),
            
            br(),br(),
            
            # name dropdown selector from module
            selectorUI('nameselector',
                       explainer = "Vessel name:",
                       all_choices = NULL,
                       selected_value = NULL,
                       please_select = please_select_name_1),
            
            br(),
            # displaying largest distance info (also on map)
            uiOutput("largest_distance"),
            br(),br()
            
        ),
        
        # signature
        div(style = "position: absolute; bottom: 0px; right: 0px; margin: 15px;",
            HTML('Demonstration project by Endre Szolnoki for Appsilon, May 2021'))
    )
  ),
  
  # card for map ====
  card(
    class = "box_map",
    style = "height: auto; width: 69%; display: inline-block; vertical-align: top; margin: 0px;",
    div(class = "content", 
        h3(class = "header", 
           "Largest distance on map"), 
        div(HTML("Interpretation: When traveling the largest distance between two observations the ship traveled from the <span style='color: red;'>Red</span> to the <span style='color: green;'>Green</span> marker. The route shown is approximate.")), 
        div(class = "the_map",
            withSpinner(leafletOutput("map", height = 500), 8)
        )
    )
  )
)

# shinyServer ----

server <- function(input, output, session) {
  
  # selection - helper functions ====
  
  base_data <- function(){
    return(ship_markers)
  }
  
  potential_types <- base_data() %>%
    distinct(ship_type) %>%
    pull(ship_type)
  
  potential_names <- reactive({
    
    selected_type <- v_typeselector$selectInputElem()
    if(is.null(selected_type)) {
      return(NULL)
    }else if(selected_type %in% c('', please_select_type)){
      return(NULL)
    }else{
      base_data() %>%
        filter(ship_type == selected_type) %>%
        distinct(ship_name) %>%
        pull(ship_name) %>%
        return()
    }
    
  })
  
  # displaying information - helper functions ====
  
  point_data <- reactive({
    
    temp_ship_name <- v_nameselector$selectInputElem()
    if(is.null(temp_ship_name)){
      return(NULL)
    }else{
      base_data() %>%
        filter(ship_name == temp_ship_name) %>%
        return()
    }
    
  })
  
  distance_pretty <- function(x){
    format(round(x, 0),
           big.mark = " ")
  }
  
  # dropdown modules - initialization and values ====
  
  v_typeselector <- selectorServer('typeselector',
                                   temp_label = "Vessel type:", 
                                   please_select = please_select_type, 
                                   all_choices = potential_types, 
                                   selected_value = NULL)
  
  v_nameselector <- selectorServer('nameselector',
                                   temp_label = "Vessel name:", 
                                   please_select = please_select_name_1, 
                                   all_choices = NULL, 
                                   selected_value = NULL)
  
  # re-rendering if type selection is changed ====
  
  observeEvent(v_typeselector$selectInputElem(), {
    
    selected_type <- v_typeselector$selectInputElem()
    if(selected_type %in% c('', please_select_type)){
      please_select <- please_select_name_1
      
      shinyjs::hide('feeling_lucky')
      
      # Displaying additional information for user's comfort
      output$num_of_vessels <- renderText({
        sprintf('There are %i vessel types you can choose from.',
                length(potential_types))
      })
      
    }else{
      please_select <- please_select_name_2
      
      shinyjs::show('feeling_lucky')
      
      # Displaying additional information for user's comfort
      output$num_of_vessels <- renderText({
        sprintf('%i vessels found with type: %s. ',
                length(potential_names()),
                selected_type)
      })
    }
    
    # update name possibilities
    v_nameselector <- selectorServer('nameselector',
                                     temp_label = "Vessel name:", 
                                     please_select = please_select, 
                                     all_choices = potential_names(), 
                                     selected_value = NULL)
    
  })
  
  # Rendering leaflet map based on selection ====
  
  output$map <- renderLeaflet({
    
    if(v_typeselector$selectInputElem() %in% c('', please_select_type) |
       v_nameselector$selectInputElem() %in% c('', please_select_name_1, please_select_name_2)){
      map <- leaflet() %>%
        addTiles()
    }else{
      
      map <- leaflet(data = point_data()) %>%
        addTiles() %>%
        addPolylines(lng = ~lon, lat = ~lat,
                     label = ~sprintf('Distance: %s meters', as.character(distance_pretty(dist_m))),
                     popup = ~sprintf('Distance: %s meters', as.character(distance_pretty(dist_m)))) %>%
        addMarkers(~lon, ~lat,
                   popup = ~popup_text,
                   label = ~label_text,
                   icon = ~shipIcons[fromto])
    }
    
    return(map)
  })
  
  # Displaying additional information for user's comfort ====
  
  output$largest_distance <- renderUI({
    
    df_point_data <- point_data()
    if(nrow(df_point_data)==0){
      show_icon <- "meteor"
      show_text <- "Largest distance will be automatically displayed here"
    }else{
      show_icon <- "bullhorn"
      show_text <- sprintf('Largest distance: %s meters', 
                           distance_pretty(unique(df_point_data$dist_m)))
    }
    
    return(HTML(sprintf('<i class="%s icon"></i>%s',
                        show_icon, show_text)))
  })
  
  # Little something for the user and the tester ====
  
  observeEvent(input$feeling_lucky, {
    
    if(!is.null(potential_names())){
      updateSelectInput(session, "nameselector-selectInputElem",
                        label = "Vessel name:",
                        choices = c(please_select_name_2, potential_names()), 
                        selected = sample(potential_names(), 1))
    }
  })
  
}


shinyApp(ui, server)


