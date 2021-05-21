####### MODULE for dropdown select input, that can be updated and reset ----

# libraries

library(shiny)

# UI part of module ----

selectorUI <- function(id,
                       explainer,
                       all_choices,
                       selected_value,
                       please_select){
  
  ns <- NS(id)
  tagList(
    
    # label-like explainer
    HTML(sprintf("<b>%s</b>", explainer)),
    
    # actual input select dropdown
    dropdown_input(ns("selectInputElem"),
                   all_choices, 
                   value = selected_value,
                   default_text = please_select)
  )
}

# Server part of module ----

selectorServer <- function(id, temp_label, please_select, all_choices, selected_value) {
  moduleServer(
    id,
    # module function
    function(input, output, session) {
      
      # filtering out invalid choices
      invalid_selection <- is.null(selected_value) | !(selected_value %in% all_choices)
      temp_selected_value <- ifelse(invalid_selection,
                                    please_select,
                                    selected_value)
      
      # updating selector's choices and selected value
      updateSelectInput(session, "selectInputElem",
                        label = temp_label,
                        choices = c(please_select, all_choices), 
                        selected = temp_selected_value)
      
      # returning corrected selection value
      return(list(selectInputElem = reactive({ 
        input$selectInputElem
      })))
      
    }
  )
}