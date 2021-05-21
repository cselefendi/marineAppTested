context("app")

types_can_be_selected <- c('Cargo', 'Fishing', 'Tug', 'Unspecified', 'Navigation', 'Tanker', 'Passenger', 'Pleasure', 'High Special')
necessary_columns <- c('ship_type', 'ship_name', 'dist_m', 'lon', 'lat', 'popup_text', 'label_text', 'fromto')
please_select_name_2 <- "Please select a vessel name"

testServer(expr = {
  
  test_that('Data frame ship_markers is loaded', {
    
    expect_true(!is.null(base_data()))
    
  })
  
  test_that('Data frame ship_markers is indeed data frame', {
    
    expect_true(is.data.frame(base_data()))
    
  })
  
  test_that('Data frame ship_markers has more than 0 rows', {
    
    rows_in_data_file <- nrow(base_data())
    expect_true(rows_in_data_file > 0)
    
  })
  
  test_that('Data frame ship_markers has the necessary columns', {
    
    column_names <- colnames(base_data())
    for(nc in necessary_columns){
      expect_true(nc %in% column_names)
    }
    
  })
  
  test_that('Each vessel type can be selected', {
    
    for(tcbs in types_can_be_selected){
      session$setInputs(`typeselector-selectInputElem` = tcbs)
      expect_equal(v_typeselector$selectInputElem(), tcbs)
    }
    
  })
  
  test_that('At least one name is listed for each vessel type', {
    
    for(tcbs in types_can_be_selected){
      session$setInputs(`typeselector-selectInputElem` = tcbs)
      expect_true(length(potential_names()) > 0)
    }
    
  })
  
  test_that('Map displays vessel name and numeric distance when vessel name is selected', {
    
    for(tcbs in types_can_be_selected){
      session$setInputs(`typeselector-selectInputElem` = tcbs)
      sample_name <- sample(potential_names(), 1)
      session$setInputs(`nameselector-selectInputElem` = sample_name)
      
      map_output <- jsonlite::fromJSON(output$map)
      
      end_position_exists <- tryCatch(is.character(map_output$x$calls$args[[3]][[7]][1]),
                                      error = function(e) FALSE)
      start_position_exists <- tryCatch(is.character(map_output$x$calls$args[[3]][[7]][1]),
                                        error = function(e) FALSE)
      
      expect_true(end_position_exists)
      expect_true(start_position_exists)
      
      end_position <- map_output$x$calls$args[[3]][[7]][1]
      start_position <- map_output$x$calls$args[[3]][[7]][2]
      
      expect_true(str_detect(end_position, fixed(sample_name)))
      
      expect_true(str_detect(start_position, fixed(sample_name)))
      
      expect_true(str_detect(end_position, '\\([0-9]+ meters\\)'))
      
      expect_true(str_detect(start_position, '\\([0-9]+ meters\\)'))
      
    }
    
  })
  
  test_that('Map does not display info when vessel name is not selected', {
    
    for(tcbs in types_can_be_selected){
      session$setInputs(`typeselector-selectInputElem` = tcbs)
      session$setInputs(`nameselector-selectInputElem` = please_select_name_2)
      
      map_output <- jsonlite::fromJSON(output$map)
      
      end_position_exists <- tryCatch(is.character(map_output$x$calls$args[[3]][[7]][1]),
               error = function(e) FALSE)
      start_position_exists <- tryCatch(is.character(map_output$x$calls$args[[3]][[7]][1]),
                                      error = function(e) FALSE)
      
      expect_true(end_position_exists == F)
      expect_true(start_position_exists == F)
      
    }
    
  })
  
})
