context("app")

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
    
    necessary_columns <- c('ship_type', 'ship_name', 'dist_m', 'lon', 'lat', 'popup_text', 'label_text', 'fromto')
    column_names <- colnames(base_data())
    for(nc in necessary_columns){
      expect_true(nc %in% column_names)
    }
    
  })
  
  test_that('Each vessel type can be selected', {
    
    types_can_be_selected <- c('Cargo', 'Fishing', 'Tug', 'Unspecified', 'Navigation', 'Tanker', 'Passenger', 'Pleasure', 'High Special')
    for(tcbs in types_can_be_selected){
      session$setInputs(`typeselector-selectInputElem` = tcbs)
      expect_equal(v_typeselector$selectInputElem(), tcbs)
    }
    
  })
  
  test_that('At least one name is listed for each vessel type', {
    
    types_can_be_selected <- c('Cargo', 'Fishing', 'Tug', 'Unspecified', 'Navigation', 'Tanker', 'Passenger', 'Pleasure', 'High Special')
    for(tcbs in types_can_be_selected){
      session$setInputs(`typeselector-selectInputElem` = tcbs)
      expect_true(length(potential_names()) > 0)
    }
    
  })
  
})
