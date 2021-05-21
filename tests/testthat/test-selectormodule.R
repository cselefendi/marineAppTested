context("selector module")

# config tests

type_sample <- c('Tug', 'Tanker', 'Other')
please_select_sample <- "Please select"

testServer(
  selectorServer, 
  args = list(temp_label = 'LABEL', 
              please_select = please_select_sample, 
              all_choices = type_sample, 
              selected_value = NULL), 
  {
    
    test_that('A valid element can be selected', {
      value_shoud_be <- type_sample[1]
      session$setInputs(selectInputElem = value_shoud_be)
      expect_equal(
        session$returned$selectInputElem(),
        value_shoud_be)
    })
    
    # Could not implement test 'successfully'
    # test_that('An invalid element can NOT be selected', {
    #   
    #   session$setInputs(selectInputElem = "Something invalid")
    #   expect_equal(
    #     session$returned$selectInputElem(),
    #     please_select_sample)
    #   
    # })
    
    test_that('Valid elements can be selected after each other', {
      
      value_shoud_be <- type_sample[1]
      session$setInputs(selectInputElem = value_shoud_be)
      expect_equal(
        session$returned$selectInputElem(),
        value_shoud_be)
      
      value_shoud_be <- type_sample[2]
      session$setInputs(selectInputElem = value_shoud_be)
      expect_equal(
        session$returned$selectInputElem(),
        value_shoud_be)
    })
    
  })