#-------------------------------------------------------------------------------
# Display results for select scenario in a table
#-------------------------------------------------------------------------------
output$t_result <- renderDataTable({
  if (nrow(g_r$s) == 0)
    return(NULL)
  
  r <- g_r$s
  if (nrow(r) != 0) {
    r <- round(r,3)
    row_names <- rownames(r)
    r <- cbind(row_names, r)
    colnames(r)[1] <- "Scenario"
    return(r)
  }
}, options = list(searching=0, ordering=0, processing=0, 
                  lengthMenu = c(10, 20, 30), pageLength = 10, scrollX = TRUE))

#-------------------------------------------------------------------------------
# Display results for select scenario in a table
#-------------------------------------------------------------------------------
output$t_scenario <- renderDataTable({
  if (input$s_scenario == 'None' || (as.numeric(input$s_scenario) > length(g_r$r)))
    return(NULL)
  
  r_a <- g_r$r[[as.numeric(input$s_scenario)]] 
  if (nrow(r_a) != 0) {
    r_a <- round(r_a, 3)
    row_names <- seq(0, nrow(r_a) - 1)
    r <- cbind(row_names, r_a)
    colnames(r)[1] <- "ITERATION"
    return(r)
  }
}, options = list(searching=0, ordering=0, processing=0,
                  lengthMenu = c(10, 20, 30), pageLength = 10, scrollX = TRUE))

#-------------------------------------------------------------------------------
# Display a list of the concepts names and description in a table
#-------------------------------------------------------------------------------
output$t_concepts <- renderDataTable({
  if (input$btn_upload == 0 || nrow(g_m$n) == 0)
    return(NULL)
  
  r <- cbind(rownames(g_m$n), g_m$n)
  colnames(r)[1] <- "CONCEPT"
  return(r)
}, options = list(searching=0, ordering=0, processing=0, 
                  lengthMenu = c(10, 20, 30), pageLength = 10, scrollX = TRUE))

#-------------------------------------------------------------------------------
# Display the queue of the selected attributes in a table
#-------------------------------------------------------------------------------
output$t_queue <- renderDataTable({
  if(nrow(g_t$q) == 0)
    return(NULL)
  
  return(g_t$q)
}, options = list(searching=0, ordering=0, processing=0, 
                  paging=0, info=0, autoWidth=0, scrollX = TRUE))
