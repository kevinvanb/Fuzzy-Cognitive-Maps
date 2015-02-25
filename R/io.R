#-------------------------------------------------------------------------------
# Create button to clear queue
#-------------------------------------------------------------------------------
output$q_clear <- renderUI({
  if (nrow(g_t$q) != 0)
    actionButton('btn_qclear', 'Clear Queue')
})

#-------------------------------------------------------------------------------
# Create button to remove last scenario from queue
#-------------------------------------------------------------------------------
output$q_remove <- renderUI({
  if (nrow(g_t$q) != 0)
    actionButton('btn_qremove', 'Remove Last')
})

#-------------------------------------------------------------------------------
# Create download button for model
#-------------------------------------------------------------------------------
output$model_down <- renderUI({
  if (nrow(g_m$m) == 0)
    return(NULL)
  
  downloadButton('btn_mdown', 'Download Model')
})

#-------------------------------------------------------------------------------
# Saves model to csv file when button pressed
#-------------------------------------------------------------------------------
output$btn_mdown <- downloadHandler(
  filename = function() {
    paste0('fcm_model_', Sys.Date(), '.csv')
  },
  content = function(file) {
    
    m_n <- g_m$n
    if (ncol(g_m$n) > 2)
      m_n <- g_m$n[,2:3]
    
    print(g_m$n)
    
    mat <- cbind(m_n, g_m$m)
    s_state <- cbind(g_ss$n, g_ss$m)
    s_func <- cbind(g_sf$n, g_sf$m)
    row_mat <- c(g_sep[1], rep("", (ncol(g_m$m) + ncol(m_n) - 1)))
    print(row_mat)
    row_ss <- c(g_sep[3], rep("", (ncol(g_m$m) + ncol(m_n) - 1)))
    row_sf <- c(g_sep[4], rep("", (ncol(g_m$m) + ncol(m_n) - 1)))
    model <- rbind(row_mat, mat, row_mat, row_ss, s_state, row_ss, 
                   row_sf, s_func, row_sf)
     
                 
    write.csv(model, file, row.names = FALSE)
  },
  contentType = "text/csv"
) 

#-------------------------------------------------------------------------------
# Create download button for scenario results
#-------------------------------------------------------------------------------
output$s_down <- renderUI({
  if (input$s_scenario == 'None')
    return(NULL)
  
  if (!(as.numeric(input$s_scenario) > length(g_r$r)))
    if (nrow(g_r$r[[as.numeric(input$s_scenario)]]) != 0)
      downloadButton('btn_sdown', 'Export Table')
})

#-------------------------------------------------------------------------------
# Saves scenario results to csv file when button pressed
#-------------------------------------------------------------------------------
output$btn_sdown <- downloadHandler(
  filename = function() {
    paste0('fcm_c_', g_r$q[as.numeric(input$s_scenario),"squash"],"_", Sys.Date(), '.csv')
  },
  content = function(file) {
    scenario <- as.numeric(input$s_scenario)
    if (scenario <= length(g_r$r)) {
      r <- g_r$r[[scenario]]
      r_s <- g_r$q[scenario,]
      fill <- rep("", max(0, ncol(r) - length(r_s) + 1))
      row_s <-  data.frame(c(r_s, fill))
      row_scols <-  t(data.frame(c(colnames(r_s), fill)))
      colnames(row_s) <- c("Interation", colnames(r))
      colnames(row_scols) <- c("Interation", colnames(r))
      r <- cbind(Interation=seq(1,nrow(r)), r)
      r <- rbind(r, row_scols, row_s)
      write.csv(r, file, row.names = FALSE)
    }
  },
  contentType = "text/csv"
)

#-------------------------------------------------------------------------------
# Shows download button for all results
#-------------------------------------------------------------------------------
output$r_down <- renderUI({
  if (nrow(g_r$s) == 0)
    return(NULL)
  
  downloadButton('btn_rdown', 'Export Table')
})

#-------------------------------------------------------------------------------
# Check the analysis button when the queue is not empty
#-------------------------------------------------------------------------------
output$analysis <- renderUI({
  if (nrow(g_t$q) != 0)
    actionButton("btn_analysis","Analysis")
})

#-------------------------------------------------------------------------------
# Saves all results to csv file when button pressed
#-------------------------------------------------------------------------------
output$btn_rdown <- downloadHandler(
  filename = function() {
    paste0('fcm_r_', Sys.Date(), '.csv')
  },
  content = function(file) {
    scenario <- as.numeric(input$s_scenario)
    write.csv(cbind(g_r$q, g_r$s), file, row.names = FALSE)
  },
  contentType = "text/csv"
) 
#-------------------------------------------------------------------------------
# Updates the iteration sliderInput for FCM loop
#-------------------------------------------------------------------------------
output$sl_iter <- renderUI({
  if (input$s_scenario == 'None' || (as.numeric(input$s_scenario) > length(g_r$r)))
    return(sliderInput('iteration', 'Display Iteration:', min = 0, 
                       max = 1, value = 0, step = 1))
  
  r_a <- g_r$r[[as.numeric(input$s_scenario)]] 
  if (nrow(r_a) != 0)
    sliderInput('iteration', 'Display Iteration:', min = 0, 
                max = (nrow(r_a) - 1), value = 0, step = 1, 
                animate = animationOptions(interval = input$n_fcmdelay))
  else
    sliderInput('iteration', 'Display Iteration:', min = 0, 
                max = 1, value = 0, step = 1)
})
