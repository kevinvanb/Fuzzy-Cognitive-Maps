#------------------------------------------------------------------------------
#
# server.R
# Contain the script that will run on the server when the user interacts 
# with the webpage interface. 
#
#-----------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  source("R/FCM.R", local=TRUE)$value
  source("R/plots.R", local=TRUE)$value
  source("R/tables.R", local=TRUE)$value
  source("R/io.R", local=TRUE)$value
  source("R/functions.R", local=TRUE)$value
  
  #-------------------------------------------------------------------------------
  # When upload button is clicked data is uploaded into global dataframes g_m and g_ss
  #-------------------------------------------------------------------------------
  observe({
    if (input$btn_upload != 0){
      # Load data into global dataset dataframe
      isolate({
        initialize()
        mm_values <- c(input$n_hpos, input$n_mpos, input$n_lpos, input$n_hneg,
                       input$n_mneg, input$n_lneg)
        res <- data.load(input$s_flocation, input$s_ftype, input$data_file, input$gs_url, 
                         input$dropbox_url, mm_values)
        g_m <<- res$mat
        g_io <<- res$io
        g_ss <<- res$ss
        g_sf <<- res$sf
      # If a error occurred during uploading the file, display the error
      if (g_error != '')
        session$sendCustomMessage(type = "show_error", g_error)
      
      # Clear the error after it has been displayed
      g_error <<- ''
      
      # If matrix was loaded from file, update the current tab to the FCM setup tab
      if (nrow(g_m$m) > 0)
        updateTabsetPanel(session, "setup_tabset", selected = "fcms_tab")
      
      # Update all the select inputs to show concept names and start state names
      if (nrow(g_ss$n) == 0)
        sstate_n <- c("All One", "All Zero", "Specify")
      else
        sstate_n <- c(as.vector(apply(g_ss$n, 1, function(x) paste(x, collapse="-"))), "All Above", "Permutations")
      if (input$s_ftype == "mmp")
        sfixed_n <- as.vector(apply(g_m$n, 1, function(x) paste(x[c(2,3)], collapse="-")))
      else
        sfixed_n <- as.vector(apply(g_m$n, 1, function(x) paste(x, collapse="-")))
      ssquash_n <- c(g_squash, as.vector(apply(g_sf$n, 1, function(x) paste(x, collapse="-"))))
      updateSelectInput(session, 's_state', 'Start State:', c(sstate_n, sstate_n[1]))
      updateSelectInput(session, 's_fixed', 'Clamp Concept (Selected = Fixed):', sfixed_n)
      # updateSelectInput(session, 's_inconcepts', 'Input concepts:', sfixed_n)
      # updateSelectInput(session, 's_sconcepts', 'Start Concepts (Selected = 1):', sfixed_n)
      updateSelectInput(session, 's_iterplot', 'Select Concepts to Plot:', sfixed_n, sfixed_n[1])
      updateSelectInput(session, 's_squash', 'Squashing Function:', ssquash_n, ssquash_n[1])
      })
    }
  }) # observe upload
  
  #-------------------------------------------------------------------------------
  # Add entry to queue for analysis
  #-------------------------------------------------------------------------------
  observe({
    if (input$btn_queue != 0) {
      isolate({
        # Get selected start state name
        s_state <- input$s_state
        # Get selected squash function
        s_squash <- input$s_squash
        
        # Determine if squashing function is from user's uploaded data
        if (!(s_squash %in% g_squash)) {
          # Since the squashing function select input include both the concept
          # name and description, we need to match it with the name and description from the 
          # uploaded data
          s_q <- as.vector(apply(g_sf$n, 1, function(x) paste(x, collapse="-")))
          s_squash <- rownames(g_sf$n)[s_q %in% s_squash]
        }
        # Since the start state and squashing function select inputs include both the concept
        # name and description, we need to match it with the name and description from the 
        # uploaded data
        s_s <- as.vector(apply(g_ss$n, 1, function(x) paste(x, collapse="-")))
        
        if (input$s_ftype == "mmp")
          s_f <- as.vector(apply(g_m$n, 1, function(x) paste(x[c(2,3)], collapse="-")))
        else 
          s_f <- as.vector(apply(g_m$n, 1, function(x) paste(x, collapse="-")))
        
        
        switch(s_state,
               "All Above" = {
                 s_state <- rownames(g_ss$n)
                 s_names <- seq(nrow(g_t$q) + 1, nrow(g_t$q) + nrow(g_ss$n))
               },
               "Permutations" = {
                 s_state <- "PER"
                 s_names <- nrow(g_t$q) + 1
               },
               "All One" = {
                 s_state <- "One"
                 s_names <- nrow(g_t$q) + 1
               },
               "All Zero" = {
                 s_state <- "Zero"
                 s_names <- nrow(g_t$q) + 1
               },
               {
                 s_state <- rownames(g_ss$n)[s_s %in% input$s_state]
                 s_names <- nrow(g_t$q) + 1
               })
        
        if (s_squash == "All Above") {
          if (length(s_state) > 1) {
              s_squash <- sort(rep(g_squash[-length(g_squash)], nrow(g_ss$n)))
              s_names <- seq(nrow(g_t$q) + 1, nrow(g_t$q) + length(s_squash))
          }
          else {
              s_squash <- g_squash[-length(g_squash)]
              s_names <- seq(nrow(g_t$q) + 1, nrow(g_t$q) + length(g_squash) - 1)
          }
        }
        
        s_fixed <- rownames(g_m$n)[s_f %in% input$s_fixed]
        
        r <- data.frame(s_names, s_state, paste(s_fixed, collapse=","), 
                        s_squash, input$n_eps, input$n_miter)
        colnames(r) <- c("scenario", "s_state", "fixed", "squash", "eps", "iter")
        queue <- g_t$q
        if (nrow(queue) == 0) {
          queue <- r
          colnames(queue) <- c("scenario", "s_state", "fixed", "squash", "eps", "iter")
        }
        else
          queue <- unique(rbind(queue, r))
        g_t$q <<- queue
      })
    }
  }) # observe btn_queue
  
  #-------------------------------------------------------------------------------
  # When analysis button is clicked fcm result in calculated
  #-------------------------------------------------------------------------------
  observe({
    if (!is.null(input$btn_analysis))
      if (input$btn_analysis != 0){
        isolate({
          # Save queue to a temporary queue. This is to remember what analysis was done when the 
          # analysis button was pressed. It is possible for the user to remove a scenario from the
          # queue after the analysism which will be lost if not saved.
          g_r$q <<- g_t$q
          # Update select inputs to include list of scenarios
          updateSelectInput(session, 's_scenario', 'Scenario:', rownames(g_r$q), rownames(g_r$q)[1])
          updateSelectInput(session, 's_scenplot', 'Select Scenarios to Plot:', rownames(g_r$q), rownames(g_r$q))
          # If permutation is part of the queue, run per.analysis() function
#           if ("PER" %in% g_r$q[, 2]) {
#             #             g_r$r <<- perm.analysis()
#             g_r$s <<- perm.analysis()
#           }
#           # else run the normal analysis
#           else {
            g_r$r <<- data.analysis()
            g_r$s <<- r.summary()
#           }
        })
        # If a error occurred during uploading the file, display the error
        if (g_error != '')
          session$sendCustomMessage(type = "show_error", g_error)
        
        # Clear the error after it has been displayed
        g_error <<- ''
      }
  }) # observe analysis
  
  #-------------------------------------------------------------------------------
  # Update Scenario Plot
  #-------------------------------------------------------------------------------
  observe({
    #  
    if (nrow(g_r$s) != 0 && !is.null(input$s_scenplot))
      plot.scenario(g_r$s, input$s_scenplot)
  })
  
  #-------------------------------------------------------------------------------
  # Update Concept Iteration Plot
  #-------------------------------------------------------------------------------
  observe({
    if (length(g_r$r) != 0 && !is.null(input$s_iterplot))
      plot.iteration()
  })
  
  #-------------------------------------------------------------------------------
  # Remove last scenario in the queue when button clicked
  #-------------------------------------------------------------------------------
  observe({
    if (!is.null(input$btn_qremove)) {
      isolate({
        if (input$btn_qremove != 0)
          if (nrow(g_t$q) != 0)
            g_t$q <<- g_t$q[-nrow(g_t$q),]
      })
    }
  }) # observe btn_qremove
  
  #-------------------------------------------------------------------------------
  # Clear all entries in the queue when button clicked
  #-------------------------------------------------------------------------------
  observe({
    if (!is.null(input$btn_qclear))
      if (input$btn_qclear != 0)
        isolate({
          g_t$q <<- data.frame()
        })
  }) # observe qclear
  
  #-------------------------------------------------------------------------------
  # Remove Googledrive from location option when MentalModeler selected
  #-------------------------------------------------------------------------------
  observe({
    file_type <- input$s_ftype
    if (file_type == "mmp")
      updateSelectInput(session, 's_flocation', 'File Location:', g_flocations[-which(g_flocations == "google")])
    else
      updateSelectInput(session, 's_flocation', 'File Location:', g_flocations)
  }) # observe s_ftype
  
})
