#-------------------------------------------------------------------------------
# Specified height of FCM plot
#-------------------------------------------------------------------------------
pfcm.height <- function() {
  input$n_fcmheight
}

#-------------------------------------------------------------------------------
# Specified width of FCM plot
#-------------------------------------------------------------------------------
pfcm.width <- function() {
  input$n_fcmwidth
}

#-------------------------------------------------------------------------------
# Plot FCM using qgraph package
#-------------------------------------------------------------------------------
output$fcm_plot <- renderPlot({
  if (input$s_scenario == 'None' || (as.numeric(input$s_scenario) > length(g_r$r)))
    return(NULL)
  
  scenario <- as.numeric(input$s_scenario)
  r_a <- g_r$r[[scenario]]
  isolate({
    q_a <- g_r$q[scenario,]
  })
  iter <- input$iteration + 1
  if (nrow(r_a) != 0 & nrow(q_a) != 0 & rownames(q_a)[1] != "NA" & !is.null(iter)) {
    isolate({
      r <- r_a[iter,]
      
      if (as.character(q_a$squash) == "tanh") {
        r[r > 0] <- "P"
        r[r == 0] <- "Z"
        r[r < 0] <- "N"
      }
      else {
        r[r >= 0.5] <- "P"
        r[r < 0.5] <- "Z"
      }
      cols <- c(N = "red", P = "green", Z = "steelblue2")
      m <- match(unique(r), names(cols))
      m <- m[order(m)]
      node_c <- cols[m] 
      l_par <- list(repulse.rad = (ncol(r_a))^2.5, area=(ncol(r_a))^2, cool.exp=2, max.delta=(ncol(r_a)))
      g_plot <- switch(as.character(q_a$squash),
                       "tanh" = qgraph(g_m$m, edge.labels=T, layout="spring", shape="square", directed=F, groups=r, 
                                       labels=colnames(r_a), legend=F, color=node_c, normalize=T, edge.label.cex=0.7, 
                                       arrows=F, esize=1.5, asize=3, layout.par=l_par, vsize=4, 
                                       scores=as.integer(r_a[iter,]*100), scores.range=c(-100,100)),
                       qgraph(g_m$m, edge.labels=T, layout="spring", shape="square", directed=F, groups=r, 
                              labels=colnames(r_a), legend=F, color=node_c, normalize=T, edge.label.cex=0.7, 
                              arrows=F, esize=1.5, asize=3, layout.par=l_par, vsize=4))
      #         title(g_ss$n[rownames(g_ss$n) == q_a$s_state,1], cex.main=1.5)
      return(g_plot)
    })
  }
}, height=pfcm.height, width=pfcm.width)  

#-------------------------------------------------------------------------------
# Display line plot for selected concepts
#-------------------------------------------------------------------------------
plot.iteration <- function() {
  if (length(input$s_scenario) == 0 || length(input$s_iterplot) == 0) {
    # ggvis() %>% bind_shiny("iter_plot")
    return(NULL)
  }
  # (as.numeric(input$s_scenario) > length(g_r$r)))
  
  # Determine which scenario is selected
  scenario <- as.numeric(input$s_scenario)
  # Extract results for the selected scenario
  result_scen <- g_r$r[[scenario]]
  isolate({
    # Extract scenario from queue
    queue_scen <- g_r$q[scenario, ]
  })
  
  # If there are no results or no entries in the queue, don't display plot
  if (nrow(result_scen) == 0 || nrow(queue_scen) == 0 || rownames(queue_scen)[1] == "NA") {
    # ggvis() %>% bind_shiny("iter_plot")
    return(NULL)
  }
  
  isolate({
  if (input$s_ftype == "mmp")
    concept_names <- as.vector(apply(g_m$n, 1, function(x) paste(x[c(2,3)], collapse="-")))
  else 
    concept_names <- as.vector(apply(g_m$n, 1, function(x) paste(x, collapse="-")))
})
  concepts <- rownames(g_m$n)[concept_names %in% input$s_iterplot]
  r <- melt(result_scen[,concepts])
  if (length(concepts) == 1)
    r <- cbind(seq(1, nrow(r)), concepts, r) 
  colnames(r) <- c("Iteration", "Concept", "Value")
  r$Iteration <- as.numeric(r$Iteration) - 1
  r$Concept <- factor(r$Concept, levels = unique(r$Concept))

  # Create ggvis plot
  p <- r %>% ggvis(x = ~Iteration, y = ~Value) %>%
    group_by(Concept) %>% layer_paths(stroke = ~Concept) %>% layer_points(fill = ~Concept) %>%
    add_axis("x", title = "Iteration", title_offset = 50) %>% 
    add_axis("y", title = "Value", title_offset = 50) %>% 
    add_tooltip(function(df) paste("Iteration:", df$Iteration, "Value:", round(df$Value, 2), "Concept:", df$Concept)) %>% 
  add_axis("x", orient = "top", ticks = 0, title = "Concept Iterations",
           properties = axis_props(axis = list(stroke = "white"), labels = list(fontSize = 0), title = list(fontSize = 20)))
  # p <- p %>% layer_text(x = ~Concept, y = ~Value, text := ~Scenario)
  
  p %>% bind_shiny("iter_plot")
}

#-------------------------------------------------------------------------------
# Display line plot for selected concepts
#-------------------------------------------------------------------------------
plot.scenario <- function(r_scenarios, scenplot) {
  if (length(scenplot) == 0) {
    # ggvis() %>% bind_shiny("scen_plot")
    return(NULL)
  }
  
  r <- cbind(scenplot, r_scenarios[scenplot, ])
  r <- melt(r, id.vars=1)  
  
  if (ncol(r) == 1)
    r <- cbind(rep(scenplot, nrow(r)), rownames(r), r)
  
  colnames(r) <- c("Scenario", "Concept", "Value")
  r$Scenario <- as.factor(r$Scenario)
  concept <- r$Concept
  
  # Create ggvis plot
  p <- r %>% ggvis(x = ~Concept, y = ~Value) %>%
       group_by(Scenario) %>% layer_paths(stroke = ~Scenario) %>% layer_points(fill = ~Scenario) %>%
       add_axis("x", title = "Concept", title_offset = 50) %>% 
       add_axis("y", title = "Value", title_offset = 50) %>%
       add_tooltip(function(df) paste("Concept:", df$Concept, "Value:", round(df$Value, 2), "Scenario:", df$Scenario)) %>% 
       add_axis("x", orient = "top", ticks = 0, title = "Scenarios",
                properties = axis_props(axis = list(stroke = "white"), labels = list(fontSize = 0), title = list(fontSize = 20)))
  
  p %>% bind_shiny("scen_plot")
}

# p <- p %>% layer_text(x = ~Concept, y = ~Value, text := ~Scenario)