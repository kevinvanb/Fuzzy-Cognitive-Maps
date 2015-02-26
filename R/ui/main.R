# Show a plot of the generated distribution
dashboardBody(
  tabsetPanel(id="result_tabset",
              tabPanel("All Scenarios", value="rsummary_tab",
                       fluidRow(
                         box(
                            width=12,
                            title = "Scenario Settings", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                            dataTableOutput("t_queue"),
                            fluidRow(
                              column(2, uiOutput("q_remove")),
                              column(2, uiOutput("q_clear"))
                            )
                         ),
                         box(
                           width=12,
                           title = "Scenario Results", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                           uiOutput("r_down"),
                           br(),
                           dataTableOutput("t_result")
                         ),
                         box(
                            width=12,
                            title = "Scenario Plot", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                            fluidRow(
                              column(4, selectInput('s_scenplot', 'Select Scenarios to Plot:', 'None', multiple = TRUE))
                            ),
                            # plotOutput("scen_plot", height="600px")
                            ggvisOutput("scen_plot")
                         ))
              ),
              tabPanel("Individual Scenario", value="rscenario_tab",
                       fluidRow(
                         column(3, selectInput('s_scenario', 'Scenario:', 'None', multiple = FALSE))
                       ),
                       tabsetPanel(id="scenario_tabset",
                                   tabPanel("Iterations", value="siter_tab",
                                            fluidRow(
                                              box(
                                                width=12,
                                                title = "Iteration Table", status = "primary", solidHeader = FALSE, collapsible = TRUE, 
                                                uiOutput("s_down"),
                                                br(),
                                                dataTableOutput("t_scenario") 
                                              ),
                                              box(
                                                width=12,
                                                title = "Iteration Plot", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                                fluidRow(
                                                  column(4, selectInput('s_iterplot', 'Select Concepts to Plot:', 'None', multiple = TRUE))
                                                ),
                                                # plotOutput("iter_plot")
                                                ggvisOutput("iter_plot")
                                              )
                                            )
                                   ),
                                   tabPanel("Network Plot", value="snetwork_tab",
                                            fluidRow(
                                              box(
                                                width=12,
                                                title = "Network Plot", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                                fluidRow(
                                                  column(4, uiOutput("sl_iter"))
                                                ),
                                                HTML("<hr>"),
                                                HTML("<font color='limegreen'>Green (+ve)</font>, <font color='steelblue'>Blue (0)</font>,  
                                                      <font color='red'>Red (-ve)</font>"),
                                                plotOutput("fcm_plot", width="auto", height="auto")
                                              ))
                                   )
                       )
              ),
              tabPanel("Network Stats", value="rnetworkstats_tab",
                       HTML("COMING SOON")
              )
  ) #tabset panel
) # main panel