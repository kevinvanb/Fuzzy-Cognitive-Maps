dashboardSidebar(
  tags$head(
    #       tags$style(type='text/css', "#t_scenario table, #t_concepts table { text-align: center; border: 0.5px solid steelblue; float:centre; }"),
    tags$style(type="text/css", '#t_scenario tfoot, #t_concepts tfoot, #t_queue tfoot, 
               #t_result tfoot  {display:none;}'),
    tags$style(type="text/css", '#t_scenario tr, #t_concepts tr, #t_queue tr, 
               #t_result tr  {border:1px solid grey;}'),
    tags$style(type="text/css", '#t_queue {width:100%;}'),
    tags$style(type="text/css", "#btn_analysis, #btn_upload {color:green;border:1px solid black;}"),
    tags$style(type="text/css", "#btn_qremove, #btn_qclear  {color:red;border:1px solid black;}"),
    tags$style(type="text/css", "#btn_queue  {border:1px solid black;}"),
    tags$style(type="text/css", '#t_scenario td, #t_concepts td, #t_queue td {word-wrap:break-word}'),
    tags$style(type='text/css', "#gs_url, #dropbox_url { width: 90%; }"), # Increase width of URL textarea
    #       tags$style(type='text/css', "#s_fixed { height: 150px; }"), # Increase height of selectInput
    tags$style(type="text/css", "#fcm_plot.recalculating { opacity: 1.0; }"),
    tags$script(type="text/javascript", "
                $(document).ready(function() {
                Shiny.addCustomMessageHandler('show_error', function(message) { alert(message); });
                });")
    ),
  tabsetPanel(id="setup_tabset",
              tabPanel("1.File Upload",value="upload_tab",
                       selectInput('s_ftype', 'File Type:', g_ftypes),
                       selectInput('s_flocation', 'File Location:', g_flocations),
                       # This panel will only display when the local option has been selected for the 
                       # file_source selectInput
                       conditionalPanel(
                         condition = "input.s_flocation == 'local'",
                         #                            selectInput('s_ftype', 'File Type:', list('csv', 'xlsx')),
                         fileInput('data_file', 'Choose File:',
                                   accept=c('.csv','text/csv', 'text/comma-separated-values','text/plain', '.mmp',
                                            'application/vnd.ms-excel', 
                                            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'))
                       ),
                       # This panel will only display when the 'local' option has been selected
                       conditionalPanel(
                         condition = "input.s_flocation == 'google'",
                         HTML("<ol><li><a href = 'https://docs.google.com/spreadsheet' target = '_blank'>If required, 
                              click here to create new Google Spreadsheet:</a></li><li>Share the Google Spreadsheet, 
                              then copy the URL into the text area below:</li></ol>"),
                         tags$textarea(id='gs_url', rows=3, cols=40, placeholder='Google URL', required=TRUE, '')
                         ),
                       # This panel will only display when the 'Google Spreadsheet' option has been selected 
                       conditionalPanel(
                         condition = "input.s_flocation == 'dropbox'",
                         HTML("Copy the Dropbox URL (including http(s)) into the text area below:"),
                         tags$textarea(id='dropbox_url', rows=2, cols=40, 
                                       placeholder='Dropbox URL', 
                                       required=TRUE, 'https://dl.dropboxusercontent.com/u/114755843/test2.csv')
                       ),
                       br(),
                       actionButton("btn_upload","Upload")
                       ),
              
              tabPanel("2.FCM",value="fcms_tab",
                       selectInput('s_state', 'Start State:', 'None', multiple = FALSE),
                       conditionalPanel(
                         condition = "input.s_state == 'Permutations'",
                         selectInput('s_inconcepts', 'Input Concepts:', 'None', multiple = TRUE)
                       ),
                       selectInput('s_fixed', 'Clamp Concept (Selected = Fixed):', 'None', multiple = TRUE),
                       #                         HTML("</br><i><b>Note 1:</b> To select multiple concepts, hold down </br>the Ctrl key 
                       #                               and left click the required concepts.</i></br>
                       #                              <i><b>Note 2:</b> To deselect a concept, hold down</br>the Ctrl key 
                       #                               and left click the concept.</i><br><br>"),
                       selectInput('s_squash', 'Squashing Function:', g_squash),
                       numericInput("n_eps","Epsilon:",0, 0, 100, 1),
                       numericInput("n_miter","Max Iterations:", 100, 1, 10000, 1),
                       br(),
                       fluidRow(
                         column(4, actionButton("btn_queue","Queue")),
                         column(4, uiOutput("analysis"))
                       )
              ),
              tabPanel("Settings",value="settings_tab",
                       br(),
                       HTML("<b><u>MentalModeler Settings</u></b>"),
                       br(),
                       fluidRow(
                         column(6, numericInput("n_hpos","H+ =", 1, 0, 1, 0.01)),
                         column(6, numericInput("n_hneg","H- =", -1, 0, -1, 0.01))
                       ),
                       fluidRow(
                         column(6, numericInput("n_mpos","M+ =", 0.66, 0, 1, 0.01)),
                         column(6, numericInput("n_mneg","M- =", -0.66, 0, -1, 0.01))
                       ),
                       fluidRow(
                         column(6, numericInput("n_lpos","L+ =", 0.33, 0, 1, 0.01)),
                         column(6, numericInput("n_lneg","L- =", -0.33, 0, -1, 0.01))
                       ),
                       br(),
                       HTML("<b><u>FCM Plot Settings</u></b>"),
                       br(),
                       fluidRow(
                         column(6, numericInput("n_fcmwidth","Width:", 0, 0, 1600, 1)), 
                         column(6, numericInput("n_fcmheight","Height:", 0, 0, 1600, 1))
                       ),
                       numericInput("n_fcmdelay","Animation Delay (ms):", 1000, 10, 10000, 1)
              ),
              tabPanel("Concepts",value="concepts_tab",
                       dataTableOutput("t_concepts")
              )
              )     
  )