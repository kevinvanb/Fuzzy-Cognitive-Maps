#-------------------------------------------------------------------------------
# Loads data from three different sources (local/google spreadsheet/dropbox) using
# the data.retrieve function, depending on the selection by the user. 
#-------------------------------------------------------------------------------
data.load <- function(s_flocation, s_ftype, data_file, gs_url, dropbox_url, mm_values = NA){
  # Retrieved data from the specified file source
  df <- data.frame()
  if (s_ftype == "mmp")
    res <- read.mmf(data_file$datapath, s_flocation, mm_values)
  else {
    df <- switch(s_flocation,
           # Local file source selected by user
           local = {
             # Determine whether a file has been chosen by the user. Message displayed to select a file
             if (is.null(data_file)) {
               g_error <<- "No File Chosen. Select a csv file using the 'Choose File' button" 
               return(data.frame())
             }
             # Retrieve the data from the specified file location
             data.retrieve(data_file$datapath, s_flocation)
           },
           # Google Spreadsheet file source selected by user (only if csv selected for file type)
           google = {
             select_query <- "select * where A!=''" # sql query to select rows of data where column A is not empty
             url_split <- unlist(strsplit(gs_url,"key="))
             if (length(url_split) < 2) {
               g_error <<- "Google spreadsheet URL does not include a key (key=)"
               return(data.frame())
             }
             url_split <- unlist(strsplit(url_split[2],"#gid="))
             file_key <- url_split[1] # Google spreadsheet key, between 'key=' and '#gid=' in URL
             # If the sheet number is not specified, then the first sheet will be selected
             file_gid <- ifelse (length(url_split) < 2, 0, file_gid <- url_split[2] ) # Spreadsheet sheet number (starting at 0), follows '#gid=' in URL
             # Create the URL for the spreadsheet using the query, key and gid
             file_url <- paste(sep="",'https://spreadsheets.google.com/tq?', 'tqx=out:csv','&tq=', 
                               curlEscape(select_query), '&key=', file_key, '&gid=', file_gid)
             # Retrieve the data from the specified spreadsheet URL
             data.retrieve(file_url, s_flocation)
           },
           # Dropbox file source selected by user
           dropbox = {
             data.retrieve(dropbox_url, s_flocation)
           }
    )# End switch
    res <- data.split(df)
  }
  # Split Uploaded data into matrix, input/outputs, start states, squash function
  return(res)
}

#-------------------------------------------------------------------------------
# Retrieves data from a local or remote csv file depending on the file_source 
#-------------------------------------------------------------------------------
data.retrieve <- function(file_path, file_loc, c_header = TRUE, r_header = FALSE, 
                          file_sep = ",", file_quote = '"'){
  df <- data.frame()
  # Determine whether data exists at the specified file location (local/google). 
  # Message returned if an error occurs.
  df <- tryCatch(
          switch(file_loc,
           local = {read.csv(file_path, sep = file_sep, quote = file_quote, header = c_header,
                             stringsAsFactors = FALSE, row.names = NULL)},
           google = {read.csv(textConnection(getURL(file_path, ssl.verifypeer = FALSE)), 
                              header=c_header,stringsAsFactors = FALSE)},
           dropbox = {read.csv(textConnection(getURL(file_path, ssl.verifypeer = FALSE)), 
                              sep=file_sep, quote = file_quote, header=c_header, 
                              stringsAsFactors = FALSE, row.names = NULL)}
    ),
    error = function(e) {
      g_error <<- conditionMessage(e)
      return(data.frame()) 
    }
  )
  # If the dataset has no rows then there is no data in the source file
  if (is.null(nrow(df))) {
    g_error <<- "Data cannot be found at specified location or chosen parameters not correct"
    return(data.frame()) 
  }
  if (nrow(df) == 0) {
    g_error <<- "Data cannot be found at specified location or chosen parameters not correct"
    return(data.frame()) 
  }
  
  if (ncol(df) > 1 && r_header)
    if (anyDuplicated(df[,1]) == 0){
      rownames(df) <- df[,1]
      df <- df[,-1]
    }
  else {
    g_error <<- "Duplicated row names, deselect row header check box"
    return(data.frame()) 
  }
  return (df)
}

#-------------------------------------------------------------------------------
# Retrieve data from mmf file
#-------------------------------------------------------------------------------
read.mmf <- function(file_path, file_loc, mm_values) {
  mat <- list(m = data.frame(), n = data.frame())
  # Extract data from XML file created by MentalModeler
  if (file_loc == "dropbox")
    data <- xmlParse(textConnection(getURL(file_path, ssl.verifypeer = FALSE)), options = NOCDATA) 
  else
    data <- xmlParse(file_path, options = NOCDATA)
  
  # Convert XML data to list
  xml_data <- xmlToList(data)
  # Extract information about the FCM 
  fcm_info <- unlist(xml_data[["info"]])
  
  # Extract Information about concepts
  concept_headers <- c("id", "name", "notes", "units", "x", "y")
  concept_info <- ldply(xml_data$concepts, function(concept) unlist(concept[concept_headers]))[, -1]
  rownames(concept_info) <- concept_info$name
  
  # Extract relationships for each concept and add concept id and name to
  # show the source of the relationship
  relation_func <- function(x) {
    relations <- ldply(x$relationships, data.frame, stringsAsFactors = F)[, -1]
    if (nrow(relations) != 0) 
      data.frame(src_id = x$id, src_name = x$name, relations, stringsAsFactors = F)
  }
  relation_info <- ldply(xml_data$concepts, relation_func)[, -1]
  
  # Convert MentalModeler influence to actual values
  na_values <- sum(is.na(mm_values)) == length(mm_values)
  relation_info$influence[relation_info$influence == "H+"] <- ifelse(na_values, 1, mm_values[1])
  relation_info$influence[relation_info$influence == "M+"] <- ifelse(na_values, 0.66, mm_values[2])
  relation_info$influence[relation_info$influence == "L+"] <- ifelse(na_values, 0.33, mm_values[3])
  relation_info$influence[relation_info$influence == "H-"] <- ifelse(na_values, -1, mm_values[4])
  relation_info$influence[relation_info$influence == "M-"] <- ifelse(na_values, -0.66, mm_values[5])
  relation_info$influence[relation_info$influence == "L-"] <- ifelse(na_values, -0.33, mm_values[6])
 
  relation_info$influence <- as.numeric(relation_info$influence)
  # Create adjacency matrix using relationships from MentalModeler
  m <- matrix(0, nrow(concept_info), nrow(concept_info), dimnames = list(concept_info$id, concept_info$id))
  src_id <- as.numeric(relation_info[, "src_id"]) + 1
  id <- as.numeric(relation_info[, "id"]) + 1
  m[matrix(c(src_id, id), ncol=2)] <- as.numeric(relation_info$influence)
  
  dimnames(m) <- list(rownames(concept_info),rownames(concept_info))
  mat$m <- m
  mat$n <- concept_info
  res <- list(mat = mat, ss = list(m = data.frame(), n = data.frame()),
              sf = list(m = data.frame(), n = data.frame()),
              io = list(m = data.frame(), n = data.frame()))
  return(res)
}

#-------------------------------------------------------------------------------
# Split Uploaded data into matrix, input/outputs, start states, squash function
#-------------------------------------------------------------------------------
data.split <- function(d){
  mat <- list(m = data.frame(), n = data.frame())
  ss <- list(m = data.frame(), n = data.frame())
  sf <- list(m = data.frame(), n = data.frame())
  io <- list(m = data.frame(), n = data.frame())
  if (nrow(d) > 0) {
    # find the start and end of each section of data from the csv file
    m_dv <- which(d[,1] == g_sep[1])
    io_dv <- which(d[,1] == g_sep[2])
    ss_dv <- which(d[,1] == g_sep[3])
    sf_dv <- which(d[,1] == g_sep[4])
    
    # Read matrix data between <M>'s 
    if (length(m_dv) == 2) {
      m <- d[(m_dv[1] + 1):(m_dv[2] - 1), ]
      col_match <- match(tolower(m[, 1]), tolower(colnames(d)))
      mat$m <- m[, col_match]
      mat$m <- apply(mat$m, 2, as.numeric)
      mat$n <- m[, -col_match]
#       mat$n <- apply(mat$n, 2, function(x) { x[is.na(x)] = "" 
#                                              x})
      
      if (ncol(mat$m) != nrow(mat$m)) {
        g_error <<- "Number of columns must be equal to the number of rows in matrix, and row and
                    column names must be the same"
        return(NULL)
      }
      dimnames(mat$m) <- list(m[, 1], m[, 1])
      dimnames(mat$n) <- list(m[, 1], c("NAME", "DESCRIPTION"))

        # Read input/output data
        if (length(io_dv) == 2) {
          if (io_dv[2] - io_dv[1] > 1) {
            io$m <- d[(io_dv[1] + 1):(io_dv[2] - 1), col_match]
            io$n <- d[(io_dv[1] + 1):(io_dv[2] - 1), -col_match]
#             io$n <- apply(io$n, 2, function(x) { x[is.na(x)] = "" 
#                                                    x})
            dimnames(io$m) <- list(io$n[, 1], colnames(mat$m))
            dimnames(io$n) <- list(io$n[, 1], c("NAME", "DESCRIPTION"))
          }
        }
#         else
#           if (length(io_dv) != 0)
#             g_error <<- paste0("Ensure ",  g_sep[2], " is included before and after the io data in the csv file")
#        
        # Read start state data
        if (length(ss_dv) == 2) {
          if (ss_dv[2] - ss_dv[1] > 1) {
            ss$m <- d[(ss_dv[1] + 1):(ss_dv[2] - 1), col_match]
            ss$n <- d[(ss_dv[1] + 1):(ss_dv[2] - 1), -col_match]
#             ss$n <- data.frame(apply(ss$n, 1, function(x) { x[is.na(x)] = "" 
#                                                  x}))
            dimnames(ss$m) <- list(ss$n[, 1] , colnames(mat$m))
            dimnames(ss$n) <- list(ss$n[, 1] , c("NAME", "DESCRIPTION"))
          }
        }
#         else
#           if (length(ss_dv) != 0)
#             g_error <<- paste0("Ensure ",  g_sep[3], " is included before and after the start state data in the csv file")
#         
        # Read squashing function data
        if (length(sf_dv) == 2) {
          if (sf_dv[2] - sf_dv[1] > 1) {
            sf$m <- d[(sf_dv[1] + 1):(sf_dv[2] - 1), col_match]
            sf$n <- d[(sf_dv[1] + 1):(sf_dv[2] - 1), -col_match]
#             sf$n <- data.frame(apply(sf$n, 1, function(x) { x[is.na(x)] = "" 
#                                                  x}))
            
            dimnames(sf$m) <- list(sf$n[, 1], colnames(mat$m))
            dimnames(sf$n) <- list(sf$n[, 1], c("NAME", "DESCRIPTION"))
          }
        }
#         else
#           if (length(sf_dv) != 0)
#             g_error <<- paste0("Ensure ",  g_sep[4], " is included before and after the squashing function data in the csv file")
#        
      res <- list(mat = mat, io = io, ss = ss, sf = sf)  
      return(res)
      # Save data to global files to be used in other functions
#         g_m <<- mat
#         g_io <<- io
#         g_ss <<- ss
#         g_sf <<- sf
      }
    }
    else
      g_error <<- paste0("Ensure ",  g_sep[1], " is included before and after the matrix data in the csv file")
  return(NULL)
}

#-------------------------------------------------------------------------------
# Conducts FCM analysis 
#-------------------------------------------------------------------------------
data.analysis <- function() {
  # Empty list for holding all the results from the analysis of the queue
  r_l <- list()
  queue <- g_r$q

  # Conduct analysis for each entry in the queue
  for (i in 1:nrow(queue)) {
    q <- queue[i,]
    # Get the start state for the ith entry in the queue
    ss <- switch(as.character(q$s_state),
                "One" = 1,
                "Zero" = 0,
                as.numeric(g_ss$m[rownames(g_ss$m) == q$s_state,]))
    # Get the concepts that will be fixed for the ith entry in the queue
    fc <- as.numeric(colnames(g_m$m) %in% unlist(strsplit(as.character(q$fixed), split=",")))
    sf <- q$squash
    if (!(sf %in% g_squash)) {
      sf <- g_sf$m[rownames(g_sf$m) == sf,]
    }
    # Calculate fcm results for ith entry in the queue
    r <- FCM(g_m$m, ss, sf, fc, q$iter, q$eps)
    # Save the results to a results list using a unique scenario name to identify
    r_l[[q$scenario]] <- r
  }
  return(r_l)
}

#-------------------------------------------------------------------------------
# Analyze all possible permutations of the start state
#-------------------------------------------------------------------------------
perm.analysis <- function() {
  #     inputs <- colnames(g_m$m) %in% input$s_inconcepts
  s_f <- as.vector(apply(g_m$n, 1, function(x) paste(x, collapse="-")))
  s_inputs <- s_f %in% input$s_inconcepts
  
  n <- length(input$s_inconcepts)
  if (n >= 10) {
    g_error <<- "Choose less than 10 inputs for analysis with permutations"
    return(data.frame())
  }
  if (nrow(g_r$q) > 1) {
    g_error <<- "Only one entry can be in the queue when using permutations"
    return(data.frame())
  }
  
  q <- g_r$q
  fc <- colnames(g_m$m) %in% unlist(strsplit(as.character(q$fixed), split=","))
  sf <- q$squash
  if (!(sf %in% g_squash)) {
    sf <- g_sf$m[rownames(g_sf$m) == sf,]
  }
  ss_opts <- digitsBase(1:(2^n), ndigits=n)
  #     ss_opts <- ss_opts[nrow(ss_opts):1,]
  r_a <- data.frame()
  for (i in 1:ncol(ss_opts)) {
    ss <- rep(0, ncol(g_m$m))
    ss[s_inputs] <- t(ss_opts[, i])
    # Calculate fcm results for ith entry in the queue
    r <- FCM(g_m$m, ss, sf, fc, q$iter, q$eps)
    # Save the results to a results list using a unique scenario name to identify
    #       mtch <- match_df(r_a[,2:ncol(r_a)], r[nrow(r),])
    #       if (nrow(mtch) > 0)
    #       else
    r_a <- rbind(r_a, r[nrow(r), ])
  } 
  
  #     r_a <- unique(r_a)
  dimnames(r_a) <- list(seq(1,nrow(r_a)), colnames(g_m$m))
  updateSelectInput(session, 's_scenplot', 'Select Scenarios to Plot:', rownames(r_a), rownames(r_a)[1])
  
  return(r_a)
}

#-------------------------------------------------------------------------------
# Summarize results into one table 
#-------------------------------------------------------------------------------
r.summary <- function() {
  if (length(g_r$r) == 0)
    return(NULL)
  
  # Select the last iteration of the first scenario result
  r <- g_r$r[[1]][nrow(g_r$r[[1]]),]
  # If there is more than one result the length of the list would be > 1
  if (length(g_r$r) > 1)
    # Select the last iteration of each other scenario result and combine into table
    for (i in 2:length(g_r$r))
      r <- rbind(r, g_r$r[[i]][nrow(g_r$r[[i]]),])
  # If there is only one scenario result, convert vector to dataframe and transpose
  if (length(g_r$r) == 1)
    r <- t(data.frame(r))
  # Assign columns names to the result matrix
  if (is.null(names(g_r$r)))
    rownames(r) <- seq(1, nrow(r), 1)
  else
    rownames(r) <- names(g_r$r)
  
  r <- data.frame(r)
  return(r)
}
