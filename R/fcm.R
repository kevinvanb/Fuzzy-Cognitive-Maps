#-----------------------------------------------------------------------------------#
# Author Kevin van Blommestein, ETA, PSU, 2014
# 
# Fuzzy Cognitive Map (FCM) function
# 
# Parameters
# mat       : concept adjacency matrix
#             size = number of concepts x number of concepts
# s.state   : start state
#             size = 1 x number of concepts
# squash    : squashing function as string
#             size = 1 (for same squashing function for all concepts) OR
#             size = 1 x number of concepts (for different squashing functions for each concept)
#             default = binary
# fixed     : fixed concept(s) (1 = fixed, 0 = not fixed)
#             size = 1 x number of concepts OR
#             size = 1 (value is then repeated for all concepts)
#             default = 0
# iter      : maximum number of iterations. Overriden by eps
#             size = 1
#             default = 100
# eps       : if concept values change by less than eps from previous iteration, exit loop 
#             size = 1
#             default = 0
#
# Return
# result    : result for each concept for each iteration
#             size = number of iterations x number of concepts
#
# Size is specified as number of rows x number of columns
#-----------------------------------------------------------------------------------#

FCM <- function(mat, s.state, squash = "ifelse(x < 0.5, 0, 1)", fixed = 0, iter = 100, eps = 0) {
  # Currently these are the only squashing functions allowed, can easily be updated
  # squash.l <- g_squash[-length(g_squash)]

  # ------- Start of parameters checks ------- #
  # Check adjacency matrix
  # Convert mat into matrix
  mat <- as.matrix(mat)
  if (sum(apply(mat, 2, is.numeric)) != ncol(mat))
    stop("Adjacency matrix must be numeric", call. = FALSE)

  if (nrow(mat) != ncol(mat))
    stop("Adjacency matrix must have same number of rows and columns", call. = FALSE)

  # Check start state
  if (!is.numeric(s.state))
    stop("Start state vector must be numeric", call. = FALSE)
  # If only one start state value is specified, repeat that value for all concepts

  if (length(s.state) == 1)
    s.state <- rep(s.state, nrow(mat))

  if (length(s.state) != nrow(mat))
    stop("Start state must be the same length as the number of concepts", call. = FALSE)
  s.state <- matrix(s.state, nrow=1)

  # Check squashing function(s)
  squash <- as.character(squash)
#   if (sum(tolower(squash) %in% squash.l) != length(squash))
#     stop(paste("Squashing function(s) not", paste(squash.l, collapse=" OR ")), call. = FALSE)
  if (length(squash) != nrow(mat) && length(squash) != 1)
    stop("Squashing function must be the same length as the number of concepts
         or of length one when using one squashing function", call. = FALSE)

  # Check fixed
  if (!is.numeric(fixed))
    stop("Fixed vector must be numeric", call. = FALSE)
  # If only one fixed value is specified, repeat that value for all concepts
  if (length(fixed) == 1)
    fixed <- rep(fixed, nrow(mat))
  if (length(fixed) != nrow(mat))
      stop("Fixed must be the same length as the number of concepts", call. = FALSE)
  fixed <- matrix(fixed, nrow=1)
  
  # Check iter
  if (!is.numeric(iter))
    stop("Iter must be numeric", call. = FALSE)
  # Convert iter into integer
  iter <- as.integer(iter)
  if (iter < 1)
    stop("Iter must be a positive integer greater than 0", call. = FALSE)
  
  # Check eps
  if (!is.numeric(eps))
    stop("eps must be numeric", call. = FALSE)
  if (eps < 0)
    stop("eps must be greater than or equal to 0", call. = FALSE)
  
  # ------- End of checking parameters ------- #
  
  # ------- Start of FCM calculations ------- #
  # Create matrix in which results for each iteration will be saved
  # Save start state as first row of matrix
  result <- matrix(s.state, nrow=1)
  
  # Loop until number of iterations is reached or eps value is met
  for (i in 1:iter) {
    # Multiply previous result with adjacency matrix
    r_i <- result[i,] %*%  mat
    
    # Squash the result by either a squashing function defined per concept
    if (length(squash) == ncol(mat))
      for (j in 1:length(squash))
        r_i[j] <- .squash(r_i[j], squash[j])
    # or by the same squashing function for all concepts
    else
      r_i <- .squash(r_i, squash[1])
    
    # Maintain fixed values
    r_i[which(fixed == 1)] <- result[i, which(fixed == 1)]
    
    # Add iteration to the results matrix
    result <- rbind(result, r_i)
    
    # If the eps value is met exit the loop
    if (mean(abs(result[i, ] - r_i)) <= eps)
      break
  }
  
  # ------- End of FCM calculations ------- #
  
  dimnames(result) <- list(paste0("I", 1:nrow(result)), colnames(mat))
  return(result)
}

#-----------------------------------------------------------------------------------#
# Squash function
# 
# Parameters
# x         : value(s) to be squashed
#             size = 1 x any length
# squash.f  : squashing function as string
#             size = 1 x number of concepts 
#
# Return
# result    : squash values
#             size = 1 x length of value
#-----------------------------------------------------------------------------------#
.squash <- function(x, squash.f) {
  # Convert squashing function as string to expression
  result <- eval(parse(text = squash.f), envir = NULL)
#   result <- switch(as.character(squash.f),
#                 'binary' = ifelse(x < 0.5, 0, 1),
#                 'tanh' = tanh(x),
#                 'sigmoid' = 1/(1 + exp(-x)))
  return(result)
}
  