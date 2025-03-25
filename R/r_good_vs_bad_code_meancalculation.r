# Good Code Example: Calculating the mean of a vector
good_code <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  mean(x, na.rm = TRUE)
}


# Bad Code Example: Calculating the mean of a vector (error handling missing, inefficient)
bad_code <- function(x){
  m <- 0
  n <- 0
  for (i in x) {
    m <- m + i
    n <- n + 1
  }
  if (n > 0) m/n else NA
}

#Example usage
good_code(c(1,2,3,NA,5))
bad_code(c(1,2,3,NA,5))

