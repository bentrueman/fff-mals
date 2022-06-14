
# this is the opposite of fill()

unfill_vec <- function(x, placeholder = "") { 
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, placeholder, as.character(x)) 
}

