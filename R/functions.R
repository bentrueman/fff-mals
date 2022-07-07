
# this is the opposite of fill()

unfill_vec <- function(x, placeholder = "") { 
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, placeholder, as.character(x)) 
}

to_list <- function(x) {
  x %>% 
    str_replace("(\\d+)([aA-zZ]+)", "^\\1^\\2") %>% 
    unique() %>% 
    glue::glue_collapse(sep = ", ", last = " and ")
}

