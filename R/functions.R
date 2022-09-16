
# this script is called by source() from paper.Rmd, after loading tidyverse

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

# calculate number of matches to a regex in references, as a percentage of total references:

percent_bib_matches <- function(regex) {
  
  bib <- read_lines(here::here("Rmarkdown/references.bib")) %>% 
    tibble(bib = .)
  rmd <- read_file(here::here("Rmarkdown/paper.Rmd"))
  
  keys <- rmd %>% 
    str_extract_all("(?<=[^\\\\]@)[^;\\]]+") %>% 
    unlist() %>% 
    tibble(key = .) %>% 
    filter(!str_detect(key, "dal.ca")) %>% # email address
    distinct()
  
  filter_bib <- function(x) {
    bib %>% 
      filter(
        str_detect(
          bib, 
          glue::glue("@[aA-zZ]+\\{`x`,|author", .open = "`", .close = "`")
        )
      ) %>% 
      filter(str_detect(lag(bib), "@[aA-zZ]"))
  }
  
  sc <- keys %>% 
    mutate(
      extr = map(
        key, 
        filter_bib
      )
    ) %>% 
    unnest(extr) %>% 
    distinct() %>% 
    pull(bib) %>% 
    str_detect(regex) %>%
    mean() %>% 
    round(2)
  
  paste0(100 * sc, "% of references matching")
  
}

