
library("tidyverse")

# 100Si-11 and 100Si-12 are very similar, use one or other, not both

rawdat <- list.files(
  "data/image-j", 
  pattern = ".+\\.csv", full.names = TRUE
) %>% 
  set_names() %>% 
  map_dfr(read_csv, .id = "file") %>% 
  mutate(image = regmatches(file, regexpr("(?<=image-j/)\\w+-\\w+", file, perl = TRUE))) %>% 
  filter(image != "100Si-11") %>% 
  separate(image, c("susp", "num"), sep = "-") 

write_csv(rawdat, "data/image-j-clean.csv")


