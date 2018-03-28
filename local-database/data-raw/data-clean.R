library("tidyverse")

## ==== Read write loop ===== #

list.files("data-raw/original-from-james/") %>%
  walk(function(x) {
    read_csv(file.path("data-raw/original-from-james/", x)) %>%
      write_csv(file.path("data-raw/read-write-looped/", x))})


## ==== oxrep wrangling ===== #

oxrep_wrangling <- function(filename){

  local_data <- read_csv(file.path("data-raw/read-write-looped", filename))
  
  colnames(local_data) <- colnames(local_data) %>%
    str_replace_all("\"", "")
  
  local_data %>%
    mutate_if(is.character, funs(str_replace_all(., "\"", ""))) %>%
    mutate_all(funs(parse_guess(.))) %>%
    write_csv(file.path("data-raw/post-oxrep-wrangler/", filename), na = "")
  
}

list.files("data-raw/read-write-looped/") %>%
  walk(~oxrep_wrangling(.x))
