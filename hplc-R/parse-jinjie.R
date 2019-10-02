library(tidyverse)


jinjie_raw <- read_lines("hplc-R/example-inputs/Javits_Cation_Set8-jinjie.txt")

file_splits <- which(jinjie_raw == "")

jinjie_split <- split(jinjie_raw, cumsum(seq_along(jinjie_raw) %in% file_splits))


out <- map_dfr(jinjie_split, ~{
  if (length(.x) <= 1) return(NULL)
  
  dat <- purrr::discard(.x, function(z) z == "")
  
  id <- str_replace(dat[str_detect(dat, "^ID#\\t")], "^ID#\\t", "")
  name <- str_replace(dat[str_detect(dat, "^Name\\t")], "^Name\\t", "")
  
  # cat(
  #   "Name:", name, "\n",
  #   "ID:", id, "\n"
  # )
  
  dat <- dat[!str_detect(dat, "^ID#\\t")]
  dat <- dat[!str_detect(dat, "^Name\\t")]
  
  dat <- read_tsv(dat) %>% 
    rename(row_number = X1) %>% 
    mutate(
      run_id = id,
      parameter = name
    ) %>% 
    mutate_all(list(~ifelse(. == "-----", 0, .))) %>% 
    janitor::clean_names() %>% 
    mutate_at(vars(vial_number:accuracy_percent), as.numeric)
  
  return(dat)
  
})


run_dat <- filter(out, !is.na(row_number))
run_stats <- filter(out, is.na(row_number))


run_dat %>% 
  select(sample_name, sample_id, parameter, conc) %>% 
  spread(key = parameter, value = conc)
