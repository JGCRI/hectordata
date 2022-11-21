# Generate comparison data for the old new test, this is to ensure that we have 
# stable results during this period of rapid development. 
devtools::load_all()

# A vector of the dates to save and use in the old new comparison
save_dates <- floor(seq(from = min(YEARS), to = max(YEARS), length.out = 10))

here::here("inst", "input", "tables") %>% 
  list.files(full.names = TRUE) %>% 
  lapply(function(f){
    name <- basename(f)
    data <- utils::read.csv(f, comment.char = ";")
    out <- cbind(scenario = name, data[data$Date %in% save_dates, ])
    return(out)
  }) %>%  
  rbindlist(fill=TRUE) -> 
  comp_data

write.csv(comp_data, here::here("tests", "testthat", "old_new_data.csv"), row.names = FALSE)
