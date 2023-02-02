
test_that("old new", {

  comparison_data <- utils::read.csv("old_new_data.csv")
  
  scn_col <- unique(comparison_data$scenario)
  save_dates <- unique(comparison_data$Date)

  here::here("inst", "input", "tables") %>%
    list.files(full.names = TRUE, pattern = paste0(scn_col, collapse = "|")) %>%
    lapply(function(f){
      name <- basename(f)
      data <- utils::read.csv(f, comment.char = ";")
      out <- cbind(scenario = name, data[data$Date %in% save_dates, ])
      return(out)
    }) %>%
    rbindlist(fill=TRUE) ->
    new_data

  # Make sure that no scenarios were dropped or added
  #expect_identical(setdiff(comparison_data$scenario, new_data$scenario), character(0), label = "unknown scenario in new_data:")
  #expect_identical(setdiff(new_data$scenario, comparison_data$scenario), character(0), label = "new_data missing scenario:")

  # For each of the scenarios compare the data frames
  scn_vector <- unique(new_data$scenario)
  for(scn in scn_vector){
    old <- comparison_data[comparison_data$scenario==scn, ]
    new <- comparison_data[new_data$scenario==scn, ]
    expect_equal(old, new, tolerance = 1e-3, label = scn)
  }
})
