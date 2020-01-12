
records <- readr::read_csv(file.path(wd$data, "cave_mine_records.csv")) %>%
  dplyr::mutate(def_alive = dplyr::case_when(
    is.na(alive) ~ "No",
    alive != 1 ~ "No",
    TRUE ~ "Yes" # neither range is between 395 and 500
  ))
