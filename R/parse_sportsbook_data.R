parse_gamelines <- function(sportsbook_data, sportsbook, sport,
                            gamelines_lu, exclude_alts = TRUE, exclude_live = TRUE) {}

parse_props <- function(sportsbook_data, sportsbook, sport, prop,
                        prop_lu = NULL, prop_lu_path = NULL) {
  # get prop lookup
  if (!is.data.frame(prop_lu)) {
    prop_lu <- read.csv(prop_lu_path)
  }
  # pare down the lookup
  lu <- prop_lu[prop_lu$prop == prop & prop_lu$sportsbook == sport, ]
  # run through elements in sportsbook_data
  output_list <- list()
  for (i in sportsbook_data) {
    prop_raw <- []

  }



}
