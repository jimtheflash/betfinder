parse_draftkings_data <- function(draftkings_data, sport, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, exclude_alts = TRUE) {

  output_list <- list()

  for (e in names(draftkings_data)) {
    game_event <- draftkings_data[[e]]
    # nuke live games if specified, which is the default
    if (exclude_live) {
      status <- game_event$event$eventStatus$state
      if (status == "STARTED") next
    }
    # get matchup name and start time
    matchup <- game_event$event$name
    tipoff <- game_event$event$startDate
    # break out the offer markets, always necessary
    offer_categories <- game_event$eventCategories
    offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))
    # get the game lines if thats what you want
    if (game_lines == TRUE) {
      if (sport %in% c('nba', 'ncaaf', 'nfl', 'mlb')) {
        output_list[[length(output_list) + 1]] <-
          parse_dk_game_lines(offer_categories, exclude_alts = exclude_alts, matchup = matchup, tipoff = tipoff)
        next
      }
    }
    # get props
    if (prop %in% c('first team to score', 'ftts')) {
      if (sport %in% c('nba', 'ncaaf', 'nfl')) {
        output_list[[length(output_list) + 1]] <-
          parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = 'First to Score', prop_name = '1st to Score',
                        prop = prop, matchup = matchup, tipoff = tipoff)
        next
      }
      if (sport == 'mlb') {
        output_list[[length(output_list) + 1]] <-
          parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = 'First/Last Run', prop_name = '1st Run',
                        prop = prop, matchup = matchup, tipoff = tipoff)
        next
      }
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = '1st Basket Props', prop_subgroup = 'First FG', prop_name = 'First Field Goal', prop = prop, matchup = matchup, tipoff = tipoff)
      next
    }
  }
  # if output_list is empty, error
  if ((!'output_list' %in% ls()) | length(output_list) == 0) stop('no draftkings ', prop, ' returned')
  output_df <- dplyr::bind_rows(output_list)
  output_df$prop <- prop
  return(output_df)
}





