parse_pointsbet_data <- function(pointsbet_data, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, exclude_alts = FALSE) {
  # loop through the pointsbet events to extract props
  output_list <- list()
  for (game_event in pointsbet_data) {
    # nuke live games if specified, which is the default
    if (exclude_live) {
      status <- game_event$isLive
      if (status == TRUE) next
    }
    # check for fixed odds markets, skip if they're not there
    if (!'fixedOddsMarkets' %in% names(game_event)) next
    fixed_odds_markets <- game_event$fixedOddsMarkets
    event_names <- unlist(lapply(fixed_odds_markets, '[[', 'eventName'))
    # get game lines
    if (game_lines == TRUE) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_game_lines(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names, exclude_alts = exclude_alts)
      next
    }
    # now extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = 'First Team to Score')
      next
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = '^First Basket$')
      next
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if ((!'output_list' %in% ls()) | length(output_list) == 0) stop('no pointsbet ', prop, ' props returned')
  output_df <- dplyr::bind_rows(output_list)
  return(output_df)
}
