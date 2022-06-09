parse_betrivers_data <- function(betrivers_data, sport, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, exclude_alts = FALSE) {
  # loop through betrivers_data and extract the correct prop
  output_list <- list()
  for (e in names(betrivers_data)) {
    # subset the game event
    game_event <- betrivers_data[[e]]
    # nuke live games if specified, which is the default
    if (exclude_live) {
      status <- game_event$state
      if (status == "STARTED") next
    }
    matchup <- game_event$name
    tipoff <- game_event$start
    if (game_lines == TRUE) {
      gl_out <- parse_br_game_lines(game_event = game_event, matchup = matchup, tipoff = tipoff)
      output_list[[length(output_list) + 1]] <- gl_out
      next
    }
    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_br_prop(game_event = game_event, category_name = 'Game', prop_name = "Next Team to Score - at Score 0-0",
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_br_prop(game_event = game_event, category_name = 'Player Specials', prop_name = 'Player to Score the First Field Goal of the Game',
                      matchup = matchup, tipoff = tipoff)
      next
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no betrivers ', prop, ' props returned')
  output_df <- do.call(rbind, output_list)
  return(output_df)
}

