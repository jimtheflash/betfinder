parse_barstool_data <- function(barstool_data, sport, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, exclude_alts = TRUE) {
  # loop through barstool_data and extract the correct prop
  output_list <- list()
  for (e in names(barstool_data)) {
    # subset the game event
    game_event <- barstool_data[[e]]
    matchup <- game_event$events[[1]]$name
    tipoff <- game_event$events[[1]]$start
    if (game_lines == TRUE) {
      gl_out <- parse_bs_game_lines(game_event = game_event, exclude_alts = exclude_alts, matchup = matchup, tipoff = tipoff)
      output_list[[length(output_list) + 1]] <- gl_out
      next
    }
    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_bs_prop(game_event = game_event, prop_name = "Next Team to Score - at Score 0-0",
                      matchup = matchup, tipoff = tipoff)
      next
    }

    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_bs_prop(game_event = game_event,
                      prop_name = 'Player to Score the First Field Goal of the Game',
                      matchup = matchup, tipoff = tipoff)
      next
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no barstool ', prop, ' props returned')
  output_df <- dplyr::bind_rows(output_list)
  return(output_df)
}

