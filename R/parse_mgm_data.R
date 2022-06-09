parse_mgm_data <- function(mgm_data, sport, prop = FALSE, game_lines = FALSE) {

  # loop through mgm_data and extract the correct prop
  output_list <- list()
  for (e in 1:length(mgm_data)) {
    # subset the game event
    game_event <- mgm_data[[e]]$fixture
    matchup <- paste(game_event$participants$name.value, collapse = ' @ ')
    tipoff <- game_event$startDate
    # # get the game lines if you're trying to do that
    # if (game_lines == TRUE) {
    #   gl_out <- parse_mgm_main(game_event = game_event, matchup = matchup, tipoff = tipoff)
    #   output_list[[length(output_list) + 1]] <-
    #     parse_mgm_main(game_event = game_event, matchup = matchup, tipoff = tipoff)
    #   next
    # }
    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_mgm_prop(game_event = game_event, prop_name = "Next Team to Score - at Score 0-0",
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_mgm_prop(game_event = game_event, prop_name = 'First Field Goal Scorer',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('fpts by team')) {
      output_list[[length(output_list) + 1]] <-
        parse_mgm_prop(game_event = game_event,
                       prop_regex = 'First Field Goal -', prop_not_regex = 'Exact Method',
                       matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('fpts exact method')) {
      output_list[[length(output_list) + 1]] <-
        parse_mgm_prop(game_event = game_event,
                       prop_name = 'First Field Goal - Exact Method',
                       matchup = matchup, tipoff = tipoff)
    }
  }

  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no mgm ', prop, ' props returned')
  output_df <- do.call(rbind, output_list)

  return(output_df)
}

