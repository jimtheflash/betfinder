parse_fanduel_data <- function(fanduel_data, sport, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, exclude_alts = FALSE, game_part = "full") {

  # loop through fanduel_data and extract the correct prop
  output_list <- list()
  for (e in names(fanduel_data)) {
    # subset the game event
    game_event <- fanduel_data[[e]]

    # nuke live games if specified, which is the default
    if (exclude_live) {
      status <- game_event$main$attachments$events[[e]]$inPlay
      if (status == TRUE) next
    }

    matchup <- game_event$main$attachments$events[[e]]$name
    tipoff <- game_event$main$attachments$events[[e]]$openDate

    if (game_lines == TRUE) {
      if (sport %in% c('nba', 'ncaaf', 'nfl', 'mlb', 'nhl')) {
        output_list[[length(output_list) + 1]] <-
          parse_fd_game_lines(game_event, matchup = matchup, tipoff = tipoff, exclude_alts = exclude_alts, game_part = game_part)
      }
    }

    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      # get the right tab
      if (sport == 'mlb') tab_name <- 'hits_runs'
      if (sport == 'nba') tab_name <- '1st_quarter'
      if (sport == 'nfl') tab_name <- 'main'
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = tab_name, prop_name = 'Team to Score First',
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'First Basket',
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('fpts by team')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'first_basket', prop_name = 'First Team Basket Scorer',
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('first team to score q2')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = '2nd_quarter', prop_name = '2nd Quarter Team to Score First',
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('first team to score q3')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = '3rd_quarter', prop_name = '3rd Quarter Team to Score First',
                      matchup = matchup, tipoff = tipoff)
      next
    }
    if (prop %in% c('first team to score q4')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = '4th_quarter', prop_name = '4th Quarter Team to Score First',
                      matchup = matchup, tipoff = tipoff)
      next
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no fanduel ', prop, ' props returned')
  # bind up the rows
  output_df <- dplyr::bind_rows(output_list)
  # keep complete cases
  output_df <- output_df[complete.cases(output_df), ]
  return(output_df)
  }

