old_props_list <- readRDS('/Users/jim/Documents/betfinder/inst/config/props/props_list.rds')

fd_props_to_add <- data.frame(
  sport = 'nba',
  site = 'fd',
  prop = c('player pts-reb-ast ou', 'player pts-ast ou', 'player pts-reb ou', 'player reb-ast ou'),
  active = 1
)

pb_props_to_add <- data.frame(
  sport = 'nba',
  site = 'pb',
  prop = c('player pts-reb-ast ou', 'player pts-ast ou', 'player pts-reb ou', 'player reb-ast ou', 'player double double', 'player triple double'),
  active = 1
)


new_props <- rbind(old_props_list, fd_props_to_add, pb_props_to_add)

saveRDS(new_props, '/Users/jim/Documents/betfinder/inst/config/props/props_list.rds')

saveRDS(old_props_list, '/Users/jim/Documents/betfinder/inst/config/props/archived_props_list.rds')
