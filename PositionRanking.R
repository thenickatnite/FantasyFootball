# Plotting the top 25 players of each position group in total fantasy points
# Goal is to show the relative scarcity of quality tight ends versus other skill
# positions like WR, RB, and QB
# General Approach: Use data base tool to get 2020 play by play. Get total 0.5
# PPR scoring. Then break into different skill groups and get the top 25. 
# Then bind the groups on top of each other. Making use of the tidyverse for data
# wrangling and nflfastR for easy pulling of NFL data.

# Necessary Libraries 
library(tidyverse)
library(nflfastR)
library(DBI)
library(RSQLite)

# Function for 0.5 ppr
HalfPointPPR <- function( df ){
  df <- df %>% 
    dplyr::mutate( fantasy_points_HP_ppr = fantasy_points + ( 0.5 * receptions ) )
  
  return( df )
}

# Set up data base connection and update DB
wd <- '/Users/NickBrown/Desktop/FantasyFootball'
nflfastR::update_db( dbdir = wd )

connection <- DBI::dbConnect( RSQLite::SQLite(), '/Users/NickBrown/Desktop/FantasyFootball/pbp_db' )

# Get output from database
pbp_db <- dplyr::tbl( connection, 'nflfastR_pbp' )

# Get 2020 data
pbp_2020 <- pbp_db %>% 
  dplyr::filter( season == 2020 ) %>% 
  dplyr::collect()

DBI::dbDisconnect( connection )

# Now get 2020 fantasy stats
top_fp_2020 <- pbp_2020 %>% 
  dplyr::filter( week <= 16 ) %>% 
  nflfastR::calculate_player_stats() %>% 
  HalfPointPPR() %>% 
  dplyr::mutate(
    ppg = fantasy_points_HP_ppr / games
  ) %>% 
  dplyr::filter( games > 5,
                 fantasy_points_HP_ppr > 10) %>% 
  dplyr::inner_join(
    nflfastR::fast_scraper_roster( 2020 ) %>% select( gsis_id, position ),
    by = c( "player_id" = "gsis_id" )
  )

# Now get top 25 for each group. Going to write this as a function.
position <- 'QB'

top25PlayersPos <- function( df, pos ){
  posFilter <- as.character(pos)
  
  df <- df %>% 
    dplyr::filter(position == !!pos) %>% 
    dplyr::arrange(-fantasy_points_HP_ppr) %>% 
    dplyr::select(player_name, position, games, fantasy_points_HP_ppr) %>% 
    dplyr::mutate( Ranking = as.numeric(row.names(.))) %>% 
    utils::head(25)
  
  return(df)
}

positions <- list('QB', 'RB', 'WR', 'TE')
topPlayersDf <- lapply(positions, top25PlayersPos, df = top_fp_2020) %>% 
  dplyr::bind_rows()

# Now Plot
ggplot(data = topPlayersDf, aes(x = Ranking, y = fantasy_points_HP_ppr,
                                group = position, color = position)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = 'Top 25 Players by Position',
       subtitle = '2020 Season (0.5 PPR)') +
  xlab('Player Ranking') +
  ylab('Total Fantasy Points')
