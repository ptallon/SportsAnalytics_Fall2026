## Shared code for Sports Analytics Course

# ---------------------------------------------------------------------------
# --------------------------- Load Libraries --------------------------------
# ---------------------------------------------------------------------------
# supply a vector of desired packages. Packages will be installed if not already installed in RStudio

load_packages <- function(packages = c()) {
  if (length(packages) == 0) {
    print('You did not specify any packages/libraries to load')
  } 
  else {
    for (i in packages){
      if(! i %in% installed.packages()){
        install.packages(i, dependencies = TRUE)
      }
      suppressMessages(suppressWarnings(library(i, character.only=T)))
    }
  }
}      

# ---------------------------------------------------------------------------------
# --------------------- Load & Merge Data for a Single Week -----------------------
# ---------------------------------------------------------------------------------
# read in the data file for a given week and optionally merge with other data
# x and y coordinates are also revised in order to have consistent play directions
load_data_for_one_week <- function(directory,
                                   weekNumber = 1,
                                   merge      = F,
                                   columns    = c()) {
  
  if (missing(directory) | missing(weekNumber) | !weekNumber %in% seq(9) ) {
    if (missing(directory)) {
      print('You did not specify a directory for your data files. Where are your files located?')
    }
    if (missing(weekNumber) | !weekNumber %in% seq(9)  ) {
      print('You did not specify a valid week number (valid numbers are 1 to 9).')
    }
    stop('Function aborting: unable to run due to missing parameters.')
  }
  
  load_packages(c("data.table", "dplyr"))
  
  filename <- paste0(directory, "/tracking_week_",weekNumber,".csv")
  df <- fread(filename)
  
  if (merge == T) {
    plays       <- fread(paste0(directory, "/plays.csv"))           #  16124 rows x 50 columns
    players     <- fread(paste0(directory, "/players.csv"))         #   1697 rows x  7 columns
    games       <- fread(paste0(directory, "/games.csv"))           #    136 rows x  9 columns
    player_play <- fread(paste0(directory, "/player_play.csv"))     # 354727 rows x 50 columns
    
    df <- left_join(df, games,       by = c("gameId"))
    df <- left_join(df, plays,       by = c("gameId", "playId"))
    df <- left_join(df, players,     by = c("nflId", "displayName"))
    df <- left_join(df, player_play, by = c("gameId", "playId", "nflId"))
  }
  
  # based on the direction of the play, map the x and y coordinates to be consistently in one direction
  df <- df %>%
    mutate( x = ifelse(playDirection == "right", 120-x, x),
            y = ifelse(playDirection == "right", 160/3-y, y),
            targetX = ifelse(playDirection == "right", 120-targetX, targetX),
            absoluteYardlineNumber = ifelse(playDirection == "right", 100 - absoluteYardlineNumber, absoluteYardlineNumber)
    ) %>%
    data.frame()
  
  df  <- df %>%
    mutate( team = ifelse( club == homeTeamAbbr, "home", ifelse( club == "football", "football", "away"))) %>%
    mutate( team = factor(team, levels = c("away", "football", "home"))) %>%
    data.frame()
  
  if(length(columns) > 0) {
    for(col in columns) {
      if(!col %in% colnames(df)) {
        stop(paste("Check the columns you want as",col,"is not in the data frame. No data frame returned."))
      }
    }
    df <- df %>%
      select(unique(columns)) %>% data.frame()
  }
  return(df)
}

# ---------------------------------------------------------------------------------
# ---------------------- Animate a Single Play in a Game --------------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe where you have filtered the gameId and playId.

visualize_single_play <- function(game_df,
                                  highlight_players_in_motion = FALSE,
                                  showBoundingBox = FALSE,
                                  show_targetXY = FALSE) {
  
  if(!length(unique(game_df$gameId)) == 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe for the exact gameId and playId you want to visualize.')
  }  
  
  load_packages(c("hms", "stringr", "gganimate", "gifski"))
  
  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  yardLine <- unique(game_df$absoluteYardlineNumber) + 20
  
  g <- ggplot(data = game_df, aes(x = x, y = y, color=team, fill=team)) +
    # customize colors, shapes, and sizes of players and the football
    scale_size_manual(values = c(6, 4, 6), guide = "none") +
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = c("firebrick1", "black", "dodgerblue1", "white"), 
                      na.value = NA,
                      guide="none") + 
    scale_colour_manual(values = c("black", "#663300", "black","black"), 
                        na.value = NA,
                        guide="none") + 
    
    gg_field(yardmin = max(-5,min(game_df$x)-5), yardmax = min(max(game_df$x)+5, 125) ) +
    
    # show the absolute line
    annotate("segment", x = yardLine, xend = yardLine, y = 0, yend = 53.33, colour = "yellow", linewidth = 1) 
  
  if(showBoundingBox) {
    g <- g +
      geom_rect(data = game_df |> filter(team != "football") |>
                  distinct(frameId, team, xmin, xmax, ymin, ymax),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    fill = "white", color = "black"),
                alpha = 0.2, linewidth = 0.8, inherit.aes = FALSE)
  }
  
  # add points to plot for all players and the football
  g <- g +     
    geom_point(aes(shape = team, colour = team, size = team, fill = team))
  print("hi4")
  if(highlight_players_in_motion) {
    g <- g +
      geom_point(
        data = game_df |>
          filter(inMotionAtBallSnap == TRUE, team != "football"),
        aes(x = x, y = y,
        colour = ifelse(inMotionAtBallSnap == TRUE, "black", NA),
        fill = ifelse(inMotionAtBallSnap == TRUE, "black", NA)),
        size = 6, shape = 21
        #,inherit.aes = FALSE
      )
  }
  
  # show the target X and Y if both are not NA and the user wants to view this
  if(show_targetXY) {
    if( sum(!is.na(game_df$targetX)) != 0 & sum(!is.na(game_df$targetY)) != 0 ) {
      g <- g + 
        geom_point(aes(x = game_df$targetX, y = game_df$targetY), 
                   shape=21, size = 6, colour="black", fill="white", alpha = 0.5) +
        annotate("text", x = game_df$targetX, y = game_df$targetY, 
                 label = "X", colour = "black", hjust = 0.5, vjust = 0.4)
    }
  }
  
  g <- g +
    # insert jersey number for each player
    geom_text( data = game_df %>% filter(team != "football"),
               aes(x = x, y = y, 
                   label = jerseyNumber), colour = "white", size = 3.5, vjust = 0.36 ) +
    
    # add some labels to report the play description
    labs(title = game_df$playDescription) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none") +
    transition_time(frameId)
  
  frames_to_display <- length(unique(game_df$frameId))
  game_df$hms <- as_hms(game_df$time)
  game_df$seconds <- hour(game_df$hms)*3600 + minute(game_df$hms)*60 + second(game_df$hms)
  fps <- frames_to_display/(max(game_df$seconds) - min(game_df$seconds))
  
  animate(g, 
          fps=fps, 
          nframe = frames_to_display, 
          width = 480, 
          height = 280, 
          renderer = gifski_renderer() 
  ) 
}