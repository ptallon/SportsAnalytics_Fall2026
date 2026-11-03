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

# ---------------------------------------------------------------------------
# --------------------------- Visualize Route -------------------------------
# ---------------------------------------------------------------------------
# visualize the route taken by one or more players during a play

visualize_route <- function(play_df,
                            animate = FALSE,
                            nflIds = c()) {
  
  if (n_distinct(play_df$game_id) != 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe with one gameId and one playId.')
  }
  
  if (n_distinct(play_df$play_id) != 1) {
    stop('There is more than one playId in your data. Please pass in a dataframe with one gameId and one playId.')
  }
  
  if(length(nflIds) == 0) {
    stop('Please pass in a list of nflIDs of players you want to track.')
  }
  
  load_packages(c("dplyr", "ggplot2", "gganimate", "gifski"))
  
  # Identify the frame where the ball was snapped
  snap_frames <- play_df |>
    filter(event == "ball_snap") |>
    pull(frameId)
  
  if (length(snap_frames) == 0) {
    stop("No ball_snap event was found in the dataframe.")
  }
  
  snap_frame <- min(snap_frames, na.rm = TRUE)
  
  # Keep selected players from ball_snap to the last frame
  route_df <- play_df |>
    filter(nfl_id %in% nflIds, frameId >= snap_frame) |>
    arrange(nfl_id, frameId) |>
    group_by(nfl_id) |>
    mutate(prior_x = ifelse(frameId >= snap_frame, lag(x), NA),
           prior_y = ifelse(frameId >= snap_frame, lag(y), NA)) |>
    ungroup() |>
    data.frame()
  
  if (nrow(route_df) == 0) {
    stop("No tracking data found for the selected players.")
  }
  
  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  yardLine <- unique(play_df$yardline_number) + 20
  
  g <- ggplot(data = route_df, aes(x = x, y = y, color=nfl_position, group = nfl_id)) +
    
    gg_field(yardmin = max(-5,min(route_df$x)-5), yardmax = min(max(route_df$x)+5, 125) ) +
    
    # show the absolute line
    annotate("segment", x = yardLine, xend = yardLine, y = 0, yend = 53.33, colour = "yellow", linewidth = 1) 
  
  if (!animate) {
    # Connect each coordinate to its previous coordinate
    g <- g +
      geom_segment(
        aes(
          x = prior_x,
          y = prior_y,
          xend = x,
          yend = y
        ),
        linewidth = 1
      ) 
  }
  
  g <- g +

    scale_color_discrete(name = "NFL Position") +
    
    # Show a marker at each coordinate
    geom_point(size = 5) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none") 
  
  if (animate) {
    g <- g + transition_manual(frameId)
    
    frames_to_display <- length(unique(route_df$frameId))
    fps <- 10
    animate(g, 
            fps=fps, 
            nframes = frames_to_display, 
            width = 480, 
            height = 280, 
            renderer = gifski_renderer() 
    ) 
    
  } else {
    g
  }

}

# ---------------------------------------------------------------------------------
# -------------------------- Load Tracking Data -----------------------------------
# ---------------------------------------------------------------------------------
# read in up to three years of tracking data and bind into one df
load_tracking_data <- function(directory,
                               years = c(2023, 2024, 2025)) {
  load_packages(c("data.table", "purrr"))
  
  if (missing(directory)) {
    stop('You did not specify a directory for your data files. Where are your files located?')
  }

  if (!all(years %in% c(2023, 2024, 2025))) {
    stop("Valid years for tracking data are 2023 through 2025")
  }
  
  df <- years |>
    purrr::map_dfr(\(y) fread(paste0(directory,"/game_tracking_", y, ".csv")))
  
  df <- df |>
    arrange(game_id, play_id, time) |>
    group_by(game_id, play_id) |>
    mutate(frameId = dense_rank(time)) |>
    ungroup() |>
    data.frame()
  
  return(df)
}
  
