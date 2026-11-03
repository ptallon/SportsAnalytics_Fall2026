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
                            animate = TRUE,
                            nflIds = c()) {
  
  if (n_distinct(game_df$gameId) != 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe with one gameId and one playId.')
  }
  
  if (n_distinct(game_df$playId) != 1) {
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
    filter(nflId %in% nflIds, frameId >= snap_frame) |>
    arrange(nflId, frameId) |>
    group_by(nflId) |>
    mutate(prior_x = ifelse(frameId >= snap_frame, lag(x), NA),
           prior_y = ifelse(frameId >= snap_frame, lag(y), NA)) |>
    ungroup() |>
    data.frame()
  
  if (nrow(route_df) == 0) {
    stop("No tracking data found for the selected players.")
  }
  
  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  yardLine <- unique(play_df$absoluteYardlineNumber) + 20
  
  g <- ggplot(data = route_df, aes(x = x, y = y, color=team, fill=team)) +
    scale_fill_manual(values = c("home" = "dodgerblue1",
                                 "away" = "firebrick1"), guide = "none") +
    scale_colour_manual(values = c("home" = "black",
                                   "away" = "black"),guide = "none") +
    scale_size_manual(values = c("home" = 6,
                                 "away" = 6), guide = "none") +
    scale_shape_manual(values = c("home" = 21, 
                                  "away" = 21), guide = "none") +
    
    gg_field(yardmin = max(-5,min(game_df$x)-5), yardmax = min(max(game_df$x)+5, 125) ) +
    
    # show the absolute line
    annotate("segment", x = yardLine, xend = yardLine, y = 0, yend = 53.33, colour = "yellow", linewidth = 1) +
    
    # Connect each coordinate to its previous coordinate
    geom_segment(
      aes(
        x = prior_x,
        y = prior_y,
        xend = x,
        yend = y
      ),
      linewidth = 1
    ) +
    
    # Show a marker at each coordinate
    geom_point(size = 2.5) +
    
    coord_fixed() +
    
    # Black jersey numbers for players not highlighted
    geom_text(
      data = route_df,
      aes(x = x, y = y, label = jerseyNumber),
      colour = "black",
      size = 3.2,
      vjust = 0.36
    ) +
    
    labs(title = first(route_df$playDescription)) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none") 

}
