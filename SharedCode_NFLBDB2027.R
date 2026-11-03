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
  
  load_packages(c("dplyr", "ggplot2", "gganimate", "gifski"))
  
  # Check that there is only one game and one play
  if (n_distinct(play_df$game_id) != 1) {
    stop("Please pass a dataframe containing only one gameId.")
  }
  
  if (n_distinct(play_df$play_id) != 1) {
    stop("Please pass a dataframe containing only one playId.")
  }
  
  # Check that player IDs have been supplied
  if (length(nflIds) == 0) {
    stop("Please specify at least one nfl_id.")
  }
  
  # Identify the frame where the ball was snapped
  snap_frames <- play_df |>
    filter(event == "ball_snap") |>
    pull(frameId)
  
  if (length(snap_frames) == 0 || all(is.na(snap_frames))) {
    stop("No ball_snap event was found in the dataframe.")
  }
  
  snap_frame <- min(snap_frames, na.rm = TRUE)
  
  # Keep selected players from ball_snap to the last frame
  route_df <- play_df |>
    filter(
      nfl_id %in% nflIds,
      frameId >= snap_frame,
      !is.na(x),
      !is.na(y),
      !is.na(frameId)
    ) |>
    arrange(nfl_id, frameId) |>
    group_by(nfl_id) |>
    mutate(
      prior_x = lag(x),
      prior_y = lag(y)
    ) |>
    ungroup() |>
    data.frame()
  
  if (nrow(route_df) == 0) {
    stop("No tracking data found for the selected players.")
  }
  
  # Check that position information exists
  if (!"nfl_position" %in% names(route_df)) {
    stop("The dataframe must contain an nfl_position column.")
  }
  
  # Load the football field function
  source(
    "https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R"
  )
  
  # Identify the line of scrimmage
  yardLine <- unique(play_df$yardline_number) + 20
  
  # Establish field boundaries
  yard_min <- max(-5, min(route_df$x, na.rm = TRUE) - 5)
  yard_max <- min(125, max(route_df$x, na.rm = TRUE) + 5)
  
  # Create the base plot
  g <- ggplot(
    data = route_df,
    aes(
      x = x,
      y = y,
      color = nfl_position,
      group = nfl_id
    )
  ) +
    
    gg_field(
      yardmin = yard_min,
      yardmax = yard_max
    ) +
    
    # Show the line of scrimmage
    annotate(
      "segment",
      x = yardLine,
      xend = yardLine,
      y = 0,
      yend = 53.33,
      colour = "yellow",
      linewidth = 1
    ) +
    
    # Create a color legend based on NFL position
    scale_color_discrete(name = "NFL Position") +
    
    # Set the football field background
    theme(
      panel.background = element_rect(
        fill = "forestgreen",
        color = "forestgreen"
      ),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  if (animate) {
    
    # Show the route developing frame by frame
    g <- g +
      geom_path(linewidth = 1) +
      geom_point(size = 5) +
      transition_reveal(frameId)
    
    frames_to_display <- n_distinct(route_df$frameId)
    
    fps <- 10
    
    animation <- gganimate::animate(
      g,
      fps = fps,
      nframes = max(2, frames_to_display),
      width = 600,
      height = 350,
      renderer = gifski_renderer()
    )
    
    return(animation)
    
  } else {
    
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
      ) +
      
      # Show a marker at each coordinate
      geom_point(size = 5)
    
    return(g)
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
  
