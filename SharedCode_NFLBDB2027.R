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
  
  load_packages(c(
    "dplyr", "ggplot2", "gganimate",
    "gifski", "ggnewscale"
  ))
  
  # Check required columns
  required_columns <- c(
    "game_id", "play_id", "nfl_id", "frameId",
    "event", "x", "y", "nfl_position",
    "target", "yardline_number"
  )
  
  missing_columns <- setdiff(required_columns, names(play_df))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Check that there is only one game and one play
  if (n_distinct(play_df$game_id) != 1) {
    stop("Please pass a dataframe containing only one game_id.")
  }
  
  if (n_distinct(play_df$play_id) != 1) {
    stop("Please pass a dataframe containing only one play_id.")
  }
  
  # Check that player IDs have been supplied
  if (length(nflIds) == 0) {
    stop("Please specify at least one nfl_id.")
  }
  
  # Identify the frame where the ball was snapped
  snap_frames <- play_df |>
    filter(event == "ball_snap", !is.na(frameId)) |>
    pull(frameId)
  
  if (length(snap_frames) == 0) {
    stop("No ball_snap event was found in the dataframe.")
  }
  
  snap_frame <- min(snap_frames)
  
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
      prior_y = lag(y),
      border_color = if_else(
        target %in% TRUE,
        "black",
        "transparent"
      )
    ) |>
    ungroup()
  
  if (nrow(route_df) == 0) {
    stop("No tracking data found for the selected players.")
  }
  
  # Check that there are at least two frames for animation
  if (animate && n_distinct(route_df$frameId) < 2) {
    stop("At least two frames are required for animation.")
  }
  
  # Load the football field function
  source(
    "https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R"
  )
  
  # Establish field boundaries
  yard_min <- max(-5, min(route_df$x) - 5)
  yard_max <- min(125, max(route_df$x) + 5)
  
  # Prepare route segments
  segment_df <- route_df |>
    filter(!is.na(prior_x), !is.na(prior_y))
  
  # Identify the final recorded position of each player
  final_df <- route_df |>
    group_by(nfl_id) |>
    slice_max(
      order_by = frameId,
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup()
  
  # Create the base plot
  g <- ggplot() +
    
    # Draw the football field
    gg_field(
      yardmin = yard_min,
      yardmax = yard_max
    ) +
    
    # Set the football field and legend backgrounds
    theme(
      panel.background = element_rect(
        fill = "forestgreen",
        color = "forestgreen"
      ),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key = element_rect(
        fill = "white",
        color = NA
      ),
      legend.background = element_rect(
        fill = "white",
        color = NA
      )
    )
  
  if (animate) {
    
    # Identify the available frames
    frames <- sort(unique(route_df$frameId))
    
    # Create cumulative route segments for each frame
    animation_df <- lapply(frames, function(f) {
      
      segment_df |>
        filter(frameId <= f) |>
        mutate(animation_frame = f)
      
    }) |>
      bind_rows()
    
    # Identify each player's current position at each frame
    current_df <- route_df |>
      mutate(animation_frame = frameId)
    
    # Draw cumulative routes
    g <- g +
      
      geom_segment(
        data = animation_df,
        aes(
          x = prior_x,
          y = prior_y,
          xend = x,
          yend = y,
          color = nfl_position
        ),
        linewidth = 1,
        key_glyph = "point"
      ) +
      
      # Display colored circles in the legend
      scale_color_discrete(
        name = "NFL Position",
        guide = guide_legend(
          override.aes = list(
            shape = 16,
            size = 5
          )
        )
      ) +
      
      # Start a new color scale for marker borders
      ggnewscale::new_scale_color() +
      
      # Draw the large moving circle
      geom_point(
        data = current_df,
        aes(
          x = x,
          y = y,
          fill = nfl_position,
          color = border_color
        ),
        shape = 21,
        size = 6,
        stroke = 1.5
      ) +
      
      scale_color_identity(guide = "none") +
      scale_fill_discrete(guide = "none") +
      
      transition_manual(animation_frame)
    
    animation <- gganimate::animate(
      g,
      fps = 10,
      nframes = length(frames),
      width = 480,
      height = 280,
      renderer = gifski_renderer()
    )
    
    return(animation)
    
  } else {
    
    # Draw all route segments through the final position
    g <- g +
      
      geom_segment(
        data = segment_df,
        aes(
          x = prior_x,
          y = prior_y,
          xend = x,
          yend = y,
          color = nfl_position
        ),
        linewidth = 1,
        key_glyph = "point"
      ) +
      
      # Display colored circles in the legend
      scale_color_discrete(
        name = "NFL Position",
        guide = guide_legend(
          override.aes = list(
            shape = 16,
            size = 5
          )
        )
      ) +
      
      # Start a new color scale for marker borders
      ggnewscale::new_scale_color() +
      
      # Draw only the final circle for each player
      geom_point(
        data = final_df,
        aes(
          x = x,
          y = y,
          fill = nfl_position,
          color = border_color
        ),
        shape = 21,
        size = 6,
        stroke = 1.5
      ) +
      
      scale_color_identity(guide = "none") +
      scale_fill_discrete(guide = "none")
    
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
  
  pp <- fread(paste0(directory,"/player_play.csv"))
  
  df <- df |>
    left_join(pp |> select(game_id, play_id, nfl_id, target,
                           yardline_number, play_direction),
              by = c("game_id", "play_id", "nfl_id")) |>
    mutate( x = ifelse(play_direction == "right", 120-x, x),
            y = ifelse(play_direction == "right", 160/3-y, y)
    )
  
  return(df)
}
  
# ---------------------------------------------------------------------------------
# -------------------------- Route Metrics -----------------------------------
# ---------------------------------------------------------------------------------
# calculate summary route stats for each route for each play

route_statistics <- function(play_df = play_df) {
  play_df <- play_df |>
    arrange(game_id, play_id, nfl_id, frameId) |>
    group_by(game_id, play_id, nfl_id) |>
    summarise(total_dis  = sum(dis),
              euclid_dis = sqrt((first(x) - last(x))^2 +
                                (first(y) - last(y))^2),
              play_time  = max(frameId, na.rm=TRUE),
              aver_speed = total_dis / play_time,
              dist_first_2_seconds = ifelse(),
              speed_first_2_seconds = ifelse(),
              max_deceler = min(a, na.rm=TRUE),
              .groups = 'keep') |>
    data.frame()
  return(play_df)
}

# ---------------------------------------------------------------------------------
# -------------------------- Separation Metrics -----------------------------------
# ---------------------------------------------------------------------------------
# calculate WR separation metrics for each frame of a play

calculate_wr_separation <- function(game_tracking,
                                    wr_cuts,
                                    wr_list,
                                    frame_rate = 10,
                                    before_seconds = 0.3,
                                    after_seconds = 0.5) {
  
  # -------------------------------------------------------
  # 1. Validate required columns
  # -------------------------------------------------------
  
  required_tracking <- c(
    "game_id", "play_id", "nfl_id",
    "frameId", "x", "y", "team"
  )
  
  required_cuts <- c(
    "game_id", "play_id", "nfl_id", "cut_frame"
  )
  
  if (!all(required_tracking %in% names(game_tracking))) {
    stop("game_tracking is missing required columns.")
  }
  
  if (!all(required_cuts %in% names(wr_cuts))) {
    stop("wr_cuts is missing required columns.")
  }
  
  if (!all(c("nfl_id", "display_name") %in% names(wr_list))) {
    stop("wr_list must contain nfl_id and display_name.")
  }
  
  if (frame_rate <= 0 ||
      before_seconds < 0 ||
      after_seconds <= 0) {
    stop("Invalid frame rate or time-window parameters.")
  }
  
  # -------------------------------------------------------
  # 2. Identify WR tracking observations
  # -------------------------------------------------------
  
  wr_tracking <- game_tracking |>
    semi_join(
      wr_list |> distinct(nfl_id),
      by = "nfl_id"
    ) |>
    semi_join(
      wr_cuts |> distinct(game_id, play_id, nfl_id),
      by = c("game_id", "play_id", "nfl_id")
    ) |>
    filter(
      team %in% c("home", "away"),
      !is.na(frameId),
      !is.na(x),
      !is.na(y)
    ) |>
    select(
      game_id, play_id, nfl_id,
      frameId, x, y, team
    ) |>
    distinct(
      game_id, play_id, nfl_id, frameId,
      .keep_all = TRUE
    ) |>
    rename(
      wr_x = x,
      wr_y = y,
      wr_team = team
    )
  
  # -------------------------------------------------------
  # 3. Identify potential defenders
  # -------------------------------------------------------
  
  defenders <- game_tracking |>
    semi_join(
      wr_tracking |> distinct(game_id, play_id),
      by = c("game_id", "play_id")
    ) |>
    filter(
      team %in% c("home", "away"),
      !is.na(nfl_id),
      !is.na(frameId),
      !is.na(x),
      !is.na(y)
    ) |>
    select(
      game_id, play_id, nfl_id,
      frameId, x, y, team
    ) |>
    distinct(
      game_id, play_id, nfl_id, frameId,
      .keep_all = TRUE
    ) |>
    rename(
      defender_id = nfl_id,
      defender_x = x,
      defender_y = y,
      defender_team = team
    )
  
  # -------------------------------------------------------
  # 4. Match each WR to opposing-team players
  # -------------------------------------------------------
  
  separation_pairs <- wr_tracking |>
    inner_join(
      defenders,
      by = c("game_id", "play_id", "frameId"),
      relationship = "many-to-many"
    ) |>
    filter(wr_team != defender_team) |>
    mutate(
      separation = sqrt(
        (wr_x - defender_x)^2 +
          (wr_y - defender_y)^2
      )
    )
  
  # -------------------------------------------------------
  # 5. Keep nearest defender at each frame
  # -------------------------------------------------------
  
  wr_separation <- separation_pairs |>
    group_by(game_id, play_id, nfl_id, frameId) |>
    slice_min(
      separation,
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup() |>
    left_join(
      wr_list |>
        select(nfl_id, display_name) |>
        distinct(nfl_id, .keep_all = TRUE),
      by = "nfl_id"
    ) |>
    select(
      display_name,
      game_id, play_id, nfl_id,
      frameId,
      wr_x, wr_y,
      defender_id,
      defender_x, defender_y,
      separation
    ) |>
    arrange(game_id, play_id, nfl_id, frameId)
  
  # -------------------------------------------------------
  # 6. Define time windows around detected cuts
  # -------------------------------------------------------
  
  cut_windows <- wr_cuts |>
    mutate(
      cut_id = row_number(),
      before_frame = cut_frame -
        round(before_seconds * frame_rate),
      after_frame = cut_frame +
        round(after_seconds * frame_rate)
    ) |>
    select(
      cut_id, game_id, play_id, nfl_id,
      cut_frame, before_frame, after_frame,
      any_of(c(
        "cut_angle", "turn_rate",
        "speed_before", "speed_after",
        "speed_retention", "cut_deceleration"
      ))
    )
  
  # -------------------------------------------------------
  # 7. Match tracking frames to each cut
  # -------------------------------------------------------
  
  cut_tracking <- cut_windows |>
    inner_join(
      wr_separation,
      by = c("game_id", "play_id", "nfl_id"),
      relationship = "many-to-many"
    ) |>
    filter(
      frameId >= before_frame,
      frameId <= after_frame
    )
  
  # -------------------------------------------------------
  # 8. Calculate separation statistics for each cut
  # -------------------------------------------------------
  
  cut_summary <- cut_tracking |>
    group_by(cut_id) |>
    arrange(frameId, .by_group = TRUE) |>
    summarise(
      separation_before = {
        v <- separation[frameId <= cut_frame]
        if (length(v) == 0) NA_real_ else first(v)
      },
      
      separation_at_cut = {
        v <- separation[frameId == cut_frame]
        if (length(v) == 0) NA_real_ else first(v)
      },
      
      separation_after = {
        v <- separation[frameId > cut_frame]
        if (length(v) == 0) NA_real_ else last(v)
      },
      
      max_separation_after = {
        v <- separation[frameId > cut_frame]
        if (length(v) == 0) NA_real_ else max(v, na.rm = TRUE)
      },
      
      .groups = "drop"
    )
  
  # -------------------------------------------------------
  # 9. Calculate separation gained
  # -------------------------------------------------------
  
  wr_cut_separation <- cut_windows |>
    left_join(cut_summary, by = "cut_id") |>
    mutate(
      separation_gained =
        separation_after - separation_before,
      
      max_separation_gained =
        max_separation_after - separation_before
    ) |>
    left_join(
      wr_list |>
        select(nfl_id, display_name) |>
        distinct(nfl_id, .keep_all = TRUE),
      by = "nfl_id"
    ) |>
    relocate(display_name, .after = nfl_id) |>
    arrange(game_id, play_id, nfl_id, cut_frame)
  
  # -------------------------------------------------------
  # 10. Return both dataframes
  # -------------------------------------------------------
  
  return(
    list(
      wr_separation = as.data.frame(wr_separation),
      wr_cut_separation = as.data.frame(wr_cut_separation)
    )
  )
}