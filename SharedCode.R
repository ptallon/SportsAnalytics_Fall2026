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
# -------------------- Add Voronoi Tessellation to each Frame ----------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe and include Tessellation Polygons for each player for each frame
make_voronoi <- function(frame_df) {
  
  # Exclude football and players without valid coordinates
  frame_df <- frame_df |>
    filter(
      team != "football",
      !is.na(nflId),
      !is.na(x),
      !is.na(y)
    ) |>
    distinct(nflId, .keep_all = TRUE)
  
  if (nrow(frame_df) < 2) return(NULL)
  
  # Get bounding box coordinates
  xmin <- first(frame_df$xmin)
  xmax <- first(frame_df$xmax)
  ymin <- first(frame_df$ymin)
  ymax <- first(frame_df$ymax)
  
  # Check bounding box validity
  if (
    any(!is.finite(c(xmin, xmax, ymin, ymax))) ||
    xmin >= xmax ||
    ymin >= ymax
  ) {
    return(NULL)
  }
  
  # Create bounding box as an sfc polygon
  bbox <- st_sfc(
    st_polygon(list(rbind(
      c(xmin, ymin),
      c(xmax, ymin),
      c(xmax, ymax),
      c(xmin, ymax),
      c(xmin, ymin)
    )))
  )
  
  # Check for duplicate player positions
  if (
    nrow(distinct(frame_df, x, y)) != nrow(frame_df)
  ) {
    warning(
      "Duplicate player coordinates in frame ",
      first(frame_df$frameId),
      " for team ",
      first(frame_df$team)
    )
    return(NULL)
  }
  
  # Convert player coordinates into sf points
  players_sf <- st_as_sf(
    frame_df,
    coords = c("x", "y"),
    remove = FALSE
  )
  
  # Create a MULTIPOINT geometry
  player_points <- st_combine(
    st_geometry(players_sf)
  )
  
  # Create an envelope larger than the clipping boundary
  margin <- max(
    xmax - xmin,
    ymax - ymin,
    1
  ) * 2
  
  envelope <- st_as_sfc(
    st_bbox(c(
      xmin = xmin - margin,
      ymin = ymin - margin,
      xmax = xmax + margin,
      ymax = ymax + margin
    ))
  )
  
  # Generate Voronoi tessellation
  voronoi <- st_voronoi(
    player_points,
    envelope = envelope
  )
  
  # Extract individual polygon geometries
  voronoi_polygons <- st_collection_extract(
    voronoi,
    "POLYGON"
  )
  
  # Convert to individual polygons if needed
  voronoi_polygons <- st_cast(
    voronoi_polygons,
    "POLYGON"
  )
  
  # Construct sf object
  polygon_sf <- st_sf(
    geometry = voronoi_polygons
  )
  
  # Match each Voronoi polygon to its player
  matches <- st_intersects(
    polygon_sf,
    players_sf
  )
  
  player_index <- vapply(
    matches,
    function(i) {
      if (length(i) == 1) {
        i
      } else {
        NA_integer_
      }
    },
    integer(1)
  )
  
  # Validate polygon-player matching
  if (
    length(player_index) != nrow(players_sf) ||
    anyNA(player_index) ||
    anyDuplicated(player_index)
  ) {
    warning(
      "Unable to uniquely match Voronoi cells to players in frame ",
      first(frame_df$frameId),
      " for team ",
      first(frame_df$team)
    )
    return(NULL)
  }
  
  # Attach player IDs
  polygon_sf$nflId <- players_sf$nflId[player_index]
  
  # Create bounding box sf object
  bbox_sf <- st_sf(
    geometry = bbox
  )
  
  # Clip Voronoi polygons to bounding box
  clipped <- suppressWarnings(
    st_intersection(
      polygon_sf,
      bbox_sf
    )
  ) 
  
  # Add identifying variables
  clipped$gameId <- first(frame_df$gameId)
  clipped$playId <- first(frame_df$playId)
  clipped$frameId <- first(frame_df$frameId)
  clipped$team <- first(frame_df$team)
  if ("side" %in% names(frame_df)) {
    clipped$side <- first(frame_df$side)
  } 
  
  # Calculate area of each polygon
  clipped$voronoi_area <- as.numeric(
    st_area(clipped)
  )
  
  return(clipped) 
}

# ---------------------------------------------------------------------------------
# -------------------- Add Voronoi Polygons to a data frame -----------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe for a single play and add Voronoi polygons to it.
# bounding box information needs to be included in the game_df beforehand
add_voronoi_to_df <- function(game_df) {
  
  load_packages(c("sf"))
  
  # check for required columns
  
  required_columns <- c("gameId", "playId", "frameId", "team", 
                        "nflId", "x", "y", 
                        "xmin", "xmax", "ymin", "ymax")
  missing_columns <- setdiff(
    required_columns,
    names(game_df)
  )
  
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", ")
    )
  }
  
  # Calculate Voronoi polygons
  voronoi_df <- game_df |>
    filter(team != "football") |>
    group_by(gameId, playId, frameId, team) |>
    group_split() |>
    map(make_voronoi) |>
    compact() |>
    bind_rows()
  
  if (nrow(voronoi_df) == 0) {
    stop("No Voronoi polygons were generated.")
  }
  
  # Extract geometry and area 
  voronoi_attributes <- voronoi_df |>
    mutate(voronoi_polygon = st_geometry(.)) |>
    st_drop_geometry() |>
    select(gameId, playId, frameId, team, nflId,
           voronoi_area, voronoi_polygon)
  
  # left join polygons to original tracking data
  result <- game_df |>
    left_join(voronoi_attributes,
              by = c("gameId", "playId", "frameId", "team", "nflId"))
  
  # return the result
  return(result)
}

# ---------------------------------------------------------------------------------
# -------------------- Add bounding box to a single play --------------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe and include xmin, xmax, ymin, and ymax details
# you can specify how many yards you want the bounding box to be from the
# line of scrimmage at the ball snap, and thereafter
add_boundingbox_to_df <- function(game_df,
                                  BufferFromLineOfScrimmage = 0,
                                  BufferFromTopOfBoundingBox = 0,
                                  BufferFromBottomOfBoundingBox = 0,
                                  BufferFromSidesFurthestFromLineOfScrimmage = 0,
                                  GeneralBuffer = 0) {
  
  buffers <- c(
    BufferFromLineOfScrimmage, BufferFromTopOfBoundingBox,
    BufferFromBottomOfBoundingBox, BufferFromSidesFurthestFromLineOfScrimmage,
    GeneralBuffer
  )
  
  if (any(buffers < 0 | buffers > 10)) {
    stop("Buffer values must be between 0 and 10.")
  }
  
  load_packages("dplyr")
  
  if (n_distinct(game_df$gameId) != 1 || n_distinct(game_df$playId) != 1) {
    stop("The dataframe you're working with must contain only one gameId and one playId.\nPlease filter the dataframe before calling this function.")
  }
 
  group_by(gameId, playId, team, frameId) |>
  # set the line of scrimmage
  mutate(
    yardLine = first(absoluteYardlineNumber) + 20,
      
    # add in the original boundaries of the box before adding any buffers
    xmin = min(x, na.rm = 0), 
    xmax = max(x, na.rm = 0),
    ymin = min(y, na.rm = 0),
    ymax = max(y, na.rm = 0)) |>
  ungroup() |>
    
  # find the frame where the ball is snapped
  group_by(gameId, playId) |>
  mutate(snap_frame = min(frameId[event=="ball_snap"], na.rm=TRUE)) |>
  ungroup() |>
    
  # determine which side of the line of scrimmage a team is on
  group_by(gameId, playId, team) |>
  arrange(frameId, .by_group = TRUE) |>
  mutate(
      side = if_else((first(xmin) + first(xmax))/2 < first(yardLine),
                     "left",
                     "right")) |>
  ungroup() |>
    
  mutate(
  # set the left boundary for xmin
    xmin = case_when(
        #before and at the ball snap
        frameId <= snap_frame & side == "left" ~
          xmin - BufferFromSidesFurthestFromLineOfScrimmage,
        frameId <= snap_frame & side == "right" ~
          pmax(xmin - BufferFromSidesFurthestFromLineOfScrimmage, yardLine + BufferFromLineOfScrimmage),
        
        # after the ball snap, unrestricted expansion
        frameId > snap_frame ~
          xmin - GeneralBuffer,
        TRUE ~ xmin
      ),
      
    # set the boundary on the right
    xmax = case_when(
        #before and at the ball snap
        frameId <= snap_frame & side == "left" ~
          pmin(xmax + BufferFromSidesFurthestFromLineOfScrimmage, 
               yardLine - BufferFromLineOfScrimmage),
        frameId <= snap_frame & side == "right" ~
          xmax + BufferFromSidesFurthestFromLineOfScrimmage,
        
        #after the ball is snapped, no restrictions
        frameId > snap_frame ~
          xmax + GeneralBuffer,
        TRUE ~ xmax
    ),
      
    # expand the Y boundaries by a buffer without going over the sideline
    ymin = ifelse(ymin - BufferFromBottomOfBoundingBox < 0, ymin, 
                    ymin - BufferFromBottomOfBoundingBox),
    ymax = ifelse(ymax + BufferFromTopOfBoundingBox > 160/3, ymax, 
                    ymax + BufferFromTopOfBoundingBox)) |>
    ungroup() |>
    data.frame()
}

# ---------------------------------------------------------------------------------
# ---------------------- Animate a Single Play in a Game --------------------------
# ---------------------------------------------------------------------------------
# pass in a dataframe where you have filtered the gameId and playId.

visualize_single_play <- function(game_df,
                                  highlight_players_in_motion = FALSE,
                                  showBoundingBox = FALSE,
                                  showVoronoi = FALSE,
                                  show_targetXY = FALSE,
                                  saveAnimation = FALSE) {
  
  if(!length(unique(game_df$gameId)) == 1) {
    stop('There is more than one gameId in your data. Please pass in a dataframe for the exact gameId and playId you want to visualize.')
  } 
  
  if(showVoronoi & !showBoundingBox) {
    #show the bounding box around the Voronoi Tesselation
    showBoundingBox = TRUE
  }
  
  if (showVoronoi) {
    # Check that bounding box variables exist
    required_columns <- c("xmin", "xmax", "ymin", "ymax", "nflId")
    missing_columns <- setdiff(required_columns, names(game_df))
    
    if (length(missing_columns) > 0) {
      stop("Missing columns required for Voronoi tessellation: ",
           paste(missing_columns, collapse = ", "))
    }
    
    load_packages(c("dplyr", "purrr", "sf"))
    
    # Create Voronoi polygons for each team and frame
    voronoi_df <- game_df |>
      filter(team != "football") |>
      group_by(gameId, playId, frameId, team) |>
      group_split() |>
      map(make_voronoi) |>
      compact() |>
      bind_rows()
    
    # Convert Voronoi polygons to ordinary coordinates
    # for compatibility with gg_field and gganimate
    
    voronoi_df <- voronoi_df |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON", warn = FALSE) |>
      mutate(polygon_id = row_number())
    
    coords <- sf::st_coordinates(voronoi_df)
    
    voronoi_plot_df <- cbind(
      sf::st_drop_geometry(voronoi_df)[coords[, "L2"], ],
      x = coords[, "X"],
      y = coords[, "Y"],
      ring = coords[, "L1"]
    )
    
    if (nrow(voronoi_df) == 0) {
      stop("No Voronoi polygons were generated. Check your dataframe.")
    }
  }
  
  load_packages(c("hms", "stringr", "gganimate", "gifski", "sf", "dplyr"))

  source('https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R')
  
  yardLine <- unique(game_df$absoluteYardlineNumber) + 20
  
  g <- ggplot(data = game_df, aes(x = x, y = y, color=team, fill=team)) +
    # customize colors, shapes, and sizes of players and the football
    scale_fill_manual(values = c("home" = "dodgerblue1",
                                 "away" = "firebrick1",
                                 "football" = "#663300"), guide = "none") +
    
    scale_colour_manual(values = c("home" = "black",
                                   "away" = "black",
                                   "football" = "#663300"),guide = "none") +
    
    scale_size_manual(values = c("home" = 6, 
                                 "football" = 4,
                                 "away" = 6), guide = "none") +
    scale_shape_manual(values = c("home" = 21, 
                                  "football" = 16, 
                                  "away" = 21), guide = "none") +

    gg_field(yardmin = max(-5,min(game_df$x)-5), yardmax = min(max(game_df$x)+5, 125) ) +
    
    # show the absolute line
    annotate("segment", x = yardLine, xend = yardLine, y = 0, yend = 53.33, colour = "yellow", linewidth = 1) 

  # Add Voronoi polygons
  if (showVoronoi) {
    g <- g + geom_polygon(data = voronoi_plot_df,
                          aes(x = x, y = y, fill = team,
                              group = interaction(frameId, polygon_id, ring)
                              ),
                          inherit.aes = FALSE,
                          alpha = 0.30,
                          colour = "white",
                          linewidth = 0.4
      )
  }
  
  if(showBoundingBox) {
    g <- g +
      geom_rect(data = game_df |> filter(team != "football") |>
                  distinct(frameId, team, xmin, xmax, ymin, ymax),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    fill = team, color = team),
                alpha = 0.2, linewidth = 0.8, inherit.aes = FALSE)
  }
  
  # add points to plot for all players and the football
  g <- g +     
    geom_point(aes(shape = team, colour = team, size = team, fill = team))
  
  if(highlight_players_in_motion) {
    g <- g +
      geom_point(
        data = game_df |>
          filter(
            inMotionAtBallSnap == TRUE,
            team != "football"
          ),
        aes(x = x, y = y),
        colour = "white",
        fill = "black",
        size = 6,
        stroke=1, 
        shape = 21,
        inherit.aes = FALSE
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
    # Black jersey numbers for players not highlighted
    geom_text(
      data = game_df |>
        filter(
          team != "football",
          !highlight_players_in_motion |
            !(inMotionAtBallSnap %in% TRUE)
        ),
      aes(x = x, y = y, label = jerseyNumber),
      colour = "black",
      size = 3.2,
      vjust = 0.36
    ) 
    
    if(highlight_players_in_motion) {
      # White jersey numbers for highlighted players in motion
      g <- g + geom_text(data = game_df |>
                  filter(team != "football",
                         highlight_players_in_motion,
                         inMotionAtBallSnap %in% TRUE),
                aes(x = x, y = y, label = jerseyNumber),
                colour = "white",
                size = 3.2,
                vjust = 0.36)
      }
    
    # add some labels to report the play description
    g <- g + labs(title = first(game_df$playDescription)) +
    
    # set the theme to dark green to color the areas beyond the end zones
    theme(panel.background = element_rect(fill = "forestgreen", 
                                          color = "forestgreen"), panel.grid = element_blank()) +
    guides(alpha = "none") +
    transition_manual(frameId)
  
  frames_to_display <- length(unique(game_df$frameId))
  game_df$hms <- as_hms(game_df$time)
  game_df$seconds <- hour(game_df$hms)*3600 + minute(game_df$hms)*60 + second(game_df$hms)
  fps <- frames_to_display/(max(game_df$seconds) - min(game_df$seconds))

  anim <- animate(g, 
          fps=fps, 
          nframes = frames_to_display, 
          width = 480, 
          height = 280, 
          renderer = gifski_renderer() 
  ) 
  anim
  
  if(saveAnimation) {
    anim_save("my_animation.gif", animation = anim)
    print(paste("Animation saved as", getwd(), "/Mod2_Animation.gif"))
  }
}