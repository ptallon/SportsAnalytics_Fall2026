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
# --------------------- Load & Merge Data for NFL BDB 2022 ------------------------
# ---------------------------------------------------------------------------------
# read in the data file for a given week and optionally merge with other data
# x and y coordinates are also revised in order to have consistent play directions
load_data_NFLBDB2022 <- function(directory,
                                 years   = c(),  
                                 merge   = F,
                                 columns = c()) {
  if (missing(directory)) {
    print('You did not specify a directory for your data files. Where are your files located?')
    stop('Function aborting: unable to run due to missing parameters.')
  }
  if (missing(years)) {
    print('You did not specify a vector of years for your data files.')
    stop('Function aborting: unable to run due to missing parameters.')
  }  
  load_packages(c("data.table", "dplyr"))
  
  # check directory for /
  directory <- sub("/$", "", directory)
  
  # Define allowable years
  valid_years <- 2018:2020
  
  # Check that all supplied years are valid
  if (!all(years %in% valid_years)) {
    stop("Invalid year. Years must be between 2018 and 2020.")
  }

  # create an empty data frame
  new_df <- data.frame()
  
  # load in the data for years
  for (year in years) {
    
    filename <- paste0(directory, "/tracking", year, ".csv")
    y <- fread(filename)
    
    new_df <- rbind(new_df, y)
    rm(y)
  }
  
  if (merge == T) {
    plays       <- fread(paste0(directory, "/plays.csv"))         
    players     <- fread(paste0(directory, "/players.csv"))       
    games       <- fread(paste0(directory, "/games.csv"))         
    
    new_df <- left_join(new_df, games,       by = c("gameId"))
    new_df <- left_join(new_df, plays,       by = c("gameId", "playId"))
    new_df <- left_join(new_df, players,     by = c("nflId", "displayName"))
    
    # based on the direction of the play, change the yard line numner
    new_df <- new_df %>%
      mutate(yardlineNumber = ifelse(playDirection == "right", 
                                      100 - yardlineNumber, 
                                      yardlineNumber)
            ) %>%
      data.frame()    
  }
  
  # based on the direction of the play, map the x and y coordinates to be consistently in one direction
  new_df <- new_df %>%
    mutate( x = ifelse(playDirection == "right", 120-x, x),
            y = ifelse(playDirection == "right", 160/3-y, y)
            ) %>%
    data.frame()
  
  
  if(length(columns) > 0) {
    for(col in columns) {
      if(!col %in% colnames(df)) {
        stop(paste("Check the columns you want as",col,"is not in the data frame. No data frame returned."))
      }
    }
    new_df <- new_df %>%
      select(unique(columns)) %>% data.frame()
  }
  return(new_df)
}

