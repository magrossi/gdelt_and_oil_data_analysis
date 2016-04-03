# ################################################################################################ #
# CA683 - Assignment 2 - GDELT loader script
#
# Loads the GDELT data using the GDELTtools and takes into account the missing day of 2014-03-19
# It will save the data into an rds file with name gdelt_{year}.rds or gdelt_{start}_to_{end}.rds
# To use from another script use: source("gdelt_loader.r")
# ################################################################################################ #
# Example:
# To read all years from 2000 to 2016 for example do:
# mydata <- load_gdelt_year(2000, 2016, filter, local_folder)

# Packages Used
# install.packages("GDELTtools")
require("GDELTtools")

# ################################################################################################ #
#
# Automatically loads the data from start_year to end_year and saves the files into ./data/ folder
#
# ################################################################################################ #
load_gdelt_year_auto <- function(start_year, end_year, filter) {
  gdelt <- NULL
  for (i in seq(start_year,end_year)) {
    cat(paste("Processing year ", as.character(i), "\n", sep = ""))
    temp <- NULL
    file_name <- paste("./data/gdelt_", i, ".rds", sep = "")
    if (file.exists(file_name)) {
      temp <- readRDS(file_name)
      cat(paste("Data for year", as.character(i), "already exists\n"))
    } else {
      temp <- load_gdelt_year(i, filter=filter, local_folder="./data/gdelt/")
      
      # Save pre-filtered yearly GDELT database to file
      saveRDS(temp, file_name)
      
      cat(paste("Saving full data for year", as.character(i), "\n"))
    }
    
    # Filtering data

    # Get only unconvencial violence events (mass killings, etc.)
    # And remove unused columns
    filtered <- temp[
      temp$EventRootCode==20,
      c("Date", "EventCode", "GoldsteinScale", "NumMentions", "NumSources", "NumArticles", "AvgTone", "ActionGeo_CountryCode", "SOURCEURL")]
    
    # Aggregate events by date and count
    gdelt_summary.count <- aggregate(filtered, by = list(filtered$Date), length)
    gdelt_summary.mean <- aggregate(filtered, by = list(filtered$Date), mean, na.action = na.omit)
    
    # Merge all GDELT indicators into one variable
    indicators <- cbind(
      # Event Count
      xts(gdelt_summary.count[,c("Date")],gdelt_summary.count[,1]),
      # Event Means
      xts(gdelt_summary.mean[,c("GoldsteinScale", "NumMentions", "NumArticles", "AvgTone")], gdelt_summary.mean[,1])
    )
    # Normalize Names
    names(indicators) <- c("EventCount", "GoldsteinScale", "NumMentions", "NumArticles", "AvgTone")
    
    
    # Append the new data to our gdelt accumulated data
    cat("Accumulating fully filtered yearly data\n")
    gdelt <- rbind(gdelt, indicators)
  }

  return(gdelt)
}

# ################################################################################################ #
#
# Load GDELT year and adds a Date column to it with the proper date type
#
# ################################################################################################ #
load_gdelt_year <- function(start_year, end_year=NA, filter=NA, local_folder=NA) {
  start_year = as.numeric(start_year)
  if (is.na(end_year)) {
    end_year = start_year
  } else {
    end_year = as.numeric(end_year)
  }
  
  end_date = as.Date(paste(as.character(end_year),"-12-31", sep = ""))
  if (end_date >= Sys.Date()) {
    end_date = as.character(Sys.Date()-1)
  }
  
  # There is a missing day in the GDELT data (2014-03-19) so we must skip this day or the GDELTools will fail
  gdelt <- NA
  if(start_year == 2014 | end_year == 2014 | (start_year < 2014 & end_year > 2015)) {
    gdelt_1 <- GetGDELT(
      start.date=paste(as.character(start_year), "-01-01", sep = ""),
      end.date="2014-03-18",
      filter=filter,
      local.folder=local_folder)

    gdelt_2 <- GetGDELT(
      start.date="2014-03-20",
      end.date=end_date,
      filter=filter,
      local.folder=local_folder)
    
    gdelt <- rbind(gdelt_1, gdelt_2)
  } else {
    gdelt <- GetGDELT(
      start.date=paste(start_year, "-01-01", sep = ""),
      end.date=end_date,
      filter=filter,
      local.folder=local_folder)
  }
  
  # Add Date column for time series analysis
  gdelt$Date <- as.Date(as.character(gdelt$SQLDATE), format="%Y%m%d")

  # Return gdelt data frame
  return(gdelt)
}