# helper functions for shiny app

# functions to save and load persistent data to and from dropbox
# shamelessly copied from https://shiny.rstudio.com/articles/persistent-data-storage.html
# authentication with rdrop2
# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
# Then pass the token to each drop_ function
# drop_acc(dtoken = token)

token <- readRDS("droptoken.rds")
outputDir <- "trivia_winnings"
saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # fileName <- "trivia_winnings.csv"
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir, dtoken = token)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir, dtoken = token)
  filePaths <- filesInfo$path_display
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE, dtoken = token)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}
get_date_MST <- function(){
  rn <- lubridate::now(tz = "MST")
  lubridate::ymd(
    paste0(lubridate::year(rn), "-", lubridate::month(rn), "-", lubridate::day(rn))
  )
}
