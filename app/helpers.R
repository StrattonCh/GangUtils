# helper functions for shiny app

# functions to save and load persistent data to and from dropbox
# as of 2021, dropbox does not allow tokens without exp
# need to create a refresh token
# https://stackoverflow.com/questions/71393752/get-a-refresh-token-for-dropbox-api-using-rdrop2-and-drop-auth
library(rdrop2)
.dstate <- new.env(parent = emptyenv())
drop_auth_RT <- function(
    new_user = FALSE, key = "mmhfsybffdom42w",
    secret = "l8zeqqqgm1ne5z0", cache = TRUE,
    rdstoken = NA
) {
  if (new_user == FALSE & !is.na(rdstoken)) {
    if (file.exists(rdstoken)) {
      .dstate$token <- readRDS(rdstoken)
    }
    else {
      stop("token file not found")
    }
  }
  else {
    if (new_user && file.exists(".httr-oauth")) {
      message("Removing old credentials...")
      file.remove(".httr-oauth")
    }
    dropbox <- httr::oauth_endpoint(authorize = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
                                    access = "https://api.dropbox.com/oauth2/token")
    # added "?token_access_type=offline" to the "authorize" parameter so that it can return an access token as well as a refresh token
    dropbox_app <- httr::oauth_app("dropbox", key, secret)
    dropbox_token <- httr::oauth2.0_token(dropbox, dropbox_app,
                                          cache = cache)
    if (!inherits(dropbox_token, "Token2.0")) {
      stop("something went wrong, try again")
    }
    .dstate$token <- dropbox_token
  }
}
# shamelessly copied from https://shiny.rstudio.com/articles/persistent-data-storage.html
# authentication with rdrop2
# token <- drop_auth_RT()
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
token <- readRDS("droptoken.rds")
# drop_acc(dtoken = token)
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

# get date in MST on server-side
get_date_MST <- function(){
  rn <- lubridate::now(tz = "MST")
  lubridate::ymd(
    paste0(lubridate::year(rn), "-", lubridate::month(rn), "-", lubridate::day(rn))
  )
}
