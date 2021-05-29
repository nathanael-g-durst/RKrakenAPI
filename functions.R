######## [START] Packages requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c("jsonlite")
################################################################################

installedPackages <- installed.packages()

for (packageName in packagesNeeded) {
  packageExists <- is.element(packageName, installedPackages)
  if(packageExists != TRUE){
    install.packages(packageName)
    library(packageName, character.only = TRUE)
    print(paste(packageName, "has been installed and the library is loaded !"))
  } else {
    library(packageName, character.only = TRUE)
    print(paste(packageName, "is installed and the library is loaded !"))
  }
}

rm(installedPackages, packageName, packagesNeeded, packageExists)

######## [END] Packages requirements ########

######## [START] Kraken ########

########## [START] Server Time ##########

getTime <- function(format = "unix") {
  # Kraken API
  url <- "https://api.kraken.com/0/public/Time"
  # Get data
  jsonFile <- jsonlite::fromJSON(url)
  ## Error check
  if (length(jsonFile[["error"]]) > 0) {
    print(jsonFile[["error"]])
    stop()
  } else {
    # Format
    if (format == "unix") {
      date <- jsonFile[["result"]][[1]]
      result <- as.POSIXct(date, origin="1970-01-01")
    } else if (format == "rfc") {
      result <- jsonFile[["result"]][[2]]
    } else {
      stop()
    }
  }
  # Return results
  return(result)
}

# Test of the function
krakenTime <- getTime()

########## [END] Server Time ##########

########## [START] System Status ##########

getStatus <- function(data="both") {
  # Kraken API
  url <- "https://api.kraken.com/0/public/SystemStatus"
  # Get data
  jsonFile <- jsonlite::fromJSON(url)
  ## Error check
  if (length(jsonFile[["error"]]) > 0) {
    print(jsonFile[["error"]])
    stop()
  } else {
    # Format
    if (data == "status") {
      result <- jsonFile[["result"]][[1]]
    } else if (data == "timestamp") {
      result <- jsonFile[["result"]][[2]]
    } else if (data == "both") {
      result <- c(jsonFile[["result"]][[1]], jsonFile[["result"]][[2]])
    }
  }
  # Return results
  return(result)
}

# Test of the function
krakenStatus <- getStatus()

########## [END] System Status ##########

########## [START] Assets ##########

getAssets <- function(assets = "All") {
  # Variables definition
  result <- NULL
  x <- 1
  # Check assets
  if (assets[1] == "All") {
    # Kraken API
    url <- "https://api.kraken.com/0/public/Assets"
    # Get data
    jsonFile <- jsonlite::fromJSON(url)
    # Error check
    if (length(jsonFile[["error"]]) > 0) {
      print(jsonFile[["error"]])
      stop()
    } else {
      # List of all assets
      assetsList <- unlist(names(jsonFile[["result"]]))
      # Format data
      for (i in 1:length(assetsList)) {
        result <- cbind(result, as.matrix(jsonFile[["result"]][[i]]))
        colnames(result)[x] <- assetsList[i]
        x <- x+1
      }
    }
  } else {
    # Edit input for query
    assetsList <- paste(assets, collapse = ",")
    # Kraken API
    url <- paste("https://api.kraken.com/0/public/Assets?asset=", assetsList, sep = "")
    # Get data
    jsonFile <- jsonlite::fromJSON(url)
    # Error check
    if (length(jsonFile[["error"]]) > 0) {
      print(jsonFile[["error"]])
      stop()
    } else {
      # Format data
      for (i in assets) {
        result <- cbind(result, as.matrix(jsonFile[["result"]][[i]]))
        colnames(result)[x] <- i
        x <- x+1
      }
    }
  }
  # Return results
  return(as.data.frame(result))
}

# Test of the function
krakenAssets <- getAssets()
krakenAssets <- getAssets(assets = c("BTC", "AAVE", "ADA"))

########## [END] Assets ##########

########## [START] Assets Pairs ##########

#### Edit - Multiple assets get all to send only one request ?

getPairs <- function(pairs = "BTCEUR") {
  # Variables definition
  result <- NULL
  x <- 0
  # Data loop
  for (i in pairs) {
    x <- x+1
    ## Kraken API
    url <- paste("https://api.kraken.com/0/public/AssetPairs?pair=", i, sep = "")
    ## Get data
    jsonFile <- jsonlite::fromJSON(url)
    ## Error check
    if (length(jsonFile[["error"]]) > 0) {
      print(jsonFile[["error"]])
      stop()
    } else {
      result <- cbind(result, as.matrix(jsonFile[["result"]][[1]]))
      colnames(result)[x] <- i
    }
  }
  # Return results
  return(as.data.frame(result))
}

# Test of the function
krakenPairs <- getPairs(pairs = c("BTCEUR", "ADAEUR"))

########## [END] Assets Pairs ##########

########## [START] Tickers ##########

getPairs <- function(pairs = "BTCEUR") {
  # Variables definition
  result <- NULL
  x <- 0
  # Data loop
  for (i in pairs) {
    x <- x+1
    ## Kraken API
    url <- paste("https://api.kraken.com/0/public/AssetPairs?pair=", i, sep = "")
    ## Get data
    jsonFile <- jsonlite::fromJSON(url)
    ## Error check
    if (length(jsonFile[["error"]]) > 0) {
      print(jsonFile[["error"]])
      stop()
    } else {
      result <- cbind(result, as.matrix(jsonFile[["result"]][[1]]))
      colnames(result)[x] <- i
    }
  }
  # Return results
  return(as.data.frame(result))
}

########## [END] Tickers ##########

########## [START] Pairs OHLC ##########



########## [END] Pairs OHLC ##########

########## [START] Depth ##########



########## [END] Depth ##########

########## [START] Trades ##########



########## [END] Trades ##########

########## [START] Spread ##########



########## [END] Spread ##########

########## PRIVATE PART ?

######## [END] Kraken ########
