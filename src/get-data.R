
# Mosquito data
# Pathogen (DP1.10041.001)
# C02 traps (DP1.10043.001) 

# To install neonUtilities
# devtools::install_github("NEONScience/NEON-utilities/neonUtilities")
# To install geoNEON
# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")

packages <- c('tidyverse', 'neonUtilities', 'httr', 'jsonlite', 'devtools', 'downloader', 'geoNEON', 'janitor', 'zoo', 'pbapply')
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

# Get the mosquito count data
urls <- fromJSON(
  content(GET(paste0("http://data.neonscience.org/api/v0/product/DP1.10043.001", file_directory)), as="text"), 
  simplifyDataFrame=TRUE, flatten=TRUE) 

# Compile a list of all the urls
file_list <- unlist(urls$data$siteCodes$availableDataUrls)

# Main function
files <- pblapply(file_list, function(x, api, file_directory, file_out) {
  
  # Packages needed to be pushed to each core for parallel processing
  packages <- c('tidyverse', 'neonUtilities', 'httr', 'jsonlite', 'devtools', 'downloader', 'geoNEON', 'janitor', 'zoo', 'pbapply')
  invisible(lapply(packages, require, character.only = TRUE, quietly = TRUE))
  
  # Makes sure the numbers displayed are accurate in the tibble
  options(pillar.sigfig = 8)
  
  # Error handler - if there is no sensor position file return NULL in the list for post-filtering
  tryCatch({ 
    # Error handler - if there is no sensor position file write the name of the file to a csv file for documentation
    tryCatch({
      # Download the data file from the API call and extract the sensor position file
      if(api == TRUE) {
        splitter <- x %>%
          str_split(., '/') %>%
          unlist()
        
        ######### NEED TO GET THIS WORKING FOR API
        date_name <- basename(dirname(tmp_filename)) %>%
          str_split(., '\\.') %>%
          unlist()%>%
          .[7]
        
        tmp_files <- fromJSON(
          content(GET(x), as="text"), 
          simplifyDataFrame=TRUE, flatten=TRUE)
        
        tmp_files <- as_tibble(read.delim(
          tmp_files$data$files$url[first(grep('sensor', tmp_files$data$files$name))], 
          sep=","))
      } else {
        # Extract the sensor position file from the folder
        tmp_filename <- list.files(x, pattern = 'sensor',
                                   recursive = TRUE, full.names = TRUE)
        splitter <- basename(tmp_filename) %>%
          str_split(., '\\.') %>%
          unlist() 
        date_name <- basename(dirname(tmp_filename)) %>%
          str_split(., '\\.') %>%
          unlist()%>%
          .[7]
        
        tmp_files <- as_tibble(readr::read_csv(tmp_filename))
      } 
    }, 
    error = function(e) {
      # Error handler - if there is no sensor position file write the name of the file to a csv file for documentation
      if(api == TRUE) {
        readr::write_lines(x, file_out, append = TRUE)
      } else {
        readr::write_lines(x, file_out, append = TRUE)
      }
    }
    )
    
    # Collect the site location ID to add to resulting bound dataframe
    if(api == TRUE) {
      siteID <- as_factor(splitter[8])
    } else {
      siteID <- as_factor(splitter[3])
    }
    # Error handler - make sure the site location ID is properly formatted
    if(regexpr("[[:alpha:]]{4}",siteID)!=1) {
      stop(paste(siteID, "is not a properly formatted site ID", sep=" "))
    }
    
    # Make sure there is a start/end column, if not create on with NAs
    if("start" %in% names(tmp_files) & "end" %in% names(tmp_files)) {
      tmp_files <- tmp_files 
    } else { 
      tmp_files <- tmp_files %>%
        dplyr::mutate(start = lubridate::ymd_hms(NA),
                      end = lubridate::ymd_hms(NA)) %>%
        dplyr::select(`HOR.VER`,
                      start, end, everything())
    }
    
    # Make sure there is a referenceStart/end column, if not create on with NAs
    if("referenceStart" %in% names(tmp_files) & "referenceEnd" %in% names(tmp_files)) {
      tmp_files <- tmp_files
    } else { 
      tmp_files <- tmp_files %>%
        dplyr::mutate(referenceStart = lubridate::ymd_hms(NA),
                      referenceEnd = lubridate::ymd_hms(NA)) %>%
        dplyr::select(`HOR.VER`, start, end,
                      referenceStart, referenceEnd, everything())
    }
    
    # Create the final table to be added to the list
    tmp_files <- tmp_files %>%
      dplyr::mutate(`HOR.VER` = as.numeric(`HOR.VER`)) %>%
      dplyr::mutate_if(is.factor, funs(lubridate::ymd_hms(.))) %>%
      dplyr::mutate(siteLocation = as.factor(siteID),
                    fileDate = zoo::as.Date(zoo::as.yearmon(date_name, "%Y-%m"))) %>%
      dplyr::select(siteLocation, fileDate,
                    everything())
  }, 
  error = function(e) {
    files <<- NULL
  }
  )
}, 
api = api, 
file_directory=file_directory,
file_out = file_out,
cl=cl
)
