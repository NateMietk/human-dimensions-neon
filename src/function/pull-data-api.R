get_sensor_positions <- function(file_directory, api = TRUE, new_job = FALSE) {
  if(api == TRUE) {
    # Collect all of the data associated with the DP address in file_directory
    urls <- fromJSON(
      content(GET(paste0("http://data.neonscience.org/api/v0/", file_directory)), as="text"), 
      simplifyDataFrame=TRUE, flatten=TRUE) 
    
    # Compile a list of all the urls
    file_list <- unlist(urls$data$siteCodes$availableDataUrls)
    
  } else {
    # List all of the unzipped data folders to pull the sensor position files from 
    file_list <- grep(list.files(file_directory, full.names = TRUE), pattern='.zip', inv=TRUE, value = TRUE)
  }
  
  # Create a csv file to document all of the sites/years/months that do not currently have a sensor position file
  if(api == TRUE) {
    file_out <- paste0('api_files_missing_sensor_positions_', basename(file_directory), '.csv')
  } else {
    file_out <- paste0('dp_files_missing_sensor_positions_', basename(file_directory), '.csv')
  }
  
  # Optional
  # Will remove the missing sensor position csv file so you do not duplicate rows.
  if(new_job == TRUE) {
    #Delete file if this is a fresh run
    file.remove(file_out)
  }
  
  # Print the total number of records to get a baseline line of percentage of sensor position files missing
  readr::write_lines(paste0('There are a total of ', length(file_list), ' records in the data product'), file_out)
  print(paste0('There are a total of ', length(file_list), ' files in the data product'))
  
  # Spin-up for parallel processing
  cl <- parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
  # If error or crash, closes all clusters
  on.exit(parallel::stopCluster(cl))
  
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
  
  # Remove all of the NULL list elements that were added when the file did not contain a sensor position file
  files <- files[files != "NULL"]
  
  print(paste0('There are a total of ', length(file_list)-length(files), ' records without sensor position files'))
  print(paste0(round((length(file_list)-length(files))/length(file_list), 2)*100, '% of the records do not have sensor position files'))
  
  # Bind all list elements into one dataframe for output
  files <- bind_rows(files) %>%
    dplyr::mutate(siteLocation = as.factor(siteLocation)) %>%
    dplyr::mutate_if(is.character, funs(lubridate::ymd_hms(.)))
  
  return(files)
}