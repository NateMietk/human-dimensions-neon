
# Mosquito data
# Pathogen (DP1.10041.001)
# C02 traps (DP1.10043.001) 

# To install neonUtilities
# devtools::install_github("NEONScience/NEON-utilities/neonUtilities")
# To install geoNEON
# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")

packages <- c('tidyverse', 'neonUtilities', 'httr', 'jsonlite', 'devtools', 'downloader', 'geoNEON', 'janitor', 'pbapply')
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

# Spin-up for parallel processing
cl <- parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
# If error or crash, closes all clusters
on.exit(parallel::stopCluster(cl))

# Main function
get_mosiquito_data <- function(dp_id, variable_name) {
  
  # Get the mosquito count data
  urls <- fromJSON(
    content(GET(paste0("http://data.neonscience.org/api/v0/products/", dp_id)), as="text"), 
    simplifyDataFrame=TRUE, flatten=TRUE) 
  
  # Compile a list of all the urls
  file_list <- unlist(urls$data$siteCodes$availableDataUrls)
  
  file_out <- pblapply(file_list, function(x, variable_name) {
    tryCatch({
      # Packages needed to be pushed to each core for parallel processing
      packages <- c('tidyverse', 'neonUtilities', 'httr', 'jsonlite', 'devtools', 'downloader', 'geoNEON', 'janitor', 'pbapply')
      invisible(lapply(packages, require, character.only = TRUE, quietly = TRUE))
      
      # Makes sure the numbers displayed are accurate in the tibble
      options(pillar.sigfig = 8)
      
      splitter <- x %>%
        str_split(., '/') %>%
        unlist()
      
      tmp_files <- fromJSON(
        content(GET(x), as="text"), 
        simplifyDataFrame=TRUE, flatten=TRUE)
      
      tmp_files <- as_tibble(read.delim(
        tmp_files$data$files$url[first(grep(variable_name, tmp_files$data$files$name))], 
        sep=","))
      },
      error = function(e) {
        file_out <<- NULL
      }
    )
  },
  variable_name=variable_name,
  cl=cl
    )
  file_out <- file_out[file_out != "NULL"]
  # Bind all list elements into one dataframe for output
  file_out <- bind_rows(file_out)
  }

mos_co2_traps <- get_mosiquito_data(dp_id='DP1.10043.001', 
                                    variable_name='trapping')
mos_co2_traps_sorting <- get_mosiquito_data(dp_id='DP1.10043.001', 
                                               variable_name='sorting')
mos_co2_traps_validation <- get_mosiquito_data(dp_id='DP1.10043.001', 
                                               variable_name='validation')

mos_pathogen_pooling <- get_mosiquito_data(dp_id='DP1.10041.001', 
                                           variable_name='pathogenpooling')
mos_pathogen_results <- get_mosiquito_data(dp_id='DP1.10041.001',
                                           variable_name='pathogenresults')
mos_pathogen_validation<- get_mosiquito_data(dp_id='DP1.10041.001', 
                                             variable_name='validation')