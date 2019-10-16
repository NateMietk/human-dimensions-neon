
# Mosquito data
# Pathogen (DP1.10041.001)
# C02 traps (DP1.10043.001) 

# To install neonUtilities
# devtools::install_github("NEONScience/NEON-utilities/neonUtilities")
# To install geoNEON
# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")

packages <- c('tidyverse', 'neonUtilities', 'httr', 'jsonlite', 'devtools', 'downloader', 'geoNEON', 'janitor', 'pbapply')
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

# Get the mosquito count data
urls <- fromJSON(
  content(GET("http://data.neonscience.org/api/v0/products/DP1.10043.001"), as="text"), 
  simplifyDataFrame=TRUE, flatten=TRUE) 

# Compile a list of all the urls
file_list <- unlist(urls$data$siteCodes$availableDataUrls)

# Spin-up for parallel processing
cl <- parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
# If error or crash, closes all clusters
on.exit(parallel::stopCluster(cl))

# Main function
files <- do.call(rbind, pblapply(file_list, function(x) {
  
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
      
      mos_trapping <- as_tibble(read.delim(
        tmp_files$data$files$url[first(grep('trapping', tmp_files$data$files$name))], 
        sep=","))
      }
  , 
cl=cl
))
