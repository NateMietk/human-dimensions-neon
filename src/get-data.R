
packages <- c('tidyverse', 'neonUtilities', 'httr', 'jsonlite', 'devtools', 'downloader', 'geoNEON', 'janitor', 'pbapply', 'sf')
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

# Get the data for the C02 Trapping
mos_co2 <- neonUtilities::loadByProduct(dpID = 'DP1.10043.001', check.size = FALSE)

# Get the data for mosiquto pathogens
mos_pathogen <- neonUtilities::loadByProduct(dpID = 'DP1.10041.001', check.size = FALSE)
  
  
  
  
  
  