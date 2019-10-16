library(sf)

sf_mos_co2_traps <- st_as_sf(mos_co2_traps, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  dplyr::filter(siteID == 'UKFS')
