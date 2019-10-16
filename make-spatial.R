
sf_mos_co2_traps <- st_as_sf(mos_co2_traps, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) 

# Get US shapefile
# Make a 'data' folder in the main R project repo
us_shp <- file.path('data/cb_2016_us_state_20m.shp')
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  dest <- paste0('data/cb_2016_us_state_20m.zip')
  download.file(loc, dest)
  unzip(dest, exdir = 'data/cb_2016_us_state_20m')
  unlink(dest)
}

if (!exists("usa_shp")){
  usa_shp <- st_read(dsn = 'data/cb_2016_us_state_20m', layer = "cb_2016_us_state_20m") %>%
    st_transform(crs = 4326) %>%  # e.g. US National Atlas Equal Area
    dplyr::select(STATEFP, STUSPS) %>%
    rename_all(tolower)
}

plot(st_geometry(sf_mos_co2_traps))
plot(st_geometry(usa_shp), add = TRUE)