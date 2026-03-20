ato_ani_from_otn <- function(otn_file) {
  # Import the file we've got.
  otn_data <- load_file(otn_file)

  # OTN doesn't have a bespoke animal file so we'll take what we get from the tag file.
  ani <- make_ani(
    animal = otn_data$ANIMAL_ID,
    capture_location = otn_data$CAPTURE_LOCATION,
    capture_datetime = as.POSIXct(NA_real_),
    capture_lat = otn_data$CAPTURE_LAT,
    capture_lon = otn_data$CAPTURE_LON,
    release_location = otn_data$RELEASE_LOCATION,
    release_datetime = as.POSIXct(otn_data$UTC_RELEASE_DATE_TIME),
    release_lat = otn_data$RELEASE_LATITUDE,
    release_lon = otn_data$RELEASE_LONGITUDE,
    tz = "UTC",
  )
}
