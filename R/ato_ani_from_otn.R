ato_ani_from_otn <- function(otn_file, type = "meta") {
  # Import the file we've got.
  otn_data <- load_file(otn_file)

  if(type == "meta")
  {
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
  
  #If we're dealing with just a detection extract, the ani object will be pretty sparse, but creating it will preserve data. 
  else if(type == "extract") {
   ani <- make_ani(
     animal = otn_data$organismID,
     capture_location = NA_character_,
     capture_datetime = as.POSIXct(NA_real_),
     capture_lat = NA_real_,
     capture_lon = NA_real_,
     release_location = NA_real_,
     release_datetime = as.POSIXct(NA_real_),
     release_lat = NA_real_,
     release_lon = NA_real_,
     tz = "UTC"
   ) 
  }
}
