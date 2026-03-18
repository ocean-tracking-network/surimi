ato_ani_from_glatos <- function(glatos_file) {
  # Load the data file we've been given- probably detections data since published glatos data will only have detections and receivers.
  glatos_data <- load_file(glatos_file)

  # We don't really have the option here to get this from an 'animal metadata' file so we're always going to be deriving.
  ani <- make_ani(
    animal = as.character(glatos_data$animal_id),
    capture_location = as.character(glatos_data$capture_location),
    capture_datetime = as.POSIXct(glatos_data$glatos_caught_date),
    capture_lat = NA_real_,
    capture_lon = NA_real_,
    release_loaction = glatos_data$release_location,
    release_datetime = as.POSIXct(glatos_data$utc_release_date_time),
    release_lat = glatos_data$release_latitude,
    release_lon = glatos_data$release_longitude,
    tz = "UTC"
  )

  return(ani)
}
