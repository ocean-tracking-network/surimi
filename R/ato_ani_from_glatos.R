ato_ani_from_glatos <- function(glatos_file) {
  # Load the data file we've been given- probably detections data since published glatos data will only have detections and receivers.
  glatos_data <- load_file(glatos_file)
  
  #Glatos detection data doesn't store releases the same way we do, i.e, each release having its own distinct row, but release/capture info is included in each detection, so we can narrow the table down to only unique animal IDs
  #and use that table.
  unique_animals <- distinct(glatos_data, animal_id, utc_release_date_time, .keep_all=TRUE)
  
  # We don't really have the option here to get this from an 'animal metadata' file so we're always going to be deriving.
  ani <- make_ani(
    animal = as.character(unique_animals$animal_id),
    capture_location = as.character(unique_animals$capture_location),
    capture_datetime = as.POSIXct(NA_real_),
    capture_lat = NA_real_,
    capture_lon = NA_real_,
    release_location = unique_animals$release_location,
    release_datetime = as.POSIXct(unique_animals$utc_release_date_time),
    release_lat = as.numeric(unique_animals$release_latitude),
    release_lon = as.numeric(unique_animals$release_longitude),
    tz = "UTC"
  )

  return(ani)
}
