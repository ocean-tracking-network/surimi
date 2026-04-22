ato_ani_from_glatos_workbook <- function(glatos_data) {
  ani <- make_ani(
    animal = as.character(glatos_data$animal_id),
    capture_location = as.character(glatos_data$capture_location),
    capture_datetime = as.POSIXct(glatos_data$glatos_caught_date),
    capture_lat = glatos_data$capture_latitude,
    capture_lon = glatos_data$capture_longitude,
    release_location = glatos_data$release_location,
    release_datetime = as.POSIXct(glatos_data$utc_release_date_time),
    release_lat = glatos_data$release_latitude,
    release_lon = glatos_data$release_longitude,
    tz = "UTC"
  )

  return(ani)
}
