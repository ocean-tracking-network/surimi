ato_dep_from_glatos_workbook <- function(glatos_data) {
  #This implementation is fairly straightforward- if we have been given a glatos workbook, then we simply load it in- there are no optional file inclusions. 
  
    dep <- make_dep(
      receiver_model = glatos_data$ins_model_no,
      receiver_serial = glatos_data$ins_serial_no,
      #receiver_codeset = glatos_data$code_map,
      deploy_location = glatos_data$station_no,
      deploy_datetime = as.POSIXct(glatos_data$deploy_date_time),
      tz = "UTC",
      deploy_lat = glatos_data$deploy_lat,
      deploy_lon = glatos_data$deploy_long,
      deploy_z = glatos_data$bottom_depth,
      recover_datetime = as.POSIXct(glatos_data$recover_date_time),
      recover_lat = glatos_data$recover_lat,
      recover_lon = glatos_data$recover_long,
      transmitter = NA_character_, # ???
      transmitter_manufacturer = NA_character_, # ???
      transmitter_ping_rate = as.numeric(glatos_data$glatos_ins_frequency), # is this accurate? I think this mapping is right.
      transmitter_model = NA_character_, # ???
      transmitter_serial = NA_integer_
    )
    return(dep)
}
