ato_tag_from_glatos_workbook <- function(glatos_data) {
  glatos_data_filtered <- dplyr::filter(glatos_data, glatos_data$glatos_tag_recovered == "NO")
  
  tag <- make_tag(
    manufacturer = glatos_data_filtered$tag_manufacturer,
    model = glatos_data_filtered$tag_model,
    power_level = NA_real_, # ???
    ping_rate = NA_real_, # ???
    ping_variation = NA_real_, # ???
    serial = glatos_data_filtered$tag_serial_number,
    transmitter = paste(glatos_data_filtered$tag_code_space, "-", glatos_data_filtered$tag_id_code),
    activation_datetime = as.POSIXct(glatos_data_filtered$tag_activation_date),
    battery_life = as.numeric(glatos_data_filtered$est_tag_life),
    sensor_type = NA_character_, # ???
    sensor_unit = NA_character_, # ???
    animal = glatos_data_filtered$animal_id,
    tz = "UTC"
  )
  
  
  return(tag)
}
