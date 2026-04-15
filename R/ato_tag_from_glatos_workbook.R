ato_tag_from_glatos_workbook <- function(glatos_data) {

    tag <- make_tag(
      manufacturer = glatos_data$tag_manufacturer,
      model = glatos_data$tag_model,
      power_level = NA_real_, # ???
      ping_rate = NA_real_, # ???
      ping_variation = NA_real_, # ???
      serial = glatos_data$tag_serial_number,
      transmitter = paste(glatos_data$tag_code_space, "-", glatos_data$tag_id_code),
      activation_datetime = as.POSIXct(glatos_data$tag_activation_date),
      battery_life = glatos_data$est_tag_life,
      sensor_type = NA_character_, # ???
      sensor_unit = NA_character_, # ???
      animal = glatos_data$animal_id,
      tz = "UTC"
    )
    return(tag)
}
