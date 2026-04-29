ato_obs_from_glatos_workbook <- function(glatos_data) {
  #Unusually, we want to start with a little filtering, since we only want to grab those records where glatos_tag_recovered is YES.
  glatos_data_filtered <- dplyr::filter(glatos_data, glatos_data$glatos_tag_recovered == "YES")
  
  obs <- make_obs(
      animal = glatos_data_filtered$animal_id,
      transmitter =  paste(glatos_data_filtered$tag_code_space, "-", glatos_data_filtered$tag_id_code),
      #Not sure how to engineer type out of this. 
      terminal = TRUE,
      location = glatos_data_filtered$capture_location, #I don't know if this is accurate- I might need something better.
      datetime = as.POSIXct(glatos_data_filtered$glatos_caught_date),
      tz = "UTC"
    )
  
  return(obs)
}
