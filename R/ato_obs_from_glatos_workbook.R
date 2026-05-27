ato_obs_from_glatos_workbook <- function(glatos_data) {
  #Unusually, we want to start with a little filtering, since we only want to grab those records where glatos_tag_recovered is YES. These represent deployments that were also recovered, so we need to record the
  #information about the recapture. Otherwise, the ATO will flag duplicates and be unable to perform any matching. 
  glatos_data_filtered <- dplyr::filter(glatos_data, glatos_data$glatos_tag_recovered == "YES")
  
  obs <- make_obs(
      animal = glatos_data_filtered$animal_id,
      transmitter =  paste(glatos_data_filtered$tag_code_space, "-", glatos_data_filtered$tag_id_code),
      #Not sure how to engineer type out of this. 
      terminal = TRUE,
      location = "Not supplied in data", #The field is mandatory but glatos doesn't require people to file the capture location, so we don't have the data. 
      datetime = as.POSIXct(glatos_data_filtered$glatos_caught_date),
      tz = "UTC"
    )
  
  return(obs)
}
