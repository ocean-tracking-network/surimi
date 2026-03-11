ato_dep_from_glatos <- function(glatos_file, type = "meta") {
  # Read in the file we've been given if we haven't been handed a dataframe.
  glatos_data <- load_file(glatos_file)
  
  # If we've been given a metadata file, we read it in as one.
  if (type == "meta") {
    # This we can pull directly from the metadata.
    dep <- make_dep(
      receiver_model = glatos_data$ins_model_no,
      receiver_serial = glatos_data$ins_serial_no,
      receiver_codeset = NA_character_, #???
      deploy_location = glatos_data$station_no,
      deploy_datetime = glatos_data$deploy_date_time,
      deploy_lat = glatos_data$deploy_lat,
      deploy_lon = glatos_data$deploy_lon,
      deploy_z = glatos_data$bottom_depth,
      recover_datetime = glatos_data$recover_Date_time,
      recover_lat = glatos_data$recover_lat,
      recover_lon = glatos_data$recover_lon,
      transmitter = NA_character_, #???
      transmitter_manufacturer = NA_character_, #???
      transmitter_ping_rate = NA_character_, #???
      transmitter_model = NA_character_, #???
      transmitter_serial = NA_integer_
    )
    return(dep)
  } else if (type == "extract") {
    # In the case where we don't have a metadata file to work with, we can use a similar setup to how we derive
    # receiver data from detection extracts for IMOS.
    
    # To start, we will filter the releases out of our detections dataframe.
    no_releases <- glatos_data %>% filter(receiver_sn != "release")
    
    # The first thing we need to do is gin up some inferred min and max deployment dates.
    # We'll use the following code to do so.
    rcvr_grouped <- NULL
    
    # Start by grouping the detections by station, and ordering them by date.
    rcvr_grouped_list <- no_releases %>%
      group_by(station) %>%
      arrange(detection_timestamp_utc, .by_group = TRUE)
    
    # Set min date and max date to null.
    minDate <- NULL
    maxDate <- NULL
    
    # Create a 'lead' dataframe for us to compare our current dataframe against.
    rcvr_grouped_list_next <- lead(rcvr_grouped_list)
    
    # For each row in the list
    for (i in 1:nrow(rcvr_grouped_list)) {
      row <- rcvr_grouped_list[i, ]
      
      # If minDate is null, set it to the currently available date. minDate being null implies that
      # we're just starting with this station (see where it's set to Null, below)
      if (is.null(minDate)) {
        minDate <- row$detection_timestamp_utc
      }
      
      # Get the next row from our "lead" frame.
      nextStation <- rcvr_grouped_list_next[i, ]
      
      # If our next station is Null (i.e, we're at the end of the frame), or the next station is different from
      # the current one (i.e, we've reached the end of this time chunk)...
      if (is.na(nextStation$receiver_sn) || nextStation$receiver_sn != row$receiver_sn) {
        # Set Maxdate to our current date.
        maxDate <- row$detection_timestamp_utc
        
        # Add the min and max dates as entries in the row.
        row <- row %>% mutate(
          minDetectionDate = minDate,
          maxDetectionDate = maxDate
        )
        
        # if rcvr_group hasn't been instantiated yet, use row to create it.
        if (is.null(rcvr_grouped)) {
          rcvr_grouped <- row
        }
        # Otherwise, just add the row to the group of receivers.
        else {
          rcvr_grouped <- rbind(rcvr_grouped, row)
        }
        
        # reset our min and max date to null so the next group will be handled properly.
        minDate <- NULL
        maxDate <- NULL
      }
    }
    
    dep <- make_dep(
      receiver_model = NA_character_,
      receiver_serial = as.integer(rcvr_grouped$receiver_sn),
      receiver_codeset = NA_character_,
      deploy_location = rcvr_grouped$station,
      deploy_datetime = as.POSIXct(NA_real_),
      deploy_lat = rcvr_grouped$deploy_lat,
      deploy_lon = rcvr_grouped$deploy_lon,
      recover_datetime = as.POSIXct(NA_real_),
      recover_lat = NA_real_,
      recover_lon = NA_real_,
      transmitter = NA_character_,
      transmitter_model = NA_character_,
      transmitter_serial = rcvr_grouped$transmitter_id,
      tz = "UTC"
    )
    
    return(dep)
  } else {
    message("Invalid type specified. Use either 'meta' (if loading from a metadata file) or 'extract' (if deriving deployment metadata from a detection extract).")
  }
}
