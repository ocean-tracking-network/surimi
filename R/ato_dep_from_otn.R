ato_dep_from_otn <- function(otn_file, type="meta") {
  
  if (!is.data.frame(otn_file)) {
    extension <- tools::file_ext(otn_detections)
    if (extension == "parquet") {
      otn_file <- read_parquet(otn_detections)
    } else {
      otn_file <- read.csv(otn_detections, na = c("", "null", "NA"))
    }
  }
  
  if(type == "meta") {
    #This we can pull directly from the metadata.
    dep <- make_dep(receiver_model = otn_file$INS_MODEL_NO,
                     receiver_serial = otn_file$INS_SERIAL_NO,
                     receiver_codeset = otn_file$CODE_SET,
                     deploy_location = otn_file$STATION_NO,
                     deploy_datetime = otn_file$DEPLOY_DATE_TIME,
                     deploy_lat = otn_file$DEPLOY_LAT,
                     deploy_lon = otn_file$DEPLOY_LON,
                     deploy_z = otn_file$BOTTOM_DEPTH,
                     recover_datetime = otn_file$RECOVER_DATE_TIME,
                     recover_lat = otn_file$RECOVER_LAT,
                     recover_lon = otn_file$RECOVER_LON,
                     transmitter = otn_file$TRANSMITTER,
                     transmitter_manufacturer = NA_character_,
                     transmitter_ping_rate = NA_character_,
                     transmitter_model = otn_file$TRANSMIT_MODEL,
                     transmitter_serial = NA_character_)
    return(dep)
  }
  
  else if(type == "extract") {
    #In the case where we don't have a metadata file to work with, we can use a similar setup to how we derive
    #receiver data from detection extracts for IMOS.
    
    # To start, we will filter the releases out of our detections dataframe.
    no_releases <- otn_file %>% filter(receiver != "release")
    
    # The first thing we need to do is gin up some inferred min and max deployment dates.
    # We'll use the following code to do so.
    rcvr_grouped <- NULL
    
    # Start by grouping the detections by station, and ordering them by date.
    rcvr_grouped_list <- no_releases %>%
      group_by(station) %>%
      arrange(dateCollectedUTC, .by_group = TRUE)
    
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
        minDate <- row$dateCollectedUTC
      }
      
      # Get the next row from our "lead" frame.
      nextStation <- rcvr_grouped_list_next[i, ]
      
      # If our next station is Null (i.e, we're at the end of the frame), or the next station is different from
      # the current one (i.e, we've reached the end of this time chunk)...
      if (is.na(nextStation$receiver) || nextStation$receiver != row$receiver) {
        # Set Maxdate to our current date.
        maxDate <- row$dateCollectedUTC
        
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
    
    dep <- make_dep(receiver_model = NA_character_,
                    receiver_serial = rcvr_grouped$receiver,
                    receiver_codeset = rcvr_grouped$codeSpace,
                    deploy_location = rcvr_grouped$station
                    deploy_datetime = as.POSIXct(NA_real_),
                    deploy_lat = rcvr_grouped$decimalLatitude,
                    deploy_lon = rcvr_grouped$decimalLongitude,
                    recover_datetime = as.POSIXct(NA_real_),
                    recover_lat = NA_real_,
                    recover_lon = NA_real_,
                    transmitter = NA_character_,
                    transmitter_model = NA_character_,
                    transmitter_serial = NA_integer_,
                    tz = "UTC")
    
    return(dep)
  }
  
  else {
    message("Invalid type specified. Use either 'meta' (if loading from a metadata file) or 'extract' (if deriving deployment metadata from a detection extract).")
  }
}