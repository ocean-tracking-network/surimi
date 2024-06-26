#' @title Map IMOS receiver metadata to an OTN-like format
#' @description In the same way that otn_imos_column_map takes OTN data and massages it into an IMOS-like format for REMORA,
#' this function and its ilk take IMOS data (in this case, receiver metadata) and massage it into an OTN-like format, for the
#' purposes of reporting and more general applicability within the OTN suite of programs.
#'
#' @param rcvr_dataframe A dataframe containing IMOS receiver metadata.
#'
#' @return A dataframe containing the above data in an OTN-like format.
#'
#' @importFrom dplyr '%>%' mutate rename
#' @importFrom tidyr separate
#' @export
#'
imos_to_otn_receivers <- function(rcvr_dataframe) {
  rcvr_return <- rcvr_dataframe %>%
    dplyr::select(
      receiver_project_name,
      station_name,
      receiver_deployment_datetime,
      receiver_deployment_latitude,
      receiver_deployment_longitude,
      depth_below_surface,
      receiver_name,
      receiver_status,
      receiver_recovery_datetime,
      receiver_recovery_latitude,
      receiver_recovery_longitude
    ) %>%
    mutate(
      BOTTOM_DEPTH = NA,
      RISER_LENGTH = NA,
      CODE_SET = NA,
      AR_MODEL_NO = NA,
      AR_SERIAL_NO = NA,
      DATA_DOWNLOADED = NA,
      DOWNLOAD_DATE_TIME = NA,
      COMMENTS = NA,
      TRANSMITTER = NA,
      TRANSMITTER_MODEL = NA,
      DEPLOYED_BY = NA,
      FILENAME = NA
    ) %>%
    rename(
      OTN_ARRAY = receiver_project_name,
      STATION_NO = station_name,
      `DEPLOY_DATE_TIME (yyyy-mm-ddThh:mm:ss)` = receiver_deployment_datetime,
      DEPLOY_LAT = receiver_deployment_latitude,
      DEPLOY_LONG = receiver_deployment_longitude,
      INSTRUMENT_DEPTH = depth_below_surface,
      RECOVERED = receiver_status,
      RECOVER_DATE_TIME = receiver_recovery_datetime,
      RECOVER_LAT = receiver_recovery_latitude,
      RECOVER_LONG = receiver_recovery_longitude
    ) %>%
    separate_wider_delim(
      cols = receiver_name,
      delim = "-",
      names = c("INS_MODEL_NO", "INS_SERIAL_NO")
    )

  # We have to do some cleaning up of date formats in the appropriate columns.
  # First we'll hit them with Lubridate to make sure they're real dates, formatted correctly.
  # Then, since they need a "T" in there, we'll cast them back to character strings and slot a 'T'
  # in where once there was whitespace. Simple! I don't love calling multiple near-identical mutates, but I also
  # don't like how unreadable and cursed it is to do the requisite jiggery-pokery to get multiple functions into a single
  # mutate(across())
  rcvr_return <- rcvr_return %>%
    mutate(
      across(ends_with("_DATE_TIME"), lubridate::ymd_hms)
    ) %>%
    mutate(
      across(ends_with("_DATE_TIME"), as.character)
    ) %>%
    mutate(
      across(ends_with("_DATE_TIME"), ~ stringr::str_replace(.x, " ", "T"))
    )

  # Have to do a little extra manipulation on the dataframe to give "RECOVERED"
  # sensible values.

  # First we have to update the comments where the receiver has been returned to vendor.
  rcvr_return <- within(
    rcvr_return,
    COMMENTS[RECOVERED == "returned to vendor"] <- "returned to vendor"
  )

  # Now we can change the values in the RECOVERED column to fit our standard.
  rcvr_return$RECOVERED[rcvr_return$RECOVERED == "damaged" |
    rcvr_return$RECOVERED == "returned to vendor"] <- "failed"

  return(rcvr_return)
}
