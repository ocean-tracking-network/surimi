#' @title Map IMOS detection data to an OTN-like format
#' @description In the same way that otn_imos_column_map takes OTN data and massages it into an IMOS-like format for REMORA,
#' this function and its ilk take IMOS data (in this case, animal detections) and massages it into an OTN-like format, for the
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

imos_to_otn_detections <- function(detection_dataframe) {
 det_return <- detection_dataframe %>%
   dplyr::select(
     detection_datetime,
     receiver_name,
     receiver_id,
     transmitter_id,
     transmitter_sensor_value,
     transmitter_sensor_unit,
     station_name,
     receiver_deployment_latitude,
     receiver_deployment_longitude
   ) %>%
   mutate(
     transmitter_name = NA,
     transmitter_serial = NA, 
   ) %>%
   rename(
     date_and_time = detection_datetime,
     receiver = receiver_name,
     transmitter = transmitter_id,
     sensor_value = transmitter_sensor_value,
     sensor_unit = transmitter_sensor_unit,
     latitude = receiver_deployment_latitude,
     longitude = receiver_deployment_longitude
   )
   
 return(det_return)
}
