#' @title Join output from Remora back onto its OTN detection extract.
#'
#' @description Take two parameters- an OTN detection extract and the output created by Remora on parsing that detection extract- and merge them back together such that the Remora QC columns are appended to the OTN extract,
#' preserving appropriate ordering and getting back all the OTN data. This function exists because, to get OTN data into Remora, we have to cut it up until it looks like IMOS data (this problem was the genesis of Surimi, in fact).
#' But that means the output from Remora has all IMOS-formatted columns and is missing some information, because we either had to discard it to get into IMOS format or because we can't re-synthesize it from what's in the IMOS
#' files. However, we do have enough information to join the two tables, thereby obviating the data loss problem by taking us all the way back to the original data, with a little something extra attached.
#'
#' @param detection_extract Path to an OTN detection extract corresponding to the remora output in the second parameter.
#' @param remora_output Path to Remora's QC output corresponding to the OTN detection extract in the first parameter.
#'
#' @return The OTN detection extract, but with the remora QC attached as appropriate.
#'
#' @importFrom dplyr '%>%' mutate rename left_join
#' @importFrom tidyr unite separate
#' @importFrom lubridate ymd_hms
#' @export
#'
#

# For what it's worth, I ad a lot of ideas for names for this function, including 'bento' (because it pairs the output of 'surimi' with something else), or
# 'mcfly' (because it takes you back to the starting point of your adventure made whole but nonetheless irrevocably altered) but those are a little obscure and if I'm being honest, I have little patience
# for that kind of tweeness. Even 'rollup' (referring to making a sushi roll, in the same vein as 'bento') is a little much.

rollup <- function(detection_extract, remora_output) {
  # Read in the two dataframes.
  otn_dets <- read.csv(detection_extract)
  remora_out <- read.csv(remora_output)

  # Select the appropriate columns from remora_output.
  remora_to_merge <- remora_out %>%
    dplyr::select(
      transmitter_id,
      tag_id,
      detection_datetime,
      receiver_id,
      ends_with("_QC")
    )

  # Get the dates into the same format for comparison.
  otn_dets <- otn_dets %>%
    mutate(
      datecollected = ymd_hms(datecollected)
    )

  remora_to_merge <- remora_to_merge %>%
    mutate(
      detection_datetime = ymd_hms(detection_datetime)
    )

  # Join them to otn_dets
  otn_det_output <- left_join(
    otn_dets,
    remora_to_merge,
    by = join_by(
      tagname == transmitter_id,
      catalognumber == tag_id,
      datecollected == detection_datetime,
      receiver == receiver_id
    )
  )

  # Return the merged columns.
  return(otn_det_output)
}
