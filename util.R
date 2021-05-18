format <- "%Y%m%d %H:%M"    # format of the timestamps

belongsToHour <- function(timestamp, hour) {
  bool = strptime(timestamp, format)$hour == hour
  return(bool)
}

belongsToMonth <- function(timestamp, month) {
  # months are numbered starting with 0 => increase by one
  bool = (strptime(timestamp, format)$month + 1) == month
  return(bool)
}
