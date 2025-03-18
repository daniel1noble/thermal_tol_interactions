#### ----------------------------------------------------- ####
## Functions. Functions used through the project.
#### ----------------------------------------------------- ####

#' @title convert_doy
#' @description Convert day of year (DOY) to date, month-day, ymd, or julian date
#' @param doy Numeric vector of day of year
#' @param year Numeric vector of year
#' @param format_type Character vector of format type. Default is "date"
#' @return Date, month, month-day, ymd, or julian date
#' @examples
#' convert_doy(120, 2024, "date") # "2024-04-29"
#' convert_doy(120, 2024, "month_day") # "April 29"	
#' convert_doy(120, 2024, "ymd") # "2024-04-29"

convert_doy <- function(doy, year, format_type = "date") {
  # Convert DOY to Date
  date <- as.Date(doy - 1, origin = paste0(year, "-01-01"))
  
  # Return different formats based on format_type
  switch(format_type,
         "date" = date,                     # Default: Date format
         "month_day" = format(date, "%B %d"), # "April 29"
		 "month" = format(date, "%B"), # "April"
         "ymd" = format(date, "%Y-%m-%d"),    # "2024-04-29"
         "julian" = format(date, "%Y%j"),     # "2024120"
         stop("Invalid format_type. Choose from 'date', 'month_day', 'ymd', or 'julian'.")
  )
}

#' @title convert_time
#' @description Convert decimal time to hh:mm, hh:mm:ss, or 12-hour format
#' @param time_minutes Numeric vector of decimal time in minutes
#' @param format_type Character vector of format type. Default is "hh:mm"
#' @return Time in hh:mm, hh:mm:ss, or 12-hour format
#' @examples
#' convert_time(13.5, "hh:mm") # "13:30"
#' convert_time(13.5, "hh:mm:ss") # "13:30:00"
#' convert_time(13.5, "12-hour") # "1:30 PM"

convert_time_for_plot <- function(time_minutes, format_type = "decimal") {
  # Extract hours and minutes
  hours <- time_minutes %/% 60  
  minutes <- time_minutes %% 60  

  # Format output based on format_type
  switch(format_type,
         "decimal" = hours + minutes / 60,  # e.g., 13.5 for 13:30
         "POSIXct" = as.POSIXct(sprintf("%02d:%02d", hours, minutes), format="%H:%M", tz="UTC"),  # e.g., "13:30:00 UTC"
         "hms" = hms::hms(hours * 3600 + minutes * 60),  # e.g., 13:30:00
         stop("Invalid format_type. Choose from 'decimal', 'POSIXct', or 'hms'.")
  )
}

