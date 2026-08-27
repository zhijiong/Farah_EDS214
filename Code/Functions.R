calc_9week_ma <- function(data, variable, new_column) {
  # Make sure data are ordered by site and date
  data <- data %>% arrange(Site, Sample_Date)

  # Create a new empty column to hold the result
  # $ → I already know the column name
  # [[ ]] → the column name is stored in another variable
  data[[new_column]] <- NA_real_

  # Go through every row
  for (i in 1:nrow(data)) {
    current_site <- data$Site[i]
    current_date <- data$Sample_Date[i]

    # First date for this site
    first_date <- min(data$Sample_Date[data$Site == current_site])

    # Only calculate after 9 weeks have passed
    if (current_date >= first_date + weeks(9)) {
      # Get values from the same site during the previous 9 weeks
      window_values <- data[[variable]][
        data$Site == current_site &
          data$Sample_Date > current_date - weeks(9) &
          data$Sample_Date <= current_date
      ]

      # Calculate moving average
      if (any(!is.na(window_values))) {
        data[[new_column]][i] <- mean(window_values, na.rm = TRUE)
      }
    }
  }
  return(data)
}
