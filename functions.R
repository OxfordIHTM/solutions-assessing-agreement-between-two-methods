# Statistical methods for assessing agreement between two methods --------------
# of clinical measurement - Solutions

## Task 1: Download and read the Bland Altman dataset ----

### Function to download and read dataset - version 1 ----

# Version 1: with option to specify where to store download
#   url      - Download URL for dataset
#   destfile - File path to download the data into

read_ba_data <- function(url, destfile) {
  download.file(url = url, destfile = destfile)

  read.table(file = destfile, header = TRUE, sep = " ")
}

### Function to download and read dataset - version 2 ----

# Version 2: with option to read the data directly and whether or not to
# download dataset
#   url      - URL to dataset
#   download - Logical (TRUE or FALSE); should dataset be downloaded? Default
#              to TRUE
#   destfile - File path to download the data into; only evaluated if
#              download = TRUE

read_ba_data <- function(url, 
                         download = TRUE, 
                         destfile) {
  ## If download = TRUE, download dataset
  if (download) {
    download.file(url = url, destfile = destfile)
  } else {
  ## If download = FALSE, set destfile as url link
    destfile <- url
  }

  ## Read dataset
  read.table(file = destfile, header = TRUE, sep = " ")
}

### Function to download and read dataset - version 3 ----

# Version 3: with option to check if download already present in destfile and
# before downloading dataset
#   url       - URL to dataset
#   download  - Logical (TRUE or FALSE); should dataset be downloaded? Default
#               to TRUE
#   destfile  - File path to download the data into; only evaluated if
#               download = TRUE
#   overwrite - Logical (TRUE or FALSE); if TRUE, overwrites existing file
#               specified by destfile; if FALSE, data will only be downloaded
#               if file specified by destfile is not present; Default to FALSE.

read_ba_data <- function(url, 
                         download = TRUE, 
                         destfile,
                         overwrite = FALSE) {  
  ## If download = TRUE, download dataset
  if (download) {
    ## If overwrite = TRUE, download dataset
    if (overwrite) {
      download.file(url = url, destfile = destfile)
    } else {
    ## If overwrite = FALSE, check if destfile exists before downloading
      if (!file.exists(destfile)) {
        download.file(url = url, destfile = destfile)
      }
    }
  } else {
  ## If download = FALSE, set destfile as url link
    destfile <- url
  }

  ## Read dataset
  read.table(file = destfile, header = TRUE, sep = " ")
}


## Task 2: Calculate the Bland and Altman metrics ----

### Function to calculate Bland and Altman metrics - vectorised approach ----

# Vectorised approach: output each metric as vectors and concatenate into a
# list
#   - ba_data - A data.frame of the Bland Altman dataset

calculate_ba_metrics <- function(ba_data) {
  ## Get per row mean of measurements
  mean_values <- (ba_data$Wright + ba_data$Mini) / 2

  ## Get per row difference of measurements
  differences <- ba_data$Wright - ba_data$Mini

  ## Mean of the differences of measurements
  mean_difference <- mean(differences)

  ## Upper and lower limits of agreement
  upper_limit <- mean_difference + 1.96 * sd(differences)
  lower_limit <- mean_difference - 1.96 * sd(differences)

  ## Concatenate metrics into a named list
  list(
    mean_values = mean_values,
    differences = differences,
    mean_differences = mean_differences,
    upper_limit = upper_limit,
    lower_limit = lower_limit
  )
}

### Function to calculate Bland and Altman metrics - data.frame approach ----

# data.frame approach: output each metric as vectors and concatenate into
# original dataset
#   - ba_data - A data.frame of the Bland Altman dataset

calculate_ba_metrics <- function(ba_data) {
  ## Get per row mean of measurements and add to ba_data
  ba_data$mean_values <- (ba_data$Wright + ba_data$Mini) / 2

  ## Get per row difference of measurements and add to ba_data
  ba_data$differences <- ba_data$Wright - ba_data$Mini

  ## Mean of the differences of measurements
  ba_data$mean_differences <- mean(ba_data$differences)

  ## Upper and lower limits of agreement
  ba_data$upper_limit <- ba_data$mean_differences + 1.96 * sd(ba_data$differences)
  ba_data$lower_limit <- ba_data$mean_differences - 1.96 * sd(ba_data$differences)

  ## Return ba_data
  ba_data
}

### Function to calculate Bland and Altman metrics - combined approach ----

# This approach gives users an option between a vectorised or a data.frame
# approach.
#   - ba_data - A data.frame of the Bland Altman dataset
#   - type - format of output - list for list or df for data.frame

calculate_ba_metrics <- function(ba_data, type = c("list", "df")) {
  type <- match.arg(type)

  mean_values <- (ba_data$Wright + ba_data$Mini) / 2
  differences <- ba_data$Wright - ba_data$Mini
  mean_differences <- mean(ba_data$differences)
  upper_limit <- ba_data$mean_differences + 1.96 * sd(ba_data$differences)
  lower_limit <- ba_data$mean_differences - 1.96 * sd(ba_data$differences)

  if (type == "list") {
    list(
      mean_values = mean_values,
      differences = differences,
      mean_differences = mean_differences,
      upper_limit = upper_limit,
      lower_limit = lower_limit
    )
  } else {
    data.frame(
      ba_data, mean_values, differences, mean_differences, 
      upper_limit, lower_limit
    )
  }
}

### Function to calculate Bland and Altman metrics - modular approach ----

# modular approach: create multiple functions that calculates each component
# metric of the BA plot and then assemble them into one overall function
#   - m1 - A vector of first measurements
#   - m2 - A vector of second measurements
#   - ba_data - A data.frame of the Bland Altman dataset
#   - type - format of output - list for list or df for data.frame

calculate_mean_values <- function(m1, m2) { (m1 + m2) / 2 }

calculate_diff_values <- function(m1, m2) { m1 - m2 }

calculate_mean_diff <- function(m1, m2) {
  mean(calculate_diff_values(m1 = m1, m2 = m2))
}

calculate_diff_limits <- function(m1, m2) {
  differences <- calculate_diff_values(m1 = m1, m2 = m2)
  mean_differences <- calculate_mean_diff(m1 = m1, m2 = m2)

  upper_limit <- mean_differences + 1.96 * sd(differences)
  lower_limit <- mean_differences - 1.96 * sd(differences)

  c(upper_limit, lower_limit)
}

calculate_ba_metrics <- function(ba_data, type = c("list", "df")) {
  type <- match.arg(type)

  mean_values <- calculate_mean_values(m1 = ba_data$Wright, m2 = ba_data$Mini)
  differences <- calculate_diff_values(m1 = ba_data$Wright, m2 = ba_data$Mini)
  mean_differences <- calculate_mean_diff(m1 = ba_data$Wright, m2 = ba_data$Mini)
  limits <- calculate_diff_limits(m1 = ba_data$Wright, m2 = ba_data$Mini)

  if (type == "list") {
    list(
      mean_values = mean_values,
      differences = differences,
      mean_differences = mean_differences,
      upper_limit = limits[1],
      lower_limit = limits[2]
    )
  } else {
    data.frame(
      ba_data, mean_values, differences, mean_differences,
      upper_limit, lower_limit
    )
  }
}

### Function to calculate Bland and Altman metrics - universal approach ----

# universal approach: create multiple functions that calculates each component
# metric of the BA plot and then assemble them into one overall function that
# can take on any dataset with any two measurements being compared with each
# other.
#   - m1 - A character value of the variable name for first measurements
#   - m2 - A character value of the variable name for second measurements
#   - ba_data - A data.frame of the Bland Altman dataset
#   - type - format of output - list for list or df for data.frame

calculate_ba_metrics <- function(df, 
                                 m1, m2, 
                                 type = c("list", "df")) {
  type <- match.arg(type)

  m1 <- df[[m1]]
  m2 <- df[[m2]]

  mean_values <- calculate_mean_values(m1 = m1, m2 = m2)
  differences <- calculate_diff_values(m1 = m1, m2 = m1)
  mean_differences <- calculate_mean_diff(m1 = m1, m2 = m2)
  limits <- calculate_diff_limits(m1 = m1, m2 = m2)

  if (type == "list") {
    list(
      mean_values = mean_values,
      differences = differences,
      mean_differences = mean_differences,
      upper_limit = limits[1],
      lower_limit = limits[2]
    )
  } else {
    data.frame(
      ba_data, mean_values, differences, mean_differences,
      upper_limit, lower_limit
    )
  }
}


## Task 3: Task 3: Create a Bland and Altman plot ----

plot_ba <- function(ba_metrics) {
  plot(
    x = ba_metrics$mean_values,
    y = ba_metrics$differences
  )

  
}
