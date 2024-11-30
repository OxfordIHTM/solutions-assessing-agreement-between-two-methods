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
  mean_differences <- mean(differences)

  ## Upper and lower limits of agreement
  upper_limit <- mean_differences + 1.96 * sd(differences)
  lower_limit <- mean_differences - 1.96 * sd(differences)

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
  mean_differences <- mean(differences)
  upper_limit <- mean_differences + 1.96 * sd(differences)
  lower_limit <- mean_differences - 1.96 * sd(differences)

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
#   - ba_data - A data.frame of the Bland Altman dataset
#   - type - format of output - list for list or df for data.frame

calculate_mean_values <- function(ba_data) { 
  (ba_data$Wright + ba_data$Mini) / 2 
}

calculate_diff_values <- function(ba_data) { 
  ba_data$Wright - ba_data$Mini 
}

calculate_mean_diff <- function(ba_data) {
  mean(calculate_diff_values(ba_data))
}

calculate_diff_limits <- function(ba_data) {
  differences <- calculate_diff_values(ba_data)
  mean_differences <- calculate_mean_diff(ba_data)

  upper_limit <- mean_differences + 1.96 * sd(differences)
  lower_limit <- mean_differences - 1.96 * sd(differences)

  c(upper_limit, lower_limit)
}

calculate_ba_metrics <- function(ba_data, type = c("list", "df")) {
  type <- match.arg(type)

  mean_values <- calculate_mean_values(ba_data)
  differences <- calculate_diff_values(ba_data)
  mean_differences <- calculate_mean_diff(ba_data)
  limits <- calculate_diff_limits(ba_data)

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
      upper_limit = limits[1], lower_limit = limits[2]
    )
  }
}

### Function to calculate Bland and Altman metrics - universal approach ----

# universal approach: create multiple functions that calculates each component
# metric of the BA plot and then assemble them into one overall function that
# can take on any dataset with any two measurements being compared with each
# other.
#   - df - A data.frame containing measurement values that can be analysed with
#     the Bland and Altman approach
#   - m1 - A character value of the variable name for first measurements
#   - m2 - A character value of the variable name for second measurements
#   - type - format of output - list for list or df for data.frame

calculate_mean_values <- function(m1, m2) { 
  (m1 + m2) / 2 
}

calculate_diff_values <- function(m1, m2) { 
  m1 - m2 
}

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

calculate_ba_metrics <- function(df, 
                                 m1, m2, 
                                 type = c("list", "df")) {
  type <- match.arg(type)

  m1 <- df[ , m1]
  m2 <- df[ , m2]

  mean_values <- calculate_mean_values(m1 = m1, m2 = m2)
  differences <- calculate_diff_values(m1 = m1, m2 = m2)
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
      df, mean_values, differences, mean_differences,
      upper_limit = limits[1], lower_limit = limits[2]
    )
  }
}


## Task 3: Task 3: Create a Bland and Altman plot ----

### Function to create a Bland and Altman plot - basic plot ----

# basic plot: scatter plot of mean values and difference of values with lines
# for mean of differences in values, upper limits of agreement, lower limits
# of agreement
#   - ba_metrics - a data.frame with metrics needed for Bland and Altman plot
#     produced by calculate_ba_metrics() function

plot_ba <- function(ba_metrics) {
  plot(
    x = ba_metrics$mean_values, 
    y = ba_metrics$differences
  )

  abline(h = ba_metrics$mean_differences)
  abline(h = ba_metrics$upper_limit)
  abline(h = ba_metrics$lower_limit)
}

### Function to create a Bland and Altman plot - plot with labels ----

# plot with labels: scatter plot of mean values and difference of values with
# lines for mean of differences in values, upper limits of agreement, lower
# limits of agreement, and appropriate labels.
#   - ba_metrics - a data.frame with metrics needed for Bland and Altman plot
#                  produced by calculate_ba_metrics() function
#   - title - A character value for the title of plot. Set to show Bland and 
#     Altman Plot
#   - xlab, ylab - A character value for the x- and y-axis labels respectively;
#     By default, set to NULL to show default axis labels used by R
#   - limits_lab - Logical value (TRUE or FALSE); if TRUE (default), labels
#     are added to the lines for mean of differences and the limits of agreement

plot_ba <- function(ba_metrics, 
                    title = "Bland and Altman plot",
                    xlab = NULL, ylab = NULL, limits_lab = TRUE) {
  plot(
    x = ba_metrics$mean_values, 
    y = ba_metrics$differences,
    main = title,
    xlab = xlab, ylab = ylab
  )

  abline(h = ba_metrics$mean_differences, lty = 2, lwd = 0.7)
  abline(h = ba_metrics$upper_limit, lty = 2, lwd = 0.7)
  abline(h = ba_metrics$lower_limit, lty = 2, lwd = 0.7)

  if (limits_lab) {
    ## Label for mean differences line
    text(
      x = max(ba_metrics$mean_values), y = ba_metrics$mean_differences, 
      labels = paste0(
        "Mean difference: ", round(ba_metrics$mean_differences, digits = 1)
      ),
      pos = 2, cex = 0.70
    )

    ## Label for upper limit of agreement
    text(
      x = max(ba_metrics$mean_values), y = ba_metrics$upper_limit, 
      labels = paste0(
        "Upper limit: ", round(ba_metrics$upper_limit, digits = 1)
      ),
      pos = 2, cex = 0.70
    )

    ## Label for lower limit of agreement
    text(
      x = max(ba_metrics$mean_values), y = ba_metrics$lower_limit, 
      labels = paste0(
        "Lower limit: ", round(ba_metrics$lower_limit, digits = 1)
      ),
      pos = 2, cex = 0.70
    )
  }
}

### Function to create a Bland and Altman plot - plot with labels & colours ----

# plot with labels and colours: scatter plot of mean values and difference of 
# values with lines for mean of differences in values, upper limits of agreement,
# lower limits of agreement, and appropriate labels.
#   - ba_metrics - a data.frame with metrics needed for Bland and Altman plot
#                  produced by calculate_ba_metrics() function
#   - title - A character value for the title of plot. Set to show Bland and 
#     Altman Plot
#   - xlab, ylab - A character value for the x- and y-axis labels respectively;
#     By default, set to NULL to show default axis labels used by R
#   - limits_lab - Logical value (TRUE or FALSE); if TRUE (default), labels
#     are added to the lines for mean of differences and the limits of agreement
#   - pch - An integer value for the character type to use for the points of the
#     plot. See ?pch for details. Default is NULL which will use default pch
#     value used by R (which is 1 for a hollow circle)
#   - col - A colour specification to use for colouring the points in the
#     scatter plot. Default is NULL which sets the colour to default colour used
#     by R (black). Note that for hollow points (pch from 0 to 14) and for
#     points with a fill element (pch from 21 to 25), col will
#     colour the outline of the point. For pch values from 15 to 19, col will
#     colour the whole point. To colour the fill element of points specified by
#     pch from 21 to 25, see bg argument.
#   - bg - A colour specification to use for colouring the fill element of
#     points with a fill element (pch from 21 to 25). Default is NULL which
#     sets the fill element to a light gray.
#   - cex - Character expansion numeric value for the points of the scatter
#     plot. Default is NULL which will use the default size of points used by R
#     which is a value of 1.

plot_ba <- function(ba_metrics, 
                    title = "Bland and Altman plot",
                    xlab = NULL, ylab = NULL, limits_lab = TRUE,
                    pch = NULL, col = NULL, bg = NULL, cex = NULL) {
  plot(
    x = ba_metrics$mean_values, 
    y = ba_metrics$differences,
    main = title,
    xlab = xlab, ylab = NULL,
    pch = pch, 
    col = ifelse(is.null(col), "black", col), 
    bg = bg,
    cex = cex
  )

  abline(h = ba_metrics$mean_differences, lty = 2, lwd = 0.7)
  abline(h = ba_metrics$upper_limit, lty = 2, lwd = 0.7)
  abline(h = ba_metrics$lower_limit, lty = 2, lwd = 0.7)

  if (limits_lab) {
    ## Label for mean differences line
    text(
      x = max(ba_metrics$mean_values), y = ba_metrics$mean_differences, 
      labels = paste0(
        "Mean difference: ", round(ba_metrics$mean_differences, digits = 1)
      ),
      pos = 2, cex = 0.70
    )

    ## Label for upper limit of agreement
    text(
      x = max(ba_metrics$mean_values), y = ba_metrics$upper_limit, 
      labels = paste0(
        "Upper limit: ", round(ba_metrics$upper_limit, digits = 1)
      ),
      pos = 2, cex = 0.70
    )

    ## Label for lower limit of agreement
    text(
      x = max(ba_metrics$mean_values), y = ba_metrics$lower_limit, 
      labels = paste0(
        "Lower limit: ", round(ba_metrics$lower_limit, digits = 1)
      ),
      pos = 2, cex = 0.70
    )
  }
}


