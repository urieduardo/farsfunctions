#' Read file
#' This is a function that reads data from a file and converts it to table data frame format. It returns error
#' if file does not exist
#' @param filename A string with the file name containing data to be read by the function
#' @return This function returns a data frame containing the data in the file
#' @importFrom dplyr tbl_df
#' @examples
#' fars_read(accident_2013.csv.bz2)

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Print the name of a file for a specific year
#' This functions prints the name of a file containing data from a specific year
#' @param year An integer with the year of interest
#' @return This function returns and prints the name of a file containing data from a specific year
#' @examples
#' make_filename(2010)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Obtain data from years
#' This function returns the month number and year from the file corresponding to the variable 'years'.
#' If a year is not included in the file, an error is returned
#' @param years An integer of list thereof containing at least one year of interest.
#' @return This function returns the data corresponding to the years in years
#' @importFrom dplyr mutate select
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2010,2011))
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Obtain summary values of data
#' This function returns the number of accidents of a specific year or set of years for each month
#' @param years An integer of list thereof containing at least one year of interest.
#' @return a data frame is returned containing the number of accidents per month (rows) and year (cols)
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr spread
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Visualize accidents by state
#' This function plots accident locations as dots in a given US state for a given year
#' @param state.num This integer corresponds to a US state
#' @param year An integer correspondonds to year of interest. Function will return an error if more than one
#' year is input as an argument
#' @return This function creates a plot showing locations of accidents in a given US state and year
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @examples fars_map_state(6,2013)
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
