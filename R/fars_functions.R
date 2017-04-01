#' Read file
#'
#' @details This is a simple function that expects a csv filename as argument, checks if the file exists and if
#' it exists it reads the file to an object and converts it to a data frame.
#'
#' @note Note that this function fails if the filename is not passed as argument.
#'
#' @param filename A character containing the file name
#'
#' @return This function returns a dataframe containing the data from the csv file passed by filename
#'
#' @importFrom readr read_csv
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2015.csv.bz2")
#'
#' \dontrun{
#' fars_read()
#' fars_read(accident_2015.csv.bz2)
#' }
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create or construct filename
#'
#' @details This is a simple function which expects a year as an argument, converts the year to integer, constructs the
#' file name by appending year to the filename and returns it.
#'
#' @note Note that this function fails if argument year is not passed as argument.
#'
#' @param year A character containing the year.
#'
#' @return This function returns a final filename having the year.
#'
#' @examples
#' make_filename("2001")
#' make_filename(1999)
#'
#' \dontrun{
#' make_filename()
#' make_filename("Test")
#' }
#'
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read list of files and returns a dataframe containing all months and years from the files.
#'
#' @details This function that expects a list of years, for each year in the list, construct the file name.
#' Once the file name is created, read the content of file to the dataframe, create new field year
#' and return month and year fields from dataframe.
#'
#'
#' @note Note that this function fails if the years is not passed as argument.
#'
#' @param years A list of characters containing the years
#'
#' @return This function returns a dataframe having the list of month and years from the files read based
#' on the list of years passed as parameter to function. This function also prints a warning if there is any error
#' while reading the file or processing the data frame.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' fars_read_years(c("2013", "2014"))
#' fars_read_years(c(2013, 2015))
#' fars_read_years(2013)
#'
#' \dontrun{
#' fars_read_years()
#' fars_read_years("1900")
#' }
#'
#'
fars_read_years <- function(years) {
  browser()
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

#' Read list of files and returns a dataframe containing list of months and list of records for each year.
#'
#' @details This function expects a list of years, reads the content from all the files of these years
#' using the internal method \code{\link{fars_read_years}}  for each year in the list and saves the result
#' to a data frame. This dataframe is then processed to get a a final dataframe having the summary with
#' list of months and total number of records for each year.
#'
#'
#' @note Note that this function fails if the years is not passed as argument.
#'
#' @param years A list of characters containing the years
#'
#' @return This function returns a dataframe having the summary with list of months and number of records
#' in the files for each year. This function also prints a warning if there is any error
#' while reading the files or processing the data frame.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(c("2013", "2014"))
#' fars_summarize_years("2014")
#'
#' \dontrun{
#' fars_summarize_years()
#' fars_summarize_years("1900")
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Creates a map of the state and plots the points where there is an accident.
#'
#' @details This function expects a valid state number and year. This function uses internal method
#' \code{\link{make_filename}} to construct a file name, \code{\link{fars_read}} for reading the
#' file to a dtaframe. Once the dataframe is populated, this function validates if the user passed
#' state number exists in the dataframe and filters the records that match the state number. The coordinates
#' in the dataframe are filtered and draws a graph based on the range of latitudes and longitudes and
#' plots the points.
#'
#'
#' @note This function fails if the state number pased is invalid
#' @note This function fails if there are no entries of the state and year passed to the function.
#'
#' @param state.num A character containing State Number
#' @param year A character containing a year
#'
#' @return This function does not return anything. It draws a plot having a map and the points.
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @import maps
#'
#' @examples
#' fars_map_state(1, "2013")
#'
#' \dontrun{
#' fars_map_state(1)
#' fars_map_state("2013")
#' fars_map_state(1, "1999")
#' }
#'
#' @export
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


