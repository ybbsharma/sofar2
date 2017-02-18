#' Reads a file data and convert to a dateframe table.
#'
#' This function will create a dataframe table by reading the file data. The functions take file name or file path as input
#'
#' @param filename A character string providing the file name or file path for which the dataframe table will be created
#'
#' @return This function returns the dataframe table for the data of \code{filename} which is passed as the argument.
#'  If the file do not exist then the function prints a error  {"file \code{filename} does not exist} message.
#'
#' #@examples
#' #fars_read("xyz.csv")
#' #fars_read("data/xyz.csv")
#'
#' @import dplyr
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Names file with year
#'
#' This function appends the year to the filename. The function take the year as an argument to append to standard file
#' name "accident_\code{%d}.csv"
#'
#' @param year A character string or numeric as year
#'
#' @return the function prints the static filename appended with year name passed as argument. The functions throws
#' an error when a non character or non numeric value is passed as an argument.
#'
#' #@examples
#' #make_filename(1976)
#' #make_filename("1976")
#' #make_filename(76)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Extract Months and years of the FARS files
#'
#' This function take year or list of years as aurgument and returns list of yearwise months and years from the FARS
#' file for the inputted years.
#'
#' @param years a character string or numeric or a list of character stings/ numeric specifying year/ years
#'
#' @return a list of months and years from the FARS files for the inputted \code{years}.
#'
#' @note The function throws a warning of invalid year if the file with year in the name is not found. Also
#'  must be in the current working directory
#'
#'
#'#@examples
#' #fars_read_years("2013")
#' #fars_read_years(2013)
#' #fars_read_years(c(2012, 2013, 2014, 2015))
#' #fars_read_years(c("2012", "2013", "2014", "2015"))
#'
#' @import dplyr
#'
#' @export
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

#' Monthwise and yearwise summary of FARS data
#'
#' This function provide the summary of count for each month of an year for the years passed as argument.
#'
#' @inheritParams fars_read_years
#'
#' @return a table summarizing the FARS data monthwise for the each inputted year.
#'
#' @note The function throws a warning of invalid year if the file with year in the name is not found. Also
#'  must be in the current working directory
#'
#'
#' #@examples
#' #fars_summarize_years("2013")
#' #fars_summarize_years(2013)
#' #fars_summarize_years(c(2012, 2013, 2014, 2015))
#' #fars_summarize_years(c("2012", "2013", "2014", "2015"))
#'
#' @import dplyr
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plots map of the state with accident data for a specified year
#'
#' \code{fars_map_state} returns a FARS report for the state number and year passed as an argument
#'
#' @inheritParams fars_read_years
#' @param state.num a numeric value
#' @param year a character string or numeric
#'
#' @return plots a map for data filtered for the inputed state number from the file where filename contains
#' the inputted year. If the state number is not found the the function will report an error message "invalid STATE
#' number". If not accident records found for the state then a message is returned "no accident to plot" with a return
#' value NULL
#'
#' @note The function replace the Longitude data with NA where the Logitude value is > 900
#' the function replace the Latitude data with NA where the Latitute value is > 90
#'
#'
#'
#' @import dplyr
#' @import maps
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
