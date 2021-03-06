# Functions to work with Instron Bluehill 2.0 RawData .csv files

# Bluehill works with samples (created when you start a test) containing specimens
# The user chooses a sample name and a folder in which to save the sample & results (the study folder)
#
# within the study folder are:
#   <sample_name>.is_<test_type>	an xml file describing the sample/test
#   <sample_name>.im_<test_type>	an xml file describing the test method
#   <sample_name>.id_<test_type>	a binary file containing the recoreded data (magic number TDAT)
#
# if the data or results have been exported they appear as:
#   <sample_name>.is_<test_type>_RawData	a folder containing exported raw data as Specimen_RawData_<sample_no>.csv
#   <sample_name>.is_<test_type>_Results.csv	a CSV file containing any calcuated results
#
# the <test_type>s available on our system are:
#   tens	tension
#   comp	compression
#
#   tcyclic	tension profile
#   ccyclic	compression profile
#
#   trelax	tension creep/relaxation
#   crelax	compression creep/relaxation
#
#   flex	bending
#

# Bluehill 2.0 exported data are structured in folders by sample & specimen.
# The sample folder name is chosen by the user at the start of a test.
#
# Each sample uses a single test method (which may be changed during the test,
# complicating matters)
#
# project
#   study
#     Sample_1
#       Specimen_1
#       ...
#       Specimen_n
#     ...
#     sample_n
#       Specimen_1
#       ...
#       Specimen_n
#
# For each sample, all specimens should have the same columns
# Specimen files can optionally contain a header, which may contain a Specimen Label
#
# Each Specimen represents one continuous run of the testing machine
# (what could normally be thought of as a 'test').
# If there are results files, they are in the study folder and named sample_n_Results.csv
#
# At RPH we have used two conventions:
# Each Specimen in a seperate test folder (so only one Specimen_Rawdata_ncsv file per folder)
# or multiple Specimens in a test folder
#

#--------------------------------------------------------#
# globalVariables(c("x.values", "y.values"))
#--------------------------------------------------------#
# This is an awful hack and I hate having to use it, but #
# it was suggested as the least worst option by @hadley. #
# The other suggested solution (favoured by @mattdowle)  #
# is to define each offending variable = NULL somewhere  #
# higher in the function. I shall try the latter.        #
#--------------------------------------------------------#

######################################
# functions to read Bluehill 2.0 files
######################################
# Bluehill files have an optional header of study parameters etc. followed by a
# blank line then two lines of column header: variable names in the first line,
# units in the second then the data in columns

# regular expression to match Bluehill RawData files
raw_regex <- ".*RawData.*\\.csv" # what a raw data filename looks like
# regex to extract rough test name and specimen ID i.e. the name of the Sample
# folder, which is the last part of the pathname of a RawData folder (minus
# Instron decoration) and the number of each Specimen file
id_regex <- paste0(".*", .Platform$file.sep,
                   "(.+).is_.+_RawData.*Specimen_RawData[\\_\\.](\\d+)\\.csv")
id_parts <- c("filename", "sample", "specimen")

#' Read any headers in a RawData file
#'
#' RawData files can contain header rows that describe the test as parameter:
#' value pairs. This function reads any parameters into a \code{data.table} with
#' columns for "type", "var", "value" and "units".
#' A special header is added to describe the location of the end of the header
#' as \code{information, blank_row, n} where\code{n} is the row number of the
#' blank line that marks the end of the header (0 is there was no header).
#' This can be passed to \code{bh_read_data} to indicate where the data starts.
#'
#' @param filename the RawData file to process
#' @return A \code{data.table} with any parameter: value pairs and
#'   the header \code{blank_row} indicating the end of the header (0 if no headers)
#' @import data.table
#' @export bh_read_header_1

bh_read_header_1 <- function(filename) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  V1 <- NULL

  # check filename for an instron header & process if there is one
  max_header <- 100 # assume that any header is <= 100 lines
  header <- fread(filename, blank.lines.skip = FALSE, sep = NULL,
                  header = FALSE, nrows = max_header)
  blank_row <- header[, which(V1 == "")]
  # get the column headers from the first row after the blank line
  # or the first row if blank_row == 0
  # there are two rows of header (var name then units)
  col_headers <- header[blank_row + (1:2), ]
  # if there is a header find where it ends & split out the contents
  if (blank_row > 0) {
    # header rows have up to 4 parts
    # type : var.name, value, units
    # so split on ' : ' or ',' (can't use ':' as times have colons)
    header <- header[1:blank_row - 1, tstrsplit(V1, split = c(" : |,"))]
  } else {
    # no blank line means there was no header, so make an empty one
    header <- data.table()
  }

  # save the blank line location
  blank_loc <- data.table(V1 = "information", V2 = "blank_row", V3 = blank_row)
  # add to header, using only matching columns as header may only have 3
  header <- rbindlist(list(header, blank_loc), use.names = TRUE)

  # name the columns, only using names that have a corresponding column
  setnames(header, c("type", "var", "value", "units")[1:ncol(header)])
  return(header)
}

#' Read the data from a RawData file
#'
#' RawData files can contain header rows that describe the test, followed by a
#' blank line and then the measured channels in columns. This function reads the
#' data channels, starting after \code{blank_row} to skip any headers. Data
#' channels have two line headers, with the channel names in the first line and
#' units in the second. This function discards channel units.
#' If \code{min_results} = TRUE then only the Time, Extension and Load channels
#' are returned.
#' By default all the channels in \code{filename} are read.
#'
#' @param filename the RawData file to process.
#' @param blank_row line number of the end of any header. Defaults to 0 (no header).
#' @param min_results if TRUE only return the minimal subset of channels. Defaults to FALSE.
#' @return A \code{data.table} with the measured channels in columns and the channel names as column names.
#' @import data.table
#' @export bh_read_data_1

bh_read_data_1 <- function(filename, blank_row = 0, min_results = FALSE) {

  col_names <- names(fread(filename, blank.lines.skip = TRUE,
                        skip = blank_row, nrows = 1))
  select_cols <- seq_along(col_names)

  if (min_results) {
    min_columns <- c("Time", "Extension", "Load")
    # throw an error unless the minimal column set is in the file
    all_in_header <- all(min_columns %chin% col_names)
    select_cols <- which(min_columns %chin% col_names)
    stopifnot(all_in_header)
    col_names <- min_columns
  }

  # there are two rows of header (var name then units)
  # so the actual data starts 3 rows below the blank line/start of file
  first_data_row <- blank_row + 3
  data <- fread(filename, blank.lines.skip = TRUE,
                skip = first_data_row, select = select_cols)
  setnames(data, col_names)
  return(data)
}

#' Minimal Bluehill columns
#'
#' A constant naming the minimal column names for a Bluehill test:
#' Time, Extension, Load
#'
#' @export bh_min_cols

bh_min_cols <- c("Time", "Extension", "Load")

#' Read the data from a RawData file
#'
#' RawData files can contain header rows that describe the test, followed by a
#' blank line and then the measured channels in columns. This function reads the
#' data channels, starting after \code{blank_row} to skip any headers. Data
#' channels have two line headers, with the channel names in the first line and
#' units in the second. This function discards channel units.
#' If \code{min_results} = TRUE then only the Time, Extension and Load channels
#' are returned.
#' By default all the channels in \code{filename} are read.
#'
#' @param filename the RawData file to process.
#' @param blank_row line number of the end of any header. Defaults to 0 (no header).
#' @param select_cols a list of columns to return by number (or all if NULL). Defaults to NULL.
#' @return A \code{data.table} with the measured channels in columns and the channel names as column names.
#' @import data.table
#' @export bh_read_data

bh_read_data <- function(filename, blank_row = 0, select_cols = NULL) {

  col_names <- names(
    fread(filename, blank.lines.skip = TRUE,
          skip = blank_row, nrows = 1)
  )
  use_col_nums <- seq_along(col_names) # start with all columns

  if (length(select_cols) > 0) {
    # check if these columns are in the file
    if (is.numeric(select_cols)) {
      all_in_header <- (max(select_cols) <= length(col_names))
      use_col_nums <- select_cols
      col_names <- col_names[select_cols]
    }
    if (is.character(select_cols)) {
      all_in_header <- all(select_cols %chin% col_names)
      # throw an error unless they are
      stopifnot(all_in_header)
      # otherwise get the column numbers for the selected column names
      use_col_nums <- which(select_cols %chin% col_names)
      # and just use the given names
      col_names <- select_cols
    }
  }

  # there are two rows of header (var name then units)
  # so the actual data starts 3 rows below the blank line/start of file
  first_data_row <- blank_row + 3
  data <- fread(filename, blank.lines.skip = TRUE,
                skip = first_data_row, select = use_col_nums)
  setnames(data, col_names)
  return(data)
}

#' Find RawData files
#'
#' Starting from \code{study_root}, grab the names of everything that looks like
#' an Instron specimen as defined by \code{raw_regex}. The default is all
#' RawData files under \code{study_root}. If only a sub-set of the sample files
#' are needed, supply a regex to select just those. for example, if there are
#' several samples under study.root and we only want those for "bone",
#' use something like
#'   bh_read_specimens(study_root, raw_regex = ".*bone.*RawData.*\\.csv")
#' Or better, gather all the file names and filter the returned data.table:
#'   bh_read_specimens(study_root)[sample %like% "bone", ]
#' Extracts the sample name is extracted from the name of the folder enclosing
#' the specimen files and the specimen number from the name of each specimen
#' (RawData) file.
#'
#' @param study_root the path where to start searching for specimen RawData files.
#' @param raw_regex defines what files to read. Defaults to all RawData files.
#' @return A \code{data.table} with the full path name to each specimen file
#'   (which should be unique) and extracted sample and specimen identifiers.
#' @import data.table
#' @import stringr
#' @export bh_find_specimens

bh_find_specimens <- function(study_root, raw_regex = ".*RawData.*\\.csv") {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  ID <- specimen <- NULL

  # harvest all the names that look suitable
  files <- list.files(path = study_root, pattern = raw_regex,
                      recursive = TRUE, full.names = TRUE,
                      ignore.case = TRUE)

  samples <- as.data.table(str_match(files, id_regex))
  samples <- stats::na.omit(samples) # drop non-matched results files
  # name the parts extracted by the identifier regex
  # (plus the filename which str_match returns first)
  setnames(samples, id_parts)

  # make a unique ID (sample.specimen) for each result
  samples[, ID := paste(sample, specimen, sep = ".")]
}


#' Get Bluehill Headers
#'
#' Given a \code{data.table} containing the path names of the desired RawData
#' files in column \code{filename}, return all the headers for each specimen in
#' a \code{data.table} keyed by filename and the header names (in column
#' \code{var}).
#'
#' @param samples A \code{data.table} with the pathname of each specimen file in column \code{filename}.
#' @return A \code{data.table} with the headers (if any) from each specimen file
#'   read using \code{bh_read_header}.
#' @import data.table
#' @import stringr
#' @export bh_get_headers

bh_get_headers <- function(samples) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  filename <- var <- value <- NULL

   # load all the headers, which incldue the data start row
  headers <- samples[, bh_read_header(filename), by = filename]
  # will be getting info by filename, so set that as the key
  # also set "var", so can index by headers[J(filename, "blank_row"),]
  setkey(headers, filename, var)
  # it turns out that the "Specimen label" values has quotes in the field
  # strip any quotes as we don't want them anywhere
  headers[, value := str_remove_all(value, "\"")]
  return(headers)
}

# early version of extracting the blank_row and label from the header
bh_header_parts <- function(headers, parts = c("blank_row", "Specimen label")) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  filename <- value <- NULL

  headers[parts,
          transpose(list(value[1:length(parts)])),
          on = "var", by = filename]
}

#' Get important sample labels from headers
#'
#' Extracts the \code{blank_row} and \code{Specimen Label} parts a specimen header.
#'
#' @param headers A \code{data.table} possibly containing a \code{blank_row} and \code{Specimen Label}.
#' @return a named list containing the \code{blank_row}, \code{Specimen Label} and \code{filename} for each specimen.
#' @import data.table
#' @export bh_get_labels

bh_get_labels <- function(headers) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  filename <- value <- NULL

  parts <- c("blank_row", "Specimen label")
  headers[parts,
          list(blank_row = as.integer(value[1]), label = value[2]),
          on = "var", by = filename]
}


# lines from headers that have var == "blank_row" or "Specimen label"
# return the values as a list of two columns, by filename
# join that data.table to samples, on filename, and assign the two columns
# to 'blank_row' and 'label' respectively.
# Will give NA if either (but not both) var missing in original headers data.table
# probably better to do this explicitly in the main code, but using specimen_labels
# which returns .(blank_row, label) as that is more readable than .(V1, V2)

#' Add header information to samples
#'
#' Copies the \code{blank_row} and \code{Specimen Label} parts from a
#' \code{data.table} of specimen headers and adds them to a \code{data.table} of
#' samples. Will give NA if either (but not both) headers are missing. It's
#' probably better to do this explicitly in the main code, but using
#' specimen_labels which returns .(blank, label) as that is more readable than
#' .(V1, V2).
#'
#' @param samples A \code{data.table} of samples.
#' @param headers A \code{data.table} of sample headers.
#' @return \code{samples} with added columns for \code{blank_row} and \code{label} for each specimen.
#' @import data.table
#' @export bh_label_samples

bh_label_samples <- function(samples, headers) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  V1 <- V2 <- NULL

  samples[bh_header_parts(headers),
          c("blank_row", "label") := list(V1, V2), on = "filename"]
}

#' Change sign of compressive test
#'
#' Compression is defined as negative, so compressive tests have negative Load and Extension.
#' This can look odd when plotted, so Bluehill provides \code{Compressive Load} and \code{Compressive Extension}
#' This function inverts \code{Load} and \code{Extension}
#'
#' @param sample_data A \code{data.table} containing at least columns for \code{Load} and \code{Extension}.
#' @return The same \code{data.table} with \code{ -1.0 * Load} and \code{-1.0 * Extension}
#' @import data.table
#' @export bh_make_compressive

bh_make_compressive <- function(sample_data) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  Extension <- Load <- NULL

  sample_data[, `:=`(Extension = -Extension, Load = -Load)]
}

#' Slack Correction
#'
#' Remove any slack at the start of a test (or cycle)
#'
#' The Bluehill version sets the limits at 2\% and 80\% of maximum then uses
#' AutoSlope to get the estimated slope and sets the new zero to where that line
#' intersects y = 0.
#'
#' This version justs fits a single line to the whole range from 2\% to 80\% of maximum
#' intercept from that to define the zero point. Rows before that point are
#' removed and the first row subtracted from both \code{Time} and \code{Extension}
#' so that they start from zero. \code{Load} is unchanged.
#'
#' Really just a wrapper for \code{trim_slack()}.
#'
#' @param DT a \code{data.table} with \code{Load} and \code{Extension} channels to be corrected.
#' @return The original series with any initial slack portion removed.
#' @export bh_slack_correct
#'

bh_slack_correct <- function(DT) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  Load <- Extension <- NULL

  # analyse from 2% to 80% of max
  trim_slack(DT, Load ~ Extension, lo = 0.02, hi = 0.80)
}

