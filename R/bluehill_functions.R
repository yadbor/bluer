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


#' Minimal Bluehill columns
#'
#' A constant naming the minimal column names for a Bluehill test:
#' Time, Extension, Load
#'
#' @export bh_options
#'
bh_options <- list(raw_regex = ".*RawData.*\\.csv", # what a raw data filename looks like
                   max_header = 100,  # assume that any header is <= 100 lines
                   min_columns = c("Time", "Extension", "Load"), # minimal useful test results
                   id_regex = file.path(".*",
                                      "(.+).is_.+_RawData.*Specimen_RawData[\\_\\.](\\d+)\\.csv"),
                   id_parts = c("filename", "sample", "specimen"),
                   min_results = TRUE
)

#------------------------------------#
# functions to read Bluehill 2.0 files
#------------------------------------#

#' Set __bluer__ options
#'
#' Provides an interface to set global options of processing BlueHill files.
#'
#' @param raw_regex defines what the names of Raw files look like.
#' @param max_header max number of rows in a header. Defaults to 100.
#' @param min_columns a BlueHill test must contain at least three columns.
#'     Time, Extension & Load
#' @param id_regex to extract common identifying parts of a pathname
#' @param id_parts the names of the common identifying parts of a pathname
#' @return a named list of options.
#'     Sets the global \code{bh_options} as a side effect.
#' @export bh_set_options
#'

bh_set_options <- function(raw_regex = ".*RawData.*\\.csv$",
                           max_header = 100,
                           min_columns = c("Time", "Extension", "Load"),
                           id_regex = file.path(".*",
                                                "(.+).is_.+_RawData.*Specimen_RawData[\\_\\.](\\d+)\\.csv"),
                           id_parts = c("filename", "sample", "specimen")

                           ) {
  bh_options$raw_regex <- raw_regex
  bh_options$max_header <- max_header
  bh_options$min_columns <- min_columns
  bh_options$id_regex <- id_regex
  bh_options$id_parts <- id_parts
}

#' Find the direction of a test (compressive or tensile)
#'
#' Compressive is defined as -ve Load or Extension (if below crosshead)
#' so test if the largest value (i.e. the furtherest from the start point)
#' is greater or less than the start point.
#' @param channel which channel to test for direction.
#'   Defaults to Load.
#' @return +1 if Tensile, -1 if Compressive

test_direction <- function(channel) {
  sign(channel[which.max(abs(channel))]-channel[1])
}

#' Read any headers in a RawData file
#'
#' RawData files can contain header rows that describe the test as
#' parameter : value pairs. This function reads any parameters into
#' a \code{data.table} with columns for "type", "var", "value" and "units".
#' A special var \code{blank_row} of type \code{information} is added at
#' the end with the row number of the end of the header, or 0 if there was
#' no header in the file. This can be passed to \code{bh_read_data}.
#'
#' @param filename the RawData file to process
#' @return A \code{data.table} with any parameter : value pairs and
#'   the special header \code{blank_row} indicating the end of the header,
#'   or 0 if there was no header.
#' @import data.table
#' @export bh_read_header

bh_read_header <- function(filename) {
  # Horrible hack to avoid R CMD Check complaining about no visible binding.
  var <- value <- V1 <- NULL

  # sep = NULL just reads lines into one column.
  header <- data.table::fread(filename,
                              blank.lines.skip = FALSE,
                              sep = NULL,
                              header = FALSE,
                              nrows = bh_options$max_header)

  # Find the empty row, or 0 if no empty row.
  blank_row <- header[, which(V1 == "")]
  # If there is a header find where it ends & split out the contents.
  if (blank_row > 0) {
    # Header rows have up to 4 parts:
    #   type : var.name, value, units
    # so split on ' : ' or ',' (can't use ':' as time values contain colons).
    header <- header[1:blank_row, data.table::tstrsplit(V1, split = c(" : |,"))]
    # Name the columns, only using names that have a corresponding column.
    use_names <- c("type", "var", "value", "units")[1:ncol(header)]
    data.table::setnames(header, use_names)
    # Add the blank row info, but need to coerce into the existing column type.
    header[blank_row, `:=`(type = "information", var = "blank_row",
                           value = as.character(blank_row))]
    # It turns out that the "Specimen label" values has quotes in the field.
    # Strip any quotes in all fields as we don't want them anywhere.
    header[, value := stringr::str_remove_all(value, "\"")]
  } else {
    # If there was no header make one with just the blank_row info.
    header <- data.table::data.table(type  = "information",
                                     var   = "blank_row",
                                     value = blank_row)
  }
  data.table::setkey(header, var) # Make it fast & easy to look up variables.

  return(header)
}

#' Read a BlueHill RawData file
#'
#' RawData files can contain optional header rows that describe the test as
#' parameter: value pairs, followed by a blank row, then the recorded data.
#'
#' The data part has two rows of column header: variable names in the first row,
#' then units in the second. Subsequent rows have the data in columns.
#'
#' This function reads any parameters into a \code{data.table} with columns for
#' "type", "var", "value" and "units" A special header is added to describe the
#' location of the end of the header as \code{information, blank_row, n}
#' where\code{n} is the row number of the blank line that marks the end of the
#' header (0 is there was no header). This can be passed to \code{bh_read_data}
#' to indicate where the data starts.
#'
#' @param filename the name of the RawData file to process.
#' @param min_results if \code{TRUE} only return three columns:
#'   (Time, Load, Position). Defaults to \code{TRUE}.
#' @param use_label if \code{TRUE} add any \code{label} field in the header
#'   as a column in the output. Defaults to \code{TRUE}.
#' @param use_filename if \code{TRUE} use the filename as the specimen_ID
#'   otherwise add a unique numeric specimen_ID for each file.
#'   Defaults to \code{FALSE}.
#' @param headers if \code{TRUE} return a \code{data.table} containing any
#'   parameter:value pairs and \code{blank_row} indicating the end of the header.
#'   \code{blank_row} = 0 if there were no headers. Defaults to \code{FALSE}.
#' @return if \code{headers == TRUE} a list with data = a \code{data.table} holding
#'   the raw data and header = a \code{data.table} holding headers
#'    if \code{headers == FALSE} just the \code{data.table} holding the raw data
#' @import data.table
#' @export bh_read_raw

bh_read_raw <- function(filename,
                        min_results = TRUE,
                        use_label = TRUE,
                        use_filename = FALSE,
                        headers = FALSE) {
  # Horrible hack to avoid R CMD Check complaining about no visible binding.
  label <- value <- specimen_ID <- NULL

  header <- bh_read_header(filename)

  if (min_results) {
    cols <- bh_options$min_columns
  } else {
    cols <- NULL
  }
  specimen <- bh_read_data(filename,
                           as.integer(header["blank_row", value]),
                           select_cols = cols
                           )
  specimen[, filename := filename]
  if (use_label) {
    specimen[, label := as.character(header["Specimen label", value])]
  }
  if (use_filename) {
    specimen[, specimen_ID := filename]
  } else {
    specimen[, specimen_ID := .GRP, by = filename]
  }

  if(headers) {
    return(list(data = specimen, header = header))
  } else {
    return(specimen)
  }
}

#' Make all tests positive going
#'
#' Compression is defined as negative, so compressive tests have negative Load and Extension.
#' Bluehill provides \code{Compressive Load} and \code{Compressive Extension}, but these are
#' not in every test, so this function checks if \code{Load} is going negative
#' (i.e. a compressive test) and if so inverts \code{Load} and \code{Extension} so
#' that the test will move from low to high extension and low to high load.
#'
#' @param study_data A \code{data.table} containing at least columns \code{Load} and \code{Extension}.
#' @return The same \code{data.table} with \code{Load} and \code{Extension} always going from low to high
#' @import data.table
#' @export bh_normalise

bh_normalise <- function(study_data) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  # to column names
  Extension <- Load <- testID <- V1 <- NULL

  compressive <- study_data[, test_direction(Load), by=testID][V1 == -1, ]
  data.table::setkey(compressive, testID)
  study_data[testID %in% compressive$testID,
             `:=`(Extension = -Extension, Load = -Load)]
}

#' Label cycles & segments using robust_peaks
#'
#' Given a data series, break into cycles (at each trough)
#' break each cycle into load (trough -> peak) and
#' unload (peak -> trough) segments, using turning points
#' from robust_peaks.
#' As tests start at a trough and go to a peak, the testing
#' direction is found by checking sign of the first peak/trough.
#' Only works on "positive going" data, that is where the Load and
#' Extension increase from start to peak.
#' In other words, compressive studies must be inverted first.
#'
#' @param study a BlueHill study to mark with peaks and cycles.
#' @param channel the data channel to search for peaks.
#' @param span size of window to use when looking for peaks. Default is 5.
#'  Larger spans are less sensitive to noise, but effectively smooth the series.
#' @return a list of cycle numbers cycle = 1:n.cycles
#'   and segments seg = rep(c("load","unload"))
#'   both padded to the length of the original series
#' @export bh_label_cycles

bh_label_cycles <- function(study, channel = "Extension", span = 5) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  testID <- Load <- NULL

  # Is this a Study, Sample or Specimen?
  # We don't really care, we just need to know what variables to group by
  # to get indiviual specimens to label. Are any of the possible ID cols
  # in the study column names?
  # If there are multiple possibilities, pick the first one
  possible_id_cols <- c("testID", "sample_ID","specimen_ID")
  id_col <-  possible_id_cols[possible_id_cols %chin% names(study)][1]

  if (is.na(id_col)) {
    stop("couldn't find an ID column in ", names(study))
  }

  # Check the directions as this only works on normalised studies
  # i.e. increasing away from start point.
  directions <- study[, test_direction(Load), by=testID]$V1
  if (any(directions<0)) {
    stop("Can only label cycles in +ve going tests.\n",
         "\tConsder using bh_invert().\n")
  }
  study[, peaks := robust_peaks(get(channel), span),
        by = id_col]
  # Need to create peaks above first so that it can be referenced here
  study[, `:=`(cycle = cycles_from_peaks(peaks),
               seg   = segs_from_peaks(peaks)),
        by = id_col]

  return(study)
}

#' Read a set of tests (specimens), given their filenames.
#' Will add a unique (to this study) testID to each test.
#'
#' @param files a list of file names/paths to read into the study.
#' @return a list containing two \code{data.table}s
#'   \code{headers} holding any headers, indexed by testID and
#'   \code{data}  containing the data rows for all of the files specified.
#'     Only returns \code{bh_options$min_columns} as all tests need
#'     to have the same columns to be collected into a table.
#'   Each specimen has a unique ID in column testID, the same as \code{headers}.
#' @export bh_read_study

bh_read_study <- function(files) {
  # Horrible hack to avoid R CMD Check complaining about no visible binding.
  testID <- pathname <- var <- value <- NULL

  # start with a data.table holding the pathnames of each file
  SH <- data.table::data.table(pathname = files)
  # read the headers
  SH <- SH[, bh_read_header(pathname), by=pathname]
  # make a unique ID number per pathname
  SH[, testID := .GRP, by = pathname]
  data.table::setkey(SH, testID)
  # read the data
  #SD <- SH[, bh_read_raw(pathname), by="pathname,testID"]
  blank <- function(SH, ID) {
    # Horrible hack to avoid R CMD Check complaining about no visible binding.
    var <- value  <- NULL
    SH[testID == ID & var == "blank_row", as.integer(value)]
  }
  SD <- SH[, bh_read_data(pathname, blank_row = blank(SH, testID),
                          select_cols = bh_options$min_columns),
           by="pathname,testID"]
  # fortify data with test labels taken from headers, if any exist
  SD <- SD[SH[var == "Specimen label", list(label=value,testID)], on="testID"]
  #
  return(list(headers = SH, data = SD))
}

# regular expression to match Bluehill RawData files
raw_regex <- ".*RawData.*\\.csv" # what a raw data filename looks like
# regex to extract rough test name and specimen ID
# i.e. the name of the Sample folder, which is the last part of the pathname of
# a RawData folder (minus Instron decoration) and the number of each Specimen file
id_regex <- paste0(".*", .Platform$file.sep,
                   "(.+).is_.+_RawData.*Specimen_RawData[\\_\\.](\\d+)\\.csv")
id_parts <- c("filename", "sample", "specimen")

#' Minimal Bluehill columns
#'
#' A constant naming the minimal column names for a Bluehill test:
#' Time, Extension, Load
#'
#' @export bh_min_cols

bh_min_cols <- c("Time", "Extension", "Load")

#' Read the data from a RawData file
#'
#' RawData files can contain header rows that describe the test, followed by a blank line
#' and then the measured channels in columns. This function reads the data channels, starting
#' after \code{blank_row} to skip any headers. Data channels have two line headers, with the
#' channel names in the first line and units in the second. This function discards channel units.
#' If \code{min_results} = TRUE then only the Time, Extension and Load channels are returned.
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
    data.table::fread(filename,
                      blank.lines.skip = TRUE,
                      skip = blank_row,
                      nrows = 1)
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
  data <- data.table::fread(filename,
                            blank.lines.skip = TRUE,
                            skip = first_data_row,
                            select = use_col_nums)
  data.table::setnames(data, col_names)
  return(data)
}

#' Find RawData files
#'
#' Starting from \code{study_root}, grab the names of everything that looks like an Instron
#' specimen as defined by \code{raw_regex}. The default is all RawData files under \code{study_root}.
#' If only a sub-set of the sample files are needed, supply a regex to select just those.
#' for example, if there are several samples under study.root and we only want those for "bone",
#' use something like
#'   bh_find_tests(study_root, raw_regex = ".*bone.*RawData.*\\.csv")
#' Or better, gather all the file names and filter the returned list.
#'
#' @param study_root the path where to start searching for specimen RawData files.
#' @param raw_regex defines what files to read. Defaults to all RawData files.
#' @return A list with the full path name to each specimen file under study_root.
#' @export bh_find_tests

bh_find_tests <- function(study_root, raw_regex = "") {
  # use the default if no regex specified
  if (raw_regex == "") {
    raw_regex <- bh_options$raw_regex
  }
  # harvest all the names that match,
  # returning the full path to ensure they are unique
  files <- list.files(path = study_root, pattern = raw_regex,
                      recursive = TRUE, full.names = TRUE,
                      ignore.case = TRUE)
  return(files)
}


#' Find RawData files
#'
#' Starting from \code{study_root}, grab the names of everything that looks like an Instron
#' specimen as defined by \code{raw_regex}. The default is all RawData files under \code{study_root}.
#' If only a sub-set of the sample files are needed, supply a regex to select just those.
#' for example, if there are several samples under study.root and we only want those for "bone",
#' use something like
#'   bh_read_specimens(study_root, raw_regex = ".*bone.*RawData.*\\.csv")
#' Or better, gather all the file names and filter the returned data.table:
#'   bh_read_specimens(study_root)[sample %like% "bone", ]
#' Extracts the sample name is extracted from the name of the folder enclosing the
#' specimen files and the specimen number from the name of each specimen (RawData) file.
#'
#' @param study_root the path where to start searching for specimen RawData files.
#' @param raw_regex defines what files to read. Defaults to all RawData files.
#' @return A list with the full path name to each specimen file
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
  data.table::setnames(samples, id_parts)

  # make a unique ID (sample.specimen) for each result
  samples[, ID := paste(sample, specimen, sep = ".")]
}


#' Get Bluehill Headers
#'
#' Given a \code{data.table} containing the path names of the desired RawData files
#' in column \code{filename}, return all the headers for each specimen in a \code{data.table}
#' keyed by filename and the header names (in column \code{var}).
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
  data.table::setkey(headers, filename, var)
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
#' Copies the \code{blank_row} and \code{Specimen Label} parts from a \code{data.table} of specimen headers
#' and adds them to a \code{data.table} of samples. Will give NA if either (but not both) headers are missing.
#' It's probably better to do this explicitly in the main code, but using specimen_labels
#' which returns .(blank, label) as that is more readable than .(V1, V2).
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
#' Bluehill provides \code{Compressive Load} and \code{Compressive Extension}, but these are
#' not in every test, so this function inverts \code{Load} and \code{Extension} so that every
#' test will be the same way up (i.e. move from low to high extension and low to high load).
#'
#' @param study_data A \code{data.table} containing at least columns for \code{Load} and \code{Extension}.
#' @return The same \code{data.table} with \code{ -1.0 * Load} and \code{-1.0 * Extension}
#' @import data.table
#' @export bh_invert

bh_invert <- function(study_data) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  Extension <- Load <- NULL

  study_data[, `:=`(Extension = -Extension, Load = -Load)]
}

#' Slack Correction
#'
#' Remove any slack at the start of a test (or cycle)
#'
#' The Bluehill version sets the limits at 2\% and 80\% of maximum then uses
#' AutoSlope to get the estimated slope and sets the new zero to where that line
#' intersects y = 0.
#'
#' This version justs fits a single line to the range from 2\% to 80\% of maximum
#' and uses the intercept from that to define the zero point. Rows before that point are
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


