# Functions to work with Instron Bluehill 2.0 RawData .csv files

# load useful libraries. Maybe tidyverse would be a better choice?
#library(data.table)
#library(stringr)

source("utility_functions.R")

# Bluehill works with samples (created when you start a test) containing specimens
# The user chooses a sample name and a folder in which to save the sample & results (the study folder)
#
# within the study folder are:
#   <sample_name>.is_<test_type>	an xml file describing the sample/test
#   <sample_name>.im_<test_type>	an xml file describing the test method
#   <sample_name>.id_<test_type>	a binary file cotaining the recoreded data (magic number TDAT)
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
# Each sample uses a single test method (which may be changed during the test, complicating matters)
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
# Each Specimen represents one continuous run of the testing machine (what could normally be thought of as a 'test').
# If there are results files, they are in the study folder and named sample_n_Results.csv
#
# At RPH we have used two conventions:
# Each Specimen in a seperate test folder (so only one Specimen_Rawdata_ncsv file per folder)
# or multiple Specimens in a test folder
#

######################################
# functions to read Bluehill 2.0 files
######################################
# Bluehill files have an optional header of study parameters etc. followed by a blank line
# then two lines of column header: variable names in the first line, units in the second
# then the data in columns

# regular expression to match Bluehill RawData files
raw_regex <- ".*RawData.*\\.csv" # what a raw data filename looks like
# regex to extract rough test name and specimen ID
# i.e. the name of the Sample folder, which is the last part of the pathname of
# a RawData folder (minus Instron decoration) and the number of each Specimen file
id_regex <- paste0(".*", .Platform$file.sep, "(.+).is_.+_RawData.*Specimen_RawData\\_(\\d+)\\.csv")
id_parts <- c("filename", "sample", "specimen")

# read any header in the given RawData file
#
# returns a data.table with the parameter: value pairs and
# the location of the end of the header (0 is there was no header)
# this last can be passed to instron.data to indicate where the data starts
bh_read_header <- function(filename) {
  # check filename for an instron header & process if there is one
  max_header <- 100 # assume that any header is <= 100 lines
  header <- fread(filename, blank.lines.skip = FALSE, sep = NULL,
                  header = FALSE, nrow = max.header)
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
    header <- header[1:blank_row-1, tstrsplit(V1, split = c(" : |,"))]
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

# read the actual data values from a RawData file
# if passed blank_row will start reading from there, skipping any header
# if this is omitted the data is presumed to start at row 0
# data columns have two line headers:
# names in the first line and units in the second
# if min.results = TRUE then only retrun the minimnal subset of columns
# that every specimen should have: Time, Extension, Load
#
# returns a data.table with the column headers and the data columns as per the RawData file
bh_read_data_1 <- function(filename, blank_row = 0, min_results = FALSE) {

  col_names <- names(
                  fread(filename, blank.lines.skip = TRUE,
                        skip = blank_row, nrow = 1)
                  )
  select_cols <- seq_along(col_names)

  if (min_results) {
    min_columns <- c('Time', 'Extension', 'Load')
    # throw an error unless the minimal column set is in the file
    all_in_header <- all(min_columns %chin% col_names)
    select_cols <- which(min_columns %chin% col_names)
    stopifnot(all_in_header)
    col_names <- min_columns
  }

  # there are two rows of header (var name then units)
  # so the actual data starts 3 rows below the blank line/start of file
  first_data_row <- blank_row + 3
  data <- fread(filename, blank.lines.skip = TRUE, skip = first_data_row, select = select_cols)
  setnames(data, col_names)
  return(data)
}

bh_min_columns <- c('Time', 'Extension', 'Load')

bh_read_data <- function(filename, blank_row = 0, select_cols = NULL) {

  col_names <- names(
    fread(filename, blank.lines.skip = TRUE,
          skip = blank_row, nrow = 1)
  )
  use_col_nums <- seq_along(col_names) # start with all columns

  if (length(select_cols) > 0) {
    # check if these columns are in the file
    all_in_header <- all(select_cols %chin% col_names)
    # throw an error unless they are
    stopifnot(all_in_header)
    # otherwise get the column numbers for the selected column names
    use_col_nums <- which(select_cols %chin% col_names)
    # and just use the given names
    col_names <- select_cols
  }

  # there are two rows of header (var name then units)
  # so the actual data starts 3 rows below the blank line/start of file
  first_data_row <- blank_row + 3
  data <- fread(filename, blank.lines.skip = TRUE, skip = first_data_row, select = use_col_nums)
  setnames(data, col_names)
  return(data)
}

# Starting from study_root, grab the names of everything that looks like an Instron specimen
# as defined by raw_regex. If this is not given, the default is all RawData files under study.root
# If only a sub-set of the files are needed, supply a regex to select just those.
# for example, if there are several samples under study.root and we only want those for "bone",
# use something like
#   bh_read_specimens(study_root, raw_regex = ".*bone.*RawData.*\\.csv")
# Or better, gather all the file names and filter the returned data.table:
#   bh_read_specimens(study_root)[sample %like% "bone", ]
#
# The sample name will be extracted from the name of the folder enclosing the specimen files
# and the specimen number from the name of each specimen file
#
# returns a data.table with the full path name to each file (which should be unique to that file)
# and the extracted sample and speciment identifiers
bh_find_specimens <- function(study_root, raw_regex = ".*RawData.*\\.csv") {
  # harvest all the names that look suitable
  files <- list.files(path = study_root, pattern = raw_regex,
                      recursive = TRUE, full.names = TRUE,
                      ignore.case = TRUE)

  samples <- as.data.table(str_match(files, id_regex))
  samples <- na.omit(samples) # drop non-matched results files
  # name the parts extracted by the identifier regex
  # (plus the filename which str_match returns first)
  setnames(samples, id_parts)

  # make a unique ID (sample.specimen) for each result
  samples[, ID := paste(sample, specimen, sep = ".")]
}

# given a data.table containing the complete path names of the desired RawData files
# in column 'filename', return the headers (if any) from each file
# returns a data.table as per instron.header()
bh_get_headers <- function(samples) {
  # load all the headers, which incldue the data start row
  headers <- samples[, bh_read_header(filename), by = filename]
  # will be getting info by filename, so set that as the key
  # also set 'var', so can index by headers[J(filename, "blank_row"),]
  setkey(headers, filename, var)
  # it turns out that the 'Specimen label' values has quotes in the field
  # strip any quotes as we don't want them anywhere
  headers[, value := str_remove_all(value, "\"")]
}

# early version of extracting the blank_row and label from the header
bh_header_parts <- function(headers, parts = c('blank_row', 'Specimen label')) {
  headers[parts,
          transpose(.(value[1:length(parts)])),
          on = 'var', by = filename]
}

# join the parts tuple to the headers data.table on 'var', so only return
# rows where var == parts, returning the two values, named appropriately
# all by filename so we output that column as well
bh_get_labels <- function(headers) {
  parts = c('blank_row', 'Specimen label')
  headers[parts,
          .(blank_row = as.integer(value[1]), label = value[2]),
          on = 'var', by = filename]
}

# lines from headers that have var == 'blank_row' or 'Specimen label'
# return the values as a list of two columns, by filename
# join that data.table to samples, on filename, and assign the two columns
# to 'blank_row' and 'label' respectively.
# Will give NA if either (but not both) var missing in original headers data.table
# probably better to do this explicitly in the main code, but using specimen.labels
# which returns .(blank, label) as that is more readable than .(V1, V2)
bh_label_samples <- function(samples, headers) {
  samples[bh_header_parts(headers), c('blank_row', 'label') := .(V1, V2), on = 'filename']
}

# given some instron data in a data.table, reverse the Extension & Load channels
# to change them from Tensile (as these two channels always are) to Compressive
bh_make_compressive <- function(sample_data) {
  sample_data[, `:=`(Extension = -Extension, Load = -Load)]
}

# ' Calculate the slope(modulus) of a data series using the Bluehill algorithm
# ' Essentially:
# '   divide the span from \code{min(y)} to \code{max(y)} into 5 segments
# '   calculate the slope of each segment
# '   find the pair of segments with the highest average slope
# '   combine those segments and recalcuate the slope for the both together
# '
# ' @param x a vector of ordinate data (often time or extension)
# ' @param y a vector of abcissa data
# ' @return the highest average slope(modulus) of \code{y/x}
# ' @examples
# ' stiffness <- bh_modulus(Extension, Load)
bh_modulus <- function(x, y, intervals = 5) {
  segment
}
