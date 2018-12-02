#' Functions to work with Instron Bluehill 2.0 RawData files
#'
#' Bluehill works with samples (created when you start a test) containing
#' specimens The user chooses a sample name and a folder in which to save the
#' sample & results (the study folder).
#'
#' within the study folder are:
#' \preformatted{
#'   <sample_name>.is_<test_type>	an xml file describing the sample/test
#'   <sample_name>.im_<test_type>	an xml file describing the test method
#'   <sample_name>.id_<test_type>	a binary file (magic number TDAT)
#'                                containing the recorded data & results
#'
#' if the data or results have been exported they appear as:
#'   <sample_name>.is_<test_type>_RawData	a folder containing exported raw data
#'                                        as Specimen_RawData_<sample_no>.csv
#'   <sample_name>.is_<test_type>_Results.csv any calcuated results
#' }
#' the <test_type>s available on our system are:
#' \tabular{rrl}{
#'   \tab tens \tab tension\cr
#'   \tab comp \tab compression\cr
#'   \tab \tab \cr
#'   \tab tcyclic \tab tension profile\cr
#'   \tab ccyclic \tab compression profile\cr
#'   \tab \tab \cr
#'   \tab trelax \tab tension creep/relaxation\cr
#'   \tab crelax \tab compression creep/relaxation\cr
#'   \tab \tab \cr
#'   \tab flex \tab bending\cr
#' }
#'
#' Bluehill 2.0 exported data are structured in folders by sample & specimen.
#' The sample folder name is chosen by the user at the start of a test.
#'
#' Each sample uses a single test method (which may be changed during the test,
#' complicating matters)
#' \preformatted{
#' project
#'   study
#'     Sample_1
#'       Specimen_1
#'       ...
#'       Specimen_n
#'     ...
#'     sample_n
#'       Specimen_1
#'       ...
#'       Specimen_n
#' }
#' For each sample, all specimens should have the same columns Specimen files
#' can optionally contain a header, which may contain a Specimen Label
#'
#' Each Specimen represents one continuous run of the testing machine (what
#' could normally be thought of as a 'test'). If there are results files, they
#' are in the study folder and named sample_n_Results.csv
#'
#' At RPH we have used two conventions: Each Specimen in a seperate test folder
#' (so only a single Specimen_Rawdata_1.csv file per folder) or multiple
#' Specimens in a test folder, which wil have several Specimen_Rawdata_n.csv
#' files in the same folder
#'
#'
#' @docType package
#' @name bluer
NULL
