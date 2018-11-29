## originally from BEE18-003_locked_plate_span_-_effect_on_strain
library(ggplot2)

# this assumes this file is in the current working folder
source('instron_functions.R') 
# fix this for production work

# Load sample information ----------------------------------------

# define the study (i.e. where to start looking for samples)
study_root <- "./instron samples"

# Find all the RawData files in the study and extract the headers
samples <- bh_find_specimens(study_root)
headers <- bh_get_headers(samples)

# augment the samples table by adding in important bits of the headers
# i.e. the blank row number and the specimen label
# get the 'blank_row' and 'label' bits from the header & join by the
# (unique) filename to samples, then replace samples with updated table
samples <- samples[bh_get_labels(headers), on = "filename"]
# makes a copy of the data.table, but doubt will ever be big enough to cause a problem 

# Parse the labels, sample, specimen and file names to 
# identify & group the RawData acording to what is important for this study.
# Includes things like labelling repeats, distinct physical samples etc.

# material in filename
material_regex <- "^[^ ]* ([^\\.]+)\\.\\d+$"
samples[, material := str_match(ID, material_regex)[, -1]]

# bone ID and repeat in label as bone.repeat, so use strsplit
samples[, c("bone", "rep") := tstrsplit(label, "\\.")]

# Make any required fix-ups for typos & other errors
# got a label wrong...
samples[bone == "22L1", bone := "221L"]

# Load the data ------------------------------------------------

# Read all the raw data files listed in samples into a single data.table
# Note that these all need to have the same number of columns.
# samples includes everything found under study_root so probably need to
# make sure only use RawData files from the same study
# by, e.g., selecting sample names that are comparable

# In this example only select the "multi" studies
sample_data <- samples[sample %like% "multi", 
                       bh_read_data(filename, blank_row, in_min_columns),
                       by = filename]

# Combine sample info with data ----------------------------------------

# Add the identifiers etc. collected in samples, keyed by the filename
sample_data <- samples[sample_data, on = "filename"]

# For compression tests can invert the Load & Extension to plot the "right" way up
bh_make_compressive(sample_data)

# For each specimen, label the cycles and segments based on Extension
sample_data[, c("cycle", "seg", "peaks") := label_cycles(Extension), by = ID]

# only interested in the loading part of cycle 3, so could subset data
useful_data <- sample_data[cycle == 3 & seg == "load", ]

# make a plot to check that sample repeats are similar
p1 <- ggplot(useful_data) + 
      aes(x = Extension, y = Load, group = ID, colour = rep) + 
      geom_path() + 
      facet_grid(cols = vars(bone), labeller = label_both)
print(p1)
