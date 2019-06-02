---
title: "Make _bluer_ great again for the first time"
author: "Robert Day"
date: "4/24/2019"
output: html_document
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Sending Messages With Gmailr}
  %\usepackage[utf8]{inputenc}
---


```r
knitr::opts_chunk$set(echo = FALSE)
```

## So the question is, what to do

What is the best way to structure the bluer object model?

The basic unit of BlueHill should be the test. Each test will produce one __.\*RawData.\*.csv__ file. In BlueHill speak these are `Specimens`.

Each `Specimen` is a `.csv` file with an optional header containing specimen data followed by a blank row, then a two row column header (name in row 1 and units in row 2), then the data points.

A sample should have a unique ID (possibly the filename?), an optional sample label, other parameters as saved, and the data points. We might want to plot against UID or label, so need them as columns. Using filename does not appear to increase the size of the object by too much; using 55978 rows of approx 67 char filenames only increases size of table by 675k, compared to using a numeric ID (would be 3,750k if all bytes counted).

A table with 55978 rows using filenames of about 67 chars each, but mostly repeats (only 12 unique names) uses 5158088 bytes. Deleting the filename column drops to 4707976 bytes.
Taing the original and adding an integer UID goes up to 5382160. Deleting the filename column again gives 4932048 bytes; so an integer UID column is 224072 (4 bytes per row).
Using the original character strings is only 8 bytes per row. Using truly random strings each 67 chareacters long for each row is 184 bytes per row. Using only 12 names where each is 67 random characters, repeated to fill each row, is 8 bytes per row. Clearly a hash/lookup table is being used, so the length of the names doesn't matter too much; only the number of unique ocurrances. For example, making each of the 12 names 500 characters long only increases to 8.1 bytes per row.

What does this mean for bluer? The filename can be used as an unambiguous UID, but we should have something shorter to identify each specimen().


```
ADF <- as.data.frame(all.data)
ADT <- copy(all.data)
cat("ADF is",object.size(ADF),"bytes\n")
cat("ADT is",object.size(ADT),"bytes\n")
```

So, a `sample` consists of one or more `specimen`s. Each `specimen` is a single test. Each `sample` is the application of a single `test` protocol. All the `specimen`s in a `sample` have (notionally) the same `test` protocol.

In _bluer_, each `specimen` is a `data.table` representing a single test, and each `sample` is a `data.table` holding all the `specimen`s, identified by `filename` or `UID`.

specimen <- bh_read_raw(filename)

OR, given a data.table samples with a column filename
files <- list.files(pattern=raw.regex, path = data_folder, 
                    recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
samples <- as.data.table(stringr::str_match(files, id.regex))

sample_data <- samples[, 
                       bh_read_raw(filename), 
                       by=filename]
                       
specimen_columns = c(Time, Extension, Load, ..., label, specimen_ID)
sample_columns   = c(specimen_columns, sample_info, sample_ID)
study_columns    = c(sample_columns, study_info, study_ID]

so, functions like

bh_read_specimen
bh_read_sample
bh_read_study

Should we read the headers first & make up the study/sample/specimen info columns,
or keep the headers in a seperate list (linked by specimen_ID),
or read them all at once with "read_study" function?

The study/sample/specimen info may be encoded in the filenames or label, or may be
colpletely separate (e.g. in a lab book). Therefore need a way to read the data in and then label with info as appropriate. As the info can be in the files (e.g. a Label or Notes field) should read the files and then label.

The data objects don't appear to be too huge; about 5 MB for 56000 observations of 13 variables, made up of 2 samples of 6 specimens each, with each specimen having a few (3-4) cycles of readings (roughly 4650 readings each). At those sizes it is reasonable to read everthing in at once.
