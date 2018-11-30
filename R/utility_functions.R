#' Make cycle labels
#'
#' Create a cycle labels, 1 to nCycles, given a list of
#' turning points found using peaksign
#' The testing direction (i.e. what is a peak)
#' is found by checking sign of the first peak
#' The output can be used for grouping a \code{data.table} by cycle
#' @param peaks the list of turning points from peaksign
#' @return list from 1 to nCycles, each repeated by the
#'   length of the cycle and the whole padded to the length
#'   of the original series.

cycles_from_peaks <- function(peaks) {
  # does the first cycle go up or down?
  direction <- peaks[peaks != 0][1] # sign of the first peak found

  if (direction == 0) {
    # no cycles, so signal that by returning all zeros
    return(rep(0, length(peaks)))
  }

  # otherwise, if there are some cycles
  # +1 = upwards, -1 = downwards, 0 = no peaks
  # cycles break at the trough of each cycle, which is opposite to the peak
  breaks <- which(peaks == -direction)

  # add the endpoints of the series, to capture the first and last cycle/segment
  breaks <- c(0, breaks, length(peaks))
  # how long is each cycle (i.e. how many points between breaks)
  cycle_lengths <- diff(breaks)
  # make a series of 1..n, each repeated for the length of that cycle
  cycles <- rep(1:length(cycle_lengths), times = cycle_lengths)
}

#' Make segment labels
#'
#' Create a list of load/unload segment labels,
#' given a list of turning points found using peaksign
#' the first segment is assumed to be "load", followed by "unload" etc.
#' The output can be used for grouping a \code{data.table} by segment
#' @param peaks the list of turning points from peaksign
#' @return list of seg = c("load","unload") padded to the length of the original series

segs_from_peaks <- function(peaks) {
  # peaks (from peaksign) is
  # +1 = upwards, -1 = downwards, 0 = not a peak
  # segments swap at trough (to "load") and peak (to "unload"),
  # so get both -1 and +1 from peaksign output
  breaks <- which(peaks != 0)
  # add the endpoints of the series, to capture the first and last cycle/segment
  breaks <- c(0, breaks, length(peaks))
  # how long is each segment (i.e. how many points between breaks)
  seg_lengths <- diff(breaks)
  # make a list of ("load", "unload") long enough to have
  # one label for each segment
  # start with load as that is the first direction of movement
  # (don't care if +ve or -ve)
  seg_labels <- rep(c("load", "unload"), length.out = length(seg_lengths))
  # repeat each label n times, where n is the length of each segment
  segs <- rep(seg_labels, times = seg_lengths)
}

#' Label cycles & segments
#'
#' Given a data series, break into cycles (at each trough)
#' break each cycle into load (trough -> peak) and
#' unload (peak -> trough) segments, using turning points
#' from peaksign.
#' As tests start at a trough and go to a peak, the testing
#' direction is found by checking sign of the first peak/trough.
#' @param series the list/vector of data to search for peaks.
#' @param span size of window to use when looking for peaks. Defaults to 3.
#'  Larger spans are less sensitive to noise, but effectively smooth the series.
#'
#' @return a list of cycle numbers cycle = 1:n.cycles
#'   and segments seg = rep(c("load","unload"))
#'   both padded to the length of the original series

label_cycles <- function(series, span = 3) {
  # find the peaks (and their direction -1, 0, +1)
  peaks <- peaksign(series, span, do.pad = TRUE)
  # add cycle & seg columns
  cycle <- cycles_from_peaks(peaks)
  seg   <- segs_from_peaks(peaks)

  list(cycle = cycle, seg = seg, peaks = peaks)
}

#' Simplified linear least squares fit
#'
#' Returns the most useful parts of a basic /code{lm()}
#' fit to simple univariate (y ~ x) data:
#' intercept, slope, p-value and r squared
#' @param formula A standard R formula object, passed to /code{lm()}
#' @param data Values to fit (must contain the columns in /code{formula})
#' @return A named list (int, slope, p, rsq)

lm_simple <- function(formula, data) {
  s  <- summary(stats::lm(formula, data))
  sc <- s$coefficients
  list(int   = sc[1, 1],
       slope = sc[2, 1],
       p     = sc[2, 4], # p of slope
       rsq   = s$r.squared)
}

#' Bluehill style Automatic Slope
#'
#' Uses the Bluehill Automatic Slope algorithm
#' 1. divide the series into six segments
#' 2. calculate the slope for each segment
#' 3. sum adjacent pairs of slope (1 + 2, 2 + 3 ... 5 + 6)
#' 4. find the pair with the maximum sum
#' 5. return the maximum slope in that pair
#' This version uses \code{lm_simple} to return other useful statistics
#' @param DT a \code{data.table} to analyse
#' @param formula a standard R formula specifying which columns to use
#' @return A named list (int, slope, p, rsq) for the segment with maximum slope
#'

slope_auto <- function(DT, formula) {
  segments <- 6 # Instron standard
  # chop into segments and get the slope for each
  cuts <- cut(seq_len(nrow(DT)), breaks = segments, labels = FALSE)
  fits <- DT[, lm_simple(formula, .SD), by = cuts]

  slopes <- fits$slope
  # add adjacent slopes & find the biggest
  max_sum <- which.max(slopes + shift(slopes, type = "lead"))
  # which slope in that pair is biggest (max_sum or max-sum + 1 ?)
  max_of_pair <- which.max(slopes[max_sum:(max_sum + 1)])
  # return the fit for that segment
  fits[max_of_pair + max_sum - 1]
}
