# Utility functions for analysing Instron test data from Bluehill

#' Find Peaks and Troughs Robustly (version 3)
#'
#' My version, starting from Brian Ripley's and informed by all the others.
#'   The previous version found peaks & toughs, but could get false hits
#'   when there was a flat run of the same value. This is not uncommon in
#'   testing data, particularly at low speeds and high data sampling rates,
#'   where the crosshead can pause at the top or bottom of travel and generate
#'   several data points all at the same load/extension.
#'
#'   We solve this by recognising that the starting poing must be a trough
#'   (as we are moving away from start towards maximum) and that the peaks
#'   and troughs must come in strict succession. That is, each peak must be
#'   followed by a trough, and so on. Any additional peaks/troughs are false.
#'
#'   This version only works with data that increase away from the start point.
#'   That means that compressive tests, where the absolute Load & Extension go
#'   negative from start, must be inverted before testing for peaks.
#'
#' @param series The vector of numbers to find peaks in.
#'   Assumed to start low and move towards a peak first.
#' @param span The number of points to check looking for each peak.
#'   Must be odd and >= 3.
#'   Default is 5.
#' @return A vector of (-1 / 0 / 1) if series[i] is ( trough / "normal" / peak ).
#'   Always padded to the length of the input, and awlays starts with a trough.
#'
robust_peaks <- function(series, span=5) {
  if ((span <- as.integer(span)) %% 2 != 1 || span == 1) {
    stop("'span' must be odd and >= 3")
  }
  z <- stats::embed(series, span)
  s <- span %/% 2
  ans <- rep.int(0L, nrow(z))
  # max is peak
  v <- max.col(z, ties.method = "first")  == (1 + s)
  ans[v] <-  1
  # no min.col so uses max(-v) for trough
  v <- max.col(-z, ties.method = "first")  == (1 + s)
  ans[v] <- -1
  # pad back to length of input vector
  pad <- rep.int(0L, s)
  ans <- c(pad, ans, pad)
  # and always start from a virtual trough,
  # as always start with a loading phase
  ans[1] <- -1

  # This algorithm gets false positives for runs of equal value, so clean
  # up by only marking peaks and troughs in strict succession. That is, a
  # peak/trough is only marked if it is different from the preceeding
  # peak/trough.
  # Only operate on elements wich are a potential peak or trough.
  maybe_pk <- ans[ans != 0]
  # Check if each value is a real peak/trough, otherwise erase it.
  # use data.table::shift for speed & as this will mainly be used on
  # data.table columns. If no data.table, use stats::lag instead
  if (require(data.table)) {
    shifted <- data.table::shift(maybe_pk, type="lag", fill = -1)
  } else {
    shifted <- c(-1, maybe_pk[-length(maybe_pk)])
  }
  real_pk <- ifelse(maybe_pk == shifted, 0, maybe_pk)
  # Stuff the corrected peaks back in to the right spots in ans.
  ans[which(ans != 0)] <- real_pk

  return(ans)
}


#' Make cycle labels
#'
#' Create a cycle labels, 1 to nCycles, given a list of turning points found
#' using peaksign The testing direction (i.e. what is a peak) is found by
#' checking sign of the first peak The output can be used for grouping a
#' \code{data.table} by cycle
#' @param peaks the list of turning points from peaksign
#' @return list from 1 to nCycles, each repeated by the length of the cycle and
#'   the whole padded to the length of the original series. If no turning points
#'   are found (i.e. if the test was a ramp) then consider it all as cycle 1, so
#'   return just 1 repeated for the whole test.
#' @export cycles_from_peaks

cycles_from_peaks <- function(peaks) {
  # does the first cycle go up or down?
  direction <- peaks[peaks != 0][1] # sign of the first peak found

  if (is.na(direction) | direction == 0) {
    # no turning points, so make it a single cycle (a ramp maybe?)
    return(rep(1L, length(peaks)))
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
#' @export segs_from_peaks

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
#' @export label_cycles

label_cycles <- function(series, span = 3) {
  # find the peaks (and their direction -1, 0, +1)
  peaks <- peaksign2(series, span, do.pad = TRUE)
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
#' @export lm_simple

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
#' @return A named \code{list(int, slope, p, rsq)} for the segment with maximum slope
#' @import data.table
#' @export slope_auto

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


#' Remove intitial slack
#'
#' Time any initial slack by
#' 1. taking the y data between 2\% and 80\% of max y (usually Load)
#' 2. fit a straight line
#' 3. calculate the x_intercept (usually Extension)
#' 4. calculate x_max, the x value at max y
#' 5. trim off any data point below x_intercept and x_max
#' The Bluehill version would use AutoSlope, but this just fits the whole data.
#' To make the trimmed test start at zero, the first row is subtracted from the
#' dependent (x) variable as specifed in the input \code{formula} and also
#' from \code{Time}, if that column exists.
#' @param DT a \code{data.table} to trim
#' @param formula a standard R formula specifying which columns to use as x & y
#' @param lo start at  \code{lo * y_max}. DEFAULT is 2\%
#' @param hi finish at \code{hi * y_max}. DEFAULT is 80\%
#' @return a \code{data.table} trimmed to the limits calculated above
#' @import data.table
#' @export trim_slack

trim_slack <- function(DT, formula, lo = 0.02, hi = 0.08) {
  # horrible hack to avoid R CMD Check complaining about no visible binding
  Time <- NULL

  x <- all.vars(formula)[2]  # get the ordinate
  y <- all.vars(formula)[1]  # get the abscissa
  limits <- DT[, {
                   max = max(get(y)); min = min(get(y)); span = (max-min);
                   list(min = min, max = max, span = span,
                        lo = min + lo * span,
                        hi = hi * max)
                 }
               ]

  fit <- DT[, lm_simple(formula, .SD[get(y) %between% limits[, c(lo, hi)]])]

  x_int <- -(fit$int/fit$slope)
  x_max <- DT[, get(x)[which.max(get(y))]]

  trimmed <- DT[get(x) %between% c(x_int, x_max), ]
  trimmed[, x := get(x) - get(x)[1]]
  if ("Time" %in% names(DT)) {
    trimmed[, Time:= Time-Time[1]]
  }
}
