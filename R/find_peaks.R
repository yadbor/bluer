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
  shifted <- data.table::shift(maybe_pk, type="lag", fill = -1)
  real_pk <- ifelse(maybe_pk == shifted, 0, maybe_pk)
  # Stuff the corrected peaks back in to the right spots in ans.
  ans[which(ans != 0)] <- real_pk

  return(ans)
}

#' Find Peaks and Troughs 2
#'
#' My version, starting from Brian Ripley's and informed by all the others
#' @param series the vector of numbers to find peaks in.
#' @param span the window size to use when looking for lower values.
#' @param do.pad should the result be padded to the input series length?
#'   Default is TRUE.
#' @return return (-1 / 0 / 1) if series[i] is ( trough / "normal" / peak )
#'
peaksign2 <- function(series, span=3, do.pad = TRUE) {
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
  if (do.pad) {
    pad <- rep.int(0L, s)
    c(pad, ans, pad)
  } else {
    ans
  }
}

#' Find Peaks 1
#'
#' Given a series find local peaks by picking values bigger than those on either side
#' functions posted to the r-help mailing list,
#' 23 November 2005 both by Marc Kirchner marc.kirchner@iwr.uni-heidelberg.de
#' This is the 2001 version by Petr Pikal, after Brian Ripley
#' === Petr Pikal in 2001 (based on Brian Ripley's idea)==
#' https://stat.ethz.ch/pipermail/r-help/2005-November/083240.html
#' Assumed to be in the public domain.
#' @param series the vector of numbers to find peaks in.
#' @param span the window size to use when looking for lower values.
#' @return a logical vector, TRUE for each peak
#'
peaks1 <- function(series, span=3) {
  z <- stats::embed(series, span)
  result <- max.col(z) == 1 + span %/% 2
  result
}

#' Find Peaks 2
#'
#' Given a series find local peaks by picking values bigger than those on either side
#' functions posted to the r-help mailing list,
#' 23 November 2005 both by Marc Kirchner marc.kirchner@iwr.uni-heidelberg.de
#' This is the 2004 version by Petr Pikal, after Brian Ripley
#' === Petr Pikal in 2004 ===
#' https://stat.ethz.ch/pipermail/r-help/2005-November/083240.html
#' Assumed to be in the public domain.
#' @param series the vector of numbers to find peaks in.
#' @param span the window size to use when looking for lower values.
#' @return a logical vector, TRUE for each peak
#'
peaks2<-function(series,span=3) {
  z <- stats::embed(series, span)
  s <- span%/%2
  v<- max.col(z) == 1 + s
  result <- c(rep(FALSE,s),v)
  result <- result[1:(length(result)-s)]
  result
}

#' Find Peaks
#'
#' Given a series find local peaks by picking values bigger than those on either side
#' functions posted to the r-help mailing list,
#' 25 November 2005 both by Martin Maechler maechler@stat.math.ethz.ch
#' (the first function after code by Petr Pikal, in turn after Brian Ripley)
#' https://stat.ethz.ch/pipermail/r-help/2005-November/083376.html
#' Assumed to be in the public domain.
#' @param series the vector of numbers to find peaks in.
#' @param span the window size to use when looking for lower values.
#' @param do.pad pad to the same length as the input series? Default = TRUE.
#' @return a logical vector, TRUE for each peak
#'
peaks <- function(series, span = 3, do.pad = TRUE) {
  if ((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
  s1 <- 1:1 + (s <- span %/% 2)
  if (span == 1) return(rep.int(TRUE, length(series)))
  z <- stats::embed(series, span)
  v <- apply(z[, s1] > z[, -s1, drop = FALSE], 1, all)
  if (do.pad) {
    pad <- rep.int(FALSE, s)
    c(pad, v, pad)
  } else v
}

peaksT <- function(series, span = 3, do.pad = TRUE) {
  if ((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
  s1 <- 1:1 + (s <- span %/% 2)
  if (span == 1) return(rep.int(TRUE, length(series)))
  z <- stats::embed(series, span)
  # misses "flat" peaks with repeated equal values of series
  # change > to >= gets these, but returns multiple TRUE for each point in peak
  v <- apply(z[, s1] >= z[, -s1, drop = FALSE], 1, all)
  if (do.pad) {
    pad <- rep.int(FALSE, s)
    c(pad, v, pad)
  } else v
}

peaksT2 <- function(series, span = 3, do.pad = TRUE) {
  if ((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
  s1 <- 1:1 + (s <- span %/% 2)
  if (span == 1) return(rep.int(TRUE, length(series)))
  eps <- abs(max(series)-min(series))/1000
  z <- stats::embed(series, span)
  # misses "flat" peaks with repeated equal values of series
  # change > to >= gets these, but returns multiple TRUE for each point in peak
  v <- apply(z[, s1] >= (z[, -s1, drop = FALSE]-eps), 1, all)
  if (do.pad) {
    pad <- rep.int(FALSE, s)
    c(pad, v, pad)
  } else v
}

#' Find Peaks and Troughs
#'
#' Given a series find local peaks (and troughs) by picking values bigger (smaller)
#' than those on either side. Returns -1 for a trough, +1 for a peak and 0 otherwise
#' functions posted to the r-help mailing list,
#' 25 November 2005 both by Martin Maechler maechler@stat.math.ethz.ch
#' (the first function after code by Petr Pikal, in turn after Biran Ripley)
#' https://stat.ethz.ch/pipermail/r-help/2005-November/083376.html
#' Assumed to be in the public domain.
#' @param series the vector of numbers to find peaks in.
#' @param span the window size to use when looking for lower values.
#' @param do.pad pad to the same length as the input seris? Default = TRUE.
#' @return return (-1 / 0 / 1) if series[i] is ( trough / "normal" / peak )
#' @export peaksign
#'
peaksign <- function(series, span = 3, do.pad = TRUE) {
  ## Purpose: return (-1 / 0 / 1) if series[i] is ( trough / "normal" / peak )
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 25 Nov 2005

  if ((span <- as.integer(span)) %% 2 != 1 || span == 1)
    stop("'span' must be odd and >= 3")
  s1 <- 1:1 + (s <- span %/% 2)
  z <- stats::embed(series, span)
  d <- z[, s1] - z[, -s1, drop = FALSE]
  ans <- rep.int(0:0, nrow(d))
  ans[apply(d > 0, 1, all)] <- as.integer(1)
  ans[apply(d < 0, 1, all)] <- as.integer(-1)
  if (do.pad) {
    pad <- rep.int(0:0, s)
    c(pad, ans, pad)
  } else ans
}
