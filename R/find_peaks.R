#' Find Peaks
#'
#' Given a series find local peaks by picking values bigger than those on either side
#' functions posted to the r-help mailing list,
#' 25 November 2005 both by Martin Maechler maechler@stat.math.ethz.ch
#' (the first function after code by Petr Pikal, in turn after Biran Ripley)
#' https://stat.ethz.ch/pipermail/r-help/2005-November/083376.html
#' Assumed to be in the public domain.
#' @param series the vector of numbers to find peaks in.
#' @param span the window size to use when looking for lower values.
#' @param do.pad pad to the same length as the input seris? Default = TRUE.
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
