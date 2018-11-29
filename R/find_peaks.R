#
# peaks functions posted to the r-help mailing list, 
# 25 November 2005 both by Martin Maechler maechler@stat.math.ethz.ch 
# (the first function after code by Petr Pikal, in turn after Biran Ripley)
# https://stat.ethz.ch/pipermail/r-help/2005-November/083376.html
#
# both assumed to be in the public domain
#

peaks <- function(series, span = 3, do.pad = TRUE) {
  if((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
  s1 <- 1:1 + (s <- span %/% 2)
  if(span == 1) return(rep.int(TRUE, length(series)))
  z <- embed(series, span)
  v <- apply(z[,s1] > z[, -s1, drop=FALSE], 1, all)
  if(do.pad) {
    pad <- rep.int(FALSE, s)
    c(pad, v, pad)
  } else v
}

peaksign <- function(series, span = 3, do.pad = TRUE)
{
  ## Purpose: return (-1 / 0 / 1) if series[i] is ( trough / "normal" / peak )
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 25 Nov 2005
  
  if((span <- as.integer(span)) %% 2 != 1 || span == 1)
    stop("'span' must be odd and >= 3")
  s1 <- 1:1 + (s <- span %/% 2)
  z <- embed(series, span)
  d <- z[,s1] - z[, -s1, drop=FALSE]
  ans <- rep.int(0:0, nrow(d))
  ans[apply(d > 0, 1, all)] <- as.integer(1)
  ans[apply(d < 0, 1, all)] <- as.integer(-1)
  if(do.pad) {
    pad <- rep.int(0:0, s)
    c(pad, ans, pad)
  } else ans
}