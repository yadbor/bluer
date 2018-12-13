# searches the data from the first point to the maximum load point.
# • determines the start value by finding the first data point equal to or greater than 2% of the
# maximum load. .
# • defines the end value as the maximum load point.
# • divides the data between the start and end values into 6 equal regions with no overlap.
# • applies a least squares fit algorithm on all the data points in the regions.
# • calculates the sum of the slopes of each pair of adjacent data regions.
# • determines which pair of regions that has the highest slope sum.
# • from this pair, determines which region has the highest slope and assigns the modulus to
# that region.
# • determines the slack correction at the point where the modulus line intersects the zero stress
# strain axis.

#' Slack Correction
#'
#' Remove any slack at the start of a test (or cycle)
#'
#' @param DT a data.table containing channels to be corrected
#' @param channel analyse this channel to find starting slack
#'
#' @return The original series with any initial slack portion trimmed off.
#' @export bh_slack_correct
#'

bh_slack_correct <- function(DT, channel) {

}
