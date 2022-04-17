#' Estimates the Welch t-statistic for two raster stacks
#' @import  raster
#' @param stack1 The base raster stack
#' @param stack2 The raster stack to be compared with
#' @return The Welch t-statistic for two raster stacks
#' @export

Welch_t <- function(stack1, stack2){

  # Calculate the sigma for the first and second rasters
  sig1 <- Stack_sigma(stack1)
  sig2 <- Stack_sigma(stack2)

  # Estimates t
  t <- (mean(stack1, na.rm = T)-mean(stack2, na.rm = T))/
    sqrt((sig1^2/raster::nlayers(stack1)) + (sig2^2/raster::nlayers(stack2)))

  t
}
