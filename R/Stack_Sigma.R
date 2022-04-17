#' Calculates Sigma for a raster stack
#' @import  raster
#' @param x The raster stack
#' @return The sigma
#' @export

Stack_sigma <- function(x){

  # Estimates Sigma
  sig <- sqrt(
    (1/(raster::nlayers(x)-1) * sum((x - mean(x, na.rm = T))^2,na.rm = T)
    )
  )
  sig
}
