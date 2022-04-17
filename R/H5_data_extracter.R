#' Coverts NASA h5 to rasters
#' @import gdalUtils
#' @param path The Path to the folder containing the .h5 files
#' @param file_type Default is 1. 1. DNB_BRDF-Corrected_NTL 2. DNB_Lunar_Irradiance 3. Gap_Filled_DNB_BRDF-Corrected_NTL 4. Latest_High_Quality_Retrieval 5. Mandatory_Quality_Flag 6. QF_Cloud_Mask 7. Snow_Flag
#' @param file_name The name of the file to be extracted
#' @return A stack of rasters
#' @export

H5_data_extracter <- function(path, file_type = 1, file_name){

  # Reading the rasters within a folder
  rasterlist <- list.files(path=path,
                           pattern='.h5',
                           all.files=TRUE,
                           full.names=FALSE)
  rasterlist <- as.data.frame(rasterlist)

  # Reading the sub directories within the .h5
  sds <- gdalUtils::get_subdatasets(paste0(path, "/",file_name))

  # Converting the .h5 to a .tif
  name <- sds[file_type]
  filename <- 'temp.tif'
  gdalUtils::gdal_translate(sds[file_type],
                            dst_dataset = filename)
  r <- raster(filename)
  r[r == 65535] <- NA

  # Removing the .tif we temporarily generated
  try(
    file.remove("temp.tif")
  )

  r
}
