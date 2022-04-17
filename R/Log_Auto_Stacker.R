#' Logs and then stacks rasters within a folder
#' @param path The Path to the folder containing the .h5 files
#' @param file_type Default is 1. 1. DNB_BRDF-Corrected_NTL 2. DNB_Lunar_Irradiance 3. Gap_Filled_DNB_BRDF-Corrected_NTL 4. Latest_High_Quality_Retrieval 5. Mandatory_Quality_Flag 6. QF_Cloud_Mask 7. Snow_Flag
#' @param projection_CRS The CRS projection. The default is "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' @return A stack of rasters
#' @export

Log_Auto_stacker <- function(path, file_type = 1, projection_CRS = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ){

  # Read the .h5s within a folder
  rasters <- list.files(path,
                        pattern='.h5',
                        all.files=TRUE,
                        full.names=FALSE)

  # Covert the first .h5 within the folder and logs it
  base_raster <- log(1 + H5_data_extracter(path,
                                           file_type,
                                           rasters[1]))

  # Place the first raster into a stack
  rasterstack <- stack(base_raster)

  # Extract the remaining rasters, logs, and stack them
  for (i in 2:length(rasters)) {
    r <- log(1 + H5_data_extracter(path,
                                   file_type,
                                   rasters[i]))
    rasterstack <- stack(rasterstack, r)
  }

  # Assigns a projection to the stack
  crs(rasterstack) <- projection_CRS

  # Removes the temp file created by this process
  try(
    file.remove("temp.tif")
  )
  rasterstack
}
