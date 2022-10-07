NC_trimmer <- function(d, ex, v, xmin, xmax, ymin, ymax, ret = FALSE) {

  nc_data <- nc_open(paste0(d,ex))

  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")

  ndvi.array <- ncvar_get(nc_data, v) # store the data in a 3-dimensional array

  fillvalue <- ncatt_get(nc_data, v, "_FillValue")

  nc_close(nc_data)

  ndvi.array[ndvi.array == fillvalue$value] <- NA

  ndvi.slice <- ndvi.array[,,1]

  r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction='y')

  x <- crop(r, c(xmin, xmax, ymin, ymax))

  writeRaster(x, paste0(d,v, ".tif"), "GTiff", overwrite=TRUE)

  if (ret == TRUE){
    return(x)
  }


}
