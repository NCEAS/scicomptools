
#' Interpolate a netcdf file to a regular World Geodetic System (WGS84) grid
#'
#' @param netcdf_file A character
#' @param variable_to_extract A character
#' @param grid_resolution A numeric
#' @param filling_value_flag A logical
#' @param lat An atomic vector
#' @param long  An atomic vector
#'
#' @return (rasterLayer) regridded raster
#' @export
#'
#' @examples \dontrun{regrid_to_regular("cesm_co2sys_2005.nc", "OARG")}
#'
regrid_to_regular <- function(netcdf_file, variable_to_extract, grid_resolution = 0.1 ,filling_value_flag = TRUE, lat = "TLONG", long = "TLONG"){
    data <- nc_open(netcdf_file)
    print(data)
    long <- ncvar_get(data,varid=long)
    lat <- ncvar_get(data,varid=lat)
    arag <- ncvar_get(data,varid=variable_to_extract)

    # Create the dataframe
    df <- as.data.frame(cbind(as.vector(long),as.vector(lat),as.vector(arag[,,1])))

    # Put back the longitude data from -180 to 180
    df$x <- ifelse(df$x>180, df$x -360, df$x)

    # remove filling values
    if (filling_value_flag) {
      df$z <- ifelse(df$z==max(df$z),NA,df$z) # Currently assuming the filling value is a large number!!!!
    }

    # project it to a regular grid
    regridded <- interp(x=df$x, y=df$y, z=df$value, xo=seq(-180,180,grid_resolution), yo=seq(-80,90,grid_resolution),linear = TRUE, extrap=TRUE, duplicate = "mean")

    r <- raster(regridded)
    crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    return(r)
}
