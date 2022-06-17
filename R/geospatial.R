#' @title Interpolate netCDF to WGS84
#'
#' @description Accepts a netCDF file and re-projects it into a new rasterLayer that has been re-gridded into World Geodetic System 84 (WGS84). Note that this relies on the `raster` package which is becoming superseded by `stars`, `terra`, and `sf`
#'
#' @param netcdf_file (character) path to netCDF file
#' @param variable_to_extract (character)
#' @param grid_resolution (numeric)
#' @param filling_value_flag (logical)
#' @param lat (atomic vector)
#' @param long (atomic vector)
#'
#' @return (rasterLayer) regridded raster
#' @export
#'
#' @examples \dontrun{
#' regrid_to_regular("cesm_co2sys_2005.nc", "OARG")
#' }
#'
regrid_to_regular <- function(netcdf_file, variable_to_extract,
                              grid_resolution = 0.1,
                              filling_value_flag = TRUE,
                              lat = "TLONG", long = "TLONG"){
  # Open netCDF file
    data <- ncdf4::nc_open(netcdf_file)

    # Print it as a diagnostic for the user
    base::print(data)

    # Read some key pieces out of the netCDF object
    long <- ncdf4::ncvar_get(nc = data, varid = long)
    lat <- ncdf4::ncvar_get(data, varid = lat)
    arag <- ncdf4::ncvar_get(data, varid = variable_to_extract)

    # Create the dataframe
    df <- base::as.data.frame(
      base::cbind(base::as.vector(long),
                  base::as.vector(lat),
                  base::as.vector(arag[,,1])))

    # Put back the longitude data from -180 to 180
    df$x <- base::ifelse(test = (df$x > 180),
                         yes = (df$x - 360),
                         no = df$x)

    # remove filling values
    if (filling_value_flag) {
      # Currently assuming the filling value is a large number!!!!
      df$z <- ifelse(test = (df$z == base::max(df$z)),
                     yes = NA, no = df$z)
    }

    # project it to a regular grid
    regridded <- akima::interp(x = df$x, y = df$y, z = df$value,
                               xo = base::seq(from = -180,
                                              to = 180,
                                        grid_resolution),
                               yo = base::seq(from = -80,
                                              to = 90,
                                              grid_resolution),
                               linear = TRUE, extrap = TRUE,
                               duplicate = "mean")

    # Create a rasterLayer object
    r <- raster::raster(regridded)

    # Give it the correct CRS label
    raster::crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

    # Return it
    return(r)
}
