#' Get grid cell (x and y) for given lon and lat coordinates of a defined regular lat-lon-grid
#' 
#' We insert lat and lon coordinates (note the order: not lon and lat) and 
#' provide grid information as parameter 'grid'. 'grid' is a nested list, which
#' contains sub-lists denoted as 'lon' and 'lat'. The sub-list 'lon' has to 
#' contain numeric values denoted as 'lonw' (westerly boundary of the grid), 
#' 'lone' (easterly boundary of the grid), and 'dlon' (delta-longitude; 
#' distance between two grid cells). The sub-list 'lat' has to contain numeric
#' values denoted as 'latn' (northern boundary of the grid), 'lats' (southern
#' boundary of the grid), 'dlat' (delta-latitude), and 'nlat' (the number of 
#' grid cells in latitudal direction). THE ORIGIN OF THE GRID IS CONSIDERED TO 
#' BE IN THE TOP LEFT CORNER.
#'
#' @param lat numeric (single value or 1d array); latitude coordinate
#' @param lon numeric (single value or 1d array); longitude coordinate
#' @param grid grid definition (see description or example for structure)
#'
#' @return list('x'=..., 'y'=...); list of x- and y-coordinates of the grid cells
#' @export
#'
#' @examples
#' 
#'   # grid definition
#'   grid = list(lon = list(lonw=-5.0, lone=10.0, dlon=0.5),
#'               lat = list(lats=49.0, latn=65.0, dlat=0.5, nlat=(65.0-49.0)/0.5))
#'   
#'   # some coordinates
#'   lon = c(-4.9, -4.1, 5.6)
#'   lat = c(64.9, 64.9, 53.2)
#'   
#'   # get grid cells 
#'   grid_cells = latlon2cell(lat, lon, grid)
#'   # result should be:
#'   #  - grid_cells$x: 1, 2, 22
#'   #  - grid_cells$y: 1, 1, 24
#'   
latlon2cell = function(lat, lon, grid) {
  x = -1
  y = -1
  
  if ( lon > grid$lon$lonw && lon < grid$lon$lone &&
       lat > grid$lat$lats && lat < grid$lat$latn) {
    x = floor((lon - grid$lon$lonw) / grid$lon$dlon) + 1
    y = grid$lat$nlat - floor((lat - grid$lat$lats) / grid$lat$dlat)
  }
  
  return(list('x' = x, 'y' = y))
}