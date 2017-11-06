#' Get basic grid data of the BSH-setup for HBM
#' 
#' Get basic grid data of the BSH-setup for HBM
#'
#' @return list with grid information (see yourself)
#' @export
#'
#' @examples
#' 
#'   # get grid information
#'   grid_info <- get_grid_data.hbm.basic()
#'   
get_grid_data.hbm.basic <- function() {

  grids <- list('fine'=list('lon' = list('lon0'=6+10/60+25/60/60, 'dlon'=5/6/60,
                                        'nlon'=630, 'len' = 630, 
                                        'long_name' = 'longitude',
                                        'units' = 'degrees_east',
                                        'standard_name' = 'longitude',
                                        'unlim' = FALSE),
                           'lat' = list('lat0'=56+26/60+0.75/60, 'dlat'=3/6/60,
                                        'nlat'=387, 'len'=387,
                                        'long_name' = 'latitude',
                                        'units' = 'degrees_north',
                                        'standard_name' = 'latitude',
                                        'unlim' = FALSE),
                           'z'   = list('nz'=25, 'len'=25,
                                        'long_name' = 'depth',
                                        'units' = '1',
                                        'positive' = 'down',
                                        'standard_name' = 'depth',
                                        'unlim' = FALSE),
                           'sur'   = list('nz'=1, 'len'=1,
                                          'long_name' = 'depth',
                                          'units' = '1',
                                          'positive' = 'down',
                                          'standard_name' = 'depth',
                                          'comment' = 'surface level',
                                          'unlim' = FALSE),
                           'sed'   = list('nz'=1, 'len'=1,
                                          'long_name' = 'depth',
                                          'units' = '1',
                                          'positive' = 'down',
                                          'standard_name' = 'depth',
                                          'comment' = 'bottom level',
                                          'unlim' = FALSE),
                           'time'   = list('ntime'=-1, 'len'=-1,
                                           'long_name' = 'time',
                                           'standard_name' = 'time',
                                           'unlim' = TRUE),
                           '_attributes' = list('name' = 'fine',
                                                'id' = '02')),
               'coarse'=list('lon' = list('lon0'=-4-2/60-0.5/60, 'dlon'=5/60,
                                          'nlon'=414, 'len'=414,
                                          'long_name' = 'longitude',
                                          'units' = 'degrees_east',
                                          'standard_name' = 'longitude',
                                          'unlim' = FALSE),
                             'lat' = list('lat0'=65+52/60+0.5/60, 'dlat'=3/60,
                                          'nlat'=347, 'len'=347,
                                          'long_name' = 'latitude',
                                          'units' = 'degrees_north',
                                          'standard_name' = 'latitude',
                                          'unlim' = FALSE),
                             'z'   = list('nz'=36, 'len'=36,
                                          'long_name' = 'depth',
                                          'units' = '1',
                                          'positive' = 'down',
                                          'standard_name' = 'depth',
                                          'unlim' = FALSE),
                             'sur'   = list('nz'=1, 'len'=1,
                                            'long_name' = 'depth',
                                            'units' = '1',
                                            'positive' = 'down',
                                            'standard_name' = 'depth',
                                            'comment' = 'surface level',
                                            'unlim' = FALSE),
                             'sed'   = list('nz'=1, 'len'=1,
                                            'long_name' = 'depth',
                                            'units' = '1',
                                            'positive' = 'down',
                                            'standard_name' = 'depth',
                                            'comment' = 'bottom level',
                                            'unlim' = FALSE),
                             'time'   = list('ntime'=-1, 'len'=-1,
                                             'long_name' = 'time',
                                             'standard_name' = 'time',
                                             'unlim' = TRUE),
                             '_attributes' = list('name' = 'coarse',
                                                  'id' = '01')))
  resols <- c('fine','coarse')
  
  for(resol in resols) {
    grids[[resol]]$lon$lonw <- grids[[resol]]$lon$lon0 - 0.5 * grids[[resol]]$lon$dlon
    grids[[resol]]$lon$lone <- grids[[resol]]$lon$lon0 + (grids[[resol]]$lon$nlon+0.5) * grids[[resol]]$lon$dlon
    grids[[resol]]$lat$latn <- grids[[resol]]$lat$lat0 + 0.5 * grids[[resol]]$lat$dlat
    grids[[resol]]$lat$lats <- grids[[resol]]$lat$lat0 - (grids[[resol]]$lat$nlat+0.5) * grids[[resol]]$lat$dlat
    grids[[resol]]$lon$values <- grids[[resol]]$lon$lon0 + (0:(grids[[resol]]$lon$nlon-1)) * grids[[resol]]$lon$dlon
    grids[[resol]]$lat$values <- grids[[resol]]$lat$lat0 - (0:(grids[[resol]]$lat$nlat-1)) * grids[[resol]]$lat$dlat
  }
  
  return(grids)
  
}