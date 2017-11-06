#' Read csv file with information on rivers and return it in a nested list
#' 
#' Columns in the csv file: id, name, lat, lon, grid, ycell, xcell, filename
#' 
#' Elements of the output list: filename, lon, lat, xcell, ycell, grid, riverid, rivername
#' 
#' @param filename path and name of the csv file with river information
#' @param grids nested list with grid definitions: grids$fine and grids$coarse are grid definitions of the fine and coarse, respectively BSH-HBM grids
#' 
#' @return nested list with grid information it contains:
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @seealso get.infos.grids.hbm.basic
#' @export
#' 
#' @examples
#'   
#'   # get grid info:
#'   grid_info <- get.infos.grids.hbm.basic()
#'   
#'   # set filename
#'   file <- 'files/river_list.dat'
#'   
#'   # get river infos
#'   riverInfos <- read.infos.rivers(file, grid_info)
#'   
read.infos.rivers <- function(filename, grids) {
  #' Elements of the output list:
  #'   filename
  #'   lon
  #'   lat
  #'   xcell
  #'   ycell
  #'   grid [fine,coarse]
  #'   riverid
  #'   rivername
  
  # initialize list
  riverInfos <- list()
  
  # read csv file
  rawData <- read.table(filename, header = TRUE, stringsAsFactors = FALSE)
  
  # iterate all rivers = all rows of data
  for (iR in 1:dim(rawData)[1]) {
    riverName <- rawData[iR,2]
    riverInfos[[riverName]] <- list()
    riverInfos[[riverName]]$rivername <- riverName
    riverInfos[[riverName]]$filename <- rawData[iR,8]
    riverInfos[[riverName]]$riverid <- rawData[iR,1]
    riverInfos[[riverName]]$lat <- rawData[iR,3]
    riverInfos[[riverName]]$lon <- rawData[iR,4]
    riverInfos[[riverName]]$grid <- rawData[iR,5]
    riverInfos[[riverName]]$xcell <- rawData[iR,7]
    riverInfos[[riverName]]$ycell <- rawData[iR,6]
    
    tmp_cell <- latlon2cell(riverInfos[[riverName]]$lat, riverInfos[[riverName]]$lon, grids$fine)
    if ( tmp_cell$x == -1 || tmp_cell$y == -1 ) {
      tmp_cell <- latlon2cell(riverInfos[[riverName]]$lat, riverInfos[[riverName]]$lon, grids$coarse)
      if ( tmp_cell$x == -1 || tmp_cell$y == -1 ) {
        tmp_grid <- 'none'
      } else {
        tmp_grid <- 'coarse'
      }
    } else {
      tmp_grid <- 'fine'
    }
    
    # warn if the expected x and y grid cell coordinates do not agree with the calculated ones
    if (riverInfos[[riverName]]$grid != tmp_grid) {
      warning(paste('River', riverName, 'should be located on the', riverInfos[[riverName]]$grid,
                    'but is located on the', tmp_grid, sep = ' '))
    }
    
    if (riverInfos[[riverName]]$xcell != tmp_cell$x) {
      warning(paste('River', riverName, 'should be located in x grid cell', riverInfos[[riverName]]$xcell,
                    'but is located in', tmp_cell$x, sep = ' '))
    }
    
    if (riverInfos[[riverName]]$ycell != tmp_cell$y) {
      warning(paste('River', riverName, 'should be located in y grid cell', riverInfos[[riverName]]$ycell,
                    'but is located in', tmp_cell$y, sep = ' '))
    }
  }
  
  # return information
  return(riverInfos)
}