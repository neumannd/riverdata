#' Write netCDF files (one for each river) with riverine nutrient input data
#' 
#' For each entry in 'riverNames' a netCDF file is created. TODO
#'
#' @param riverNames character array with river names
#' @param riverInfo list() of grid-information-lists(); to each element in riverNames one list element with the same name needs to exist in riverInfos
#' @param riverData list() of river-inflow-data-lists(); to each element in riverData one list element with the same name needs to exist in riverInfos
#' @param netCDF2input list() with two character arrays netCDF2input$untagged and netCDF2input$tagged; these contain the names of rivers, which are tagged and untagged, respectively
#' @param dOt character; directory into which the final file should be written
#' @param year integer; year to write out
#' @param overwrite logical; overwrite 'dOt/filename' it already exists
#' @param warn logical; allow/suppress warnings
#' @param cmd_ncgen character: ncgen-call; if ncgen is not in the path (or the wrong verson of ncgen) please provide here '/path/ncgen-exec' for the ncgen binary
#'
#' @return
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @seealso write.river.newNML, write.river.newInflow
#' @export
#'
#' @examples
#' 
#'   # read a file:
#'   test.mom.monthly <- list()
#'   test.mom.monthly$Warnow <- read.river.mom('files/GER_Dan_Str_Warnow.dat')
#'   test.mom.monthly$Trave <- read.river.mom('files/GER_Dan_Str_Trave.dat')
#'   
#'   # calculate annual means from monthly data
#'   test.mom.annual <- list()
#'   test.mom.daily$Warnow <- interpolate.river.mom(test.mom.monthly$Warnow, to = 'daily', method = 'step')
#'   test.mom.daily$Trave <- interpolate.river.mom(test.mom.monthly$Trave, to = 'daily', method = 'step')
#'   
#'   # get netCDF file variable mapping
#'   varmapping.hbm <- get.varmapping.hbm.netcdf()
#'   
#'   # get grid info
#'   grid_info <- get.infos.grids.hbm.basic()
#'   
#'   # get river infos
#'   file <- 'files/river_list.dat'
#'   riverInfos <- read.infos.rivers(file, grid_info)
#'   
#'   # write new namelist
#'   write.river.netCDF(c('Warnow', 'Trave'), riverInfos, test.mom.daily, varmapping.hbm, dOt, 'files', 'out_dir', 2012, month = 1, day = 1:5, overwrite=FALSE)
#' 
write.river.netCDF = function(riverNames, riverInfo, riverData, netCDF2input, dOt, year, overwrite=TRUE, warn=TRUE, cmd_ncgen = 'ncgen') {
  
  strYear='2012'
  nDaysM = c(31,28+ifelse(is.leapyear(year),1,0),31,30,31,30,31,31,30,31,30,31)
  nDaysY = 365+ifelse(is.leapyear(year),1,0)
  
  
  ## CREATE A CDL DEFINITION FOR THE FILE
  ##  - create file via ncgen (?)
  ##  - write data into file (standard vars)
  ##  - write data into river variables: po4_river01
  ##  - update 'river_name' attribute
  ##  - finalize
  
  ## UNITS:
  #'  input vals: mol/kg
  #'  output vals: mmol/m3
  #'  output = input * 20^6
  
  for (iR in riverNames) {
    print(iR)
    
    tIdx = which(substr(riverDataDaily[[iR]]$time,1,4)==strYear)
    nTime = length(tIdx)
    dummyData = rep(0.0, nTime)
    
    intRiverId = riverInfos[[iR]]$riverid
    strRiverId = formatC(intRiverId, format = 'd')
    
    iG=riverInfos[[iR]]$grid
    jG=ifelse(iG=='fine','2',ifelse(iG=='coarse','1','0'))
    
    myNetCDF2input = netCDF2input$untagged
    for (jR in riverNames) {
      tmpList = netCDF2input$tagged
      names(tmpList) = gsub('RIVERNAME', tolower(jR), names(tmpList))
      if (jR != iR) {
        for (iV in names(tmpList)) {
          tmpList[[iV]] = ''
        }
      }
      myNetCDF2input = append(myNetCDF2input, tmpList)
    }
    
    for (iV in names(myNetCDF2input)) if(myNetCDF2input[[iV]] == '') myNetCDF2input[[iV]] = NULL
    
    pOt=paste(dOt, '/river', strRiverId, '_', jG, '.nc', sep = '')
    system2(cmd_ncgen, args = paste0('-k nc4 -o ', pOt, ' scripts/filedef.cdl'))
    
    ncId = nc_open(pOt, write = TRUE)
    
    for (iV in names(ncId$var)) {
      # print(iV)
      if (is.null(myNetCDF2input[[iV]])) {
        ncvar_put(ncId, iV, dummyData, start = rep(1, 4), count = c(1,1,1,nTime))
      } else {
        jV = which(names(riverData[[iR]]$data) == myNetCDF2input[[iV]])
        # print(jV)
        ncvar_put(ncId, iV, riverData[[iR]]$data[tIdx,jV]*10^6, start = rep(1, 4), count = c(1,1,1,nTime))
      }
      
      ncatt_put(ncId, iV, 'river_name', iR)
    }
    
    ncvar_put(ncId, 'longitude', riverInfos[[iR]]$lon)
    ncvar_put(ncId, 'latitude', riverInfos[[iR]]$lat)
    ncvar_put(ncId, 'depth', 0.0)
    ncvar_put(ncId, 'time', formatC(paste0(substr(riverData[[iR]]$data$time[tIdx],1,4), 
                                           substr(riverData[[iR]]$data$time[tIdx],6,7),
                                           substr(riverData[[iR]]$data$time[tIdx],9,10)), format = 'f'),
              start = c(1), count = c(nTime))
    
    nc_close(ncId)
  
  }
}