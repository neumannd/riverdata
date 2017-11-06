#' Creates a new riverinflow* file for the HBM model
#'
#' A new text file is created. The first row contains " DAILY RIVER RUNOFF" and
#' the second row "[date in format 'YYYY.MM.DD hh:mm:ss'] [annual inflow 1] [annual inflow 2]"
#' The 'annual inflow n' has to be printed in the Fortran format 'F8.6' 
#' (width=8; 6 digits in front of the '.'). One file is created per day.
#'
#' @param riverInfo list with grid information
#' @param riverData list with river inflow data
#' @param dOt character; directory into which the final file should be written 
#' @param year integer; year to write out
#' @param month integer; month to write out; if month==0 => write all months
#' @param day integer; day to write out; if day==0 => write all days
#' @param overwrite logical; overwrite 'dOt/filename' it already exists
#' @param warn logical; allow/suppress warnings
#'
#' @return file written
#' @export
#'
#' @examples
#' 
#'   # read a file:
#'   test.mom.monthly <- read.mom('files/GER_Dan_Str_Warnow.dat')
#'   
#'   # interpolate from monthly to daily
#'   test.mom.daily <- interpolate.river.mom(test.mom.monthly, to = 'daily', method = 'step')
#'   
#'   # get grid info
#'   grid_info <- get.infos.grids.hbm.basic()
#'   
#'   # get river infos
#'   file <- 'files/river_list.dat'
#'   riverInfos <- read.infos.rivers(file, grid_info)
#'   
#'   # write new namelist
#'   write.river.newNML(riverInfos$Warnow, test.mom.daily, 'example_dir', 2012, month = 1, day = 1:5, overwrite=FALSE)
#'   
#' 
#'  ## example file content
#'  # DAILY RIVER RUNOFF
#'  # 2012.07.27 12:00:00     2.1
#'  
write.river.newInflow = function(riverInfo, riverData, dOt, year, month=0, day=0, overwrite=TRUE, warn=TRUE) {
  
  # initialize variables ----
  # unit: (m3/s)
  iG=riverInfo$grid
  jG=ifelse(iG=='fine','02',ifelse(iG=='coarse','01','00'))
  iR=riverInfo$rivername
  
  strYear='2012'
  
  nDaysM = c(31,28+ifelse(is.leapyear(year),1,0),31,30,31,30,31,31,30,31,30,31)
  strDays = list()
  
  strData = rep('', 2)
  strData[1] = ' DAILY RIVER RUNOFF'
  
  
  # prepare dates ----
  # we need to do sum(month==0)!=0 instead of month==0 because month might
  # be more than one value
  if (sum(month == 0)!=0) {
    strMonths = formatC(1:12, format = 'd', width = 2, flag = '0')
  } else {
    strMonths = c(formatC(month, format = 'd', width = 2, flag = '0'))
  }
  
  if (sum(day == 0)!=0) {
    for (iM in strMonths) {
      strDays[[iM]] = formatC(1:nDaysM[strtoi(iM, base=10L)], format = 'd', width = 2, flag = '0')
    }
  } else {
    for (iM in strMonths) {
      strDays[[iM]] = c(formatC(day, format = 'd', width = 2, flag = '0'))
    }
  }
  
  
  # start ----
  for (iM in strMonths) {
    print(paste0('Year: ', strYear, '; Month: ', iM))
    for (iD in strDays[[iM]]) {
      # prepare data 1 ----
      # index in riverData$vals to write out
      iT = which(substr(riverData$data$time, 1, 10) == paste(strYear, iM, iD, sep = '-'))
      valOt = riverData$data$runoff[iT]*10^-3
      
      strAppnd = formatC(valOt, format='f', width = 8, digits = 1)
      
      for (iH in c('00', '12')) {
        
        # prepare data 2 ----
        strData[2] = paste0(paste(paste(strYear, iM, iD, sep = '.'), paste(iH, '00', '00', sep = ':'), sep = ' '), strAppnd)
        
        # write data ----
        fOt=paste0('riverinflow_', jG, '_', iR, '-', strYear, iM, iD, iH)
        pOt=paste(dOt, fOt, sep = '/')
        # print(pOt)
        
        if(file.exists(pOt) && (!overwrite)) {
          warning(paste0('File already exists and overwrite set to false. No file written! Filename: ', pOt))
        } else {
          if (file.exists(pOt) && warn) {
            warning(paste0('File already exists and overwrite set to true. Old file will be overwritten. Filename: ', pOt))
          }
          
          file.create(pOt, overwrite = TRUE, showWarnings = TRUE)
          fId = file(pOt)
          write(strData, file = fId, append = FALSE, sep = '\n')
          close(fId)
        }
      }
    }
  }
}