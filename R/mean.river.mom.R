#' Title
#'
#' @param inData 
#' @param from 
#' @param to 
#'
#' @return
#' @export
#'
#' @examples
mean.river.mom = function(inData, from='none', to='none') {
  if (to == 'year') to = 'annual'
  if (from == 'year') from = 'annual'
  if (to == 'month') to = 'monthly'
  if (from == 'month') from = 'monthly'
  
  if (from == to) {
    return(inData)
  } else if ((from == 'none' && to != 'none') ||
             (from != 'none' && to == 'none')) {
      stop('mean.river.mom: "from" and "to" both need to be set.')
  } else {
    
    if (from == 'monthly' && to == 'annual') {
      nDaysM = c(31,28,31,30,31,30,31,31,30,31,30,31)
      
      tmp_years = substr(inData$data$time, 1,4)
      strYears = tmp_years[!duplicated(tmp_years)]
      intYears = strtoi(strYears)
      
      minYear = min(intYears, na.rm = TRUE)
      maxYear = max(intYears, na.rm = TRUE)
      nYears = maxYear-minYear+1
      nTime = dim(inData$data)[1]
      nVars = dim(inData$data)[2]
      
      tmp_valsY = as.data.frame(array(0.0, dim = c(nYears, nVars)))
      names(tmp_valsY) = names(inData$data[,-1])
      
      for (iY in 1:length(strYears)) {
        strY = strYears[iY]
        intY = intYears[iY]
        iDsIn = array(FALSE, dim = c(nTime,12))
        for (iM in 1:12) {
          iDsIn[,iM] = (substr(inData$data$time, 1,7)==paste(strY, formatC(iM, format='d', width=2, flag='0'), sep = '-'))
        }
        iDsOt = intY - minYear + 1
        
        nDaysM[2] = 28
        if (is.leapyear(intY)) nDaysM[2] = 29
        nDaysY = sum(nDaysM)
        
        # iterate all columns and calculate averages
        # index 1 is time => we start with 2
        for (iV in 2:nVars) {
          tmp_valsM = rep(0.0, 12)
          for (iM in 1:12) tmp_valsM[iM] = mean(inData$data[iDsIn[,iM],iV], na.rm = TRUE)
          tmp_valsY[iDsOt,iV] = sum(tmp_valsM*nDaysM)/nDaysY
        }
      }
      
      # create output list
      otData = list()
      # copy units
      otData$units = inData$units
      # insert data
      otData$data <- cbind(formatC(minYear:maxYear, format = 'd', width = 4), tmp_valsY)
      names(otData$data)[1] <- 'time'
    } else {
      stop("mean.river.mom: only 'from==monthly && to==annual' is implemented")
    }
    
    # return output data
    return(otData)
  } 
}