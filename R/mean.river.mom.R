#' Calculates mean values from MOM river input data
#'
#' Calculates mean values from river inflow data. Currently, only averaging 
#' from monthly to annual means is implemented.
#'
#' @param inData list containing a data.frame (in$data) with river inflow data, a character array (in$units) with corresponding units, and two character variables (in$format and in$tstep).
#' @param from character: current data averaging interval; does not need to be set if in$tstep is set properly; if not, it can be manually set to e.g. 'annual', 'monthly', or 'daily'
#' @param to character: future averaging interval; e.g. 'annual', 'monthly', or 'daily'
#'
#' @return list of newly averaged data; same structure as input data; 
#'
#'           list containing a data.frame (out$data), a character array 
#'           (out$units), and two character variables (out$format and
#'           out$tstep). The first contains the actual data formatted as a 
#'           data.frame. The second contains the units to the corresponding 
#'           columns of the data.frame. The third contains the source/format 
#'           of data (here: 'mom'; can also be 'swat'). The fourth contains
#'           information on the time step of the data (resp.: on which time
#'           interval they are averaged).
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @seealso read.river.mom, write.river.append2NML, write.river.newNML, interpolate.river.mom
#' @export
#'
#' @examples
#' 
#'   # read a file:
#'   test.mom.monthly <- read.river.mom('files/GER_Dan_Str_Warnow.dat')
#'   
#'   # calculate annual means from monthly data
#'   test.mom.annual <- mean.river.mom(test.mom.monthly, to = 'annual')
#'   
mean.river.mom = function(inData, from=inData$tstep, to='none') {
  if ( tolower(inData$format) != 'mom' ) {
    stop('mean.river.mom stop: inData is no MOM data')
  }
  
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
      names(tmp_valsY) = names(inData$data)
      
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
      # copy format
      otData$format = inData$otData
      # set time step
      otData$tstep = to
      # insert data
      otData$data <- cbind(formatC(minYear:maxYear, format = 'd', width = 4), tmp_valsY[,-1])
      names(otData$data)[1] <- 'time'
    } else {
      stop("mean.river.mom: only 'from==monthly && to==annual' is implemented")
    }
    
    # return output data
    return(otData)
  } 
}