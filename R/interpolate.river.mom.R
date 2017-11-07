#' Interpolates river inflow data
#'
#' Interpolates river inflow data in the time dimensions. Currently, only
#' interpolation from monthly to values is implemented. Additionally, only 
#' the interpolation methods 'step' is implemented.
#'
#' @param inData list containing a data.frame (in$data) with river inflow data, a character array (in$units) with corresponding units, and a character (in$format).
#' @param from character: current data averaging interval; does not need to be set if in$tstep is set properly; if not, it can be manually set to e.g. 'annual', 'monthly', or 'daily'
#' @param to character, target time interval; e.g. 'annual', 'monthly', or 'daily'
#' @param method character, interpolation method
#'
#' @return list of newly averaged data; same structure as input data
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
#' @seealso read.river.mom, write.river.netCDF, write.river.append2Inflow, write.river.newInflow, mean.river.mom
#' @export
#'
#' @examples
#' 
#'   # read a file:
#'   test.mom.monthly <- read.river.mom('files/GER_Dan_Str_Warnow.dat')
#'   
#'   # interpolate from monthly to daily
#'   test.mom.daily <- interpolate.river.mom(test.mom.monthly, to = 'daily', method = 'step')
#'   
interpolate.river.mom <- function(inData, from=inData$tstep, to='none', method = 'step') {
  if ( tolower(inData$format) != 'mom' ) {
    stop("interpolate.river.mom.R stop: in$format needs to be of value 'mom' or 'MOM'")
  }
  
  if (to == 'year') to <- 'annual'
  if (from == 'year') from <- 'annual'
  if (to == 'month') to <- 'monthly'
  if (from == 'month') from <- 'monthly'
  if (to == 'day') to <- 'daily'
  if (from == 'day') from <- 'daily'
  
  if (from == to) {
    return(inData)
  } else if ((from == 'none' && to != 'none') ||
             (from != 'none' && to == 'none')) {
    stop('riverInterpolate: "from" and "to" both need to be set.')
  } else {
    
    ## testing:
    # inData <- list(data = as.data.frame(cbind(as.POSIXct(c('1995-10-16 00:00 GMT', '1995-11-16 00:00 GMT',
    #                                                       '1995-12-16 00:00 GMT', '1996-01-16 00:00 GMT',
    #                                                       '1996-02-16 00:00 GMT', '1996-03-16 00:00 GMT',
    #                                                       '1996-04-16 00:00 GMT', '1996-05-16 00:00 GMT',
    #                                                       '1996-06-16 00:00 GMT', '1996-07-16 00:00 GMT')),
    #                                          as.data.frame(array(c(2,3,4,4,2,1,1,5,6,5), dim = c(10,1))))),
    #               units = c('time', 'kg'),
    #               format = 'mom')
    # names(inData$data) <- c('time', 'var')
    
    
    if (from == 'monthly' && to == 'daily') {
      nDaysM <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      
      tmp_years <- substr(inData$data$time, 1,4)
      strYears <- tmp_years[!duplicated(tmp_years)]
      intYears <- strtoi(strYears, base = 10L)
      
      minYear <- min(intYears, na.rm = TRUE)
      maxYear <- max(intYears, na.rm = TRUE)
      
      nTime <- dim(inData$data)[1]
      nVars <- dim(inData$data)[2] - 1
      
      if (method == 'step') {
        midPoint <- array(0.0, dim = c(nTime,nVars))
        crossPoint <- array(0.0, dim = c(nTime+1,nVars))
        # we need to distinguish two cases here:
        #  (a) if inData$data[,-1] has only one column, rbind does not work 
        #       properly because inData$data[c(-1,-nTime),-1] is considered 
        #       as one row.
        #  (b) if inData$data[,-1] has more than one column, we use rbind
        if ( dim(inData$data[,-1])[2] == 1 ) {
          midPoint <- rbind(inData$data[1,-1]*1.1             - inData$data[2,-1]*0.1,
                           array(inData$data[c(-1,-nTime),-1]*1.25 - inData$data[c(-1,-2),-1]*0.125 - inData$data[c(-nTime+1,-nTime),-1]*0.125, dim = c(nTime-2, nVars)),
                           inData$data[nTime,-1]*1.1         - inData$data[nTime-1,-1]*0.1)
          crossPoint <- rbind(midPoint[1,],
                             array((inData$data[-nTime,-1] + inData$data[-1,-1])*0.5, dim = c(nTime-1, nVars)),
                             midPoint[nTime,])
        } else {
          midPoint <- rbind(inData$data[1,-1]*1.1             - inData$data[2,-1]*0.1,
                            inData$data[c(-1,-nTime),-1]*1.25 - inData$data[c(-1,-2),-1]*0.125 - inData$data[c(-nTime+1,-nTime),-1]*0.125,
                            inData$data[nTime,-1]*1.1         - inData$data[nTime-1,-1]*0.1)
          crossPoint <- rbind(midPoint[1,],
                              (inData$data[-nTime,-1] + inData$data[-1,-1])*0.5,
                              midPoint[nTime,])
        }
        startDate <- as.character(inData$data$time[1])
        substr(startDate,9,10) <- '01'
        endDate <- as.character(inData$data$time[nTime])
        endM <- strtoi(substr(endDate,6,7), base = 10L)
        endY <- strtoi(substr(endDate,1,4), base = 10L)
        substr(endDate,9,10) <- formatC(nDaysM[endM]+ifelse(is.leapyear(endY) && endM == 2, 1, 0), format = 'd', width = 2, flag='0')
        
        nDays <- ceiling((as.numeric(as.POSIXct(paste0(endDate, ' 00:00 GMT'))) - as.numeric(as.POSIXct(paste0(startDate, ' 00:00 GMT'))))/24/60/60 + 1)
        
        tmp_valsD <- array(0.0, dim = c(nDays, nVars))
        tmp_timeD <- rep(as.POSIXct('1900-01-01 00:00 GMT'), nDays)
        # day_of_year
        
        iD <- 1
        # indices <- array(-1,dim = c(nTime,2))
        # test_mean <- array(0.0, dim = c(nTime,nVars))
        for (iT in 1:nTime) {
          cDate <- as.character(inData$data$time[iT])
          # print(cDate)
          iM <- strtoi(substr(cDate,6,7), base = 10L)
          iY <- strtoi(substr(cDate,1,4), base = 10L)
          
          nD <- nDaysM[iM]+ifelse(is.leapyear(iY) && iM == 2, 1, 0)
          # print(paste(iD, nD, iM, iY, sep = ' - '))
          
          tmp_tmp_valsD <- array(0.0, dim = c(nD, nVars))
          for (iV in 1:nVars) {
            tmp_tmp_valsD[,iV] <- c(crossPoint[iT,iV] + (midPoint[iT,iV] - crossPoint[iT,iV]) * ((1:10)-0.5)/10,
                                    rep(midPoint[iT,iV],nD-20),
                                    midPoint[iT,iV] + (crossPoint[iT+1,iV] - midPoint[iT,iV]) * ((1:10)-0.5)/10)
            
            ## testing:
            # test_mean[iT,iV] <- mean(tmp_tmp_valsD[,iV])
            
            # normalize so that the interpolated data actually fits to the monthly means
            tmp_tmp_valsD[,iV] <- tmp_tmp_valsD[,iV] * inData$data[iT,iV+1] / mean(tmp_tmp_valsD[,iV])
          }
          
          tmp_timeD[iD:(iD+nD-1)] <- as.POSIXct(paste0(substr(cDate,1,7), '-', formatC(1:nD, format='d', width=2, flag='0'), ' 00:00 GMT'), format = "%Y-%m-%d %H:%M")
          tmp_valsD[iD:(iD+nD-1),] <- tmp_tmp_valsD
          
          # indices[iT,1] <- iD
          iD <- iD + nD
          # indices[iT,2] <- iD
        } 
      } else {
        stop("interpolate.river.mom.R stop: only method=='step' is implemented")
      }
      
      ## testing:
      # iEnd <- iD
      # iV <- 2
      # plot(1:(iEnd-1), tmp_valsD[,iV])
      # lines(t(cbind(indices,rep(NA,nTime))),rep(inData$vals[,iV], each = 3), col = 'blue', lwd = 2)
      # points(c(indices[,1],iEnd-1),crossPoint[,iV], pch = 4, col = 'red')
      # points(rowMeans(indices),midPoint[,iV], pch = 4, col = 'green')
      
      # create output list
      otData <- list()
      # copy units
      otData$units <- inData$units
      # copy format
      otData$format <- inData$format
      # set time step
      otData$tstep <- to
      # insert data
      otData$data <- cbind(tmp_timeD, as.data.frame(tmp_valsD))
      names(otData$data) <- names(inData$data)
      
    } else {
      stop("interpolate.river.mom.R stop: only 'from==monthly && to==daily' is implemented")
    }
    
    # return data
    return(otData)
  } 
}