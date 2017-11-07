#' Reads a river runoff/nutrient-load input file for the ocean model MOM into R
#' 
#' Reads a MOM input file, which contains river runoff and riverine nutrient 
#' loads, and writes the data into a data.frame nested into a list. The list
#' also contains units corresponding to the columns of the data.frame.
#'
#' @param filename character: path/filename of the file to read
#'
#' @return list containing a data.frame (out$data), a character array 
#'           (out$units), and two character variables (out$format and
#'           out$tstep). The first contains the actual data formatted as a 
#'           data.frame. The second contains the units to the corresponding 
#'           columns of the data.frame. The third contains the source/format 
#'           of data (here: 'mom'; can also be 'swat'). The fourth contains
#'           information on the time step of the data (resp.: on which time
#'           interval they are averaged). The latter is set to 'monthly'.
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @seealso read.swat, read.swat.data.rch.2012
#' @export
#'
#' @examples
#' 
#'   # read a file:
#'   test.mom <- read.mom('files/GER_Dan_Str_Warnow.dat')
#'   
read.river.mom = function(filename) {
  
  # extract raw data
  rawData = read.table(filename, header = FALSE, stringsAsFactors = FALSE, skip = 4)
  
  # process time
  varTime = strptime(paste(rawData[,1], '-', rawData[,2], '-', rawData[,3], ' ', 
                           rawData[,4], ':', rawData[,5], ':', rawData[,6], ' GMT', sep = ''), format = '%Y-%m-%d %H:%M:%S', tz='GMT')
  
  # get header information
  rawHeader = strsplit(trimws(substr(scan(filename, skip = 3, nlines = 1, what = character(), sep = ';')[[1]], 6,9999)), split = ']', fixed=TRUE)[[1]]
  nVar = length(rawHeader)
  
  # process header information: split it into 'variable name' and 'unit'
  varName = rep('', nVar)
  varUnit = rep('', nVar)
  for (iV in 1:nVar) {
    tmpSplit = strsplit(rawHeader[iV], split = '[', fixed = TRUE)[[1]]
    varName[iV] = trimws(tmpSplit[1])
    varUnit[iV] = trimws(tmpSplit[2])
  }
  
  ## write output data
  # create list for output
  otData = list()
  # add a list entry for the data (a data.frame)
  otData$data = cbind(varTime, rawData[,7:(6+nVar)])
  # set column/variable names in the data.frame
  names(otData$data) = c('time', varName)
  # add another list entry containing the units
  otData$units = c('time', varUnit)
  # source format of data
  otData$format = 'mom'
  # set time step
  otData$tstep = 'monthly'
  
  # return data
  return(otData)
}

