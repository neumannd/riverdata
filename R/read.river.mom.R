#' Reads a river runoff/nutrient-load input file for the ocean model MOM into R
#' 
#' Reads a MOM input file, which contains river runoff and riverine nutrient 
#' loads, and writes the data into a data.frame nested into a list. The list
#' also contains units corresponding to the columns of the data.frame.
#'
#' @param filename character: path/filename of the file to read
#'
#' @return list containing a data.frame (out$data), a character array 
#'           (out$units), and a character (out$type). The first contains the 
#'           actual data formatted as a data.frame. The second contains the 
#'           units to the corresponding columns of the data.frame. The third
#'           contains the type of data (here: 'mom'; can also be 'swat').
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
  data.out = list()
  # add a list entry for the data (a data.frame)
  data.out$data = cbind(varTime, rawData[,7:(6+nVar)])
  # set column/variable names in the data.frame
  names(data.out$data) = c('time', varName)
  # add another list entry containing the units
  data.out$units = c('time', varUnit)
  # source type of data
  data.out$type = 'mom'
  
  # return data
  return(data.out)
}

