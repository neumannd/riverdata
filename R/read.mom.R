
# filename='../level-1b/GER_Dan_Str_Warnow.dat'

readRiverData = function(filename) {

  riverData = list()
  
  rawData = read.table(filename, header = FALSE, stringsAsFactors = FALSE, skip = 4)
  
  rawHeader = strsplit(trimws(substr(scan(filename, skip = 3, nlines = 1, what = character(), sep = ';')[[1]], 6,9999)), split = ']', fixed=TRUE)[[1]]
  nVar = length(rawHeader)
  
  varName = rep('', nVar)
  varUnit = rep('', nVar)
  for (iV in 1:nVar) {
    tmpSplit = strsplit(rawHeader[iV], split = '[', fixed = TRUE)[[1]]
    varName[iV] = trimws(tmpSplit[1])
    varUnit[iV] = trimws(tmpSplit[2])
  }
  
  riverData$time = strptime(paste(rawData[,1], '-', rawData[,2], '-', rawData[,3], ' ', 
               rawData[,4], ':', rawData[,5], ':', rawData[,6], ' GMT', sep = ''), format = '%Y-%m-%d %H:%M:%S', tz='GMT')
  riverData$vals = rawData[,7:(6+nVar)]
  riverData$names = varName
  riverData$units = varUnit
  
  return(riverData)
}

