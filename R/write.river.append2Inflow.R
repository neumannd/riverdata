write.river.append2Inflow = function(riverNames, riverInfos, riverData, grids, dIn, dOt, year, month=0, day=0, overwrite=TRUE, warn=TRUE) {
  
  
  # initialize variables ----
  # unit: (m3/s)
  strYear='2012'
  nDaysM = c(31,28+ifelse(is.leapyear(year),1,0),31,30,31,30,31,31,30,31,30,31)
  strDays = list()
  
  
  # prepare dates ---
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
      # prepare data ----
      strAppnd = list('fine'='', 'coarse'='')
      for (iR in riverNames) {
        if (!(iR%in%names(riverInfos))) stop(paste0('River ', iR, ' is not in listed in riverInfos!'))
        if (!(iR%in%names(riverData))) stop(paste0('River ', iR, ' is not in listed in riverData!'))
        
        # index in riverData$vals to write out
        iT = which(substr(riverData[[iR]]$data$time, 1, 10) == paste(strYear, iM, iD, sep = '-'))
        valOt = riverData[[iR]]$data$runoff[iT]*10^-3
        
        iG=riverInfos[[iR]]$grid
        
        strAppnd[[iG]] = paste0(strAppnd[[iG]],formatC(valOt, format='f', width = 8, digits = 1))
      }
      
      for (iH in c('00', '12')) {
        strData=list()
        for (iG in names(grids)) {
          
          # read data ----
          jG=ifelse(iG=='fine','02',ifelse(iG=='coarse','01','00'))
          pIn=paste(dIn, paste0('riverinflow_', jG, '-', strYear, iM, iD, iH), sep = '/')
          # print(pIn)
          strData[[iG]] = scan(pIn, what=character(), sep = '\n', quiet = TRUE)
          
          
          # modify data ----
          strData[[iG]][2] = paste0(strData[[iG]][2], strAppnd[[iG]])
          
          
          # write data ----
          fOt=paste0('riverinflow_', jG, '-', strYear, iM, iD, iH)
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
            write(strData[[iG]], file = fId, append = FALSE, sep = '\n')
            close(fId)
          }
        }
      }
    }
  }
}