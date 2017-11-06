write.river.newNML = function(riverInfo, riverData, dOt, year, overwrite=TRUE, warn = TRUE) {
  
  errCode = 0
  
  if ( riverData$tstep != 'annual' ) {
    stop('write.river.newNML stop: data has to be on an annual interval')
  }
  
  ## NAMELIST: data_GRID_RIVER.nml
  #' &DIMENSIONS
  #' EW=630, NS=387, LAYERS=25, NZBND=858, NUBND=0, NVBND=0, NRIVERS=8, NUDAMS=0, NVDAMS=0, NWEIRS=0
  #' /
  #' &RIVERS
  #' KRQI=0, 0, 341, 606, 376, 82, 386, 169, 364, 302, 263, 193, 84, 579, 26, 477, 65, 159, RWQI=0.000000000000000000E+00, 522.000000000000000, 100.000000000000000, 450.000000000000000, 90
  #' 0.000000000000000, 37.0000000000000000, 48.0000000000000000, 21.0000000000000000, 125.000000000000000
  #' /
  #'
  #' TODOs:
  #'  - write NRIVERS in DIMENSIONS namelist
  #'  - write coordinates (grid cell indices!) and inflow to RIVERS:
  #'       --Y,X,VAL
  #'       --MONTHLY (m3/s)
  
  # init output array (to write to file):
  strData = rep('', 6)
  
  # increment NRIVERS
  strData[[1]] = ' &DIMENSIONS'
  strData[[2]] = 'EW=0, NS=0, LAYERS=0, NZBND=0, NUBND=0, NVBND=0, NRIVERS=1, NUDAMS=0, NVDAMS=0, NWEIRS=0'
  strData[[3]] = ' /'
  
  
  # init more values
  strYear=formatC(year, format='d', width = 4)
  iG=riverInfo$grid
  iR=riverInfo$rivername
  
  # index in riverData$data to write out
  iT = which(riverData$data$time == strYear)
  valOt = riverData$data$runoff[iT]*10^-3
  
  strData[[4]] = ' &RIVERS'
  strData[[5]] = paste(' KRQI=0, 0', formatC(riverInfo$ycell, format='d'), formatC(riverInfo$xcell, format='d'), 
                'RWQI=0.000000000000000000E+00', formatC(valOt, format='f', width = 19, digits = 18-ceiling(log10(valOt))), sep = ', ')
  strData[[6]] = ' /'
  
  fOt=paste(paste('data', iG, iR, strYear, sep = '_'), 'nml', sep = '.')
  pOt=paste(dOt, fOt, sep = '/')
  
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