write.river.append2NML = function(riverNames, riverInfos, riverData, grids, dIn, dOt, year, overwrite=TRUE, warn = TRUE) {
  
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
  #'  - increment NRIVERS by number of new rivers in DIMENSIONS namelist
  #'  - append coordinates (grid cell indices!) and inflow to RIVERS:
  #'       --Y,X,VAL
  #'       --MONTHLY (m3/s)
  
  strYear=formatC(year, format='d', width = 4)
  
  # read data ----
  strData=list()
  iRIVERSs = list()
  iDIMENSIONSs = list()
  nmlDIMENSIONS = list()
  nRivers = list()
  KRQI = list()
  RWQI = list()
  for (iG in names(grids)) {
    pIn = paste(dIn, paste0('data_', iG, '.nml'), sep = '/')
    strData[[iG]] = scan(pIn, what=character(), sep = '\n', quiet = TRUE)
    
    # get line in which DIMENSIONS namelist starts
    iDIMENSIONSs[[iG]] = which(strData[[iG]] == ' &DIMENSIONS')
    if (length(iDIMENSIONSs[[iG]]) == 0) {
      iDIMENSIONSs[[iG]] = which(strData[[iG]] == '&DIMENSIONS')
      if (length(iDIMENSIONSs[[iG]]) == 0) {
        stop(paste0('No DIMENSIONS parameter list in file ', pIn))
      }
    }
    
    # get line in which RIVERS namelist starts
    iRIVERSs[[iG]] = which(strData[[iG]] == ' &RIVERS')
    if (length(iRIVERSs[[iG]]) == 0) {
      iRIVERSs[[iG]] = which(strData[[iG]] == '&RIVERS')
      if (length(iRIVERSs[[iG]]) == 0) {
        stop(paste0('No RIVERS parameter list in file ', pIn))
      }
    }
    
    # disensemble and copy line with DIMENSIONS namelist:
    tmpLine = strData[[iG]][iDIMENSIONSs[[iG]]+1]
    tmp_river_loc1 = gregexpr('NRIVERS', tmpLine)[[1]]
    tmp_river_loc2 = gregexpr('NRIVERS=[0-9]*', tmpLine)[[1]]
    nmlDIMENSIONS[[iG]] = c(substr(tmpLine,1,tmp_river_loc1[1]+attr(tmp_river_loc1,'match.length')[1]),
                            substring(tmpLine,tmp_river_loc2[1]+attr(tmp_river_loc2,'match.length')[1]))
    nRivers[[iG]] = as.numeric(substr(tmpLine,tmp_river_loc1[1]+attr(tmp_river_loc1,'match.length')[1]+1, tmp_river_loc2[1]+attr(tmp_river_loc2,'match.length')[1]-1))
    
    
    # disensemble and copy line with RIVERS namelist:
    tmp_split_line = strsplit(strData[[iG]][iRIVERSs[[iG]]+1], ', RWQI', fixed = TRUE)[[1]]
    KRQI[[iG]]=tmp_split_line[1]
    RWQI[[iG]]=paste0(', RWQI', tmp_split_line[2])
  }
  
  
  # modify data ----
  for (iR in riverNames) {
    if (!(iR%in%names(riverInfos))) stop(paste0('River ', iR, ' is not in listed in riverInfos!'))
    if (!(iR%in%names(riverData))) stop(paste0('River ', iR, ' is not in listed in riverData!'))
    
    # index in riverData$vals to write out
    iT = which(riverData[[iR]]$data$time == strYear)
    valOt = riverData[[iR]]$data$runoff[iT]*10^-3
    
    iG=riverInfos[[iR]]$grid
    
    nRivers[[iG]] = nRivers[[iG]] + 1
    
    KRQI[[iG]] = paste(KRQI[[iG]], 
                                formatC(riverInfos[[iR]]$ycell, format='d'), 
                                formatC(riverInfos[[iR]]$xcell, format='d'), 
                                sep = ', ')
    RWQI[[iG]] = paste(RWQI[[iG]], 
                                formatC(valOt, format='f', width = 19, digits = 18-ceiling(log10(valOt))), 
                                sep = ', ')
  }
  
  
  
  # write data ----
  for (iG in names(grids)) {
    
    strData[[iG]][iDIMENSIONSs[[iG]]+1] = paste0(nmlDIMENSIONS[[iG]][1], formatC(nRivers[[iG]], format='d'), nmlDIMENSIONS[[iG]][2])
    strData[[iG]][iRIVERSs[[iG]]+1]     = paste0(KRQI[[iG]], RWQI[[iG]])
    
    fOt=paste(paste('data', iG, sep = '_'), 'nml', sep = '.')
    pOt=paste(dOt, fOt, sep = '/')
    
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