#' Read data from a reach (RCH) SWAT output file
#' 
#' This function reads data from an already opened SWAT reach (*.rch) file of 
#' the SWAT 2012 format.
#'
#' @param fileHandle open connection to a file; the pointer in the file has to 
#'                    point to the header row if 'header=TRUE'; if 
#'                    'header=FALSE' it has to point to the first data row
#' @param nrow.data number of data rows to read
#'
#' @return as.data.frame, which contains the data from the output.rch file; 
#'          column names of as.data.frame should be equal to column names in
#'          the reach file
#' @export
#'
#' @examples
#'   
#'   # open file
#'   fileHandle <- file('output.rch', open="rt")
#'   
#'   # skip first 8 lines of meta data
#'   tmp <- readLines(fileHandle, n = 8)
#'   
#'   # read 100 lines of data
#'   data.out <- read.swat.data.2012(fileHandle, 100)
#'   
#'   # close file
#'   close(fileHandle)
#'   
read.swat.data.rch.2012 <- function(fileHandle, nrow.data, header = TRUE) {
  
  # str.diag <- rep('', nrow.data)
  
  # some constants
  izero = as.integer(0)
  rzero = as.numeric(0.0)
  
  # column indices; first column is the start index and the second column the end index
  ncol = 49
  int.cols = array(c(7,12,21,(1:46)*12+14, 10, 19, 25, (1:46)*12+25), dim = c(ncol,2))
  
  # data.frame for output data:
  data.out = as.data.frame(list('RCH' = rep(izero, nrow.data) ,
                                'GIS' = rep(izero, nrow.data) ,
                                'MON' = rep(izero, nrow.data) ,
                                'AREA' = rep(rzero, nrow.data) ,
                                'FLOW_IN' = rep(rzero, nrow.data) ,
                                'FLOW_OUT' = rep(rzero, nrow.data) ,
                                'EVAP' = rep(rzero, nrow.data) ,
                                'TLOSS' = rep(rzero, nrow.data) ,
                                'SED_IN' = rep(rzero, nrow.data) ,
                                'SED_OUT' = rep(rzero, nrow.data) ,
                                'SEDCONC' = rep(rzero, nrow.data) ,
                                'ORGN_IN' = rep(rzero, nrow.data) ,
                                'ORGN_OUT' = rep(rzero, nrow.data) ,
                                'ORGP_IN' = rep(rzero, nrow.data) ,
                                'ORGP_OUT' = rep(rzero, nrow.data) ,
                                'NO3_IN' = rep(rzero, nrow.data) ,
                                'NO3_OUT' = rep(rzero, nrow.data) ,
                                'NH4_IN' = rep(rzero, nrow.data) ,
                                'NH4_OUT' = rep(rzero, nrow.data) ,
                                'NO2_IN' = rep(rzero, nrow.data) ,
                                'NO2_OUT' = rep(rzero, nrow.data) ,
                                'MINP_IN' = rep(rzero, nrow.data) ,
                                'MINP_OUT' = rep(rzero, nrow.data) ,
                                'CHLA_IN' = rep(rzero, nrow.data) ,
                                'CHLA_OUT' = rep(rzero, nrow.data) ,
                                'CBOD_IN' = rep(rzero, nrow.data) ,
                                'CBOD_OUT' = rep(rzero, nrow.data) ,
                                'DISOX_IN' = rep(rzero, nrow.data) ,
                                'DISOX_OUT' = rep(rzero, nrow.data) ,
                                'SOLPST_IN' = rep(rzero, nrow.data) ,
                                'SOLPST_OUT' = rep(rzero, nrow.data) ,
                                'SORPST_IN' = rep(rzero, nrow.data) ,
                                'SORPST_OUT' = rep(rzero, nrow.data) ,
                                'REACTPST' = rep(rzero, nrow.data) ,
                                'VOLPST' = rep(rzero, nrow.data) ,
                                'SETTLPST' = rep(rzero, nrow.data) ,
                                'RESUSP_PST' = rep(rzero, nrow.data) ,
                                'DIFFUSEPST' = rep(rzero, nrow.data) ,
                                'REACBEDPST' = rep(rzero, nrow.data) ,
                                'BURYPST' = rep(rzero, nrow.data) ,
                                'BED_PST' = rep(rzero, nrow.data) ,
                                'BACTP_OUT' = rep(rzero, nrow.data) ,
                                'BACTLP_OUT' = rep(rzero, nrow.data) ,
                                'CMETAL#1' = rep(rzero, nrow.data) ,
                                'CMETAL#2' = rep(rzero, nrow.data) ,
                                'CMETAL#3' = rep(rzero, nrow.data) ,
                                'TOT N' = rep(rzero, nrow.data) ,
                                'TOT P' = rep(rzero, nrow.data) ,
                                'NO3CONC' = rep(rzero, nrow.data)))
  
  # expected default header
  str.header.dflt <- '       RCH      GIS   MON     AREAkm2  FLOW_INcms FLOW_OUTcms     EVAPcms    TLOSScms  SED_INtons SED_OUTtons SEDCONCmg/L   ORGN_INkg  ORGN_OUTkg   ORGP_INkg  ORGP_OUTkg    NO3_INkg   NO3_OUTkg    NH4_INkg   NH4_OUTkg    NO2_INkg   NO2_OUTkg   MINP_INkg  MINP_OUTkg   CHLA_INkg  CHLA_OUTkg   CBOD_INkg  CBOD_OUTkg  DISOX_INkg DISOX_OUTkg SOLPST_INmgSOLPST_OUTmg SORPST_INmgSORPST_OUTmg  REACTPSTmg    VOLPSTmg  SETTLPSTmgRESUSP_PSTmgDIFFUSEPSTmgREACBEDPSTmg   BURYPSTmg   BED_PSTmg BACTP_OUTctBACTLP_OUTct  CMETAL#1kg  CMETAL#2kg  CMETAL#3kg     TOT Nkg     TOT Pkg NO3ConcMg/l    WTMPdegc'
  
  if (header) {
    # real header in the file
    str.header.real <- readLines(fileHandle, n = 1L)
    
    # look whether we have the header which we expect
    if ( str.header.dflt != str.header.real ) {
      warning('read.swat.data.2012 warning: expected and actual content of header line differed! Be careful with the output data')
      if ( nchar(str.header.real) < max(int.cols) ) {
        close(fileHandle)
        stop(paste0('read.swat.data.2012 stop: length of actual header row is too short; should be: ', max(int.cols), ', but is: ', nchar(str.header.real)))
      }
      
      # copy new header
      for (icol in 1:ncol) {
        names(data.out)[icol] = trimws(substr(str.header.real, int.cols[icol, 1], int.cols[icol, 2]))
      }
    }
  }
  
  
  # read the data
  for (irow in 1:nrow.data) {
    str.tmp <- readLines(fileHandle, n = 1L)
    # str.diag[irow] = str.tmp
    
    # copy integers (first three columns)
    for (icol in 1:3) {
      data.out[irow, icol] = strtoi(substr(str.tmp, int.cols[icol, 1], int.cols[icol, 2]))
    }
    
    # copy numerics/floats/doubles
    for (icol in 4:ncol) {
      data.out[irow, icol] = as.numeric(substr(str.tmp, int.cols[icol, 1], int.cols[icol, 2]))
    }
  }
  
  # return(list('df'=data.out, 'diag'=str.diag))
  return(data.out)
}