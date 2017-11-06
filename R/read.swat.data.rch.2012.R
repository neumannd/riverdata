#' Read data from a reach (RCH) SWAT output file
#' 
#' This function reads data from an already opened SWAT reach (*.rch) file of 
#' the SWAT 2012 format.
#'
#' @param fileHandle open connection to a file; the pointer in the file has to 
#'                    point to the header row if 'header=TRUE'; if 
#'                    'header=FALSE' it has to point to the first data row
#' @param nrow.data number of data rows to read
#' @param header logical: consider the first row as header row (or not) [default = TRUE]
#' @param nreach integer: number of reaches in the file
#' @param ireach integer: index of the reach's data to extract
#'
#' @return list containing a data.frame (out$data), a character array 
#'           (out$units), and two character variables (out$format and
#'           out$tstep). The first contains the actual data formatted as a 
#'           data.frame. The second contains the units to the corresponding 
#'           columns of the data.frame. The third contains the source/format 
#'           of data (here: 'swat'; can also be 'mom'). The fourth contains
#'           information on the time step of the data (resp.: on which time
#'           interval they are averaged).
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @seealso read.swat, read.river.mom
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
read.swat.data.rch.2012 <- function(fileHandle, nrow.data, header = TRUE, nreach = 1, ireach = 1) {
  
  if (ireach > nreach) {
    step('read.swat.data.rch.2012 stop: ireach > nreach; ireach has to be lower-equal than nreach.')
  }
  
  # column indices; first column is the start index and the second column the end index
  ncol = 49
  int.cols = array(c(7,12,21,(1:46)*12+14, 10, 19, 25, (1:46)*12+25), dim = c(ncol,2))
  
  # data.frame for output data:
  data.out = list()
  data.out$data = as.data.frame(list('RCH' = rep(0L, nrow.data) ,
                                     'GIS' = rep(0L, nrow.data) ,
                                     'MON' = rep(0L, nrow.data) ,
                                     'AREA' = rep(0.0, nrow.data) ,
                                     'FLOW_IN' = rep(0.0, nrow.data) ,
                                     'FLOW_OUT' = rep(0.0, nrow.data) ,
                                     'EVAP' = rep(0.0, nrow.data) ,
                                     'TLOSS' = rep(0.0, nrow.data) ,
                                     'SED_IN' = rep(0.0, nrow.data) ,
                                     'SED_OUT' = rep(0.0, nrow.data) ,
                                     'SEDCONC' = rep(0.0, nrow.data) ,
                                     'ORGN_IN' = rep(0.0, nrow.data) ,
                                     'ORGN_OUT' = rep(0.0, nrow.data) ,
                                     'ORGP_IN' = rep(0.0, nrow.data) ,
                                     'ORGP_OUT' = rep(0.0, nrow.data) ,
                                     'NO3_IN' = rep(0.0, nrow.data) ,
                                     'NO3_OUT' = rep(0.0, nrow.data) ,
                                     'NH4_IN' = rep(0.0, nrow.data) ,
                                     'NH4_OUT' = rep(0.0, nrow.data) ,
                                     'NO2_IN' = rep(0.0, nrow.data) ,
                                     'NO2_OUT' = rep(0.0, nrow.data) ,
                                     'MINP_IN' = rep(0.0, nrow.data) ,
                                     'MINP_OUT' = rep(0.0, nrow.data) ,
                                     'CHLA_IN' = rep(0.0, nrow.data) ,
                                     'CHLA_OUT' = rep(0.0, nrow.data) ,
                                     'CBOD_IN' = rep(0.0, nrow.data) ,
                                     'CBOD_OUT' = rep(0.0, nrow.data) ,
                                     'DISOX_IN' = rep(0.0, nrow.data) ,
                                     'DISOX_OUT' = rep(0.0, nrow.data) ,
                                     'SOLPST_IN' = rep(0.0, nrow.data) ,
                                     'SOLPST_OUT' = rep(0.0, nrow.data) ,
                                     'SORPST_IN' = rep(0.0, nrow.data) ,
                                     'SORPST_OUT' = rep(0.0, nrow.data) ,
                                     'REACTPST' = rep(0.0, nrow.data) ,
                                     'VOLPST' = rep(0.0, nrow.data) ,
                                     'SETTLPST' = rep(0.0, nrow.data) ,
                                     'RESUSP_PST' = rep(0.0, nrow.data) ,
                                     'DIFFUSEPST' = rep(0.0, nrow.data) ,
                                     'REACBEDPST' = rep(0.0, nrow.data) ,
                                     'BURYPST' = rep(0.0, nrow.data) ,
                                     'BED_PST' = rep(0.0, nrow.data) ,
                                     'BACTP_OUT' = rep(0.0, nrow.data) ,
                                     'BACTLP_OUT' = rep(0.0, nrow.data) ,
                                     'CMETAL#1' = rep(0.0, nrow.data) ,
                                     'CMETAL#2' = rep(0.0, nrow.data) ,
                                     'CMETAL#3' = rep(0.0, nrow.data) ,
                                     'TOT N' = rep(0.0, nrow.data) ,
                                     'TOT P' = rep(0.0, nrow.data) ,
                                     'NO3CONC' = rep(0.0, nrow.data)))
  data.out$units <- c('',
                      '',
                      '',
                      'km2',
                      'm3/s',
                      'm3/s',
                      'm3/s',
                      'm3/s',
                      't',
                      't',
                      'mg/L',
                      'kg N',
                      'kg N',
                      'kg P',
                      'kg P',
                      'kg N',
                      'kg N',
                      'kg N',
                      'kg N',
                      'kg N',
                      'kg N',
                      'kg P',
                      'kg P',
                      'kg chl-a',
                      'kg chl-a',
                      'kg O2',
                      'kg O2',
                      'kg O2',
                      'kg O2',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      'mg active ingredient',
                      '#cfu/100 mL',
                      '#cfu/100 mL',
                      'kg',
                      'kg',
                      'kg',
                      'kg N',
                      'kg P',
                      'mg NO3-N/kg sed')
  data.out$format = 'swat'
  otData$tstep = 'annual'
  warning('read.swat.data.rch.2012 warning: tstep might not be properly set')
  
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
        names(data.out$data)[icol] = trimws(substr(str.header.real, int.cols[icol, 1], int.cols[icol, 2]))
      }
    }
  }
  
  
  # read the data
  irow.out = 1
  for (irow in 1:nrow.data) {
    str.tmp <- readLines(fileHandle, n = 1L)
    
    # copy integers (first three columns)
    # if we are in a row that should be written out
    if ( ((irow-1)%%nreach)+1 == ireach ) {
      for (icol in 1:3) {
        data.out$data[irow.out, icol] = strtoi(substr(str.tmp, int.cols[icol, 1], int.cols[icol, 2]))
      }
      
      # copy numerics/floats/doubles
      for (icol in 4:ncol) {
        data.out$data[irow.out, icol] = as.numeric(substr(str.tmp, int.cols[icol, 1], int.cols[icol, 2]))
      }
    }
  }
  
  # return output
  return(data.out)
}