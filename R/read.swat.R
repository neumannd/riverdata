#' Reads in a SWAT output file
#' 
#' Reads a SWAT output file of the *.rch (reach), *.sub, and *.hru formats. 
#' Currently, only the formats as documented in the SWAT 2012 handbook are 
#' supported.
#'
#' @param filename character: name of the SWAT file
#' @param filetype character: official file extension of SWAT output file ('rch', 'sub', 'hru')
#' @param version integer/numeric: SWAT output version, only 2012 and 2016 (equal) are implemented
#' @param variables character array of variable names (==columns) to read in from the file; if == NULL [default], all variables are read
#'
#' @return as.data.frame, which contains the data from the file 'filename';
#'          column names of as.data.frame should be equal to column names in
#'          the reach file; only variables/columns listed in 'variables' are
#'          returned;
#' @export
#'
#' @examples
#'   
#'   # read in a swat *.rch (reach) file
#'   data.swat <- read.swat('output.rch')
#'   
#'   # read in a swat *.sub file
#'   data.swat <- read.swat('output.sub')
#'   # NOTE: not implemented yet
#'   
#'   # read in a swat reach file with another file extension
#'   data.swat <- read.swat('output.txt', filetype = 'rch')
#'   
#'   # read in specific variables of a swat *.rch (reach) file
#'   data.swat <- read.swat('output.rch', variables = c('NO3_OUT', 'NH4_OUT'))
#'   # NOTE: not implemented yet
#'   
read.swat <- function(filename, filetype = substr(filename,nchar(filename)-2,nchar(filename)), version = 2016, variables = NULL) {

  # error handling ----
  if ( !file.exists(filename) ) {
    stop(paste0('read.swat stop: file does not exist - ', filename))
  }

  if ( !(tolower(filetype)%in%c('rch', 'hru', 'sub')) ) {
    stop(paste0("read.swat stop: filetype ", filetype, " not supported. Please set filetype to 'rch', 'sub', or 'hru'"))
  }

  if ( tolower(filetype) == 'hru' ) {
    stop(paste0("read.swat stop: reading of filetype hru not implemeted yet"))
    str.firstcol = 'LULC  HRU '
  }

  if ( tolower(filetype) == 'sub' ) {
    stop(paste0("read.swat stop: reading of filetype sub not implemeted yet"))
    str.firstcol = '       SUB'
  }
  
  if ( tolower(filetype) == 'rch' ) {
    str.firstcol = '       RCH'
  }

  
  # initialize variables
  izero = as.integer(0)
  ione = as.integer(1)
  itwo = as.integer(2)
  
  nrows.data = izero
  nrows.total = izero
  nrows.meta = as.integer(8)
  row.header = as.integer(9)
  row.data = as.integer(10)
  

  # start processing ----
  if ( version %in% c(2012, 2016) ) {
    
    # open file
    fileHandle <- file(filename, open="rt")
    
    # count lines in file
    if (isOpen(fileHandle)) {
      str.tmp = readLines(fileHandle, n=1)
      
      # count rows of meta data
      while ( (length(str.tmp)) > 0 && (substr(str.tmp,1,10) != str.firstcol) ) {
        nrows.total = nrows.total + ione
        str.tmp = readLines(fileHandle, n=1)
      }
      
      # set some line numbers
      if ( nrows.total != nrows.meta ) {
        warning(paste0('read.swat warning: number of lines of meta data (', nrows.total, ') not as expected (', nrows.meta, ').'))
        nrows.meta = nrows.total
      }
      nrows.total = nrows.total + ione
      row.header = nrows.total
      row.data = nrows.total + ione
      
      # count further rows
      while ( (length(readLines(fileHandle, n=1))) > 0 ) {
        nrows.total = nrows.total + ione
      }
      nrows.data = nrows.total - ione - nrows.meta
      
      # close and re-open file to reset the line pointer to 1
      close(fileHandle)                    # close file
      fileHandle <- file(filename, open="rt") # re-open file
    }
    
    if (isOpen(fileHandle)) {
      # read rows of meta data
      str.tmp <- readLines(fileHandle, n = row.header-ione)
      print(nrows.data)
      
      if ( filetype == 'rch' ) {
        data.out <- read.swat.data.rch.2012(fileHandle, nrows.data)
      } else if ( filetype == 'sub' ) {
        stop('not implemented')
      } else if ( filetype == 'hru' ) {
        stop('not implemented')
      }
      
      close(fileHandle)
    }
  } else {
    close(fileHandle)
    stop(paste0('read.swat stop: version ', version, ' not supported.'))
  }

  
  # return output ----
  return(data.out)

}
