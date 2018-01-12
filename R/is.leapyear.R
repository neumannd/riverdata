#' Is a year a leap year?
#' 
#' Provides information (TRUE/FALSE) whether a year is a leap year (TRUE) or not (FALSE).
#'
#' @param year a numeric giving a year
#'
#' Dies ist ein Test
#'
#' @return Returns a logical. It is TRUE when 'year' is a leap year according to the Gregorian calender. Else it is false.
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @export
#'
#' @examples
#'   is.leapyear(2012)
#'   # return: TRUE
#'   
#'   is.leapyear(2001)
#'   # return: FALSE
#'   
#'   is.leapyear(1998:2010)
#'   # return: FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE
is.leapyear=function(year){
  ## More information::
  # http://en.wikipedia.org/wiki/Leap_year
  # https://www.r-bloggers.com/leap-years/
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}