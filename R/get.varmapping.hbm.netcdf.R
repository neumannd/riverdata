#' Get mapping of "netCDF variables names for HBM river input files" to short names
#'
#' This mapping maps netCDF variable names as used in HBM river input files
#' to corresponding short names, which are often used. 
#'
#' @return nested lists; look at the output yourself
#' @author Daniel Neumann, daniel.neumann@io-warnemuende.de
#' @seealso write.river.netCDF
#' @export
#'
#' @examples
#' 
#'   # get variable mapping
#'   varmapping <- get.varmapping.hbm.netcdf()
#'   
get.varmapping.hbm.netcdf <- function() {
  
  mapping <- list('untagged' = list('mole_concentration_of_ammonium_in_seawater' = 'nh4',
                                    'mole_concentration_of_nitrate_in_seawater' = 'no3',
                                    'mole_concentration_of_phosphate_in_seawater' = 'po4',
                                    'mole_concentration_of_diatoms_expressed_as_nitrogen_in_seawater' = '',
                                    'mole_concentration_of_flagellates_expressed_as_nitrogen_in_seawater' = '',
                                    'mole_concentration_of_cyanobacteria_expressed_as_nitrogen_in_seawater' = '',
                                    'mole_concentration_of_zooplankton_expressed_as_nitrogen_in_seawater' = '',
                                    'mole_concentration_of_detritus_expressed_as_nitrogen_in_seawater' = 'det',
                                    'mole_concentration_of_protozooplankton_expressed_as_nitrogen_in_seawater' = '',
                                    'mole_concentration_of_oxygen_in_seawater' = '',
                                    'mole_concentration_of_silicate_in_seawater' = '',
                                    'mole_concentration_of_detritus_expressed_as_silicate_in_seawater' = '',
                                    'mole_concentration_of_labile_dissolved_organic_nitrogen_expressed_as_nitrogen_in_seawater' = ''),
                  'tagged' = list('mole_concentration_of_ammonium_from_RIVERNAME_in_seawater' = 'nh4',
                                  'mole_concentration_of_nitrate_from_RIVERNAME_in_seawater' = 'no3',
                                  'mole_concentration_of_phosphate_from_RIVERNAME_in_seawater' = 'po4',
                                  'mole_concentration_of_diatoms_from_RIVERNAME_expressed_as_nitrogen_in_seawater' = '',
                                  'mole_concentration_of_flagellates_from_RIVERNAME_expressed_as_nitrogen_in_seawater' = '',
                                  'mole_concentration_of_cyanobacteria_from_RIVERNAME_expressed_as_nitrogen_in_seawater' = '',
                                  'mole_concentration_of_zooplankton_from_RIVERNAME_expressed_as_nitrogen_in_seawater' = '',
                                  'mole_concentration_of_detritus_from_RIVERNAME_expressed_as_nitrogen_in_seawater' = 'det',
                                  'mole_concentration_of_protozooplankton_from_RIVERNAME_expressed_as_nitrogen_in_seawater' = ''))
  
  return(mapping)
}