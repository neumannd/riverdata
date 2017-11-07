# load libs ----
library('riverdata')
library('ncdf4')

cmd_ncgen = '/media/neumannd/sw/linux/packages/netcdf/netcdf-4.4.1.1/gcc-5.4.0/noMPI/bin/ncgen'


# use controls ----
readBasic=TRUE
readData=TRUE
processData=TRUE
writeData=FALSE

fRivers='../control_files/river_list.dat'
dBaseFiles='./basefiles'
dOut='../level-2'
year=2012


# read basic data ----
if (readBasic) {
  grids      = get.infos.grids.hbm.basic()
  riverInfos = read.infos.rivers(fRivers, grids)
  riverNames = names(riverInfos)
  netCDF2input = get.varmapping.hbm.netcdf()
}


# read data ----
if (readData) {
  riverDataRaw = list()
  riverDataMonth = list()
  
  for (iR in riverNames) {
    print(paste0('reading river ', iR))
    riverDataMonth[[iR]] = readRiverData(riverInfos[[iR]]$filename)
  }
}


# process data ----
if (processData) {
  riverDataDaily = list()
  riverDataAnnual = list()
  
  for (iR in riverNames) {
    print(paste0('processing river ', iR))
    print(' interpolating data')
    riverDataDaily[[iR]] = riverInterpolate(riverDataMonth[[iR]], from='monthly', to='daily', method = 'step')
    print(' averaging data')
    riverDataAnnual[[iR]] = riverAverage(riverDataMonth[[iR]], from = 'monthly', to = 'annual')
  }
}


# write data ----
if (writeData) {
  for (iR in riverNames) {
    print(paste0('writing for river ', iR))
    # print(' writing new nml (one river per file) [extra]')
    # writeRiverNewNML(riverInfos[[iR]], riverDataAnnual[[iR]],
    #                  paste(dOut, 'nml', sep = '/'),
    #                  year, warn = FALSE)
    # print(' writing new riverinflow (one river per file) [extra]')
    # writeRiverNewInflow(riverInfos[[iR]], riverDataDaily[[iR]],
    #                     paste(dOut, 'RIVERS_NAME', sep = '/'),
    #                     year, warn = FALSE)
  }
  
  # print(' writing nml (append to existing) [default]')
  # writeRiverAppend2NML(riverNames, riverInfos, riverDataAnnual, grids,
  #                      paste(dBaseFiles, 'nml', sep = '/'),
  #                      paste(dOut, 'nml', sep = '/'),
  #                      year, warn = FALSE)
  # print(' writing riverinflow (append to existing) [default]')
  # writeRiverAppend2Inflow(riverNames, riverInfos, riverDataDaily, grids,
  #                         paste(dBaseFiles, 'RIVERS', sep = '/'),
  #                         paste(dOut, 'RIVERS', sep = '/'),
  #                         year, warn = FALSE)
  # print(' writing new netCDF (one river per file) [default]')
  # writeRiverNetCDF(riverNames, riverInfo, riverDataDaily,
  #                  netCDF2input, year, warn = FALSE, cmd_ncgen = cmd_ncgen)
}
