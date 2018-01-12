library('riverdata')
library('ncdf4')
source('../R/read.river.mom.R')
source('../R/interpolate.river.mom.R')
source('../R/mean.river.mom.R')
source('../R/write.river.append2NML.R')
source('../../riverdata.dirty/R/write.river.netCDF.R')


# read a file:
test.mom.monthly <- list()
test.mom.monthly$Warnow <- read.river.mom('files/GER_Dan_Str_Warnow.dat')
test.mom.monthly$Trave <- read.river.mom('files/GER_Dan_Str_Trave.dat')

# calculate annual means from monthly data
test.mom.annual <- list()
test.mom.daily$Warnow <- interpolate.river.mom(test.mom.monthly$Warnow, to = 'daily', method = 'step')
test.mom.daily$Trave <- interpolate.river.mom(test.mom.monthly$Trave, to = 'daily', method = 'step')

# get netCDF file variable mapping
varmapping.hbm <- get.varmapping.hbm.netcdf()

# get grid info
grid_info <- get.infos.grids.hbm.basic()

# get river infos
file <- 'files/river_list.dat'
riverInfos <- read.infos.rivers(file, grid_info)
# (you will get some warnings here)

# write new namelist
write.river.netCDF(c('Warnow', 'Trave'), riverInfos, test.mom.daily, varmapping.hbm, 'out_dir', 2012, overwrite=FALSE, cmd_ncgen = '/media/neumannd/sw/linux/packages/netcdf/netcdf-4.4.1.1/gcc-5.4.0/noMPI/bin/ncgen')
