source('read.swat.R')
source('read.swat.data.2012.R')
source('/media/neumannd/work_dell/88_MeRamo/61_cmod_run_setup/11_iow_rivers/r_tools/support/readRiverData.R')
source('/media/neumannd/work_dell/88_MeRamo/61_cmod_run_setup/11_iow_rivers/r_tools/support/riverAverage.R')
source('/media/neumannd/work_dell/88_MeRamo/61_cmod_run_setup/11_iow_rivers/r_tools/support/is.leapyear.R')

filename.swat = '/media/neumannd/work_dell/89_PhosWAM/61_SWAT_Data/iow/output_test.rch'
filename.mom = '/media/neumannd/work_dell/88_MeRamo/61_cmod_run_setup/11_iow_rivers/level-1b/GER_Dan_Str_Warnow.dat'

readSWAT <- FALSE
readMOM <- FALSE
plotA <- FALSE
plotB <- FALSE
plotC <- TRUE

if (readSWAT) {
  data.swat <- read.swat(filename.swat)
}

if (readMOM) {
  data.mom.month <- readRiverData(filename.mom)
  data.mom.annual <- riverAverage(data.mom.month, from = 'monthly', to = 'annual')
}


if ( plotA ) {
  time.swat = data.swat$MON[(1:16)*29-28]
  orgP.swat = data.swat$ORGP_OUT[(1:16)*29-28]/10^3
  minP.swat = data.swat$MINP_OUT[(1:16)*29-28]/10^3
  totP.swat = data.swat[['TOT.P']][(1:16)*29-28]/10^3
  
  unitConvert.mom=31*86.4*365
  time.mom = data.mom.annual$time
  flow.mom = data.mom.annual$vals[,1]
  po4.mom = data.mom.annual$vals[,5] * flow.mom * unitConvert.mom/10^3
  
  plot(time.swat, totP.swat, col = 'black', type = 'l',
       ylim = c(0, max(c(totP.swat, po4.mom))),
       ylab = 'P [t/a]', xlab = 'year')
  points(time.swat, totP.swat, col = 'black', pch = 1)
  lines(time.swat, orgP.swat, col = 'blue', lty = 'solid')
  points(time.swat, orgP.swat, col = 'blue', pch = 5)
  lines(time.swat, minP.swat, col = 'red', lty = 'dashed')
  points(time.swat, minP.swat, col = 'red', pch = 0)
  lines(time.mom, po4.mom, col = 'darkgreen', lty = 'dotted')
  points(time.mom, po4.mom, col = 'darkgreen', pch = 4)
  
  legend('topright', c('SWAT, total P', 'SWAT, org. P', 'SWAT, mineral P', 'MOM, PO4-P'),
         col = c('black', 'blue', 'red', 'darkgreen'),
         pch = c(1, 5, 0, 4), 
         lty = c('solid', 'solid', 'dashed', 'dotted'))
  axis(1, at = time.swat, labels = NA)
  axis(1, at = c(min(time.swat), max(time.swat)))
}

if ( plotB ) {
  unitConvert.swat <- 31*86400*365
  time.swat = data.swat$MON[(1:16)*29-28]
  flow.swat = data.swat$FLOW_OUT[(1:16)*29-28]
  orgP.swat = data.swat$ORGP_OUT[(1:16)*29-28]/flow.swat/unitConvert.swat
  minP.swat = data.swat$MINP_OUT[(1:16)*29-28]/flow.swat/unitConvert.swat
  totP.swat = data.swat[['TOT.P']][(1:16)*29-28]/flow.swat/unitConvert.swat
  
  time.mom = data.mom.annual$time
  po4.mom = data.mom.annual$vals[,5]
  
  plot(time.swat, totP.swat, col = 'black', type = 'l',
       ylim = c(0, max(c(totP.swat, po4.mom))),
       ylab = 'P [mol/kg]', xlab = 'year')
  points(time.swat, totP.swat, col = 'black', pch = 1)
  lines(time.swat, orgP.swat, col = 'blue', lty = 'solid')
  points(time.swat, orgP.swat, col = 'blue', pch = 5)
  lines(time.swat, minP.swat, col = 'red', lty = 'dashed')
  points(time.swat, minP.swat, col = 'red', pch = 0)
  lines(time.mom, po4.mom, col = 'darkgreen', lty = 'dotted')
  points(time.mom, po4.mom, col = 'darkgreen', pch = 4)
  
  legend('topright', c('SWAT, total P', 'SWAT, org. P', 'SWAT, mineral P', 'MOM, PO4-P'),
         col = c('black', 'blue', 'red', 'darkgreen'),
         pch = c(1, 5, 0, 4), 
         lty = c('solid', 'solid', 'dashed', 'dotted'))
  axis(1, at = time.swat, labels = NA)
  axis(1, at = c(min(time.swat), max(time.swat)))
}


if ( plotC ) {
  time.swat = data.swat$MON[(1:16)*29-28]
  flow.swat = data.swat$FLOW_OUT[(1:16)*29-28]
  
  time.mom = data.mom.annual$time
  flow.mom = data.mom.annual$vals[,1]*10^(-3)
  
  plot(time.swat, flow.swat, col = 'black', type = 'l',
       ylim = c(0, max(c(flow.swat, flow.mom))),
       ylab = 'runoff [m3/s]', xlab = 'year')
  points(time.swat, flow.swat, col = 'black', pch = 1)
  lines(time.mom, flow.mom, col = 'darkgreen', lty = 'dotted')
  points(time.mom, flow.mom, col = 'darkgreen', pch = 4)
  
  legend('topright', c('SWAT, runoff', 'MOM, runoff'),
         col = c('black', 'darkgreen'),
         pch = c(1, 4), 
         lty = c('solid', 'dotted'))
  axis(1, at = time.swat, labels = NA)
  axis(1, at = c(min(time.swat), max(time.swat)))
}


if ( plotA ) {
  time.swat = data.swat$MON[(1:16)*29-28]
  orgP.swat = data.swat$ORGP_OUT[(1:16)*29-28]/10^3
  minP.swat = data.swat$MINP_OUT[(1:16)*29-28]/10^3
  totP.swat = data.swat[['TOT.P']][(1:16)*29-28]/10^3
  
  unitConvert.mom=31*86.4*365
  time.mom = data.mom.annual$time
  flow.mom = data.mom.annual$vals[,1]
  po4.mom = data.mom.annual$vals[,5] * flow.mom * unitConvert.mom/10^3
  
  plot(time.swat, totP.swat, col = 'black', type = 'l',
       ylim = c(0, max(c(totP.swat, po4.mom))),
       ylab = 'P [t/a]', xlab = 'year')
  points(time.swat, totP.swat, col = 'black', pch = 1)
  lines(time.swat, orgP.swat, col = 'blue', lty = 'solid')
  points(time.swat, orgP.swat, col = 'blue', pch = 5)
  lines(time.swat, minP.swat, col = 'red', lty = 'dashed')
  points(time.swat, minP.swat, col = 'red', pch = 0)
  lines(time.mom, po4.mom, col = 'darkgreen', lty = 'dotted')
  points(time.mom, po4.mom, col = 'darkgreen', pch = 4)
  
  legend('topright', c('SWAT, total P', 'SWAT, org. P', 'SWAT, mineral P', 'MOM, PO4-P'),
         col = c('black', 'blue', 'red', 'darkgreen'),
         pch = c(1, 5, 0, 4), 
         lty = c('solid', 'solid', 'dashed', 'dotted'))
  axis(1, at = time.swat, labels = NA)
  axis(1, at = c(min(time.swat), max(time.swat)))
}