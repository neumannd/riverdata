

readData <- FALSE
plotA <- TRUE
plotB <- TRUE

if (readData) {
  source('process_rivesr.R')
}


if ( plotA ) {
  unitConvert=31*86.4*365
  time = riverDataAnnual$Warnow$time
  flow = riverDataAnnual$Warnow$vals[,1]
  minP = riverDataAnnual$Warnow$vals[,5] * flow * unitConvert/10^3
  plot(time, minP, col = 'red', type = 'l',
       ylim = c(0, max(minP)),
       xlim = c(1992, 2007), 
       ylab = 'P [t/a]', xlab = 'year',
       lty = 'dashed')
  points(time, minP, col = 'red', pch = 0)
  
  legend('right', c('total P', 'org. P', 'mineral P'),
         col = c('black', 'blue', 'red'),
         pch = c(1, 5, 0), 
         lty = c('solid', 'solid', 'dashed'))
  axis(1, at = time, labels = NA)
  axis(1, at = c(1992, 2007))
}


if ( plotB ) {
  time = riverDataAnnual$Warnow$time
  minP = riverDataAnnual$Warnow$vals[,5]
  plot(time, minP, col = 'red', type = 'l',
       ylim = c(0, max(minP)),
       xlim = c(1992, 2007), 
       ylab = 'P [mol/kg]', xlab = 'year',
       lty = 'dashed')
  points(time, minP, col = 'red', pch = 0)
  
  legend('right', c('total P', 'org. P', 'mineral P'),
         col = c('black', 'blue', 'red'),
         pch = c(1, 5, 0), 
         lty = c('solid', 'solid', 'dashed'))
  axis(1, at = time, labels = NA)
  axis(1, at = c(1992, 2007))
}