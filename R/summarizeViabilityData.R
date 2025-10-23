#'summarizeViabilityData
#'
#'Summarize viability Data from dataframe
#'
#'@export

summarizeViabilityData = function(rawData, dose.levels, time.levels, channels = c('FL1-H','FL18-H'), breaks.via = c(3,3.2)){
  if(length(channels) != length(breaks.via)) stop("Number of channels doesn't match number of breakpoints")
  if(length(channels) < 2) stop('Two Flow Channels needed for Viability Gating')
  rawData$annexin = rawData[,channels[1]]
  rawData$dna = rawData[,channels[2]]
  sumData = ddply(rawData, .(Sample, Date, Condition, Timepoint, Cells), summarize, dna.pos = round((length(which(dna > breaks.via[2]))/Cells[1])*100, digits = 1),
                  annexin.neg = round((length(which(dna < breaks.via[2] & annexin < breaks.via[1]))/Cells[1])*100, digits = 1),
                  annexin.pos = round((length(which(dna < breaks.via[2] & annexin > breaks.via[1]))/Cells[1])*100,digits = 1))
  #error checking
  for(i in 1:nrow(sumData)){
    total =sumData$dna.pos[i] + sumData$annexin.neg[i] + sumData$annexin.pos[i]
    if(total < 99.8 | total > 100.2) warning(paste('Perentages in', paste(sumData$Sample[i], sumData$Date[i],sumData$Condition[i], sumData$Timepoint[i], sep = '_'),
                                                   'add up to',total), immediate. = T)
  }
  return(sumData)
}
