#'readRFlowData
#'
#'Read flow data from rData file for line plots
#'
#'
#'@export

readRFlowData = function(flowData, channels = c('B1-H','V1-H'), dose.levels, time0 = 0, EDTA.point = NA, sample, date, type = 'VIA',
                         diff.cells = F){
  lineplotData = data.frame()

  for(i in 1:(length(flowData)-1)){
    temp.flowSet = flowData[[i]]
    timepoint = flowData$timepoints[i]

    if(!(timepoint %in% time0) && length(dose.levels) != length(temp.flowSet))
      warning(paste('Length of',names(flowData)[i], 'does not match length of conditions'), immediate. = T)
    for(j in 1:length(temp.flowSet)){
      flowframe = temp.flowSet[[j]]
      condition = dose.levels[j]
      newData = exprs(flowframe)[,channels]
      newData = as.matrix(newData)
      len = nrow(newData)
      metadata = data.frame(Sample = rep(sample,len), Date = rep(date, len),
                            Type = rep(type, len), Cells = rep(len,len), Condition = rep(condition,len),
                            Timepoint = rep(timepoint,len))
      newData = cbind(metadata,newData)
      lineplotData = rbind(lineplotData, newData)
    }
  }
  if(!diff.cells){
    time.levels = paste(flowData$timepoints, 'hrs')
    lineplotData$Timepoint = paste(lineplotData$Timepoint, 'hrs')
  } else {
    time.levels = flowData$timepoints
  }
  lineplotData$Timepoint = factor(lineplotData$Timepoint, levels = time.levels)
  lineplotData$Condition = factor(lineplotData$Condition, levels = dose.levels)
  return(lineplotData)
}
