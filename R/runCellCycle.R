#'reunCellCycle
#'
#'Plot Cell Cycle Data
#'
#'
#'@export
#'
runCellCycle = function(flowData, channels = 'V1-H', dose.levels, sample, date, breaks = breaks, xlab = 'DNA - DAPI', ylab = 'BrdU - FITC',
                        plot.settings, dir){
  ccFilter = gateCells(flowData[[1]][[1]],parameters = channels[1], minVals = breaks$cut[1], maxVals = breaks$cut[2])
  for(i in 1:(length(flowData)-1)){
    flowData[[i]] = Subset(flowData[[i]], filter(flowData[[i]], ccFilter))
  }
  lineplotData = readRFlowData(flowData = flowData, channels = channels,dose.levels= dose.levels, sample = sample, date = date)
  out = brduGates(rawData = lineplotData, xp = breaks$xp, yp = breaks$yp, channel_x = channels[1], channel_y = channels[2])
  gate = out[[2]]
  sumData = out[[1]]

  plots= list()
  x = 1
  len = length(flowData)
  timepoints = flowData[c(-len)]
  if(is.numeric(flowData$timepoints)){
    times = paste(flowData$timepoints, 'hrs')
  } else {
    times = flowData$timepoints
  }
  for(i in 1:length(timepoints)){
    for(j in c(1:length(timepoints[[i]]))){
      name = paste(dose.levels[j], times[i],sep = ' - ')
      plots[[x]] = xyplot(`B1-H` ~ `V1-H` , data = timepoints[[i]][[j]], smooth = FALSE, filter = gate,
                          main = name, scales = list(tick.number = 6, alternating = 1), par.settings = plot.settings,
                          stats = T, digits = 1, abs = F, pos = c(0.50,0.3), ylab  = ylab, xlab = xlab,
                          xlim = c(0, breaks$cut[2]), ylim = c(0,4.5))
      x = x + 1
    }
  }
  sumData.gather = gather(sumData, 'Category','Percentage', g0, s, g2, factor_key = T)
  sumData.gather$Category = factor(sumData.gather$Category, levels = c('g2',"s",'g0'))
  sumData.gather$Type = 'BrdU'
  summaryData = sumData.gather
  save(summaryData, file = file.path(dir,paste0(sample, 'SummaryData.rData')))

  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                strip.text = element_text(size = 12, face = "bold", hjust = 0.5),
                axis.text = element_text(size = 12, face = "bold", hjust = 0.5),
                axis.title = element_text(size = 13, face = "bold", hjust = 0.5),
                legend.text = element_text(size = 13, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 13, face = "bold", hjust = 0.5),
                panel.grid.major.y  = element_line(size = 1.3))
  conditionColors =  c('firebrick1','royalblue','orange') #wes_palette(n=3, name="Moonrise2")#
  names(conditionColors) = c('g2','s','g0')
  barplot = ggplot(sumData.gather, aes(fill = Category, x = Condition, y = Percentage)) +
    geom_bar(position="fill", stat="identity") +
    # stat_count(y = Percentage, geom = 'text') +
    xlab('Condition') + ylab('Percentage') +
    scale_y_continuous(breaks = seq(0,1, by = .1)) +
    ggtitle(paste(sample,date, 'BrdU Cell Cycle', sep = ' '))+
    scale_fill_manual(values = conditionColors, name = 'Cell Cycle Divisions', labels = c('g0' = 'G0/G1', 's' = 'S Phase', 'g2' = 'G2/M'))+ theme
  return(list(ccPlot = plots, barplot = barplot))
}
