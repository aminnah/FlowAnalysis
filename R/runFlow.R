#'runFlow
#'
#'Main Flow Cytometry functions
#'
#'
#'@export


runFlow = function(dir, dose.levels, cutoffs = NULL, sample, date, plot.settings, ctrl, time0 = 0, analyzeCellCycle = F, time.name = 'Time',
                   channels = NULL){
  files = list.files(dir, pattern = '.fcs', full.names = T)
  plots = list()
  if(length(files) != 0){
    names = strsplit(basename(files), split = '_')
    names = unique(unlist(lapply(names, function(x){x[1]})))
    comp = match(names, dose.levels)
    if(any(is.na(comp))){
      stop(paste(names[which(is.na(comp))], collapse = ', '), " do not match the flow dose conditions")
    }
    data = readFlowData(files = files, dir = dir, cutoffs = cutoffs,
                             sample = sample, date = date, plot.settings = plot.settings, analyzeCellCycle = analyzeCellCycle, time.name = time.name,
                        channels = channels)
    plots[['flowAnalysis']] = data$plots
    Cells = data$Cells
    cutoffs = data$cutoffs
    finalCells = saveFlowCells(Cells, dir, dose.levels, ctrl, time0 = time0)
    save(finalCells, file = file.path(dir,paste(sample, date, 'VIA.rData',sep = '_')))
    flowplot.settings = list(gate.text = list(lineHeight = 1, background = list(fill = 'transparent'), cex = 1.4), fontsize = list(text = 10),
                             axis.text = list(col = 'black'))
    if(!analyzeCellCycle){
      plots[['viabilityPlots']] = plotFlow(finalCells,dose.levels = dose.levels, plot.settings = flowplot.settings, cutoffs = cutoffs)
    }

    #save(plots,file = file.path(dir,paste(sample, date, 'VIA_plots.rData',sep = '_')))
    return(list(plots = plots, finalCells = finalCells, cutoffs = cutoffs))
  } else {
    warning('No fcs files found in Flow Cytometry folder.', immediate. = T)
    return(list(plots = plots, finalCells = list(), cutoffs = list()))
  }
}
