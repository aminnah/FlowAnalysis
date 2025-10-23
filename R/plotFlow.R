#'plotFlow
#'
#'Get Flow Cytometry plots
#'
#'
#'@export


plotFlow = function(finalCells, dose.levels, plot.settings, cutoffs, xlab = 'Annexin V - FITC', ylab = 'DNA - DAPI'){

  LiveDeadFilter = quadGate(filterId = 'LiveDead', 'FL1-H' = cutoffs$annexin, 'FL18-H' = cutoffs$dapi)
  results = filter(finalCells[[1]][c(1)], LiveDeadFilter)
  updatedFilter = updateQuad(cutoffs$annexin, cutoffs$dapi, 'FL1-H', 'FL18-H', results[[1]]@filterDetails[[1]]$populations, xs = 6, ys = 6)

  plots = list()
  x = 1
  len = length(finalCells)
  timepoints = finalCells[c(-len)]
  if(is.numeric(finalCells$timepoints)){
    times = paste(finalCells$timepoints, 'hrs')
  } else {
    times = finalCells$timepoints
  }
  for(i in 1:length(timepoints)){
    for(j in c(1:length(timepoints[[i]]))){
      name = paste(dose.levels[j], times[i],sep = ' - ')
      plots[[x]] = xyplot(`FL18-H` ~ `FL1-H` , data = timepoints[[i]][[j]], filter = updatedFilter, smooth = FALSE,
                          main = name, scales = list(tick.number = 6, alternating = 1), par.settings = plot.settings,
                          stats = T, digits = 1, abs = F, pos = c(0.30,0.5), xlab  = xlab, ylab = ylab)
      x = x + 1
    }
  }
  return(plots)
}
