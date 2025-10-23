#'saveFlowCells
#'
#'
#'Add time zero to flow cells and return rData flow files
#'
#'@export


saveFlowCells = function(Cells, flow.dir, dose.levels, ctrl, time0 = 0){
  names = names(Cells@frames)
  x = unlist(strsplit(names, split = '_'))
  x = unlist(strsplit(x[grep('.fcs',x)], split = '.fcs'))
  p = sort(unique(na.omit(as.numeric(unlist(strsplit(x, split = 'T'))))))
  if(length(p) == 0){
    p = sort(unique(na.omit(unlist(strsplit(x, split = 'T')))))
    p = p[-1]
  }
  x = p

  finalCells = list()
  for(i in x){
    matched = grep(paste0('T',i),names)
    if(length(matched) == length(dose.levels)){
      finalCells[[paste0('T',i)]] = Cells[c(paste0(dose.levels, '_T',i,'.fcs'))]
    } else if(length(matched) == 1 && i %in% time0){
      temp = Cells[[names(Cells@frames)[matched]]]
      temp = lapply(1:length(dose.levels), function(x) temp)
      names(temp) = paste0(dose.levels, '_T',i,'.fcs')
      temp = as(temp, 'flowSet')
      finalCells[[paste0('T',i)]] = temp
    } else {
      stop('Number of flow Conditions does not match number of files. Check ',names[-matched])
    }
  }
  finalCells[['timepoints']] = x
  return(finalCells)
}
