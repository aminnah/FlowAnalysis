#'readFlowData
#'
#'
#'Read fcs files
#'
#'
#'@export

readFlowData = function(files = choose.files(filters = matrix(nrow = 1, data = c('FCS files', '*.fcs'))),dir,
                        cutoffs = NULL, sample, date, plot.settings, analyzeCellCycle = F, time.name = 'Time', channels = NULL){

  data = lapply(files, read.FCS, emptyValue = FALSE, transformation = FALSE )
  names(data) = basename(files)
  data = as(data, 'flowSet')
  cols <- BiocGenerics::colnames(data)
  time.index = which(cols == 'Time')
  if(!cols[time.index] == 'Time') {
    cols[time.index] = 'Time'
    BiocGenerics::`colnames<-`(data, cols)
  }
  if(is.null(channels)){
    data = cleanFCS(data, dirname(files[1]))
  } else {
    inx = grep(paste0(gsub('-H','-',channels),collapse = '|'),cols)
    data = cleanFCS(data, dirname(files[1]), vectMarkers = inx)
  }

  cols = cols[-which(cols == 'Time' | cols == 'GoodVsBad')]
  if(analyzeCellCycle){
    cols = cols[-grep('FL18',cols)]
  }
  logicleTrans = logicleTransform(transformationId = 'logicle-transform')
  newData = flowCore::transform(data, transformList(cols,logicleTrans))

  flowclean.filter = rectangleGate(filterId = 'flowClean', .gate = matrix(c(0,9999), ncol = 1, dimnames = list(c('min', 'max'), 'GoodVsBad')))
  cleanCells = Subset(newData, filter(newData, flowclean.filter))
  #cleanCells.plot = xyplot(`SSC-A` ~ `FSC-A` , data = cleanCells, smooth = FALSE, main = 'Clean Cells')

  #debris gate
  if(is.null(cutoffs)){
    cutoffs = list(debris = 4.2, singlets = c(c(4.2,3.75,5.15,5.5), c(4,4.2,6.25,5.75)), annexin = 3, dapi = 3.6)
  } else if(analyzeCellCycle){
    cutoffs = cutoffs$cellcycle
  } else {
    index = pmatch(sample, names(cutoffs))
    if(is.na(index) || length(index) > 1){
      cutoffs = cutoffs$generic
      warning('Sample matching was unsucessful with gating points', immediate. = T)
    } else {
      cutoffs = cutoffs[[index]]
    }
  }

  cellFilter = gateCells(cleanCells[[1]],parameters = c('FSC-A'), minVals = cutoffs$debris)
  Cells = Subset(cleanCells, filter(cleanCells, cellFilter))
  debriscells = xyplot(`SSC-A` ~ `FSC-A` , data = cleanCells, filter=cellFilter, smooth = FALSE, stats = T, digits = 3, pos = c(0.5,0.85),
         par.settings = plot.settings, abs = F, main = 'Cell-Debris Cutoff')

  #gate single Cells
  singleCellsFilter = gateSingleCells(parameters = c('FSC-H', 'FSC-A'), points = cutoffs$singlets)
  singleCells = Subset(Cells, filter(Cells, singleCellsFilter))
  singlets = xyplot(`FSC-A` ~ `FSC-H` , data = Cells, filter = singleCellsFilter, smooth = FALSE,xlim = c(2.8,7), ylim = c(2.8,7), stats = T, digits = 3,
         par.settings = plot.settings, abs = F, pos = c(0.9,0.5), main = 'Single Cells Gating')
  finalCells = xyplot(`SSC-A` ~ `FSC-A` , data = singleCells, smooth = FALSE, main = 'FSC-A v SSC-A, Single Cells', par.settings = plot.settings)

  data = list(plots = list(debriscells = debriscells, singlets = singlets, finalCells = finalCells), Cells = singleCells, cutoffs = cutoffs)

  #plot annexin and dapi, fix to be generic in future
  if(!analyzeCellCycle){
    if(length(channels)!=2) stop("You need to specify two channels for viability.")
    LiveDeadFilter = quadGate(filterId = 'LiveDead', 'FL1-H' = cutoffs$annexin, 'FL5-H' = cutoffs$dapi)
    viabilityplot = xyplot(`FL5-H` ~ `FL1-H` , data = singleCells, filter = LiveDeadFilter,smooth = FALSE, main = 'Viability Summary', digits = 3, pos = c(0.15,0.4))

    png(filename = file.path(dir,paste(sample, date, 'Viability Summary.png')), width = 1400, height = 700)
    print(viabilityplot)
    dev.off()
    data$plots$viability = viabilityplot
  }

  #png(filename = paste(paste0(dir,'/'),sample, date, 'CleanCells.png'), width = 1400, height = 700)
  #print(cleanCells.plot)
  #dev.off()
  png(filename = file.path(dir,paste(sample, date, 'DebrisGate.png')), width = 1400, height = 700)
  print(debriscells)
  dev.off()
  png(filename = file.path(dir,paste(sample, date, 'SingleCells.png')), width = 1400, height = 700)
  print(singlets)
  dev.off()
  png(filename = file.path(dir,paste(sample, date, 'FinalCells.png')), width = 1400, height = 700)
  print(finalCells)
  dev.off()
  return(data)
}



