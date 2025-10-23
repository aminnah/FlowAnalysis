#'gateSingleCells
#'
#'
#'Gate single cells from doublets
#'
#'@export


gateSingleCells = function(parameters = c('FSC-H', 'FSC-A'), points){
  mat <- matrix(points, ncol=2, nrow = 4)
  colnames(mat) =  parameters
  singleCells.gate = polygonGate(filterId = 'Single Cells', .gate = mat)
  return(singleCells.gate)
}
