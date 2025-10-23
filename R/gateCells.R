#'gateCells
#'
#'Gate out debris from flow cells
#'
#'@export


gateCells = function(data, parameters = c('FSC-A'), minVals, maxVals = NULL){
  if(is.null(maxVals)) maxVals = max(data@exprs[,parameters[1]])
  mat = matrix(c(minVals[1], maxVals[1]), ncol=1, dimnames=list(c("min", "max"), parameters))
  cell.gate = rectangleGate(filterId = 'Non-Debris', .gate = mat)
  return(cell.gate)
}
