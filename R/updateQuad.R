#'updateQuad
#'
#'Update Rectangular gate to a vaibility gate
#'
#'
#'@export


updateQuad = function(x,y,channel_x,channel_y, population, xs = NULL, ys = NULL){
  if(is.null(xs)) xs = 4
  if(is.null(ys)) ys = 4
  indices = list(c(x,y), c(x-xs,y),c(x,y-ys), c(x-xs,y-ys))
  result = list()
  for(i in 2:4){
    xo = indices[[i]][1]
    yo = indices[[i]][2]
    if(i == 2){
      mat = matrix(c(xo,yo, xo + (2*xs), yo, xo+ (2*xs), yo+ys, xo, yo + ys), byrow = T, ncol = 2)
      colnames(mat) = c(channel_x, channel_y)
      filter = polygonGate(filterId = population[i-1],.gate = mat)
      result[[population[i-1]]] = filter
    } else {
      mat = matrix(c(xo,yo, xo + xs, yo, xo+xs, yo+ys, xo, yo + ys), byrow = T, ncol = 2)
      colnames(mat) = c(channel_x, channel_y)
      filter = polygonGate(filterId = population[i],.gate = mat)
      result[[population[i]]] = filter
    }
  }
  result = filters(result)
  return(result)
}
