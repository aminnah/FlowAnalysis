#'brduGates
#'
#'
#'Get Gates for BrdU gating
#'
#'@export

brduGates = function(xp, yp, channel_x,channel_y){
  x0 = c(xp[1],yp[1])
  xs = c(xp[2]-xp[1], xp[4]-xp[1], xp[4]-xp[3])
  ys = c(yp[3]-yp[1], yp[4]-yp[3], yp[3]-yp[2])

  x0 = c(x0, c(x0[1],x0[2]+ys[1]), c(x0[1]+xs[2]-xs[3],x0[2]+ys[1]-ys[3]) )

  g0.num = matrix(getBox(x0[c(1,2)], xs[1], ys[1]), ncol = 2, nrow = 4, byrow = T)
  colnames(g0.num) = c(channel_x,channel_y)
  g0 = polygonGate(filterId = 'G0/G1', .gate = g0.num)

  s.num = matrix(getBox(x0[c(3,4)], xs[2], ys[2]), ncol = 2, nrow = 4, byrow = T)
  colnames(s.num) = c(channel_x,channel_y)
  s = polygonGate(filterId = 'S', .gate = s.num)

  g2.num = matrix(getBox(x0[c(5,6)], xs[3], ys[3]), ncol = 2, nrow = 4, byrow = T)
  colnames(g2.num) = c(channel_x,channel_y)
  g2 = polygonGate(filterId = 'G2/M', .gate = g2.num)

  result = list(g0,s,g2)
  names(result) = c('G0/G1','S','G2/M')
  result = filters(result)
  return(result)
}
