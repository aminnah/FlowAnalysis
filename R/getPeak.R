#'getPeak
#'
#'Get Peak Value within interval
#'
#'@export



getPeak = function(values, bins = 200, range = NULL, g2.mul = 2){
  if(is.null(range)) range = c(min(values), max(values))
  plot.data = invisible(hist(values, breaks = bins))
  counts = plot.data$counts
  breaks = plot.data$breaks
  #names(counts) <- breaks[-length(breaks)]
  valid = between(breaks,range[1],range[2])
  max.val = which.max(counts[valid])
  max.val = c(breaks[valid][max.val],counts[valid][max.val])
  plot = ggplot(as.data.frame(values)) +
    geom_histogram(aes(x = values),bins = bins,col="grey51",na.rm = T, alpha = 0.3) +
    geom_vline(xintercept = c(max.val[1], max.val[1]*g2.mul) ,lty = 6)
  print(plot)
  return(max.val)
}
