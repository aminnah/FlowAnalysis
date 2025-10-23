#'editLegend
#'
#'Edit plotly plot legends
#'
#'
#'
#'@export


editLegend = function(plot, labels, data, plotDose){
  p = 1
  if(plotDose){
    p = 4
    k = 3
    if(length(unique(data$Timepoint)) > 1) k = 6
    for(i in 1:k) plot[["x"]][["data"]][[i]][["showlegend"]] = F
  }
  for(i in p:length(plot[['x']][['data']])){
    name = plot[["x"]][["data"]][[i]][['name']]
    name = strsplit(name, split = ',')[[1]][1:2]
    name[1] = strsplit(name[1], split = "\\(" )[[1]][2]
    cat = name[2]
    name[2] = labels[name[2]]
    plot[["x"]][["data"]][[i]][['name']] = paste(name, collapse = ' - ')
    plot[["x"]][["data"]][[i]][["legendgroup"]] = name[1]
    for(j in 1:length(plot[['x']][['data']][[i]][['text']])){
      text = plot[['x']][['data']][[i]][['text']][[j]]
      text = strsplit(text, split = '<br />')[[1]]
      text[2] = paste('Condition:', paste(name, collapse = ' - '))
      if(!plotDose){
        timepoint = strsplit(text[1], split = ': ')[[1]][2]
        count = subset(data, Condition == name[1] & Category == cat & Timepoint == timepoint)
      } else{
        count = subset(data, Condition == name[1] & Category == cat)
        timepoint = NULL
      }
      if(nrow(count) != 1) warning(paste(name[1], cat, 'at',timepoint, 'has more than one value'), immediate. = T)
      text[3] = paste('Count:', round(count$Cells*(count$Percentage/100)), 'cells')
      rm = 4
      if(plotDose) rm = c(4,5)
      text = text[-c(rm)]
      plot[['x']][['data']][[i]][['text']][[j]] = paste(text, collapse = '<br />')
    }
  }
  plot[["x"]][["layout"]][["annotations"]][[2]][["x"]] = -0.025
  plot[["x"]][["layout"]][["annotations"]][[1]][["y"]] = -0.025
  l = length(plot[["x"]][["layout"]][["annotations"]])
  for(k in 3:l){
    if(k != l){
      title = plot[["x"]][["layout"]][["annotations"]][[k]][["text"]]
      plot[["x"]][["layout"]][["annotations"]][[k]][["text"]] = paste0('<b>',title,'<b>')
    } else if(k == l){
      plot[["x"]][["layout"]][["annotations"]][[k]][["text"]] = '<b>Conditions<b>'
    }
  }
  return(plot)
}
