#'editLegendDouble
#'
#'Edit plotly plot legends
#'
#'
#'
#'@export


editLegendDouble = function(plot, labels, data, plotDose){
  for(i in 1:length(plot[['x']][['data']])){
    name = plot[["x"]][["data"]][[i]][['name']]
    if(i <= (3*length(unique(data$Timepoint)))){
      name = strsplit(name, split = ',')[[1]][1:2]
      name[1] = strsplit(name[1], split = "\\(" )[[1]][2]
      cat = name[2]
      name[2] = labels[name[2]]
      plot[["x"]][["data"]][[i]][['name']] = paste(name, collapse = ' - ')
      plot[["x"]][["data"]][[i]][["legendgroup"]] = name[1]
    } else{
      name = strsplit(name, split = ',')[[1]][1:3]
      name[1] = strsplit(name[1], split = "\\(" )[[1]][2]
      cat = name[1]
      plot[["x"]][["data"]][[i]][["showlegend"]] = F
      for(j in 1:length(plot[['x']][['data']][[i]][['text']])){
        text = plot[['x']][['data']][[i]][['text']][[j]]
        text = strsplit(text, split = '<br />')[[1]]
        text[2] = text[4]
        count = subset(data, Condition == name[2] & Category == cat & Timepoint == unique(data$Timepoint)[j])
        #if(nrow(count) != 1) warning(paste(name[1], cat, 'at',timepoint, 'has more than one value'), immediate. = T)
        text[3] = paste('Count:', round(count$Cells*(count$Percentage/100)), 'cells')
        text[4] = paste('Timepoint:', unique(data$Timepoint)[j])
        text = text[-c(5)]
        plot[['x']][['data']][[i]][['text']][[j]] = paste(text, collapse = '<br />')
        #plot[["x"]][["data"]][[i]][["legendgroup"]] = unique(data$Timepoint)[j]
      }
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
      plot[["x"]][["layout"]][["annotations"]][[k]][["text"]] = '<b>Timepoints<b>'
    }
  }
  return(plot)
}
