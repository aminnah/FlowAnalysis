#'runLinePlots
#'
#'Main plotly line plot function
#'
#'
#'@export


runLinePlots = function(finalCells, plotDose = FALSE, dose.levels, channels  = c('B1-H','V1-H'),
                        time0 = 0, sample, date, cutoffs, selectedTimepoints = NA, dir, breaks.via = c(2.5, 3.1), diff.cells = F){
  #line plots--------------
  if(plotDose || length(finalCells$timepoints) > 1){
    lineplotData = readRFlowData(flowData = finalCells, channels = channels, dose.levels = dose.levels, time0 = time0, sample = sample, date = date, type = 'VIA', diff.cells = diff.cells)
    conditionColors = c('gray', rev(wes_palette("Zissou1", length(dose.levels)-1, type = "continuous")))
    names(conditionColors) = dose.levels
    time.levels = levels(lineplotData$Timepoint)
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = 'black'),
                  strip.text = element_text(size = 12, face = "bold", hjust = 0.5),
                  axis.text.x = element_text(angle = 45),
                  axis.text = element_text(size = 12, face = "bold", hjust = 0.5, color = 'black'),
                  axis.title = element_text(size = 14, face = "bold", hjust = 0.5, color = 'black'),
                  legend.text = element_text(size = 14, face = "bold", hjust = 0.5, color = 'black'),
                  legend.title = element_text(size = 14, face = "bold", hjust = 0.5, color = 'black'),
                  panel.grid.major.y  = element_line(size = 1.3))
    sumData.viability = summarizeViabilityData(subset(lineplotData, Type == 'VIA'), dose.levels, time.levels, channels, breaks.via = breaks.via)
    write.xlsx(sumData.viability, file = file.path(dir,paste(sample, date, 'SummaryData.xlsx',sep = '_')), overwrite = T)
    viabilityData.gather = gather(sumData.viability, 'Category','Percentage', annexin.neg, annexin.pos, dna.pos, factor_key = T)
    newViability = viabilityData.gather
    viabilityData.gather$ConditionCategory = paste(viabilityData.gather$Condition, viabilityData.gather$Category, sep = ' - ')
    viability.labels =  c(annexin.neg = 'Annexin Negative', annexin.pos = 'Annexin Positive', dna.pos = 'Dead Cells')


    newViability$Category = factor(newViability$Category, levels = c('dna.pos','annexin.pos', 'annexin.neg'))
    summaryData = newViability
    theme2 = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  strip.text = element_text(size = 12, face = "bold", hjust = 0.5),
                  axis.text = element_text(size = 12, face = "bold", hjust = 0.5),
                  axis.text.x = element_text(angle = 45),
                  axis.title = element_text(size = 13, face = "bold", hjust = 0.5),
                  legend.text = element_text(size = 13, face = "bold", hjust = 0.5),
                  legend.title = element_text(size = 13, face = "bold", hjust = 0.5),
                  panel.grid.major.y  = element_line(size = 1.3))
    conditionColors =  c('firebrick1','royalblue','orange')
    names(conditionColors) = c('annexin.neg','annexin.pos', 'dna.pos')

    if(length(unique(newViability$Timepoint)) > 1){
      viability.barplot2 = ggplot(newViability, aes(fill = Category, x = Timepoint, y = Percentage)) +
        geom_bar(position="fill", stat="identity") +
        facet_grid(~Condition) + xlab('Condition') + ylab('Percentage') +
        scale_y_continuous(breaks = seq(0,1, by = .1)) +
        ggtitle(paste(sample,date, 'Viability Barplot', sep = ' '))+
        scale_fill_manual(values = conditionColors, name = 'Cell Cycle Divisions',
                        labels = c('annexin.neg' = 'Live Cells', 'annexin.pos' = 'Apoptotic Cells', 'dna.pos' = 'Necrotic Cells'))+ theme2
      png(filename = file.path(dir,paste(sample, date, 'Viability Barplot by Time.png')), width = 1400, height = 700)
      print(viability.barplot2)
      dev.off()
    } else {
      viability.barplot2 = NULL
    }
      viability.barplot = ggplot(newViability, aes(fill = Category, x = Condition, y = Percentage)) +
        geom_bar(position="fill", stat="identity") +
        facet_grid(~Timepoint) + xlab('Condition') + ylab('Percentage') +
        scale_y_continuous(breaks = seq(0,1, by = .1)) +
        ggtitle(paste(sample,date, 'Viability Barplot', sep = ' '))+
        scale_fill_manual(values = conditionColors, name = 'Cell Cycle Divisions',
                          labels = c('annexin.neg' = 'Live Cells', 'annexin.pos' = 'Apoptotic Cells', 'dna.pos' = 'Necrotic Cells'))+ theme2

    png(filename = file.path(dir,paste(sample, date, 'Viability Barplot by Dose.png')), width = 1400, height = 700)
    print(viability.barplot)
    dev.off()
    save(viability.barplot,viability.barplot2, file = file.path(dir,paste(sample, date, 'Viability Barplot.rData',sep = '_')))

    if(length(finalCells$timepoints) > 1){
      viability.ggplot.byTime = ggplot(viabilityData.gather, aes(x = Timepoint))+
        geom_line(aes(y = Percentage, color = Condition, linetype = Category, group = ConditionCategory), size = 1.2)+
        geom_point(aes(y = Percentage,color = Condition, group = ConditionCategory, shape = Category), size = 3)+
        facet_grid(~Category, labeller = labeller(Category = viability.labels))+
        xlab('Time') +
        scale_y_continuous('Percentage (%)',breaks = seq(0, 100, by = 10), limits = c(0,100)) +
        ggtitle(paste('Change in Viability Over Time. Sample:', sample))+
        scale_color_manual(name = 'Condition', values = conditionColors)+
        scale_linetype_manual(name = 'Category', labels = viability.labels, values = c('solid', 'dashed','twodash')) +
        scale_shape_manual(name = 'Category', labels = viability.labels, values = c(15,16,17))+ theme
      viability.plotly = ggplotly(viability.ggplot.byTime)
      viability.plotly = editLegend(viability.plotly, viability.labels, viabilityData.gather, plotDose = FALSE)
      saveWidget(viability.plotly, file = paste(dir,"/",sample,date,'_Viability Plot by Time', ".html", sep = ""))
    }
    if(!any(is.na(selectedTimepoints))){
      if(!diff.cells) selectedTimepoints = paste(selectedTimepoints, 'hrs')
      if(any(!selectedTimepoints %in% time.levels)) stop(paste(selectedTimepoints[which(!selectedTimepoints %in% time.levels)], collapse = ', '),' not a timepoint for line plots.')
      viabilityData.gather = subset(viabilityData.gather, Timepoint %in% selectedTimepoints)
      viabilityData.gather$TimepointCategory = paste(viabilityData.gather$Timepoint, viabilityData.gather$Category, sep = ' - ')
    }
    if(plotDose && length(unique(viabilityData.gather$Timepoint)) > 1){
      viability.ggplot.byDose = ggplot(viabilityData.gather, aes(x = Condition))+
        geom_line(aes(y = Percentage, linetype = Category, color = Timepoint, group = TimepointCategory), size = 1.6)+
        geom_point(aes(y = Percentage, group = ConditionCategory, shape = Category, size = Condition))+
        facet_grid(~Category, labeller = labeller(Category = viability.labels))+
        xlab('Time') +
        scale_y_continuous('Percentage (%)',breaks = seq(0, 100, by = 10), limits = c(0,100)) +
        ggtitle(paste('Change in Viability Condition. Sample:', sample))+
        scale_color_manual(name = 'Timepoints', values = c(wes_palette("Zissou1", length(selectedTimepoints), type = "continuous")))+
        scale_linetype_manual(name = 'Category', labels = viability.labels, values = c('solid', 'dashed','twodash')) +
        scale_shape_manual(name = 'Category', labels = viability.labels, values = c(15,16,17))+
        scale_size_manual(name = 'Condition', labels = unique(viabilityData.gather$Condition), values =seq(2,length.out = length(unique(viabilityData.gather$Condition)), by = .4))+theme
      viability.plotly = ggplotly(viability.ggplot.byDose)
      viability.plotly = editLegendDouble(viability.plotly, viability.labels, viabilityData.gather, plotDose = plotDose)
      saveWidget(viability.plotly, file = paste(dir,"/",sample,date,'_Viability Plot by Dose', ".html", sep = ""))
    } else if(plotDose) {
      viability.ggplot.byDose = ggplot(viabilityData.gather, aes(x = Condition))+
        geom_line(aes(y = Percentage, linetype = Category, group = Category), size = 1.5, color= 'darkred')+
        geom_point(aes(y = Percentage,color = Condition, group = ConditionCategory, shape = Category), size = 5)+
        facet_grid(~Category, labeller = labeller(Category = viability.labels))+
        xlab('Conditions') +
        scale_y_continuous('Percentage (%)',breaks = seq(0, 100, by = 10), limits = c(0,100)) +
        ggtitle(paste('Change in Viability Condition. Sample:', sample, unique(viabilityData.gather$Timepoint)))+
        scale_color_manual(name = 'Condition', values = conditionColors)+
        scale_linetype_manual(name = 'Category', labels = viability.labels, values = c('solid', 'dashed','twodash')) +
        scale_shape_manual(name = 'Category', labels = viability.labels, values = c(15,16,17))+ theme
      viability.plotly = ggplotly(viability.ggplot.byDose)
      viability.plotly = editLegend(viability.plotly, viability.labels, viabilityData.gather, plotDose = plotDose)
      saveWidget(viability.plotly, file = paste(dir,"/",sample,date,'_Viability Plot by Dose', ".html", sep = ""))
    }
  }
  return(viabilityData.gather)
}
