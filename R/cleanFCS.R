#'cleanFCS
#'
#'Identify and gate cytometry points with bad flow, see flowClean::clean
#'
#'
#'@export


cleanFCS = function(dat, folder, vectMarkers = NULL){
  folder = file.path(folder,'flowClean')
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  cols = BiocGenerics::colnames(dat)
  if (is.null(vectMarkers)) vectMarkers = cols[-c(grep('Time',cols),grep('FSC',cols), grep('SSC',cols))]
  newData = fsApply(dat, function(x){
    clean(x, vectMarkers = vectMarkers,
          filePrefixWithDir = paste0(folder,'/',basename(strsplit(keyword(x,'FILENAME')$FILENAME, split = '.fcs')[[1]]),'_clean'),
          ext = '.fcs', diagnostic = T, nCellCutoff = 500)})
  return(newData)
}
