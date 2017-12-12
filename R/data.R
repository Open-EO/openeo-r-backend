ls7.filepaths = list.files(path="data/landsat7/", pattern='*.tif$')


parseDateFromLS7FileName = function (filename) {
  doyFormat="%Y%j"
  dayOfYearString = substring(filename,10,16)
  return(strptime(dayOfYearString,format=doyFormat))
}


landsat7 <- lapply(ls7.filepaths, function(file) {
  list(date=parseDateFromLS7FileName(file),source=file)
})

openeo$data <- list(landsat7_ndvi = Product$new("landsat7_ndvi", "Landsat7 NDVI calculation", "Marius Appel"))
