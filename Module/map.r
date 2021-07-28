convertMap <- function(url) {
  
  library(purrr)
  tmpfile <- tempfile(fileext = ".json")
  download.file("https://code.highcharts.com/mapdata/countries/lu/lu-all.js", tmpfile)
  
  worldgeojson <- readLines(tmpfile)
  worldgeojson <- gsub(".* = ", "", worldgeojson)
  worldgeojson <- jsonlite::fromJSON(worldgeojson, simplifyVector = FALSE)
  
  worldgeojson$features <- map(worldgeojson$features, function(x) {
    # x <- worldgeojson$features[[10]]
    x$properties <- x$properties[!grepl("hc", names(x$properties))]
    names(x$properties) <- gsub("-", "", names(x$properties))
    names(x$properties) <- gsub("isoa", "iso", names(x$properties))
    x$properties <- map(x$properties, function(x) {
      ifelse(is.null(x), NA, iconv(x, to = "UTF-8"))
    })
    x
  })
  return(worldgeojson) #vielleicht redundant
}
