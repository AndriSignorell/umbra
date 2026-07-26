

.packageData <- function(filename, stringsAsFactors = FALSE) {
  
  data.frame(readxl::read_xlsx(
    gettextf("%s/%s", 
             file.path(find.package("umbra"), "extdata"), 
             filename)), stringsAsFactors = stringsAsFactors)
  
}



