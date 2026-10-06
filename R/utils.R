

.packageData <- function(filename, stringsAsFactors = FALSE) {
  
  # system.file() rather than find.package(): with devtools::load_all() the
  # package is its source directory, where the files sit in inst/extdata.
  # system.file() resolves both, find.package() only the installed layout.
  path <- system.file("extdata", filename, package = "umbra")
  
  if (!nzchar(path)) {
    stop(
      sprintf("Datei '%s' fehlt im Verzeichnis 'extdata' des Pakets.", filename),
      call. = FALSE
    )
  }
  
  data.frame(readxl::read_xlsx(path), stringsAsFactors = stringsAsFactors)
  
}



