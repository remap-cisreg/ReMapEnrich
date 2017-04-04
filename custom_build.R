### This is a custom build for the Roken package.
devtools::clean_dll()
devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))
devtools::build_vignettes()
devtools::install()
