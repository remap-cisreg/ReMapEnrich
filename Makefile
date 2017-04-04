## This is a custom build Makefile for the roken R package.
## First we build the documentation using the devtools package
echo "====> Build documentation"
Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"
##Second we build the vignettes using the devtools package
echo "====> Build vignettes"
Rscript -e "devtools::build_vignettes()"
## Third we build the roken package using R CMD
echo "====> Build source"
R CMD INSTALL --no-multiarch --with-keep.source ../roken
