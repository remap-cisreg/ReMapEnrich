all:
	@echo Build all
	@echo "====> Build documentation"
	@Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))"
	@echo "====> Build vignettes"
	@Rscript -e "devtools::build_vignettes()"
	@echo ====> "Build source"
	@R CMD INSTALL --no-multiarch --with-keep.source ../roken

clean:
	@echo Clean all
	@Rscript -e "unlink(find.package("roken"), recursive = TRUE)"
	@Rscript -e "devtools::clean_vignettes()"
	@Rscript -e "unlink("man, recursive = TRUE)"
	@Rscript -e "unlink("NAMESPACE")"
