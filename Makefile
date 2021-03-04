## manuscript : Generates the manuscript pdf file
manuscript: manuscript/manuscript.pdf

manuscript/manuscript.pdf: manuscript/manuscript.Rmd manuscript/bibliography.bib manuscript/nlm.csl code/02_explore.R data/mechanical_load_data.rda
	Rscript -e 'rmarkdown::render("$<")'

## figures    : Generates all figures
figures: figures/fig1.tiff figures/fig2.tiff figures/fig3.tiff figures/figS1.tiff figures/figS2.tiff figures/figS3.tiff figures/figS4.tiff

figures/fig1.tiff: data/mechanical_load_data.rda figures/fig1.R
	R CMD BATCH figures/fig1.R

figures/fig2.tiff: data/mechanical_load_data.rda figures/fig2.R
	R CMD BATCH figures/fig2.R

figures/fig3.tiff: output/loocv_data.rda figures/fig3.R
	R CMD BATCH figures/fig3.R

figures/figS1.tiff: data/mechanical_load_data.rda figures/figS1.R
	R CMD BATCH figures/figS1.R

figures/figS2.tiff: data/mechanical_load_data.rda figures/figS2.R
	R CMD BATCH figures/figS2.R

figures/figS3.tiff: output/loocv_data.rda figures/figS3.R
	R CMD BATCH figures/figS3.R

figures/figS4.tiff: output/loocv_data.rda figures/figS4.R
	R CMD BATCH figures/figS4.R

## output     : Generates all output
output: output/loocv_data.rda output/prediction_models.rda output/sub_analyses_accuracy.rda

output/loocv_data.rda: data/mechanical_load_data.rda code/03_predict.R
	R CMD BATCH code/03_predict.R

output/prediction_models.rda: data/mechanical_load_data.rda code/03_predict.R
	R CMD BATCH code/03_predict.R

output/sub_analyses_accuracy.rda: data/mechanical_load_data.rda output/loocv_data.rda
	R CMD BATCH code/04_sub_analyses.R

## data       : Processes raw data
data: data/mechanical_load_data.rda

data/mechanical_load_data.rda: data/anthropometric_data.csv data/GRF_ACC_data_all.xlsx data/max_rates_IMU_running.csv code/01_tidy_data.R
	R CMD BATCH code/01_tidy_data.R

## install    : Installs all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean      : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata manuscript/*.log

## cleanall   : Removes auto-generated files, including processed data, figures and the manuscript pdf
cleanall:
	\rm -f *.Rout .Rdata data/*.rda output/* figures/*.tiff manuscript/manuscript.pdf

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
