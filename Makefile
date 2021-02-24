## figures  : Generates all figures
figures: figures/scatterplot_GRF.tiff figures/scatterplot_LR.tiff figures/bland-altman_GRF.tiff figures/bland-altman_LR.tiff

figures/scatterplot_GRF.tiff figures/scatterplot_LR.tiff: data/mechanical_load_data.rda figures/scatterplot.R
	R CMD BATCH figures/scatterplot.R

figures/bland-altman_GRF.tiff figures/bland-altman_LR.tiff: output/loocv_data.rda figures/bland-altman.R
	R CMD BATCH figures/bland-altman.R

## output   : Generates all output
output: output/loocv_data.rda output/prediction_models.rda output/sub_analyses_accuracy.rda

output/loocv_data.rda: data/mechanical_load_data.rda code/03_predict.R
	R CMD BATCH code/03_predict.R

output/prediction_models.rda: data/mechanical_load_data.rda code/03_predict.R
	R CMD BATCH code/03_predict.R

output/sub_analyses_accuracy.rda: data/mechanical_load_data.rda output/loocv_data.rda
	R CMD BATCH code/04_sub_analyses.R

## data     : Processes raw data
data: data/mechanical_load_data.rda

data/mechanical_load_data.rda: data/anthropometric_data.csv data/GRF_ACC_data_all.xlsx data/max_rates_IMU_running.csv code/01_tidy_data.R
	R CMD BATCH code/01_tidy_data.R

## install  : Installs all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean    : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata

## cleanall : Removes auto-generated files, including processed data
cleanall:
	\rm -f *.Rout .Rdata data/*.rda output/* figures/*.tiff

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
