## data/running_df.rda : Generates data to be used in the analyses
data/running_df.rda: data/anthropometric_data.csv data/GRF_ACC_data_all.xlsx data/max_rates_IMU_running.csv code/01_tidy_data.R
	R CMD BATCH code/01_tidy_data.R

## install             : Install all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean               : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata

## cleanall            : Removes auto-generated files, including processed data
cleanall:
	\rm -f *.Rout .Rdata data/*.rda

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
