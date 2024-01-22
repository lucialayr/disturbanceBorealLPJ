vegetation: run_vegetation
albedo: run_albedo
evapotrans: run_evapotrans
factors: run_factors
all: run_vegetation run_albedo run_evapotrans run_factors


run_vegetation: data/final/final_vegetation*.csv data/final/shp/vegetation*.shp
	Rscript code/vegetation_plot.R 

data/final/final_vegetation*.csv: data/processed/*_fpc_*processed.csv data/processed/*_lai_processed.csv
	Rscript code/vegetation_processed_final.R 

data/final/shp/vegetation*.shp: data/processed/*_fpc_*processed.csv data/processed/*_lai_processed.csv
	Rscript code/vegetation_processed_final.R $^

	
	
run_albedo:
	Rscript code/albedo_plot.R
	
run_evapotrans:
	Rscript code/evapotranspiration_plot


	
run_factors: final_factors.csv
	Rscript code/factors_plots.R
	
final_factors.csv: $(wildcard data/processed/*smoothed_mean*.csv) $(wildcard data/processed/*global_mean*.csv)
	Rscript code/factors_processed_final.R $^
	
	
	
		
