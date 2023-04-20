

.PHONY: all
all: ../data/cn_divorces.rda \
     ../data/cn_population.rda \
     ../data/nz_divorces.rda \
     ../data/nz_population.rda \
     ../data/nz_divorces_2020.rda \
     ../data/nz_population_2020.rda \
     ../data/nz_age_width_df.rda

../data/cn_divorces.rda: cn_divorces.R \
  cn_divorces.csv.gz
	Rscript $<

../data/cn_population.rda: cn_population.R \
  cn_population.csv.gz
	Rscript $<

../data/nz_divorces.rda: nz_divorces.R \
  VSM480501_20230322_102557_40.csv.gz
	Rscript $<

../data/nz_population.rda: nz_population.R \
  DPE403905_20230322_103055_56.csv.gz
	Rscript $<

../data/nz_divorces_2020.rda: nz_divorces_2020.R \
  ../data/nz_divorces.rda
	Rscript $<

../data/nz_population_2020.rda: nz_population_2020.R \
  ../data/nz_population.rda
	Rscript $<

../data/nz_age_width_df.rda: nz_age_width_df.R \
  ../data/nz_divorces_2020.rda
	Rscript $<



## Clean

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
