

.PHONY: all
all: ../data/nz_divorces.rda

popn.rds: popn.R \
  DPE403905_20230322_103055_56.csv
	Rscript $^

divorces.rds: divorces.R \
  VSM480501_20230322_102557_40.csv
	Rscript $^

../data/nz_divorces.rda: nz_divorces.R \
  popn.rds \
  divorces.rds
	Rscript $<



## Clean

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data
