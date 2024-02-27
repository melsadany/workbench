#######################################################################################
#                            generate RDS from BED file of MDD                        #
#######################################################################################

source("/Ded")

library(tidyverse)
library(bigsnpr)
options(bigstatsr.check.parallel.blas = FALSE)
options(default.nproc.blas = NULL)

snp_readBed("/Dedicated/jmichaelson-wdata/trthomas/array/merged_2022_ABCD_iWES1_WGS_2-4/genotypes/final.bed")
