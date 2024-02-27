#######################################################################################
#                            PGS calculation for blood RNA-seq                        #
#######################################################################################

source("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/LDpred2_PGS_function.R")

library(data.table)
# library(RUVSeq, lib.loc = lib.location)
library(edgeR, lib.loc = lib.location)

#######################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/MDD/transcriptomics"
setwd(project.dir)

#######################################################################################
