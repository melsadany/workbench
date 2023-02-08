####################################################################################
#                            main source file for Muhammad                         #
####################################################################################


# library(lifecycle, lib.loc = "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/test2/lib/R/library")
set.seed(123)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)
# library(ggpubr, lib.loc= "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library")
library(RColorBrewer)


lib.location <- "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library"
library(ggpubr, lib.loc = lib.location)
# projects.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects"
library(ggstatsplot, lib.loc = lib.location)
library(Hmisc, lib.loc = lib.location)
library(lubridate)
library(ggpubr, lib.loc = lib.location)
library(ggh4x, lib.loc = lib.location)
hash.sep <- "#######################################################################################"


theme_set(theme_minimal() +
            theme(axis.line = element_line(colour = "black", size = 0.5), 
                  axis.text = element_text(face = "bold"), 
                  axis.text.y = element_text(size = 8),
                  axis.text.x = element_text(angle = 90, hjust = 1, size = 8, vjust = 0.5),
                  axis.ticks = element_blank(), 
                  axis.title = element_text(face = "bold"), 
                  strip.text = element_text(face = "bold"),
                  strip.switch.pad.grid = unit(0, "points"),
                  strip.placement = "outside", 
                  panel.spacing = unit(1, "points"), 
                  panel.grid = element_blank(),
                  legend.position = "bottom", 
                  # legend.title = element_blank(), 
                  legend.text = element_text(face = "bold", size = 5),
                  plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8),
                  title = element_text(face = "bold", size = 10),
                  plot.caption = element_text(hjust = 0)
            ))



boxplot.colors <- c("#aaf0d1", "#b39eb5")
build.directory <- "mkdir -p archive; mkdir -p logs; mkdir -p figs"
redblu.col <-  c("#ff6961", "#89cff0")
####################################################################################
quantile_normalization <- function(df){
  df_rank <- apply(df,2,rank,ties.method="min")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_mean <- apply(df_sorted, 1, mean)
  
  index_to_mean <- function(my_index, my_mean){
    return(my_mean[my_index])
  }
  
  df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
  rownames(df_final) <- rownames(df)
  return(df_final)
}
####################################################################################
get_upper_tri <- function(data2){
  total <- data2
  total[lower.tri(data2)]<- NA
  return(total)
}
get_lower_tri <- function(data2){
  total <- data2
  total[t(lower.tri(data2))]<- NA
  return(total)
}
####################################################################################
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

####################################################################################
tissue.ls <- c("Adipose_Subcutaneous","Adipose_Visceral_Omentum","Adrenal_Gland","Artery_Aorta",
               "Artery_Coronary","Artery_Tibial","Brain_Amygdala","Brain_Anterior_cingulate_cortex_BA24" 
               ,"Brain_Caudate_basal_ganglia","Brain_Cerebellar_Hemisphere","Brain_Cerebellum","Brain_Cortex"                         
               ,"Brain_Frontal_Cortex_BA9","Brain_Hippocampus","Brain_Hypothalamus", "Brain_Nucleus_accumbens_basal_ganglia"
               ,"Brain_Putamen_basal_ganglia","Brain_Spinal_cord_cervical_c-1" ,"Brain_Substantia_nigra","Breast_Mammary_Tissue"                
               ,"Cells_Cultured_fibroblasts","Cells_EBV-transformed_lymphocytes","Colon_Sigmoid", "Colon_Transverse"                     
               ,"Esophagus_Gastroesophageal_Junction","Esophagus_Mucosa","Esophagus_Muscularis","Heart_Atrial_Appendage"               
               ,"Heart_Left_Ventricle","Kidney_Cortex","Liver","Lung"                                 
               ,"Minor_Salivary_Gland","Muscle_Skeletal","Nerve_Tibial","Ovary"                                
               ,"Pancreas","Pituitary","Prostate","Skin_Not_Sun_Exposed_Suprapubic"      
               ,"Skin_Sun_Exposed_Lower_leg","Small_Intestine_Terminal_Ileum" ,"Spleen","Stomach"                              
               ,"Testis","Thyroid", "Uterus", "Vagina"                               
               ,"Whole_Blood")

####################################################################################
pload <- function(fname,envir=.GlobalEnv){
  con <- pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -d <",fname),"rb")
  load(con,envir=envir); close(con)
}
####################################################################################

