####################################################################################
#                            main source file for Muhammad                         #
####################################################################################


# library(lifecycle, lib.loc = "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/test2/lib/R/library")
set.seed(123)
library(tidyverse)
library(data.table)
library(ggplot2)

# library(ggpubr, lib.loc= "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library")
library(RColorBrewer)


lib.location <- "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library"
library(ggstatsplot, lib.loc = lib.location)
library(Hmisc, lib.loc = lib.location)
library(lubridate)
# library(ggpubr, lib.loc = lib.location)
# library(ggh4x, lib.loc = lib.location)
# library(ggrepel, lib.loc = lib.location)
library(foreach)
library(doMC)
# library(ggExtra)
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
my.guides <- guides(fill = guide_colorbar(barwidth = 6, barheight = 0.5))
# poster_theme <- theme_set(theme_minimal() +
#                             theme(axis.line = element_line(colour = "black", size = 0.5), 
#                                   axis.text = element_text(face = "bold"), 
#                                   axis.text.y = element_text(size = 14),
#                                   axis.text.x = element_text(angle = 90, hjust = 1, size = 14, vjust = 0.5),
#                                   axis.ticks = element_blank(), 
#                                   axis.title = element_text(face = "bold"), 
#                                   strip.text = element_text(face = "bold"),
#                                   strip.switch.pad.grid = unit(0, "points"),
#                                   strip.placement = "outside", 
#                                   panel.spacing = unit(1, "points"), 
#                                   panel.grid = element_blank(),
#                                   legend.position = "bottom", 
#                                   # legend.title = element_blank(), 
#                                   legend.text = element_text(face = "bold", size = 8),
#                                   plot.title = element_text(size = 14),
#                                   plot.subtitle = element_text(size = 12),
#                                   title = element_text(face = "bold", size = 14),
#                                   plot.caption = element_text(hjust = 0)
#                             ))


boxplot.colors <- c("#aaf0d1", "#b39eb5")
build.directory <- "mkdir -p archive; mkdir -p logs; mkdir -p figs"
redblu.col <-  c("#ff6961", "#89cff0")
six.colors <- c("#800000", "#cc7277", "#4f6162", "#e65236", "#56483a", "#73937e")
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

####################################################################################
pload <- function(fname,envir=.GlobalEnv){
  con <- pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -d <",fname),"rb")
  load(con,envir=envir); close(con)
}
# psave <- function(object,envir=.GlobalEnv, fname, threads){
#   # con <- pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -p", threads," <",object),"rb")
#   system2(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -p", threads," -o", fname, " -i "), input = object)
#   # load(con,envir=envir); close(con)
# }

####################################################################################
cosine_similarity <- function(x, y) {
  dot_prod <- sum(x * y)
  norm_x <- sqrt(sum(x^2))
  norm_y <- sqrt(sum(y^2))
  return(dot_prod / (norm_x * norm_y))
}
####################################################################################
# create a function to compute correlation and return a tidy table with r and pval
corr.table <- function(x, y, method = "pearson") {
  corr <- Hmisc::rcorr(x%>%as.matrix(),y%>%as.matrix(), type = method)
  p.r <- corr$r %>%
    as.data.frame() %>%
    rownames_to_column("V1") %>%
    pivot_longer(cols = colnames(corr$r), names_to = "V2", values_to = "r")
  p.pval <- corr$P %>%
    as.data.frame() %>%
    rownames_to_column("V1") %>%
    pivot_longer(cols = colnames(corr$P), names_to = "V2", values_to = "pval")
  p.ready <- inner_join(p.r, p.pval)
  return(p.ready)
}







