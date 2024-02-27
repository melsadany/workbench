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
# library(ggstatsplot, lib.loc = lib.location)
# library(Hmisc, lib.loc = lib.location)
library(lubridate)
# library(ggpubr, lib.loc = lib.location)
# library(ggh4x, lib.loc = lib.location)
# library(ggrepel, lib.loc = lib.location)
library(foreach)
library(doMC)
# library(ggExtra)
hash.sep <- "#######################################################################################"


theme_set(theme_minimal() +
            theme(axis.line = element_line(colour = "black", linewidth = 0.5), 
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
redblack.col <- c("#800000", "black")
six.colors <- c("#800000", "#cc7277", "#4f6162", "#e65236", "#56483a", "#73937e")
ten.colors <- c("#800000", "#cc7277", "#4f6162", "#e65236", "#56483a", 
                "#73937e", "#06241b", "#b8860b", "#e07c4c", "#9a81b0")

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
psave <- function(...,file){  
  con = pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  save(...,file=con,envir=.GlobalEnv); close(con) 
} 

# It works
pdsload <- function(fname,envir=.GlobalEnv){
  con <- pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -d <",fname),"rb")
  return(readRDS(con))
}
pdssave <- function(...,file){  
  con = pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  saveRDS(...,file=con)
} 

pload_multi <- function(fname,envir=.GlobalEnv, type = "rda"){
  con <- pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -d <",fname),"rb")
  if (type == "rda") {
    load(con,envir=envir); close(con)
  }else if (type == "rds") {
    return(readRDS(con)); close(con)
  }
}
psave_multi <- function(...,file, type = "rda"){  
  con = pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  if (type == "rda") {
    save(...,file=con,envir=.GlobalEnv); close(con) 
  }else if (type == "rds") {
    saveRDS(...,file=con); close(con) 
  }
}

psave.image <- function(file){ 
  con = pipe(paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  save(list = names(.GlobalEnv),file=con,envir=.GlobalEnv); close(con) 
} 
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
# ggplot tiles
redblu.col.gradient <- scale_fill_gradient2(low = redblu.col[2], high = redblu.col[1], name ="ρ")
redblack.col.gradient <- scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1], name ="ρ")
null_labs <- labs(x="",y="")
####################################################################################
rho <- "ρ"
####################################################################################
corr.func <- function(x, y, method = "pearson", cores = 6) {
  registerDoMC(cores = cores)
  all <- foreach(i = 1:ncol(x), .combine = rbind) %dopar% {
    tname <- colnames(x)[i]
    t <- x[,i]
    a2 <- foreach(j=1:ncol(y), .combine = rbind) %dopar% {
      sname <- colnames(y)[j]
      s <- y[,j]
      r <- as.numeric(cor.test(as.matrix(t),as.matrix(s), method = method)$estimate)
      p.val <- as.numeric(cor.test(as.matrix(t),as.matrix(s), method = method)$p.value)
      return(data.frame(r = r, pval = p.val, V2 = sname))
    }
    return(a2 %>% mutate(V1 = tname))
  }
  # do.call(rbind, apply(x, MARGIN = 2,function(t) {
  #   do.call(rbind, apply(y, MARGIN = 2, function(s) {
  #     r <- as.numeric(cor.test(as.matrix(t),as.matrix(s), method = method)$estimate)
  #     p.val <- as.numeric(cor.test(as.matrix(t),as.matrix(s), method = method)$p.value)
  #     return(data.frame(r = r, pval = p.val))
  #   })) %>% as.data.frame() %>% rownames_to_column("V2")
  # })) %>% as.data.frame() %>% rownames_to_column("V1") %>%
  #   mutate(V1 = sub("\\..*", "", V1))
  # # r <- as.numeric(cor.test(x,y, method = method)$estimate)
  # # p.val <- as.numeric(cor.test(x,y, method = method)$p.value)
}




