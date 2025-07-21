####################################################################################
#                            main source file for Muhammad                         #
####################################################################################

device <- ifelse(any(grepl("LSS", system("ls ~", intern = T))), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))

# library(lifecycle, lib.loc = "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/test2/lib/R/library")
set.seed(123)
library(tidyverse)
library(data.table)
library(ggplot2)

# library(ggpubr, lib.loc= "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library")
library(RColorBrewer)


lib.location <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library")
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

my_theme=theme_minimal() +
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
  )
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
my.guides <- guides(fill = guide_colorbar(barwidth = 6, barheight = 0.5),
                    color = guide_colorbar(barwidth = 6, barheight = 0.5))
poster_theme <- theme_minimal() +
  theme(axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14, vjust = 0.5),
        axis.ticks = element_blank(),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.switch.pad.grid = unit(0, "points"),
        strip.placement = "outside",
        panel.spacing = unit(1, "points"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        # legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 8),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        title = element_text(face = "bold", size = 14),
        plot.caption = element_text(hjust = 0)
        )
bw.theme <- theme_linedraw() +
  theme(strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black"),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0), 
        panel.grid = element_blank())

boxplot.colors <- c("#aaf0d1", "#b39eb5")
build.directory <- "mkdir -p archive; mkdir -p logs; mkdir -p figs"
redblu.col <-  c("#ff6961", "#89cff0")
redblu.col.2 <- c("#ff4600","#4782b4")
redblack.col <- c("#800000", "black")
six.colors <- c("#800000", "#cc7277", "#4f6162", "#e65236", "#56483a", "#73937e")
ten.colors <- c("#800000", "#cc7277", "#4f6162", "#e65236", "#56483a", 
                "#73937e", "#06241b", "#b8860b", "#e07c4c", "#9a81b0")
antique.colors <- c("#855C75","#D9AF6B","#AF6458","#736F4C","#526A83",
                    "#625377","#68855C","#9C9C5E","#A06177","#8C785D",
                    "#467378","#7C7C7C")
redblu.ni.col <- c("#E43339", "#0033FF")
warm.cold.col <- c("blue", "deepskyblue", "white", "yellow", "orange", "red")
abstract.colors <- c("#CC0A7D", "#EAA91E", "#A46CE1", "#7B8CB2", "#798632", "#4A9A3B", "#FFD9B8", "#781285", "#C746FF", "#F9FF32")
abstract.colors.2 <- c("#86324A", "#333286", "#798632", "#4A9A3B", "#643264", "#3B39D2", "#DCD813", "#00FF00", "#DD9FDD", "#2D8B57", "#86CDEB")
density.colors <- c("#0d0887", "#6a00a8", "#b12a90", "#e16462", "#fca636", "#f0f921")
####################################################################################
# correct_path <- function(file) {
#   device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
#   if (device == "IDAS") {
#     if (grepl("wdata", file)) {
#       f <- sub(".*wdata", "~/LSS/jmichaelson-wdata", file)
#     }else if (grepl("wdata", file)) {
#       f <- sub(".*sdata", "~/LSS/jmichaelson-sdata", file)
#     }
#   }else {
#     if (grepl("wdata", file)) {
#       f <- sub(".*wdata", "/Dedicated/jmichaelson-wdata", file)
#     } else if (grepl("wdata", file)) {
#       f <- sub(".*sdata", "/Dedicated/jmichaelson-sdata", file)
#     }
#   }
#   print(f)
#   return(f)
# }
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
ggsave2 <- function(file, bg = "white", width = 8, height = 8, units = "in", dpi = 360) {
  ggsave(filename = file, bg = bg,
         width = width, height = height, units = units, dpi = dpi)
}
####################################################################################
pload <- function(fname,envir=.GlobalEnv){
  con <- pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz")," -d <",fname),"rb")
  load(con,envir=envir); close(con)
}
psave <- function(...,file){  
  con = pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz"), " -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  save(...,file=con,envir=.GlobalEnv); close(con) 
} 

# It works
pdsload <- function(fname,envir=.GlobalEnv){
  con <- pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz")," -d <",fname),"rb")
  return(readRDS(con))
}
pdssave <- function(...,file){  
  con = pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz"), " -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  saveRDS(...,file=con)
} 

pload_multi <- function(fname,envir=.GlobalEnv, type = "rda"){
  con <- pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz"), " -d <",fname),"rb")
  if (type == "rda") {
    load(con,envir=envir); close(con)
  }else if (type == "rds") {
    return(readRDS(con)); close(con)
  }
}
psave_multi <- function(...,file, type = "rda"){  
  con = pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz"), " -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  if (type == "rda") {
    save(...,file=con,envir=.GlobalEnv); close(con) 
  }else if (type == "rds") {
    saveRDS(...,file=con); close(con) 
  }
}

psave.image <- function(file){ 
  con = pipe(paste(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pixz"), " -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
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
redblu.col.gradient <- function(label="ρ"){scale_fill_gradient2(low = redblu.col[2], high = redblu.col[1], name =label)}
redblu.col.gradient.2 <- function(label="ρ"){scale_fill_gradient2(low = redblu.col.2[2], high = redblu.col.2[1], name =label)}
redblack.col.gradient <- function(label="ρ"){scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1], name =label)}
null_labs <- labs(x="",y="")
####################################################################################
## log axes in ggplot
library(scales)
log10.axes <- scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                            labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks()
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
####################################################################################
####################################################################################

### x is your data.frame of covariates, y is the "trait"
### that you want to residualize
### the values returned are Z-statistics that indicate how
### significant a departure from the model each observation is;
### these are analogous to and somewhat correlated with the 
### residuals, but they do a more rigorous job of taking
### the various forms of uncertainty into account
z_from_lm = function(y,x){ 
  df = data.frame(x,y=y) 
  fit = lm(y~.,data=df)  
  prd = predict(fit,df,se.fit=T) 
  z = (df$y-prd$fit)/sqrt(summary(fit)$sigma^2 + prd$se.fit^2) 
  return(z)
}




z_from_rf <- function(y,x){ 
  df <- data.frame(x,y=y) 
  fit <- randomForest::randomForest(y~.,data=df)  
  prd <- fit$predicted
  z <- scale(df$y-prd,T,T)[,1]
  return(z)
}

####################################################################################
####################################################################################
####################################################################################
# errorh example
# ggplot(aes(x=Estimate, y = feature, color = region)) +
#   geom_point(aes(alpha = sig),  
#              position = position_dodge(width = 0.6), size =2.5) +
#   geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
#   scale_alpha_manual(values = c(1, 0.5)) +
#   scale_shape_manual(values = c(1, 2)) + 
#   geom_errorbarh(aes(xmin = confin_min, xmax = confin_max, 
#                      alpha = sig), 
#                  linewidth = 0.8, height = 0, show.legend = F, 
#                  position = position_dodge(width = 0.6)) +
#   scale_color_manual(values = six.colors) +
#   ggh4x::facet_grid2(cols = vars(x), 
#                      # rows = vars(feature),
#                      scales = "free") +
#   labs(caption = paste0("n(samples): ", length(unique(tt7$te_id)))) +
#   theme_linedraw() +
#   theme(strip.background = element_rect(fill = "white", color = "white"),
#         strip.text = element_text(color = "black"),
#         legend.position = "bottom",
#         legend.box = "vertical",
#         plot.caption = element_text(hjust = 0))

estimate.plot <- function(df = df) {
  df %>%
    ggplot(aes(x=x,y=y,alpha=sig)) +
    geom_point(position = position_dodge(width = 0.6), size =2.5) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
    geom_errorbarh(aes(xmin = confin_min, xmax = confin_max),
                   linewidth = 0.8, height = 0, show.legend = F,
                   position = position_dodge(width = 0.6)) +
    labs(x="Estimate", y="")
}

####################################################################################
archetypes_summ <- function(obj, k, points_labels) {
  ss <- simplexplot(obj)
  arc <- rbind(cbind(ss$proj_z, 
                     text = paste0("A", c(1:k)))) %>% 
    as.data.frame()
  a.df <- cbind(ss$proj_h,
                text = points_labels) %>%
    rbind(cbind(ss$proj_z, 
                text = paste0("A", c(1:k)))) %>%
    as.data.frame() %>%
    mutate(x=as.numeric(x),
           y=as.numeric(y),
           lab = ifelse(text %in% paste0("A", c(1:k)), T, F),
           size2 = ifelse(grepl("A", text), 1, 0.7)) %>%
    full_join(arc %>% complete(nesting(x,y), text) %>% 
                select(text, xend=x, yend=y) %>%
                left_join(arc, .,by = 'text') %>%
                filter(!(x==xend & y==yend)) %>%
                mutate_at(.vars = c(1,2,4,5), 
                          .funs = function(x) as.numeric(x)) %>% 
                select(-text) %>% 
                mutate(lab = F)) %>%
    mutate(text = ifelse(is.na(xend), text, ""))
  
  alpha.mat <- coef(obj)
  arch.mat <- parameters(obj)
  b.df <- as.matrix(alpha.mat) %*% as.matrix(arch.mat)
  
  ret <- list(simplex_plot = a.df,
              arch_coef = alpha.mat,
              arch.weights = arch.mat,
              reduced_data = b.df)
  return(ret)
}
####################################################################################
####################################################################################
remove_outliers_iqr <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  df_filtered <- df[df[[column]] >= lower_bound & df[[column]] <= upper_bound, ]
  return(df_filtered)
}

####################################################################################
####################################################################################
coefs_table <- function(model, lmer = F) {
  require(jtools)
  if(!lmer) {
    jtools::summ(model, pval = T, confin=T)$coeftable %>%
      as.data.frame() %>%
      rownames_to_column("x") %>%
      rename("Estimate" = `Est.`,
             confin_min = `2.5%`,
             confin_max = `97.5%`,
             pval = `p`)
  } else if(lmer) {
    summary(model)$coefficients %>% as.data.frame() %>%
      rownames_to_column("x") %>% 
      rename(Estimate = 2, pval = 6) %>% 
      inner_join(as.data.frame(confint(model)) %>% rownames_to_column("x") %>% 
                   rename(confin_min=2,confin_max=3)) %>%
      select(x,Estimate, pval, confin_min,confin_max)
  }
}
####################################################################################
####################################################################################
fisher_table <- function(x, y) {
  tt <- fisher.test(x = x, y = y)
  data.frame(OR = as.numeric(tt$estimate),
             pval = as.numeric(tt$p.value),
             conf_min = as.numeric(tt$conf.int[1]),
             conf_max = as.numeric(tt$conf.int[2]))
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

