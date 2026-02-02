####################################################################################
#                            main source file for Muhammad                         #
####################################################################################
device <- ifelse(any(grepl("LSS", system("ls ~", intern = T))), "IDAS", ifelse(any(grepl("iCloud", system("ls ~", intern = T))),"me","argon"))
source(paste0(ifelse(device == "IDAS", "~/LSS", ifelse(device == "argon","/Dedicated","/Volumes")),
              "/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))
# set.seed(123)
library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
lib.location <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/miniconda3/envs/tximpute/lib/R/library")
library(lubridate)
library(foreach)
library(doMC)
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
                  plot.caption = element_text(hjust = 0),
                  panel.background = element_rect(fill='transparent',color=NA),
                  plot.background = element_rect(fill='transparent', color=NA),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.background = element_rect(fill='transparent',color=NA),
                  legend.box.background = element_rect(fill='transparent',color=NA)
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
palette.1 <- c("#ff4600","#4782b4", "#39C08F","#C1624A","#88ADE1", "#627899","#F3B199","#55433C","#A6665F","#00C0C5","#3C4856","#AE6885","#783753")
palette.2 <- c("#F26419","#2F4858","#F6AE2D","#33658A","#86BBD8")

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
ggsave2 <- function(file, width = 8, height = 8, bg = "white", units = "in", dpi = 360) {
  ggsave(filename = file, bg = bg,limitsize = F,
         width = width, height = height, units = units, dpi = dpi)
}
transparent.theme <- theme(panel.background = element_rect(fill='transparent',color=NA),
                           plot.background = element_rect(fill='transparent', color=NA),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           legend.background = element_rect(fill='transparent',color=NA),
                           legend.box.background = element_rect(fill='transparent',color=NA))
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
corr.table <- function(x, method = "pearson") {
  corr <- Hmisc::rcorr(x%>%as.matrix(), type = method)
  p.r <- corr$r %>%
    as.data.frame() %>%
    rownames_to_column("V1") %>%
    pivot_longer(cols = colnames(corr$r), names_to = "V2", values_to = "r")
  p.pval <- corr$P %>%
    as.data.frame() %>%
    rownames_to_column("V1") %>%
    pivot_longer(cols = colnames(corr$P), names_to = "V2", values_to = "pval")
  p.ready <- inner_join(p.r, p.pval) %>% mutate(FDR = p.adjust(pval,"fdr")) %>% filter(V1!=V2)
  return(p.ready)
}
# ggplot tiles
redblu.col.gradient <- function(label="ρ"){scale_fill_gradient2(low = redblu.col[2], high = redblu.col[1], name =label)}
redblu.col.gradient.2 <- function(label="ρ"){scale_fill_gradient2(low = redblu.col.2[2], high = redblu.col.2[1], name =label)}
redblack.col.gradient <- function(label="ρ"){scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1], name =label)}
null_labs <- labs(x="",y="")
####################################################################################
## log axes in ggplot
# library(scales)
# log10.axes <- scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                             labels = trans_format("log10", math_format(10^.x))) +
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))) +
#   annotation_logticks()
####################################################################################
rho <- "ρ"
small.circle <- "◦"
big.circle <- "○"
double.circle <- "⊚"
less.equal <- "≤"
more.equal <- "≥"
up.triangle="∆"
down.triangle="∇"
beta <- "β"
checkmark <- "✔"
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
  } %>% mutate(FDR = p.adjust(pval,"fdr")) %>% filter(V1!=V2)
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

ci_ribbon1 <- geom_ribbon(stat="smooth",method="lm",aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
                          fill = NA, alpha = 1, linetype = 2,show.legend=F,col=redblu.col.2[2])
ci_ribbon.multi <- geom_ribbon(stat="smooth",method="lm",aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
                          fill = NA, alpha = 1, linetype = 2,show.legend=F)
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
coefs_table <- function(model) {
  require(jtools)
  
  jtools::summ(model, pval = T, confin=T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("x") %>%
    dplyr::rename("Estimate" = `Est.`,
                  confin_min = `2.5%`,
                  confin_max = `97.5%`,
                  pval = `p`)
  # if(!lmer) {
  #   
  # } else if(lmer) {
  #   summary(model)$coefficients %>% as.data.frame() %>%
  #     rownames_to_column("x") %>% 
  #     dplyr::rename(Estimate = 2, pval = 6) %>% 
  #     mutate(confin_min = Estimate-`Std. Error`,confin_max = Estimate+`Std. Error`) %>%
  #     dplyr::select(x,Estimate, pval, confin_min,confin_max)
  # }
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
img_ME <- function(mat,xlab="",ylab="",do.breaks=T,breaks,axes,
                   cex_row = 0.5,cex_col = 0.5,
                   main = "Heatmap",...) {
  
  if (!is.matrix(mat)) stop("Input must be a matrix.")
  if (is.null(rownames(mat))) rownames(mat) <- paste0("Row", 1:nrow(mat))
  if (is.null(colnames(mat))) colnames(mat) <- paste0("Col", 1:ncol(mat))
  if(missing(axes)) axes=F
  cc = colorRampPalette(c("royalblue","royalblue4","black","orangered","goldenrod1"))
  if(all(mat>=0) & missing(breaks)){
    cc = colorRampPalette(c("black","chartreuse"))
  }
  
  # Plot
  if(do.breaks & missing(breaks)){
    mx = max(abs(mat))
    qt = max(abs(quantile(mat,c(0.01,0.99))))
    bk = c(-1*mx,seq(-1*qt,qt,length.out=255),mx)
    image(0:ncol(mat),0:nrow(mat),t(mat),ylim=c(nrow(mat),0),ylab=ylab,
          xlab=xlab,axes=axes,col=cc(256),breaks=bk,...)
  }else{
    image(0:ncol(mat),0:nrow(mat),t(mat),ylim=c(nrow(mat),0),ylab=ylab,
          xlab=xlab,axes=axes,col=cc(256),breaks=breaks,...)
  }
  box()
  # Add names
  mtext(colnames(mat), 1, at = (1:ncol(mat))-0.5, las = 2, line=0.5,cex = cex_col)
  mtext(rownames(mat), 2, at = (1:nrow(mat))-0.5, las = 2, line=0.5,cex = cex_row)
}

## jake's img
img_JM = function(x,ylab,xlab,axes,col,na.zero=F,breaks,do.breaks=T,do.labels=T,...){
  cc = colorRampPalette(c("royalblue","royalblue4","black","orangered","goldenrod1"))
  if(all(x>=0) & missing(breaks)){
    cc = colorRampPalette(c("black","chartreuse"))
  }
  if(missing(ylab)) ylab=""
  if(missing(xlab)) xlab=""
  if(missing(axes)) axes=F
  if(missing(col)) col=cc(256)
  if(na.zero) x[is.na(x)] = 0
  if(do.breaks & missing(breaks)){
    mx = max(abs(x))
    qt = max(abs(quantile(x,c(0.01,0.99))))
    bk = c(-1*mx,seq(-1*qt,qt,length.out=255),mx)
    image(0:ncol(x),0:nrow(x),t(x),ylim=c(nrow(x),0),ylab=ylab,
          xlab=xlab,axes=axes,col=col,breaks=bk,...)
  }else{
    image(0:ncol(x),0:nrow(x),t(x),ylim=c(nrow(x),0),ylab=ylab,
          xlab=xlab,axes=axes,col=col,breaks=breaks,...)
  }
  box()
  if(!is.null(rownames(x)) & do.labels){
    mtext(rownames(x),side=2,at=(1:nrow(x))-0.5,las=2,line=0.5,cex=0.5)
  }
  if(!is.null(colnames(x)) & do.labels){
    mtext(colnames(x),side=1,at=(1:ncol(x))-0.5,las=2,line=0.5,cex=0.5)
  }
  
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
## outliers and PCs
identify_outliers_per_column <- function(df){
  return(do.call(cbind,lapply(df, function(x) ((x>=quantile(x,0.25) - 1.5 * IQR(x))&
                                                 (x>=quantile(x,0.75) + 1.5 * IQR(x))))))
}
pca_wo_outliers <- function(df, max_outlier_per_feature=NULL,max_outlier_features_per_row=NULL){
  df.outliers <- identify_outliers_per_column(df)
  if(is.null(max_outlier_per_feature)){max_outlier_per_feature=round(0.05*nrow(df))}
  if(is.null(max_outlier_features_per_row)){max_outlier_features_per_row=round(0.05*ncol(df))}
  
  feat.out.count <- colSums(df.outliers) %>% as.data.frame() %>% 
    rownames_to_column("feature") %>% rename(count=2) %>%
    mutate(print=paste("feature", feature, "has\t\t", count, "outliers"))
  row.out.count <- rowSums(df.outliers) %>% as.data.frame() %>% 
    rownames_to_column("row") %>% rename(count=2) %>%
    mutate(print=paste("row",row, "has\t\t", count, "outliers"))
  
  cat(paste0(hash.sep,"\n",paste(feat.out.count$print[feat.out.count$count>0],collapse ="\n"),"\n",hash.sep,"\n"))
  cat(paste0(hash.sep,"\n",paste(row.out.count$print[row.out.count$count>0],collapse ="\n"),"\n",hash.sep,"\n"))
  cat(paste0("\n", hash.sep,"\n",
             "dropping ", sum(feat.out.count$count>max_outlier_per_feature), " features with >", max_outlier_per_feature, " outliers","\n",
             "dropping ", sum(row.out.count$count>max_outlier_features_per_row)," rows identified as an outlier in >", max_outlier_features_per_row, " features","\n",
             hash.sep,"\n"))
  
  df.clean <- df[!((rowSums(df.outliers))>max_outlier_features_per_row),!((colSums(df.outliers))>max_outlier_per_feature)]
  df.pc <- prcomp(scale(df.clean))
  df.pc.pred <- predict(df.pc, scale(df))
  return(list("prcomp_obj"=df.pc, "pred_PCs"=df.pc.pred))
}

####################################################################################
####################################################################################
####################################################################################
## dark slides theme
dark.slides.theme <- theme(panel.background = element_rect(fill = "#272D32"),
                           plot.background = element_rect(fill = "#272D32"),
                           panel.border = element_rect(color = "white"),
                           axis.ticks = element_line(color = "white"),
                           text = element_text(color = "white"),
                           axis.text = element_text(color = "white"))
dark.slides.palette <- c("#272D32","#CC377C","#B7E3DB","#1D4A42","#E087B0","#698C8F","#35594F","#75AD9D",
                         "#C77C40","#DDB08C","#6D974B","#A7C68D","#7E375E","#C376A0","#72553F","#B8977E",
                         "#516271","#0B1E1A")
####################################################################################
####################################################################################

