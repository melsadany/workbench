
### plotting function
require(tidyverse);require(ggh4x);require(ggrepel)

twas.plot <- function(twas_res, p_thresh = 5e-8, chr_colors = c("gray35","gray72"),txt_size=2) {
  df <- twas_res %>% arrange(lhs,CHR, P0) %>%
    filter(!is.na(TWAS.P))
  max.bases <-  df %>% group_by(lhs,CHR) %>% slice_max(order_by = P0, n=1) %>% ungroup() %>%
    select(lhs,CHR, max_base=P0) %>% mutate(max_base = replace_na(lag(max_base), 0))
  df2 <- df %>% left_join(max.bases) %>%
    mutate(pos = P0+max_base,
           sig = TWAS.P < p_thresh)
  
  ticks <- NULL; df2$pos <- NA; lastbase <- 0
  for (i in unique(df2$CHR)) {
    if (i==1) {
      df2[df2$CHR==i, ]$pos=df2[df2$CHR==i, ]$P0
    }	else {
      lastbase <- lastbase + tail(subset(df2,CHR==i-1)$P0, 1)
      df2[df2$CHR==i, ]$pos=df2[df2$CHR==i, ]$P0+lastbase
    }
    ticks <- c(ticks, df2[df2$CHR==i, ]$pos[floor(length(df2[df2$CHR==i, ]$pos)/2)+1])
  }
  ticklim <- c(min(df2$pos),max(df2$pos))
  tt.cols <- rep(chr_colors,60)
  chr_labs <- sub("19|21", "",unique(df2$CHR))
  
  p <- df2 %>% 
    ggplot(aes(x=pos,y=-log10(TWAS.P),colour=factor(CHR))) +
    geom_point(aes(shape = TWAS.Z>0), size=0.5, show.legend = F) +
    scale_shape_manual(values = c("TRUE" = 2, "FALSE"=6)) +
    scale_x_continuous(name="Chromosome", breaks=ticks, labels=chr_labs) +
    scale_colour_manual(values=tt.cols) +
    geom_hline(yintercept=0,colour="black") + 
    geom_hline(yintercept = -log10(p_thresh), color = "pink", linetype=2) +
    ggrepel::geom_text_repel(aes(label = ifelse(sig, gene, "")), max.overlaps = 200,
                             size=txt_size, show.legend = F, color = "black") +
    ggh4x::facet_grid2(rows = vars(lhs), scales = "free", space = "free") +
    bw.theme
  return(p)
}
