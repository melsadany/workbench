library(tidyverse)
library(runonce)
library(remotes)
library(bigsnpr)
options(bigstatsr.check.parallel.blas = FALSE)
options(default.nproc.blas = NULL)
library(data.table)
library(magrittr)
################################################################################
#### script is from Lucas, with minor changes in the saving
#### extended function ####
calculate_ldpred_pgs <- function(sumstats, 
                                 device, 
                                 n_core, 
                                 plink_rds, 
                                 sd_y = 1, 
                                 pheno_name = 'pheno_not_given', 
                                 build = 'hg19', 
                                 output_path){
  
  ### STEP 1: load in the bigsnp test genotype object (by test here I mean the ABCD&SPARK merged cohort)
  obj.bigsnp <- snp_attach(rdsfile = plink_rds)
  G <- obj.bigsnp$genotypes
  map <- dplyr::transmute(obj.bigsnp$map,
                          chr = chromosome, pos = physical.pos,
                          a0 = allele2, a1 = allele1)
  ### STEP 2: load the LD reference (we are using an external LD reference found here: https://figshare.com/articles/dataset/European_LD_reference/13034123)
  # "1,054,330 HapMap3 variants based on 362,320 European individuals of the UK biobank."
  # This external LD reference is referred to in the tutorial from here: https://privefl.github.io/bigsnpr-extdoc/polygenic-scores-pgs.html
  map_ldref <- readRDS(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
                              "/jmichaelson-wdata/lcasten/tools/LDPred2/map_hm3_plus.rds")) 
  if(build == 'hg19'){
    map_ldref <- map_ldref %>% 
      select(chr, pos, a0, a1, rsid, af_UKBB, ld, block_id)
  }
  if(build == 'hg38'){
    map_ldref <- map_ldref %>% 
      select(chr, pos_hg38, a0, a1, rsid, af_UKBB, ld, block_id) %>%
      rename(pos = pos_hg38)
  }
  ### STEP 3: match summary statistics with LD ref and test genotypes
  # Match the sumstats to the HapMap SNPs
  info_snp <- snp_match(sumstats, map_ldref)
  snps_matched_ld_ref <- nrow(info_snp)
  str_c(snps_matched_ld_ref, " SNPs matched LD ref")
  if (sd_y == 0) {
    df_beta = info_snp
    in_test <- vctrs::vec_in(df_beta[, c("chr", "pos")], map[, c("chr", "pos")])
    df_beta <- df_beta[in_test, ]
    print(str_c("the final number of variants being used to calculate the LD matrix for PGS is: ", nrow(df_beta)))
  }
  # Removing SNPs from summary stats that have SD inconsistent with the LD ref
  if (sd_y %in% c(1,2)) {
    sd_ldref <- with(info_snp, sqrt(2 * af_UKBB * (1 - af_UKBB)))
    sd_ss <- with(info_snp, sd_y / sqrt(n_eff * beta_se^2))
    is_bad <- sd_ss < (0.5 * sd_ldref) | sd_ss > (sd_ldref + 0.1) | sd_ss < 0.1 | sd_ldref < 0.05
    df_beta <- info_snp[!is_bad, ]
    print(str_c(sum(is_bad), " additional SNPs removed due to inconsistent SD with the LD ref"))
    # Removing SNPs not in test genotypes
    in_test <- vctrs::vec_in(df_beta[, c("chr", "pos")], map[, c("chr", "pos")])
    df_beta <- df_beta[in_test, ]
    print(str_c(sum(in_test == F), " additional SNPs removed due to not being genotyped in the test cohort"))
    print(str_c("the starting number of variants in the summary statistics file was: ", nrow(sumstats)))
    print(str_c("the final number of variants being used to calculate the LD matrix for PGS is: ", nrow(df_beta)))
  }
  ### STEP 4: calculate the genetic correlation matrix from the LD ref
  NCORES <- n_core
  tmp <- tempfile(tmpdir = paste0("tmp-", pheno_name))
  for (chr in 1:22) {
    cat(chr, ".. ", sep = "")
    ind.chr <- which(df_beta$chr == chr)
    ind.chr2 <- df_beta$`_NUM_ID_`[ind.chr]
    ind.chr3 <- match(ind.chr2, which(map_ldref$chr == chr))
    corr_chr <- readRDS(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
                               "/jmichaelson-wdata/lcasten/tools/LDPred2/ldref_hm3_plus/LD_with_blocks_chr",
                               chr, ".rds"))[ind.chr3, ind.chr3]
    if (chr == 1) {
      corr <- as_SFBM(corr_chr, tmp)
    } else {
      corr$add_columns(corr_chr, nrow(corr))
    }
  }
  # registerDoMC(cores = NCORES)
  # chr = 1
  # cat(chr, ".. ", sep = "")
  # ind.chr <- which(df_beta$chr == chr)
  # ind.chr2 <- df_beta$`_NUM_ID_`[ind.chr]
  # ind.chr3 <- match(ind.chr2, which(map_ldref$chr == chr))
  # corr_chr <- readRDS(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
  #                            "/jmichaelson-wdata/lcasten/tools/LDPred2/ldref_hm3_plus/LD_with_blocks_chr", 
  #                            chr, ".rds"))[ind.chr3, ind.chr3]
  # corr <- as_SFBM(corr_chr, tmp)
  # foreach (chr = 2:22) %dopar% {
  #   cat(chr, ".. ", sep = "")
  #   ind.chr <- which(df_beta$chr == chr)
  #   ind.chr2 <- df_beta$`_NUM_ID_`[ind.chr]
  #   ind.chr3 <- match(ind.chr2, which(map_ldref$chr == chr))
  #   corr_chr <- readRDS(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
  #                              "/jmichaelson-wdata/lcasten/tools/LDPred2/ldref_hm3_plus/LD_with_blocks_chr", 
  #                              chr, ".rds"))[ind.chr3, ind.chr3]
  #   corr$add_columns(corr_chr, nrow(corr))
  # }
  
  
  ### STEP 5: calculate the estimated heritability from the LD ref
  ldsc <- with(df_beta, snp_ldsc(ld, ld_size = nrow(map_ldref),
                                 chi2 = (beta / beta_se)^2,
                                 sample_size = n_eff, ncores = NCORES))
  # snp_ldsc(df_beta$ld, ld_size = nrow(map_ldref),
  #          chi2 = (df_beta$beta / df_beta$beta_se)^2,
  #          sample_size = df_beta$n_eff, ncores = NCORES)
  h2_est <- ldsc[["h2"]]
  print(str_c("the h2 estimate is: ", h2_est))
  
  ldsc_out <- tribble(
    ~variable, ~variable_description, ~value,
    "h2", "LDSC regression estimate of SNP heritability", ldsc[["h2"]],
    "h2_SE", "SE of heritability estimate", ldsc[["h2_se"]],
    "int", "LDSC regression intercept", ldsc[["int"]],
    "int_SE", "SE of regression intercept", ldsc[["int_se"]]
  )
  set.seed(123)
  write_tsv(ldsc_out, str_c(output_path,"/", pheno_name, "_LDSC.tsv"))
  ### Step 6: generate the polygenic score predictions for the test genotypes IF h2 > 0
  if (h2_est > 0) {
    ####################
    #    inf model     #
    ####################
    # map the betas to the genotypes
    beta_inf <- snp_ldpred2_inf(corr, df_beta, h2_est)
    map_pgs <- df_beta[1:4]; map_pgs$beta <- 1
    # map_pgs_diff = snp_match(map_pgs, map, strand_flip = F)
    map_pgs2 <- snp_match(map_pgs, map)
    # missing_snps = map_pgs %>%
    #   select(chr, pos) %>%
    #   anti_join(map_pgs2)
    # drop_snp_beta <- rownames(map_pgs[map_pgs$chr %in% missing_snps$chr & map_pgs$pos %in% missing_snps$pos, ])
    map_pgs2$beta_inf <- beta_inf  ## error given
    beta_inf_vec = beta_inf
    rm(beta_inf)
    map_pgs2 <- map_pgs2 %>%
      mutate(beta = as.numeric(beta),
             beta_inf = as.numeric(beta_inf),
             final_inf_beta = beta * beta_inf)
    map_pgs2 <- map_pgs2 %>%
      select(chr, pos, a0, a1, beta = final_inf_beta, `_NUM_ID_`)
    write_tsv(map_pgs2 %>% select(chr, pos, a0, a1, beta),
              file = str_c(output_path,"/",  pheno_name, "_ldpred2_inf_betas.tsv"))
    G2 <- snp_fastImputeSimple(G, ncores = NCORES)
    pred_inf <- big_prodVec(G2,
                            y.col = map_pgs2[["beta"]],
                            ind.col = map_pgs2[["_NUM_ID_"]],
                            ncores = NCORES)
    out <- data.frame(IID = obj.bigsnp$fam$sample.ID,
                      PGS = as.numeric(pred_inf))
    colnames(out)[2] <- pheno_name
    write_tsv(out, str_c(output_path, "/", pheno_name, "_PGS.tsv"))
    ####################
    #     auto model   #
    ####################
    # this is the new auto model from LDPred
    # coef_shrink <- 0.95
    # multi_auto <- snp_ldpred2_auto(
    #   corr, df_beta, h2_init = h2_est,
    #   vec_p_init = seq_log(1e-4, 0.2, length.out = 10), ncores = NCORES,
    #   # use_MLE = FALSE,  # uncomment if you have convergence issues or when power is low (need v1.11.9)
    #   allow_jump_sign = FALSE, shrink_corr = coef_shrink)
    # range <- sapply(multi_auto, function(auto) diff(range(auto$corr_est)))
    # keep <- which(range > (0.95 * quantile(range, 0.95, na.rm = TRUE)))
    # beta_auto <- rowMeans(sapply(multi_auto[keep], function(auto) auto$beta_est))
    # map_pgs <- df_beta[1:4]; map_pgs$beta <- 1
    # map_pgs2 <- snp_match(map_pgs, map)
    # map_pgs2$beta_auto <- beta_auto
    # beta_auto_vec = beta_auto
    # map_pgs2 <- map_pgs2 %>% 
    #   mutate(beta = as.numeric(beta),
    #          beta_auto = as.numeric(beta_auto), 
    #          final_auto_beta = beta * beta_auto)
    # map_pgs2 <- map_pgs2 %>%
    #   select(chr, pos, a0, a1, beta = final_auto_beta, `_NUM_ID_`)
    # write_tsv(map_pgs2 %>% select(chr, pos, a0, a1, beta),
    #           file = str_c(output_path,"/",  pheno_name, "_ldpred2_inf_betas.tsv"))
    # G2 <- snp_fastImputeSimple(G, ncores = NCORES)
    # pred_auto <- big_prodVec(G2, 
    #                          y.col = map_pgs2[["beta"]], 
    #                          ind.col = map_pgs2[["_NUM_ID_"]],
    #                          ncores = NCORES)
    # out <- data.frame(IID = obj.bigsnp$fam$sample.ID, 
    #                   PGS = as.numeric(pred_auto))
    # colnames(out)[2] <- pheno_name
    # write_tsv(out, str_c(output_path, "/", pheno_name, "_PGS.tsv"))
    ####################
  }
  # clean temporary files, please! it can get up to 50GB
  system(paste0("rm -rf ", tmp, ".sbk"))
  system(paste0("rm -rf -R ", getwd(), "/tmp-", pheno_name))
  file.remove(paste0(tmp, ".sbk"))
}
##
