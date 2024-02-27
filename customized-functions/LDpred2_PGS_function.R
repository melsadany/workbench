calculate_ldpred_pgs <- function(rds_path, sumstats){
  
  ### STEP 1: load in the bigsnp test genotype object (by test here I mean the merged cohort)
  obj.bigsnp <- snp_attach(rds_path)
  G <- obj.bigsnp$genotypes
  map <- dplyr::transmute(obj.bigsnp$map,
                          chr = chromosome, pos = physical.pos,
                          a0 = allele2, a1 = allele1)
  
  ### STEP 2: load the LD reference (we are using an external LD reference found here: https://figshare.com/articles/dataset/European_LD_reference/13034123)
  # "1,054,330 HapMap3 variants based on 362,320 European individuals of the UK biobank."
  # This external LD reference is referred to in the tutorial from here: https://privefl.github.io/bigsnpr-extdoc/polygenic-scores-pgs.html
  map_ldref <- readRDS("/Dedicated/jmichaelson-wdata/trthomas/abcd_spark_merge/polygenic_scores/ldpred2/ld_ref/map.rds")
  map_ldref <- map_ldref %>% 
    select(chr, pos, a0, a1, rsid, af_UKBB, ld)
  
  ### STEP 3: match summary statistics with LD ref and test genotypes
  # Match the sumstats to the HapMap SNPs
  info_snp <- snp_match(sumstats, map_ldref)
  snps_matched_ld_ref <- nrow(info_snp)
  str_c(snps_matched_ld_ref, " SNPs matched LD ref")
  # Removing SNPs from summary stats that have SD inconsistent with the LD ref
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
  
  ### STEP 4: calculate the genetic correlation matrix from the LD ref
  NCORES <- nb_cores() 
  tmp <- tempfile(tmpdir = "tmp-data")
  for (chr in 1:22) {
    cat(chr, ".. ", sep = "")
    ind.chr <- which(df_beta$chr == chr)
    ind.chr2 <- df_beta$`_NUM_ID_`[ind.chr]
    ind.chr3 <- match(ind.chr2, which(map_ldref$chr == chr))
    corr_chr <- readRDS(paste0("/Dedicated/jmichaelson-wdata/trthomas/abcd_spark_merge/polygenic_scores/ldpred2/ld_ref/LD_chr", chr, ".rds"))[ind.chr3, ind.chr3]
    if (chr == 1) {
      corr <- as_SFBM(corr_chr, tmp)
    } else {
      corr$add_columns(corr_chr, nrow(corr))
    }
  }
  
  ### STEP 5: calculate the estimated heritability from the LD ref
  ldsc <- with(df_beta, snp_ldsc(ld, ld_size = nrow(map_ldref),
                                 chi2 = (beta / beta_se)^2,
                                 sample_size = n_eff, ncores = NCORES))
  h2_est <- ldsc[["h2"]]
  print(str_c("the h2 estimate is: ", h2_est))
  
  ldsc_out <- tribble(
    ~variable, ~variable_description, ~value,
    "h2", "LDSC regression estimate of SNP heritability", ldsc[["h2"]],
    "h2_SE", "SE of heritability estimate", ldsc[["h2_se"]],
    "int", "LDSC regression intercept", ldsc[["int"]],
    "int_SE", "SE of regression intercept", ldsc[["int_se"]]
  )
  
  write_tsv(ldsc_out, str_c(output_path, pheno_name, "_LDSC.tsv"))
  
  
  ### Step 6: generate the polygenic score predictions for the test genotypes
  # map the betas to the genotypes
  
  beta_inf <- snp_ldpred2_inf(corr, df_beta, h2_est)
  map_pgs <- df_beta[1:4]; map_pgs$beta <- 1
  map_pgs <- snp_match(map_pgs, map)
  map_pgs <- cbind(map_pgs, beta_inf)
  map_pgs <- map_pgs %>% 
    mutate(beta = as.numeric(beta)) %>% 
    mutate(beta_inf = as.numeric(beta_inf)) %>% 
    mutate(final_inf_beta = beta * beta_inf)
  map_pgs <- map_pgs %>%
    select(chr, pos, a0, a1, beta = final_inf_beta, `_NUM_ID_`)
  
  write_tsv(map_pgs %>% select(chr, pos, a0, a1, beta),
            file = str_c(output_path, pheno_name, "_ldpred2_inf_betas.tsv"))
  pred_inf <- big_prodVec(G, 
                          y.col = map_pgs[["beta"]], 
                          ind.col = map_pgs[["_NUM_ID_"]],
                          ncores = NCORES)
  out <- cbind(obj.bigsnp$fam$sample.ID, pred_inf) %>% 
    as_tibble() %>% 
    mutate(pgs_name = pheno_name)
  out <- out %>% 
    select(IID = V1, pgs_name, PGS = pred_inf)
  
  write_tsv(out, str_c(output_path, pheno_name, "_PGS.tsv"))
  
}
