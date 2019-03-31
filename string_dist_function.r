
name_test_legal_name <- function(company_string){
  df <- tibble(name = kase_legal_name_list, test_string = company_string)
  
  df <- df %>% mutate(estab_dist_lv        = stringdist(name,test_string,method = 'lv'),
                      estab_dist_osa       = stringdist(name,test_string,method = 'osa'),
                      estab_dist_dl        = stringdist(name,test_string,method = 'dl'),
                      estab_dist_lcs       = stringdist(name,test_string,method = 'lcs'),
                      estab_dist_qgram     = stringdist(name,test_string,method = 'qgram'),
                      estab_dist_cosine    = stringdist(name,test_string,method = 'cosine'),
                      estab_dist_jaccard   = stringdist(name,test_string,method = 'jaccard'),
                      estab_dist_jw        = stringdist(name,test_string,method = 'jw', p = 0.1)
  )
  
  
  df_summary_sd <- df %>% summarize(estab_sd_lv        = sd(estab_dist_lv),
                                    estab_sd_osa       = sd(estab_dist_osa),
                                    estab_sd_dl        = sd(estab_dist_dl),
                                    estab_sd_lcs       = sd(estab_dist_lcs),
                                    estab_sd_qgram     = sd(estab_dist_qgram),
                                    estab_sd_cosine    = sd(estab_dist_cosine),
                                    estab_sd_jaccard   = sd(estab_dist_jaccard),
                                    estab_sd_jw        = sd(estab_dist_jw))
  
  
  df_summary_mean <- df %>% summarize(estab_mean_lv        = mean(estab_dist_lv),
                                      estab_mean_osa       = mean(estab_dist_osa),
                                      estab_mean_dl        = mean(estab_dist_dl),
                                      estab_mean_lcs       = mean(estab_dist_lcs),
                                      estab_mean_qgram     = mean(estab_dist_qgram),
                                      estab_mean_cosine    = mean(estab_dist_cosine),
                                      estab_mean_jaccard   = mean(estab_dist_jaccard),
                                      estab_mean_jw        = mean(estab_dist_jw))                               
  
  
  
  
  df <- df %>% mutate(
    estab_sd_lv        = df_summary_sd$estab_sd_lv,
    estab_sd_osa       = df_summary_sd$estab_sd_osa,
    estab_sd_dl        = df_summary_sd$estab_sd_dl,
    estab_sd_lcs       = df_summary_sd$estab_sd_lcs,
    estab_sd_qgram     = df_summary_sd$estab_sd_qgram,
    estab_sd_cosine    = df_summary_sd$estab_sd_cosine,
    estab_sd_jaccard   = df_summary_sd$estab_sd_jaccard,
    estab_sd_jw        = df_summary_sd$estab_sd_jw,
    estab_mean_lv        = df_summary_mean$estab_mean_lv,
    estab_mean_osa       = df_summary_mean$estab_mean_osa,
    estab_mean_dl        = df_summary_mean$estab_mean_dl,
    estab_mean_lcs       = df_summary_mean$estab_mean_lcs,
    estab_mean_qgram     = df_summary_mean$estab_mean_qgram,
    estab_mean_cosine    = df_summary_mean$estab_mean_cosine,
    estab_mean_jaccard   = df_summary_mean$estab_mean_jaccard,
    estab_mean_jw        = df_summary_mean$estab_mean_jw)
  
  
  
  df <- df %>% mutate(estab_dist_lv_z        = (estab_dist_lv - estab_mean_lv)/estab_sd_lv ,
                      estab_dist_osa_z       = (estab_dist_osa - estab_mean_osa)/estab_sd_osa ,
                      estab_dist_dl_z        = (estab_dist_dl - estab_mean_dl)/estab_sd_dl ,
                      estab_dist_lcs_z       = (estab_dist_lcs - estab_mean_lcs)/estab_sd_lcs ,
                      estab_dist_qgram_z     = (estab_dist_qgram - estab_mean_qgram)/estab_sd_qgram ,
                      estab_dist_cosine_z    = (estab_dist_cosine - estab_mean_cosine)/estab_sd_cosine ,
                      estab_dist_jaccard_z   = (estab_dist_jaccard - estab_mean_jaccard)/estab_sd_jaccard ,
                      estab_dist_jw_z        = (estab_dist_jw - estab_mean_jw)/estab_sd_jw )
  
  
  df <- df %>% mutate(estab_avg = (estab_dist_dl_z + estab_dist_lcs_z + estab_dist_qgram_z +
                                     estab_dist_cosine_z+ estab_dist_jaccard_z)/ 6)
  
  
  df <- df %>% arrange(estab_avg)
  
  return(df)
}