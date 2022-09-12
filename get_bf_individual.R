# BF for specified bins, a given tau value, and a given subject
get_bf_individual = function(df, bins_indiff = NULL, bins_pref_a = NULL, bins_pref_b = NULL, tau_value, subject){
  
  if (length(bins_indiff) > 0) {
    bfs_indiff = df %>% filter(tau == tau_value & Subject %in% subject) %>%
    filter(Bin %in% paste0("bin", bins_indiff)) %>%
    select(bf_indiff) %>% pull(-1)
  } else {
    bfs_indiff = NA
  }
  
  
  if (length(bins_pref_a) > 0){
    bfs_pref_a = df %>%
    filter(tau == tau_value & Subject %in% subject) %>%
    filter(Bin %in% paste0("bin", bins_pref_a)) %>%
    select(bf_pref_a) %>% pull(-1)
  } else {
    bfs_pref_a = NA
  }
  
  
  if (length(bins_pref_b) > 0){
    bfs_pref_b = df %>%
    filter(tau == tau_value & Subject %in% subject) %>%
    filter(Bin %in% paste0("bin", bins_pref_b)) %>%
    select(bf_pref_b) %>% pull(-1)
  } else {
    bfs_pref_b = NA
  }
  

  bf = prod(bfs_indiff, bfs_pref_a, bfs_pref_b, na.rm = TRUE)
  return(bf)
}




