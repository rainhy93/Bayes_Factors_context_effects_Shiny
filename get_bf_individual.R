# BF for specified bins, a given tau value, and given subject(s)
get_bf_individual = function(df, bins_indiff = NULL, bins_pref_a = NULL, bins_pref_b = NULL, tau_value_indiff, tau_value_pref, subjects){
  
  if (length(bins_indiff) > 0) {
    bfs_indiff = df %>% filter(Bin %in% paste0("bin", bins_indiff)) %>%
      filter(Subject %in% subjects) %>%
      filter(round(tau,2) == round(tau_value_indiff,2)) %>%
      select(indiff) %>% pull(-1)
  } else {
    bfs_indiff = NA
  }
  
  
  if (length(bins_pref_a) > 0){
    bfs_pref_a = df %>% filter(Bin %in% paste0("bin", bins_pref_a)) %>%
      filter(Subject %in% subjects) %>%
      filter(round(tau,2) == round(tau_value_pref,2)) %>%
      select(a) %>% pull(-1)
  } else {
    bfs_pref_a = NA
  }
  
  
  if (length(bins_pref_b) > 0){
    bfs_pref_b = df %>% filter(Bin %in% paste0("bin", bins_pref_b)) %>%
      filter(Subject %in% subjects) %>%
      filter(round(tau,2) == round(tau_value_pref,2)) %>%
      select(b) %>% pull(-1)
  } else {
    bfs_pref_b = NA
  }
  

  bf = prod(bfs_indiff, bfs_pref_a, bfs_pref_b, na.rm = TRUE)
  return(bf)
}




