classification_strength<- function(dist, grouping, season, typology, permutations){

      perms <- lapply(1:permutations, function(x) grouping[permute::shuffle(n = length(grouping))])
      org_score   <-  compute_cs(dist, grouping, season, typology)
      perm_scores <- lapply(1:permutations, function(x) compute_cs(dist, perms[[x]], season, typology))
      perm_scores <- data.table::rbindlist(perm_scores)
      org_score$p_value <- sum(perm_scores$cs > org_score$cs)/(permutations + 1)
      return(org_score)

}


