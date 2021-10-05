#' Compute classification strength of clustering
#'
#'Computes the classification strength of a classification.
#'
#' @param dist distance matrix
#' @param grouping character vector with group membership of sites
#' @param season character, for which season is the test run?
#' @param typology character, for this typology is this test run?
#' @param permutations
#'
#' @return data.table
#' @export
#'
#' @examples
classification_strength<- function(dist, grouping, season, typology, permutations){


      library(data.table)
      perms             <- lapply(1:permutations, function(x) grouping[permute::shuffle(n = length(grouping))])
      org_score         <- jjmisc::compute_cs(dist, grouping, season, typology)
      perm_scores       <- lapply(1:permutations, function(x) jjmisc::compute_cs(dist, perms[[x]], season, typology))
      perm_scores       <- data.table::rbindlist(perm_scores)
      org_score$p_value <- sum(perm_scores$cs > org_score$cs)/(permutations + 1)
      return(org_score)

}


