#' Compute indicator value statistics
#'
#' This function returns the number of indicator taxa and the mean indicator value of significant indicator taxa.
#' To compute indicator values the indicspec::multipatt() with 999 permutations and the group-equalized static are used.
#'
#' @param community data.frame with sites as rows and taxa as columns
#' @param grouping character vector with group membership of sites
#' @param perm number of permutations to compute p-value of number of indicators and mean indicator statistic. NOT permutations in the call to multipatt.
#' @param typology character, for this typology is this test run?
#' @param season charachter, for which season is the test run?
#'
#' @return data.table with typology, season, variable (number of indicator taxa or mean indicator value of significant indicator taxa), test statistic values and permutation p_vaules.
#' @export
#'
#' @examples
indvalstat <- function(community, grouping, perm,typology, season){

        perms <- lapply(1:perm, function(x) grouping[permute::shuffle(n = length(grouping))])
        org_score   <- compute_indvalstat(community, grouping)
        perm_scores <- lapply(1:perm, function(x) compute_indvalstat(community, perms[[x]]))
        perm_scores <- data.table::rbindlist(perm_scores)
        p_values <- c()
        p_values[1] <- sum(perm_scores$n_indi>org_score$n_indi, na.rm = TRUE)/(perm+1)
        p_values[2] <- sum(perm_scores$mean_stat > org_score$mean_stat, na.rm = TRUE)/(perm+1)

        out <- data.table::data.table(typology=typology, season = season, statistic = c("n_indicators", "mean_indval"),
                                      value = c(org_score$n_indi, org_score$mean_stat),
                                      p_value = p_values)

        return(out)
}
