#' Compute statistical significance of typical communities
#'
#' @param community data.frame with sites as rows and taxa as columns
#' @param grouping character vector with group membership of sites
#' @param perm number of permutations to compute p-value of number of indicators and mean indicator statistic. NOT permutations in the call to multipatt.
#' @param typology character, for this typology is this test run?
#' @param season character, for which season is the test run?
#' @param threshold numeric. Relative frequency a taxon must attain to be regarded as common.
#'
#' @return data.table with mean similarity and p-value.
#' @export
#'
#' @examples
typical_comm <- function(community, grouping, perm,typology, season, threshold = 0.66){

        perms <- lapply(1:perm, function(x) grouping[permute::shuffle(n = length(grouping))])
        org_score   <- compute_typical_comm(community = community, grouping = grouping, threshold = threshold, out = "similarity")
        perm_scores <- sapply(1:perm, function(x) compute_typical_comm(community, perms[[x]], threshold = threshold,out = "similarity"))
        p_values <- sum(perm_scores<org_score, na.rm = TRUE)/(perm+1)

        out <- data.table::data.table(typology=typology,
                                      season = season,
                                      value = org_score,
                                      p_values)

        return(out)
}
