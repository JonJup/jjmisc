#' Compute indval statitics
#'
#' Internal function used to compute test statistics based on the indicator value.
#' Indicator values are computed with indicspecies::multipatt and 999 permutations.
#'
#' @param community data.frame with sites as rows and taxa as columns
#' @param grouping character vector with group membership of sites
#'
#' @return data.table with number of statistically significant indicator taxa and the mean indicator value of statistically significant indicator taxa.
#' @export
#'
#' @examples
compute_indvalstat <- function(community, grouping){

        x1 <- indicspecies::multipatt(x = community, cluster = grouping, duleg = TRUE, permutations = 999)
        x1 <- x1$sign
        x1$taxon <- rownames(x1)
        setDT(x1)
        x1 <- x1[p.value <= 0.05]
        n_indi <- nrow(x1)
        mean_stat <- mean(x1$stat)
        out <- data.table::data.table(n_indi, mean_stat)
        return(out)
}
