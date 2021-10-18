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

        #- compute indicator value
        x1 <- indicspecies::multipatt(x = community, cluster = grouping, duleg = TRUE, permutations = 999)

        #- which taxa are indicators?
        sign <- x1$sign
        sign$taxon <- rownames(sign)
        sign <- sign[which(sign$p.value <= 0.05), ]

        #- what are the B values of indicator taxa?
        B <- x1$B[which(rownames(x1$B) %in% sign$taxon)]
        sign$B <- B

        #- what are the A values of indicator taxa?
        A <- x1$A[which(rownames(x1$A) %in% sign$taxon)]
        sign$A <- A

        #- drop columns
        sign2 <- sign[, c("A", "B", "stat")]
        # sign2$n_indi <- nrow(sign)
        # mean_stat <- mean(x1$stat)
        # out <- data.table::data.table(n_indi, mean_stat)
        return(sign2)
}
