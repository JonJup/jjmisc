#' Compute indicator value statistics
#'
#' This function returns the number of indicator taxa and the mean indicator value of significant indicator taxa.
#' To compute indicator values the indicspec::multipatt() with 999 permutations and the group-equalized static are used.
#'
#' @param community data.frame with sites as rows and taxa as columns
#' @param grouping character vector with group membership of sites
#' @param perm number of permutations to compute p-value of number of indicators and mean indicator statistic. NOT permutations in the call to multipatt.
#' @param typology character, for this typology is this test run?
#' @param season character, for which season is the test run?
#'
#' @return data.table with typology, season, variable (number of indicator taxa or mean indicator value of significant indicator taxa), test statistic values and permutation p_vaules.
#' @export
#'
#' @examples
indvalstat <- function(community, grouping, perm,typology, season, least.impaired){

        # ——— Create Variables  ——— #
        #- copy of the number of permutations that will be altered if any of the permutations have no indicator families.
        #- perm_work is used as an index for apply-type functions.
        perm_work = perm
        # #- numeric vector to hold p-values
        # p_values <- vector(mode = "numeric", length = 2)
        # names(p_values) <- c("n.ind", "stat")
        #- output list
        out <- vector(mode = "list", length = 2)
        names(out) <- c("data", "p.values")
        #- number of zero elements. If different from zero this value will be altered below
        n.zero <- 0

        #- create permutations of the grouping variable
        perms       <- lapply(1:perm, function(x) grouping[permute::shuffle(n = length(grouping))])
        org_score   <- compute_indvalstat(community, grouping)
        perm_scores <- lapply(1:perm, function(x) compute_indvalstat(community, perms[[x]]))
        if (any(sapply(perm_scores, nrow) == 0)){
                zero.id <- which(sapply(perm_scores, nrow)==0)
                n.zero = sum(sapply(perm_scores, nrow)==0)
                perm_scores[zero.id] <- NULL
                perm_work = length(perm_scores)
        }
        perm_scores2 <- lapply(1:perm_work, function(x) cbind(perm_scores[[x]], data.frame(id = rep(x, nrow(perm_scores[[x]])))))
        perm_scores3 <- data.table::rbindlist(perm_scores2)

        org_score$id = 0
        all <- rbind(org_score, perm_scores3)
        all$season = season
        all$typology = typology
        all$least.impaired = least.impaired
        # ——— PSEUDO P VALUES  ——— #
        #- number of indicators in null models
        n.indi.null <- sapply(perm_scores, nrow)
        #- were there any zeros?
        n.indi.null <- append(n.indi.null, rep(0,n.zero))
        #- Compute pseudo-p-value for the number of indicator families
        n.indi.p <- sum(nrow(org_score)<n.indi.null)/(perm + 1)

        #- Kruskal Wallis tests
        kruskal.stat <- kruskal.test(all$stat, all$id)
        kruskal.A    <- kruskal.test(all$A, all$id)
        kruskal.B    <- kruskal.test(all$B, all$id)

        p.values <- data.frame (typology = typology,
                    season   = season,
                    least.impaired = least.impaired,
                    variable = c("A", "B", "stat", "n.ind"),
                    p.value  = c(kruskal.A$p.value, kruskal.B$p.value, kruskal.stat$p.value, n.indi.p))

        out[["data"]] <- all
        out[["p.values"]] <- p.values

        return(out)
}

