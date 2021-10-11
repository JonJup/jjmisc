#' Compute classification strength
#'
#' @param dist distance matrix
#' @param grouping character vector with group membership of sites
#' @param season character, for which season is the test run?
#' @param typology character, for this typology is this test run?
#'
#' @return data.table
#' @export
#'
#' @examples
compute_cs <- function(dist, grouping, season, typology){

                 #- unique types
                grouping.u <- unique(grouping)
                #- transform to matrix
                dist2 <- as.matrix(dist)
                dist2 <- 1 - dist2
                # ———> for every type: how similar are observations within type compared to between types
                for (k in seq_along(grouping.u)) {

                        if (k == 1) wts <- bts <- c()
                        k.id1    <- which(grouping == grouping.u[k])
                        k.id.n1  <- which(grouping != grouping.u[k])
                        k.sim1   <- dist2[k.id1, k.id1]
                        k.sim.n1 <- dist2[k.id1, k.id.n1]
                        k.ut     <- k.sim1[upper.tri(k.sim1)]
                        k.lt     <- k.sim1[lower.tri(k.sim1)]
                        k.ut.n   <- k.sim.n1[upper.tri(k.sim.n1)]
                        k.lt.n   <- k.sim.n1[lower.tri(k.sim.n1)]
                        wts[k] <-  mean(append(k.ut, k.lt), na.rm = T)
                        bts[k] <-  mean(append(k.ut.n, k.lt.n), na.rm = T)

                        rm(list = ls()[grepl(x = ls(), pattern = "^k\\.")])
                        rm(k)

                }
                csj <- data.frame(within_type = wts,
                                  between_type = bts,
                                  type = grouping.u,
                                  typlogy = typology,
                                  season = season)
               return(csj)
}
