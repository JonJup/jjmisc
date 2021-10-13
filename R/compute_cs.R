#' Compute classification strength
#'
#' @param dist A distance matrix
#' @param grouping character vector with group membership of sites
#' @param season character, for which season is the run? The value is supplied as value for the output table.
#' @param typology character, for this typology is this test run? The value is supplied as value for the output table.
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
                # ———> for every type: how similar are observations within types and between types
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
                #- Relative frequencies of types
                props <- grouping |>
                        table() |>
                        proportions() |>
                        round(2)
                props <- data.frame(type = names(props),
                                    proportion = c(props))
                #- collect loop results in table
                csj <- data.frame(
                        within_type = wts,
                        between_type = bts,
                        type = grouping.u,
                        typlogy = typology,
                        season = season
                )
                #- combine loop results with relative frequencies
                csj <- dplyr::left_join(x = csj,
                                        y = props,
                                        by = "type")
                csj <-
                        csj |>
                        dplyr::mutate(within_weighted = within_type * proportion,
                                      between_type_mean = mean(csj$between_type)) |>
                        dplyr::mutate(within_weighted_sum = sum(within_weighted)) |>
                        dplyr::mutate(classification_strength = within_weighted_sum - between_type_mean)

               return(csj)
}
