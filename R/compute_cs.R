compute_cs <- function(dist, grouping, season, typology){




                 #- unique types
                grouping.u <- unique(grouping)
                #- transform to matrix
                dist2 <- as.matrix(dist)
                dist2 <- 1 - dist2
                # ———> for every type: how similar are observations within type compared to between types
                for (k in seq_along(grouping.u)) {

                        if (k == 1) j.csk <- c()
                        k.id1    <- which(grouping == grouping.u[k])
                        k.id.n1  <- which(grouping != grouping.u[k])
                        k.sim1   <- dist2[k.id1, k.id1]
                        k.sim.n1 <- dist2[k.id1, k.id.n1]
                        k.ut     <- k.sim1[upper.tri(k.sim1)]
                        k.lt     <- k.sim1[lower.tri(k.sim1)]
                        k.ut.n   <- k.sim.n1[upper.tri(k.sim.n1)]
                        k.lt.n   <- k.sim.n1[lower.tri(k.sim.n1)]
                        j.csk[k] <- mean(append(k.ut, k.lt), na.rm = T) - mean(append(k.ut.n, k.lt.n), na.rm = T)
                        rm(list = ls()[grepl(x = ls(), pattern = "^k\\.")])
                        rm(k)

                }
                i.csj <- mean(j.csk, na.rm = T)

                # ———— reshape and store results  ———— #
                i.cs.eval <- data.table(cs = i.csj,
                                        typology = typology,
                                        season = season)
               return(i.cs.eval)
}
