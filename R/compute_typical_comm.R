#' Compute typical communities
#'
#' @param community data.frame with sites as rows and taxa as columns
#' @param grouping character vector with group membership of sites
#' @param out character. Prefered output. Either the mean similarity between typical communities or list of typical communities.
#' @param threshold numeric. Relative frequency a taxon must attain to be regarded as common.
#'
#' @return Either the mean similarity between typical communities or list of typical communities.
#' @export
#'
#' @examples
compute_typical_comm <- function(community, grouping, out = "similarity", threshold = 0.66){

        if (!out %in% c("similarity", "typical_communities"))
                stop("out must be \"similarity\" or \"typical_communities\"")


        x1 <- indicspecies::multipatt(x = community, cluster = grouping, duleg = TRUE, permutations = 1)
        x1 <- x1$B
        typical_comm <- list()
        for (i in 1:ncol(x1)){
                typical_comm[[i]] <-  rownames(x1)[which(x1[,i] >= threshold)]
        }
        names(typical_comm) <- colnames(x1)
        all_similarities <- c()
        for (i in 1:(ncol(x1)-1)){
                for (k in 2:ncol(x1)){
                        if (i == k) next()
                        k.a <- sum(typical_comm[[i]] %in% typical_comm[[k]])
                        k.b <- sum(!typical_comm[[i]] %in% typical_comm[[k]])
                        k.c <- sum(!typical_comm[[k]] %in% typical_comm[[i]])
                        k.sim <- k.a/(k.a+k.b+k.c)
                        all_similarities <- append(all_similarities, k.sim)
                        rm(list = ls()[grepl(x=ls(),pattern="^k\\.")])
                }
        }
        mean_sim <- mean(all_similarities)
        if (out == "similarity")
                return(mean_sim)
        if (out == "typical_communities")
                return(typical_comm)

}
