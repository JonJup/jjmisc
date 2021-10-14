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

        if (!out %in% c("similarity", "typical_communities", "both"))
                stop("out must be \"similarity\" or \"typical_communities\" or \"both\"")

        #- Indicator Value analysis
        x1 <- indicspecies::multipatt(x = community, cluster = grouping, duleg = TRUE, permutations = 1)
        #- Reduce to B matrix
        x1 <- x1$B
        for (i in 1:ncol(x1)){
                if (i == 1) typical_comm <- list()
                typical_comm[[i]] <-  rownames(x1)[which(x1[,i] >= threshold)]
                names(typical_comm)[i] <- colnames(x1)[i]
        }

        #- prepare data frame for results
        lt <- length(typical_comm)
        type1 <- rep(names(typical_comm), each = lt)
        type2 <- rep(names(typical_comm), times = lt)
        type3 <-data.frame(type1, type2)
        type3 <- type3[-which(type3$type1 == type3$type2), ]
        type1.f <- as.numeric(as.factor(type3$type1))
        type2.f <- as.numeric(as.factor(type3$type2))
        type3 <- type3[-which(type1.f>type2.f),]

        for (i in 1:(ncol(x1)-1)){
                for (k in (i+1):ncol(x1)){
                        if (!i < k) next()

                        name1 <- names(typical_comm)[i]
                        name2 <- names(typical_comm)[k]

                        k.a <- sum(typical_comm[[i]] %in% typical_comm[[k]])
                        k.b <- sum(!typical_comm[[i]] %in% typical_comm[[k]])
                        k.c <- sum(!typical_comm[[k]] %in% typical_comm[[i]])
                        k.sim <- k.a/(k.a+k.b+k.c)
                        append.id <- which(type3$type1 == name1 & type3$type2 == name2)
                        type3$similarity[append.id] <- k.sim
                        #all_similarities <- append(all_similarities, k.sim)
                        rm(list = ls()[grepl(x=ls(),pattern="^k\\.")])
                }
        }
        # Create table to output similarities. The table has three columns 1. Type 2. Similarity to other types 3. mean similarity
        type3$mean = mean(type3$similarity)
        type3$sd   = sd(type3$similarity)
        if (out == "similarity")
                return(type3)
        if (out == "typical_communities")
                return(typical_comm)
        if (out == "both"){
                return(list(mean_sim = type3, typical_comm = typical_comm))
        }
}
