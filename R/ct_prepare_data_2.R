#' Compute distance matrix from result of ct_preparedata1
#'
#' @param data data.table
#' @param ncores integer. Number of cores to be used in the computation of the distance matrix
#' @param distance_metric character. What distance metric should be used in the distance matrix
#'
#' @return dist.
#' @export
#'
#' @examples
ct_prepare_data2 <- function(data, ncores = 1, distance_metric = "binary"){

        x1  <- as.matrix(select(data, !append("gr_sample_id", typologies)))
        x2  <- parallelDist(x1, method = distance_metric, threads = ncores)
        return(x2)

}

