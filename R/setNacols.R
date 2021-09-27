#' Select columns with NAs
#'
#' @param x data.table
#'
#' @return integer.
#' @export
#'
#' @examples
setNAcols <- function(x){
        #- How many columns are in x that are not in append("gr_sample_id", typologies)
        x1 <- length(setdiff(names(x), append("gr_sample_id", typologies)))
        #- How many columns are in x
        x2 <- ncol(x)
        #- Columns in append("gr_sample_id", typologies) + 1
        x3 <- x2 - x1 + 1
        #- Column range between all columns and columns in append("gr_sample_id", typologies) + 1
        #- This corresponds to all taxa columns.
        x4 <- x3:x2
        #- return output
        x4
}
