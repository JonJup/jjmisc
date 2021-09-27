#' Prepare data for comparison of typologies
#'
#' @param data data.table
#' @param taxon character
#' @param abundance.to.pa boolean
#'
#' @return data.table
#' @export
#'
#' @examples
#'
ct_prepare_data1 <- function(dataset, taxon) {

        dataset <- data.frame(dataset)

        #- Check inputs
        if (!"data.frame" %in% class(dataset)) stop("data must be a data.table")
        if (!"character" %in% class(taxon)) stop("taxon must be a character")

        #- Character vector with column names to keep
        #col.vec <- append(c("gr_sample_id", taxon, "abundance"))
        col.vec <- c("gr_sample_id", taxon, "abundance", "brt12")
        #- Subset data to columns from col.vec
        #x1 <- data[, .SD, .SDcols = col.vec]
        x1 <- dataset[, col.vec]
        #- remove any NA in taxon column
        names(x1)[which(names(x1) %in% c("species", "genus", "family"))] <- "taxon"
        x1 <- x1[which(!is.na(x1$taxon)), ]
        #- Make sure that each taxon only occurs once per sample. This might be violated
        #- when a coarse taxonomic resolution is used and several taxa from this group were
        #- observed.
        x1 <- dplyr::distinct(dplyr::group_by(x1, gr_sample_id), taxon,  .keep_all = T)
        x1$abundance = 1
        #- Turn to wide format with one column for each taxon and one row per site.
        x1 <-
                tidyr::pivot_wider(
                        x1,
                        id_cols = c("gr_sample_id", "brt12"),
                        names_from = taxon,
                        values_from = abundance,
                        values_fill = 0
                )
        #- Turn NAs to 0 i.e. absence.
        x2 <- jjmisc::setNAcols(x1)
        x1 <- data.table::setnafill(x = x1, type = "const", cols = x2, fill = 0)
        #- remove columns of taxa that are always absent.
        rm.col <- names(which(colSums(x1[,x2]) == 0))
        if (length(rm.col) > 0) {
                x1[, (rm.col) := NULL]
        }
        return(x1)
}
