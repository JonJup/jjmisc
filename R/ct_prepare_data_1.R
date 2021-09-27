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
ct_prepare_data1 <- function(data, taxon, typologies) {

        #- Check inputs
        if (!"data.table" %in% class(data)) stop("data must be a data.table")
        if (!"character" %in% class(taxon)) stop("taxon must be a character")
        if (!"character" %in% class(typologies)) stop("typologies must be a character vector")

        #- Character vector with column names to keep
        col.vec <- append(c("gr_sample_id", taxon, "abundance"), typologies)
        #- Subset data to columns from col.vec
        x1 <- data[, .SD, .SDcols = col.vec]
        #- remove any NA in taxon column
        if ("species" %in% names(x1)) {
                x1 <- x1[!is.na(species)]
                names(x1)[which(names(x1) == "species")] <- "taxon"
        }
        if ("genus"   %in% names(x1)) {
                x1 <- x1[!is.na(genus)]
                names(x1)[which(names(x1) == "genus")] <- "taxon"
        }
        if ("family"  %in% names(x1)) {
                x1 <- x1[!is.na(family)]
                names(x1)[which(names(x1) == "family")] <- "taxon"
        }

        #- Make sure that each taxon only occurs once per sample. This might be violated
        #- when a coarse taxonomic resolution is used and several taxa from this group were
        #- observed.
        setDT(x1)
        # x1 <- x1[, abundance := sum(abundance), by = c("gr_sample_id", "taxon")]
        x1 <- unique(x1, by = c("gr_sample_id", "taxon"))
        x1 <- x1[, abundance := 1]

        x1 <- unique(x1, by = c("gr_sample_id", taxon))
        #- Drop rows where the taxonomic level is coarser then what is required by the
        #- taxon argument.
        x1 <- x1[!is.na(get(taxon))]
        #- Turn to wide format with one column for each taxon and one row per site.
        x1 <- pivot_wider(x1, id_cols = append(c("gr_sample_id"), typologies),
                     names_from = all_of(taxon),
                     values_from = abundance,
                     values_fill = 0)
        #- Turn to data.table.
        x1 <- setDT(x1)
        #- Turn NAs to 0 i.e. absence.
        x2 <- setNAcols(x1)
        x1 <- setnafill(x = x1, type = "const", cols = x2, fill = 0)
        #- remove columns of taxa that are always absent.
        rm.col <- names(which(colSums(x1[,x2,with=F]) == 0))
        if (length(rm.col) > 0) {
                x1[, (rm.col) := NULL]
        }
        return(x1)
}
