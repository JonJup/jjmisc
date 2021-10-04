#' Update the taxonomy table
#'
#' Update the taxontable with the taxa in TU. The taxontable must already be loaded.
#'
#' @param TU character vector
#'
#' @return data.table; updated taxontable.
#' @export
#'
#' @examples

update_taxonomy <- function(TU){

        library(data.table)
        fill_new_table <- character(length(TU))

        taxontable_new <-
                data.table::data.table(
                original_name = TU,
                species  = fill_new_table,
                genus    = fill_new_table,
                family   = fill_new_table,
                order    = fill_new_table,
                subclass = fill_new_table,
                class    = fill_new_table,
                phylum   = fill_new_table,
                kingdom  = fill_new_table,
                clean    = FALSE
        )

        taxontable <- data.table::rbindlist(list(taxontable, taxontable_new))

        for (i in seq_along(TU)){

                #- skip this iteration of the loop if the focal taxon has already been evaluated.
                if (taxontable[original_name == TU[i], clean]) next()

                i.co <-
                        taxize::classification(TU[i], db = "gbif") |>
                        {\(x) x[[1]]}()

                # skip this iteration of the taxon is not found
                if (is.na(i.co)) next()
                #- assign taxon levels
                taxontable[original_name == TU[i], species  := ifelse("species"  %in% i.co$rank, i.co$name[which(i.co$rank == "species")], NA)]
                taxontable[original_name == TU[i], genus    := ifelse("genus"    %in% i.co$rank, i.co$name[which(i.co$rank == "genus")], NA)]
                taxontable[original_name == TU[i], family   := ifelse("family"   %in% i.co$rank, i.co$name[which(i.co$rank == "family")], NA)]
                taxontable[original_name == TU[i], order    := ifelse("order"    %in% i.co$rank, i.co$name[which(i.co$rank == "order")], NA)]
                taxontable[original_name == TU[i], subclass := ifelse("subclass" %in% i.co$rank, i.co$name[which(i.co$rank == "subclass")], NA)]
                taxontable[original_name == TU[i], class    := ifelse("class"    %in% i.co$rank, i.co$name[which(i.co$rank == "class")], NA)]
                taxontable[original_name == TU[i], phylum   := ifelse("phylum"   %in% i.co$rank, i.co$name[which(i.co$rank == "phylum")], NA)]
                taxontable[original_name == TU[i], kingdom  := ifelse("kingdom"  %in% i.co$rank, i.co$name[which(i.co$rank == "kingdom")], NA)]
                taxontable[original_name == TU[i], clean := TRUE]
                rm(i.co)

        }
        return(taxontable)
}
