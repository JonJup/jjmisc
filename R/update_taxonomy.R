#' Update the taxonomy table
#'
#' Update the taxontable with the taxa in TU. The taxontable must already be loaded.
#'
#' @param TU character vector
#'
#' @return data.frame; updated taxontable.
#' @export
#'
#' @examples

update_taxonomy <- function(TU){

        fill_new_table <- character(length(TU))

        taxontable_new <-
                data.frame(
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

        #- combine old and new taxontables
        taxontable2 <- rbind( taxontable, taxontable_new)
        #- loop over new taxa and try to find them in gbif
        for (i in seq_along(TU)){

                i.id <- which(taxontable$original_name == TU[i])
                #- skip this iteration of the loop if the focal taxon has already been evaluated.
                if (taxontable$clean[i.id])
                        next()
                #- look for taxon on gbif and extract first element of output
                i.co <-
                        taxize::classification(TU[i], db = "gbif") |>
                        {\(x) x[[1]]}()

                #- skip this iteration of the taxon is not found
                if (is.na(i.co))
                        next()
                #- assign taxon levels
                taxontable$species[i.id] <- ifelse("species"  %in% i.co$rank, i.co$name[which(i.co$rank == "species")], NA)
                taxontable$genus[i.id] <-      ifelse("genus"    %in% i.co$rank, i.co$name[which(i.co$rank == "genus")], NA)
                taxontable$family[i.id] <-     ifelse("family"   %in% i.co$rank, i.co$name[which(i.co$rank == "family")], NA)
                taxontable$order[i.id] <-      ifelse("order"    %in% i.co$rank, i.co$name[which(i.co$rank == "order")], NA)
                taxontable$subclass[i.id] <-   ifelse("subclass" %in% i.co$rank, i.co$name[which(i.co$rank == "subclass")], NA)
                taxontable$class[i.id] <-      ifelse("class"    %in% i.co$rank, i.co$name[which(i.co$rank == "class")], NA)
                taxontable$phylum[i.id] <-     ifelse("phylum"   %in% i.co$rank, i.co$name[which(i.co$rank == "phylum")], NA)
                taxontable$kingdom[i.id] <-    ifelse("kingdom"  %in% i.co$rank, i.co$name[which(i.co$rank == "kingdom")], NA)
                taxontable$clean[i.id] <- TRUE
                rm(i.co)
                rm(i.id)
                gc()

        }
        return(taxontable)
}
