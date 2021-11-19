#' Fill taxon table
#'
#' @param o character. Original Name.
#' @param s character. New species name.
#' @param g character. New genus name.
#' @param f character. New family name.
#' @param o character. New order name.
#' @param sc character. New subclass name.
#' @param c character. New class name.
#' @param p character. New phylum name.
#' @param k character. New kingdom name.
#'
#' @return data.table
#' @export
#'
#' @examples
fill_taxon_table <- function(o, s = NULL,g = NULL, f = NULL, or = NULL, sc = NULL, c = NULL, p = NULL, k = NULL){

        o.id <- which(taxontable$original_name == o)

        if(!is.null(s))  taxontable$species[o.id]  <- s
        if(!is.null(g))  taxontable$genus[o.id]    <- g
        if(!is.null(f))  taxontable$family[o.id]   <- f
        if(!is.null(or))  taxontable$order[o.id]    <- or
        if(!is.null(sc)) taxontable$subclass[o.id] <- sc
        if(!is.null(c))  taxontable$class[o.id]    <- c
        if(!is.null(p))  taxontable$phylum[o.id]   <- p
        if(!is.null(k))  taxontable$kingdom[o.id]   <- k
        print(taxontable[o.id,])
        x <- readline("ok?")
        if (x != "no") return(taxontable)
}
