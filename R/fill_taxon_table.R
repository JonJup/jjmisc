#' Fill taxon table
#'
#' @param o character. Original Name.
#' @param a character. New species name.
#' @param b character. New genus name.
#' @param c character. New family name.
#' @param d character. New order name.
#' @param e character. New subclass name.
#' @param f character. New class name.
#' @param g character. New phylum name.
#'
#' @return data.table
#' @export
#'
#' @examples
fill_taxon_table <- function(o,a,b,c,d,e,f,g){

        taxontable[original_name == o,
                   `:=` (
                           species = a,
                           genus = b,
                           family = c,
                           order = d,
                           subclass = e,
                           class = f,
                           phylum = g,
                           kingdom = "Animalia"
                   )]
}
