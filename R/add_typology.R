#' Add freshwater typologies to sampling sites
#'
#' @param data data.table
#'
#' @return data.table
#' @export
#'
#' @examples
#'
add_typologies <- function(data){

        sites <-
                unique(data, by = "site_id") |>
                sf::st_as_sf(coords = c("x.coord", "y.coord"), crs = data4$EPSG[1]) |>
                dplyr::select(site_id) |>
                sf::st_transform(crs = sf::st_crs(typologies))

        nn <- sf::st_nearest_feature(sites, typologies)
        nn <- typologies[nn,]
        distances <- sf::st_distance(sites,
                                     y = nn,
                                     by_element = TRUE)

        sites <- dplyr::mutate(sites,
                               distance = as.numeric(distances),
                               brt12    = nn$brt12,
                               ife      = nn$illies,
                               bgr      = nn$bgr,
                               brtXife  = nn$brtXife,
                               brtXbgr  = nn$brtXbgr,
                               least.impacted = nn$least.impacted
        )
        sites <- sf::st_drop_geometry(sites)
        #- join sites with data
        data.out <- dplyr::left_join(data,
                                     sites,
                                     by = "site_id")
        data.table::setDT(data.out)
        return(data.out)
}
