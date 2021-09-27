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
                st_as_sf(coords = c("x.coord", "y.coord"), crs = data4$EPSG[1]) |>
                select(site_id) |>
                st_transform(crs = st_crs(typologies))

        nn <- st_nearest_feature(sites, typologies)
        nn <- typologies[nn,]

        distances <- st_distance(sites, y = nn, by_element = TRUE)

        sites %<>% mutate(distance = as.numeric(distances),
                          brt12    = nn$brt12,
                          ife      = nn$ife,
                          bgr      = nn$bgr,
                          brtXife  = nn$brtXife,
                          brtXbgr  = nn$brtXbgr,
                          least.impacted = nn$least.impacted
        )
        sites %<>% st_drop_geometry() |> setDT()

        #- join sites with data
        data.out <- sites[data, on = "site_id"]
        return(data.out)
}
