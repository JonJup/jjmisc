#' Reduce a data set with multiple samples per site to the most recent sample for each site.
#'
#' @param x data set to reduce
#'
#' @return x data set with only the most recent samples for each site
#' @export
#'
#' @examples
newest_sample <- function(x) {


        sping =
        spring <- x[which(x$season == "spring")]
        if (nrow(spring) > 0){
                spring[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                spring[, newest_date := max(date), by = "site_id"]
                spring <- spring[sampling.events == 1 | date == newest_date]
        }
        summer <- x[season == "spring"]
        if (nrow(summer) > 0){
                summer[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                summer[, newest_date := max(date), by = "site_id"]
                summer <- summer[sampling.events == 1 | date == newest_date]
        }
        autumn <- x[season == "autumn"]
        if (nrow(autumn) > 0){
                autumn[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                autumn[, newest_date := max(date), by = "site_id"]
                autumn <- autumn[sampling.events == 1 | date == newest_date]
        }
        winter <- x[season == "winter"]
        if (nrow(winter) > 0){
                winter[, sampling.events := uniqueN(gr_sample_id), by = "site_id"]
                winter[, newest_date := max(date), by = "site_id"]
                winter <- winter[sampling.events == 1 | date == newest_date]
        }


        out <- rbindlist(list(spring, summer, autumn))
}



