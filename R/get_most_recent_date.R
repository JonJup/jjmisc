#' Get most recent date for files with same name
#'
#'  If you have multiple files with the same name but different dates as part of the name, e.g. 21-05-03_test.rds and 21-05-04_test.rds this function will extract the most recent date.
#'
#' @param folder character; path to the file.
#' @param file character; name of file
#'
#' @return character; most recent date
#' @export
#'
#' @examples
most_recent_date <- function(folder, file) {

        fs::dir_ls(folder, regexp = file) |>
        stringr::str_remove(folder) |>
        dplyr::tibble() |>
        dplyr::rename(date = everything()) |>
        dplyr::mutate(date = lubridate::ymd(date)) |>
        dplyr::slice_max(date) |>
        dplyr::pull(date)
}
