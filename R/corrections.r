#' Fixes known errors in the data.
#'
#' Fixes errors known to be present in specific files.
#'
#' @param fname file name.
#' @param data character vector.
#'
#' @return Patched data.
#' @noRd

apply.patches <- function(fname, data) {
    patch <- .patch.fun[[md5sum(fname)]]
    if (is.null(patch)) {
        data
    } else {
        patch(data)
    }
}

#' @noRd
patch.0203.E0 <- function(data) {
    sub('Middlesboro', 'Middlesbrough', data, fixed=TRUE)
}

#' @noRd
patch.0304.P1 <- function(data) {
    data[181] <- sub(',,2,1.8,-1.75,,', ',,,,,', data[181], fixed=TRUE)
    data
}

#' @noRd
.patch.fun <- list(
    `7415c64a4b7940f381e28aef62be2c0d`=patch.0203.E0,
    `2505163a420843e27ebe8c60d29d7625`=patch.0304.P1
)

