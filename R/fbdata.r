#' Historical football data
#'
#' This package includes historical football results and match
#' statistics for several European leagues.  It also provides a
#' mechanism for keeping the data up-to-date by fetching the latest
#' football results from
#' \href{http://www.football-data.co.uk/}{Football-Data.co.uk}.  (This
#' particular feature requires that the package's directory be
#' writable by the user.)
#'
#' @name fbdata-package
#' @docType package
NULL

#' Historical football data
#'
#' Football scores and match statistics for several European leagues.
#'
#' To use a particular data set, first it must be loaded with
#' \code{\link{data}}.
#'
#' @section Datasets:
#' \describe{
#'   \item{\code{scores.d1}}{Bundesliga scores & match statistics}
#'   \item{\code{scores.e0}}{Premier League scores & match statistics}
#'   \item{\code{scores.f1}}{Ligue 1 scores & match statistics}
#'   \item{\code{scores.i1}}{Serie A scores & match statistics}
#'   \item{\code{scores.n1}}{Eredivisie scores & match statistics}
#'   \item{\code{scores.p1}}{Primeira Liga scores & match statistics}
#'   \item{\code{scores.sp1}}{La Liga scores & match statistics}
#' }
#' @source \url{http://www.football-data.co.uk/}
#' @seealso \code{\link{data}}
#' @name fbdata-datasets
NULL

#' Write informative message to stdout
#' @noRd

info <- function(...) {
    cat(..., "\n", sep="")
}

#' Return copy of logical vector, with NAs set to FALSE
#' @noRd

na.false <- function(x) {               # note: NA & FALSE = FALSE
    x & !is.na(x)                       # but   NA & TRUE = NA
}
