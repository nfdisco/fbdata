
#' Update data sets
#'
#' Updates data sets by fetching new data from the Internet.
#'
#' @param dataset character vector.  Names of data sets to update, or
#' \code{NULL} to update every data set.
#' @param full If true, do a full update, including non-current
#' seasons.
#' @export

do.update <- function(dataset=NULL, full=FALSE) {

    if (is.null(dataset)) {
        dataset <- .dataset$name
    } else {
        dataset <- match.arg(dataset, .dataset$name, several.ok=TRUE)
    }

    need.datalist <- FALSE

    need.update <- .source$dataset %in% dataset
    if (!full) {
        need.update <- need.update & (.source$season == current.season)
    }

    changed <- mapply(fetch.newer, .source$url[need.update],
                      .source$path[need.update])

    changed <- replace(need.update, which(need.update), changed == 1)

    for (name in unique(.source$dataset[changed])) {
        assemble(name, save=TRUE)
    }

    if (any(changed)) {
        write.metadata()
    }
}


#' (Over)Write metadata
#' @noRd

write.metadata <- function() {
    fname <- Sys.glob(sprintf('%s/*.rda', datadir))
    dataset <- sub('.rda', '', basename(fname), fixed=TRUE)
    saveRDS(with(.dataset[.dataset$name %in% dataset, ],
                 unname(cbind(basename(path), desc))),
            file.path(metadir, 'data.rds'))
}


#' Fetch and save remote file, if it's newer than the local copy
#'
#' @return One if local file has changed, zero otherwise.
#' @import RCurl
#' @noRd

fetch.newer <- function(uri, fname) {

    mtime.local <- file.info(fname)[1,"mtime"]
    if (is.na(mtime.local)) {
        mtime.local <- 0
    }

    headers <- url.exists(uri, .header=TRUE)

    status <- as.numeric(headers['status'])

    if (status > 299 || status < 200) {
        warning("not found: ", uri)
        return(0)
    }

    locale <- Sys.getlocale("LC_TIME")

    Sys.setlocale("LC_TIME", "C")       # to correctly parse the date string

    mtime.remote <- tryCatch(
        as.POSIXct(headers["Last-Modified"],
                   format="%a, %d %b %Y %T GMT", tz="GMT"),
        error=function(...) 1)

    Sys.setlocale("LC_TIME", locale)

    if (mtime.local >= mtime.remote) {
        return(0)
    }

    if (!file.exists(dirname(fname)))
        dir.create(dirname(fname), recursive=TRUE)

    writeBin(getBinaryURL(uri), fname)

    1
}
