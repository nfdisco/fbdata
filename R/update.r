
#' Update data sets
#'
#' Updates data sets by fetching new data from the Internet.  By
#' default, only results from the current season are updated.  The
#' value of \code{getOption('current.season')} determines the current
#' season at run time.
#'
#' @param dataset character vector.  Names of data sets to update, or
#' \code{NULL} to update every data set.
#' @param all.seasons If true, do a full update, including non-current
#' seasons.
#' @seealso \code{\link{rebuild.datasets}} \code{\link{fbdata-datasets}}
#' @export update.scores

update.scores <- function(dataset=NULL, all.seasons=FALSE) {

    current.season <- getOption('current.season', NULL)
    if (is.null(current.season))
        stop('required option \'current.season\' not set')

    if (is.null(dataset)) {
        dataset <- .dataset$name
    } else {
        dataset <- match.arg(dataset, .dataset$name, several.ok=TRUE)
    }

    need.update <- .source$dataset %in% dataset
    if (!all.seasons) {
        need.update <- need.update & (.source$season == current.season)
    }

    changed <- mapply(fetch.newer, .source$url[need.update],
                      .source$path[need.update])

    changed <- replace(need.update, which(need.update), changed == 1)

    for (name in unique(.source$dataset[changed])) {
        write.rdata(name, assemble(name))
        info('updated: ', name)
    }

    if (any(changed)) {
        write.metadata()
    }
}

#' Rebuild data sets
#'
#' Rebuilds R data files from the raw data files.
#'
#' Data sets must be reloaded with \code{\link{data}} for any changes
#' to make effect.  In general, it is not necessary to call this
#' function after an update, because \code{\link{update.scores}}
#' already rebuilds any data sets that need to be rebuilt.
#' @seealso \code{\link{update.scores}} \code{\link{fbdata-datasets}}
#' @export

rebuild.datasets <- function() {
    for (name in unique(.source$dataset)) {
        write.rdata(name, assemble(name))
        info('written: ', name)
    }
    write.metadata()
}

#' Write data object in the package data directory.
#' @noRd

write.rdata <- function(name, data) {
    fname <- file.path(datadir, sprintf('%s.rda', name))

    if (!file.exists(dirname(fname)))
        dir.create(dirname(fname), recursive=TRUE)

    assign(name, data)
    save(list=name, file=fname)         # save a single object named <name>
}

#' Write metadata
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
