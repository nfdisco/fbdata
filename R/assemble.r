
#' Assemble data set
#'
#' Reads the corresponding CSV files and creates a data frame,
#' optionally saving it on disk in the package's data directory.
#'
#' @param dataset name of the data set.
#'
#' @return A data frame.
#' @noRd

assemble <- function(dataset) {

    set <- .source$dataset == dataset

    exist <- file.exists(.source$path[set])
    for (i in which(!exist)) {
        warning('missing: ', .source$path[set][i])
    }
    set[set] <- set[set] & exist

    fname <- .source$path[set]

    vars <- variables(dataset)
    idv <- vars$id == 1
    idvars <- .source[set, vars[idv, ]$value, drop=FALSE]
    names(idvars) <- vars[idv, ]$name

    noidv <- !idv
    cols <- vars$value[noidv]
    names <- vars$name[noidv]

    data.agg <- NULL

    for (i in 1:length(fname)) {

        lines <- readLines(fname[i], n=-1, encoding='latin1')
        lines <- apply.patches(fname[i], lines)
        lines <- sub(',+$','',lines)    # delete empty fields to avoid errors

        data <- read.csv(textConnection(lines),
                         sep=',', quote='\"', strip.white=TRUE, as.is=TRUE)

        if (!is.null(cols)) {
            for (col in setdiff(cols, colnames(data)))
                data <- within(data, assign(col, NA))

            data <- data[, cols]
        }

        if (!is.null(names)) {
            colnames(data) <- names
        }

        if (!is.null(idvars)) {
            for (col in names(idvars))  # undef columns???
                data <- within(data, assign(col, idvars[[col]][i]))
        }

        data.agg <- rbind(data.agg, data)
    }

    for (x in conversions(dataset)) {
        data.agg[, x[[2]]] <- do.call(  # make sure arguments are unnamed
            x[[1]], unname(data.agg[, x[[2]], drop=FALSE]))
    }

    rownames(data.agg) <- NULL

    invisible(data.agg)
}
