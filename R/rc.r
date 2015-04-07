
baseurl <- "http://www.football-data.co.uk"
current.season <- '2013-14'

#' Raw data
#'
#' @format Data frame with columns
#' \describe{
#'   \item{url}{location of the remote file}
#'   \item{dataset}{data set where the data belongs}
#'   \item{season}{the season the data is from}
#'   \item{league}{league where the data comes from}
#'   \item{path}{location of the local copy}
#' }
#' Note: \code{path} is added automatically by \code{.onLoad}.
#' @noRd

.source <-
read.table(text='
url                   dataset     season   league
mmz4281/0001/D1.csv   scores.d1   2000-01  d1
mmz4281/0102/D1.csv   scores.d1   2001-02  d1
mmz4281/0203/D1.csv   scores.d1   2002-03  d1
mmz4281/0304/D1.csv   scores.d1   2003-04  d1
mmz4281/0405/D1.csv   scores.d1   2004-05  d1
mmz4281/0506/D1.csv   scores.d1   2005-06  d1
mmz4281/0607/D1.csv   scores.d1   2006-07  d1
mmz4281/0708/D1.csv   scores.d1   2007-08  d1
mmz4281/0809/D1.csv   scores.d1   2008-09  d1
mmz4281/0910/D1.csv   scores.d1   2009-10  d1
mmz4281/1011/D1.csv   scores.d1   2010-11  d1
mmz4281/1112/D1.csv   scores.d1   2011-12  d1
mmz4281/1213/D1.csv   scores.d1   2012-13  d1
mmz4281/1314/D1.csv   scores.d1   2013-14  d1
mmz4281/0001/E0.csv   scores.e0   2000-01  e0
mmz4281/0102/E0.csv   scores.e0   2001-02  e0
mmz4281/0203/E0.csv   scores.e0   2002-03  e0
mmz4281/0304/E0.csv   scores.e0   2003-04  e0
mmz4281/0405/E0.csv   scores.e0   2004-05  e0
mmz4281/0506/E0.csv   scores.e0   2005-06  e0
mmz4281/0607/E0.csv   scores.e0   2006-07  e0
mmz4281/0708/E0.csv   scores.e0   2007-08  e0
mmz4281/0809/E0.csv   scores.e0   2008-09  e0
mmz4281/0910/E0.csv   scores.e0   2009-10  e0
mmz4281/1011/E0.csv   scores.e0   2010-11  e0
mmz4281/1112/E0.csv   scores.e0   2011-12  e0
mmz4281/1213/E0.csv   scores.e0   2012-13  e0
mmz4281/1314/E0.csv   scores.e0   2013-14  e0
mmz4281/0001/I1.csv   scores.i1   2000-01  i1
mmz4281/0102/I1.csv   scores.i1   2001-02  i1
mmz4281/0203/I1.csv   scores.i1   2002-03  i1
mmz4281/0304/I1.csv   scores.i1   2003-04  i1
mmz4281/0405/I1.csv   scores.i1   2004-05  i1
mmz4281/0506/I1.csv   scores.i1   2005-06  i1
mmz4281/0607/I1.csv   scores.i1   2006-07  i1
mmz4281/0708/I1.csv   scores.i1   2007-08  i1
mmz4281/0809/I1.csv   scores.i1   2008-09  i1
mmz4281/0910/I1.csv   scores.i1   2009-10  i1
mmz4281/1011/I1.csv   scores.i1   2010-11  i1
mmz4281/1112/I1.csv   scores.i1   2011-12  i1
mmz4281/1213/I1.csv   scores.i1   2012-13  i1
mmz4281/1314/I1.csv   scores.i1   2013-14  i1
mmz4281/0001/SP1.csv  scores.sp1  2000-01  sp1
mmz4281/0102/SP1.csv  scores.sp1  2001-02  sp1
mmz4281/0203/SP1.csv  scores.sp1  2002-03  sp1
mmz4281/0304/SP1.csv  scores.sp1  2003-04  sp1
mmz4281/0405/SP1.csv  scores.sp1  2004-05  sp1
mmz4281/0506/SP1.csv  scores.sp1  2005-06  sp1
mmz4281/0607/SP1.csv  scores.sp1  2006-07  sp1
mmz4281/0708/SP1.csv  scores.sp1  2007-08  sp1
mmz4281/0809/SP1.csv  scores.sp1  2008-09  sp1
mmz4281/0910/SP1.csv  scores.sp1  2009-10  sp1
mmz4281/1011/SP1.csv  scores.sp1  2010-11  sp1
mmz4281/1112/SP1.csv  scores.sp1  2011-12  sp1
mmz4281/1213/SP1.csv  scores.sp1  2012-13  sp1
mmz4281/1314/SP1.csv  scores.sp1  2013-14  sp1
mmz4281/0001/F1.csv   scores.f1   2000-01  f1
mmz4281/0102/F1.csv   scores.f1   2001-02  f1
mmz4281/0203/F1.csv   scores.f1   2002-03  f1
mmz4281/0304/F1.csv   scores.f1   2003-04  f1
mmz4281/0405/F1.csv   scores.f1   2004-05  f1
mmz4281/0506/F1.csv   scores.f1   2005-06  f1
mmz4281/0607/F1.csv   scores.f1   2006-07  f1
mmz4281/0708/F1.csv   scores.f1   2007-08  f1
mmz4281/0809/F1.csv   scores.f1   2008-09  f1
mmz4281/0910/F1.csv   scores.f1   2009-10  f1
mmz4281/1011/F1.csv   scores.f1   2010-11  f1
mmz4281/1112/F1.csv   scores.f1   2011-12  f1
mmz4281/1213/F1.csv   scores.f1   2012-13  f1
mmz4281/1314/F1.csv   scores.f1   2013-14  f1
mmz4281/0001/N1.csv   scores.n1   2000-01  n1
mmz4281/0102/N1.csv   scores.n1   2001-02  n1
mmz4281/0203/N1.csv   scores.n1   2002-03  n1
mmz4281/0304/N1.csv   scores.n1   2003-04  n1
mmz4281/0405/N1.csv   scores.n1   2004-05  n1
mmz4281/0506/N1.csv   scores.n1   2005-06  n1
mmz4281/0607/N1.csv   scores.n1   2006-07  n1
mmz4281/0708/N1.csv   scores.n1   2007-08  n1
mmz4281/0809/N1.csv   scores.n1   2008-09  n1
mmz4281/0910/N1.csv   scores.n1   2009-10  n1
mmz4281/1011/N1.csv   scores.n1   2010-11  n1
mmz4281/1112/N1.csv   scores.n1   2011-12  n1
mmz4281/1213/N1.csv   scores.n1   2012-13  n1
mmz4281/1314/N1.csv   scores.n1   2013-14  n1
mmz4281/0001/P1.csv   scores.p1   2000-01  p1
mmz4281/0102/P1.csv   scores.p1   2001-02  p1
mmz4281/0203/P1.csv   scores.p1   2002-03  p1
mmz4281/0304/P1.csv   scores.p1   2003-04  p1
mmz4281/0405/P1.csv   scores.p1   2004-05  p1
mmz4281/0506/P1.csv   scores.p1   2005-06  p1
mmz4281/0607/P1.csv   scores.p1   2006-07  p1
mmz4281/0708/P1.csv   scores.p1   2007-08  p1
mmz4281/0809/P1.csv   scores.p1   2008-09  p1
mmz4281/0910/P1.csv   scores.p1   2009-10  p1
mmz4281/1011/P1.csv   scores.p1   2010-11  p1
mmz4281/1112/P1.csv   scores.p1   2011-12  p1
mmz4281/1213/P1.csv   scores.p1   2012-13  p1
mmz4281/1314/P1.csv   scores.p1   2013-14  p1
', header=TRUE, as.is=TRUE)


#' Data sets
#'
#' @format Data frame with columns
#' \describe{
#'   \item{name}{name of the data set}
#'   \item{vars}{name of variables table}
#'   \item{desc}{description of the data set}
#'   \item{path}{location of the data file}
#' }
#' Note: \code{path} is added automatically by \code{.onLoad}.
#' @noRd


.dataset <-
read.table(text='
name        vars     desc
scores.d1   .scores  "Bundesliga scores & match statistics"
scores.e0   .scores  "Premier League scores & match statistics"
scores.f1   .scores  "Ligue 1 scores & match statistics"
scores.i1   .scores  "Serie A scores & match statistics"
scores.n1   .scores  "Eredivisie scores & match statistics"
scores.p1   .scores  "Primeira Liga scores & match statistics"
scores.sp1  .scores  "La Liga scores & match statistics"
', header=TRUE, as.is=TRUE)


#' Variables
#'
#' @format Data frame with columns
#' \describe{
#'   \item{name}{name of the variable}
#'   \item{value}{a name of a variable from the original data, for
#' measurement variables, or a column name from the source table, for
#' identifier variables}
#'   \item{id}{equal to one for identifier variables}
#'   \item{conv}{conversion code}
#'   \item{desc}{description of the variable}
#' }
#' @noRd

.scores <-
read.table(text='
name    value     id  conv  desc
Date    Date      0   1     "Date"
HT      HomeTeam  0   2     "Home team"
AT      AwayTeam  0   2     "Away team"
HG      FTHG      0   3     "Full-time home team goals"
AG      FTAG      0   4     "Full-time away team goals"
HTHG    HTHG      0   5     "Half-time home team goals"
HTAG    HTAG      0   6     "Half-time away team goals"
HS      HS        0   7     "Home team shots"
AS      AS        0   8     "Away team shots"
HST     HST       0   9     "Home team shots on target"
AST     AST       0   10    "Away team shots on target"
HSW     HSW       0   11    "Home team shots hit woodwork"
ASW     ASW       0   12    "Away team shots hit woodwork"
HC      HC        0   13    "Home team corners"
AC      AC        0   14    "Away team corners"
HF      HF        0   15    "Home team fouls"
AF      AF        0   16    "Away team fouls"
HY      HY        0   17    "Home team yellow cards"
AY      AY        0   18    "Away team yellow cards"
HR      HR        0   19    "Home team red cards"
AR      AR        0   20    "Away team red cards"
Div     league    1   21    "Division"
Season  season    1   22    "Season"
', header=TRUE, as.is=TRUE)

#' Conversions
#'
#' @format Data frame with columns
#' \describe{
#'   \item{conv}{conversion code}
#'   \item{func}{function name}
#' }
#' @noRd

.conversion <-
read.table(text='
conv  func
1     conv.dates
2     conv.names
3     as.integer
4     as.integer
5     as.integer
6     as.integer
7     as.integer
8     as.integer
9     as.integer
10    as.integer
11    as.integer
12    as.integer
13    as.integer
14    as.integer
15    as.integer
16    as.integer
17    as.integer
18    as.integer
19    as.integer
20    as.integer
21    as.factor
22    as.ordered
', header=TRUE, as.is=TRUE)


#' Build a path using the host and the path components of a URL
#' @import httr
#' @noRd

url2path <- function(url) {
    sapply(url, function(url) {
        parts <- parse_url(url)
        do.call(file.path, as.list(
            c(parts$host, strsplit(parts$path, '/', fixed=TRUE)[[1]])))
    }, USE.NAMES=FALSE)
}


#' Return true if string is a URL
#' @import httr
#' @noRd

is.url <- function(x) {
    sapply(x, function(x) !is.null(parse_url(x)$scheme),
           USE.NAMES=FALSE)
}

#' Return variables table
#' @noRd

variables <- function(dataset) {
    get(.dataset[.dataset$name == dataset, 'vars'])
}

#' Return list of variable conversions
#' @noRd

conversions <- function(dataset) {
    vars <- variables(dataset)
    lapply(unique(na.omit(vars$conv)),
           function(v) list(with(.conversion, func[conv == v]),
                            with(vars, name[na.false(conv == v)])))
}


#' Date object from character
#' @noRd

conv.dates <- function(x) {
    as.Date(x, format={              # some files have 2-digit years
        ifelse(grepl('^../../....$', x), '%d/%m/%Y', '%d/%m/%y')
    })
}

#' Character to factor with shared and abbreviated level labels.
#'
#' @return Factor or a list of factors, depending on the number or
#' arguments.
#' @noRd

conv.names <- function(...) {
    args <- list(...)
    names <- Reduce(union, args, init=c())
    out <- lapply(args, factor, levels=names,
                  labels=abbreviate(names, minlength=5))
    if (length(out) == 1)
        return(out[[1]])
    out
}

metadir <- "../Meta"
datadir <- "../inst/data"
extdatadir <- "../inst/extdata"

.onLoad <- function(libname, pkgname) {

    pkgdir <- system.file(package=pkgname)
    metadir <<- file.path(pkgdir, "Meta")
    datadir <<- file.path(pkgdir, "data")
    extdatadir <<- file.path(pkgdir, "extdata")

    .source <<- within(.source, {
        url <- ifelse(is.url(url), url, sprintf("%s/%s", baseurl, url))
        path <- file.path(extdatadir, url2path(url))
    })

    .dataset <<- within(.dataset, {
        path <- file.path(datadir, sprintf('%s', name))
    })

    write.metadata()
}
