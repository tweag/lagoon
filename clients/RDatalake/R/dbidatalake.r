
# This file is following a template discussed in
# https://cran.rstudio.com/web/packages/DBI/vignettes/backend.html
#
# It implements a DBI driver to query datalake-server.

#' @import DBI
#' @import methods
setClass("DatalakeDriver", contains = "DBIDriver")

setMethod("dbUnloadDriver", "DatalakeDriver", function(drv, ...) {
  TRUE
})

setMethod("show", "DatalakeDriver", function(object) {
  cat("<DatalakeDriver>\n")
})

Datalake <- function() {
  new("DatalakeDriver")
}

# Connection

setClass("DatalakeConnection",
  contains = "DBIConnection",
  slots = list(
    host = "character",
    port = "character",
    prot = "character"
  )
)

setMethod("dbConnect", "DatalakeDriver", function(drv, host, port, prot="http", ...) {
  new("DatalakeConnection", host = host, port = port, prot = prot, ...)
})

setMethod("show", "DatalakeConnection", function(object) {
  cat("<DatalakeConnection>\n")
})

setMethod("dbDisconnect", "DatalakeConnection", function(conn) {
  TRUE
})

#' @import DBI
#' @import methods
#' @export
setMethod("dbListTables", "DatalakeConnection", function(conn, ...) {
    out <- dbGetQuery(conn, paste("select tablename from pg_tables where schemaname !='information_schema'"
                                 ,"and schemaname !='pg_catalog'", ...))
    if (is.null(out) || nrow(out) == 0)
        out <- character(0)
    else out <- out[, 1]
    out
})

# Queries

#' @import DBI
#' @import methods
#' @export
setClass("DatalakeResult",
  contains = "DBIResult",
  slots = list(conn = "DatalakeConnection", sql = "character")
)

setMethod("dbSendQuery", "DatalakeConnection", function(conn, statement, ...) {
  # cat(paste0(statement,"\n"))
  new("DatalakeResult", conn = conn, sql = statement)
})

setMethod("dbClearResult", "DatalakeResult", function(res, ...) {
  # free resources
  TRUE
})

#' @import jsonlite
setMethod("dbFetch", "DatalakeResult", function(res, n = -1, ...) {
  req <- httr::POST(paste0(res@conn@prot,"://", res@conn@host, ":", res@conn@port, "/sql")
            , httr::add_headers("Content-Type" = "application/json")
            , body = paste0('{ "sql" : "', gsub('"', '\\\\"', res@sql), '" }')
            );
  if (httr::status_code(req) == 200L) {
    txt <- httr::content(req, as = "text", encoding = "UTF-8");
    # Yields one list per row. List elements might be lists themselves which represent
    # values at each column.
    d <- fromJSON(txt, simplifyDataFrame=FALSE);
    # Transposes the lists so we get one list per column instead.
    d <- do.call(rbind, d);
    d <- data.frame( d, stringsAsFactors=FALSE)
    if (dim(d)[2] > 0) {
      # Convert columns to atomic vectors where that is possible. Columns display
      # better when they are atomic.
      # There might be a better way to do the conversion, but I couldn't find
      # one which satisfy the following criteria:
      #
      # 1. must convert lists of atomic values to atomic vectors.
      # 2. must leave the original list structure unmodified when any of the
      #    elements is not atomic.
      #
      is_vectorizeable <- function(x) is.atomic(x) && length(x) == 1 && !is.null(x);
      for(i in c(1:dim(d)[2])) {
        if (all(sapply(d[[i]],is_vectorizeable)))
          d[[i]] <- do.call(c,d[[i]]);
      }
    }
    d
  } else
    stop(httr::content(req, as = "text", encoding = "UTF-8"), call.=FALSE);
})

setMethod("dbHasCompleted", "DatalakeResult", function(res, ...) {
  TRUE
})
