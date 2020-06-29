# Copyright 2020 Pfizer Inc.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     https://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This file is following a template discussed in
# https://cran.rstudio.com/web/packages/DBI/vignettes/backend.html
#
# It implements a DBI driver to query lagoon-server.

#' @import DBI
#' @import methods
setClass("LagoonDriver", contains = "DBIDriver")

setMethod("dbUnloadDriver", "LagoonDriver", function(drv, ...) {
  TRUE
})

setMethod("show", "LagoonDriver", function(object) {
  cat("<LagoonDriver>\n")
})

Lagoon <- function() {
  new("LagoonDriver")
}

# Connection

setClass("LagoonConnection",
  contains = "DBIConnection",
  slots = list(
    host = "character",
    port = "character",
    prot = "character"
  )
)

setMethod("dbConnect", "LagoonDriver", function(drv, host, port, prot="http", ...) {
  new("LagoonConnection", host = host, port = port, prot = prot, ...)
})

setMethod("show", "LagoonConnection", function(object) {
  cat("<LagoonConnection>\n")
})

setMethod("dbDisconnect", "LagoonConnection", function(conn) {
  TRUE
})

#' @import DBI
#' @import methods
#' @export
setMethod("dbListTables", "LagoonConnection", function(conn, ...) {
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
setClass("LagoonResult",
  contains = "DBIResult",
  slots = list(conn = "LagoonConnection", sql = "character")
)

setMethod("dbSendQuery", "LagoonConnection", function(conn, statement, ...) {
  # cat(paste0(statement,"\n"))
  new("LagoonResult", conn = conn, sql = statement)
})

setMethod("dbClearResult", "LagoonResult", function(res, ...) {
  # free resources
  TRUE
})

#' @import jsonlite
setMethod("dbFetch", "LagoonResult", function(res, n = -1, ...) {
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

setMethod("dbHasCompleted", "LagoonResult", function(res, ...) {
  TRUE
})
