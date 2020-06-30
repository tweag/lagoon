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

# This file follows the template described in
# https://cran.rstudio.com/web/packages/dplyr/vignettes/new-sql-backend.html

user_attribute_name <- "datalake_user"

#' Call this function to connect to a Datalake server.
#'
#' See the documentation of sql_translate_env.DatalakeConnection to learn
#' how JSON operators are translated from R.
#'
#' @export
#' @examples
#' \dontrun{
#' require(dplyr); require(RDatalake)
#' db <- src_datalake(host = "localhost", port = "3000", user = "mary")
#' tbl(db, "someTable")
#' }
src_datalake <- function( host = "localhost", port = "22089"
                         , user = unname(Sys.info()["user"]), prot="http", ...) {
  con <- dbConnect(Datalake(), host = host, port = port, prot = prot,...)
  con2 <- src_dbi(con)
  attr(con2, user_attribute_name) <- user
  structure(con2, class=c("src_datalake", "src_dbi", "src_sql", "src"))
}

#' @export
db_desc.src_datalake <- function(src) {
  paste0("datalake", " [http://", src$con@host, ":", src$con@port, "/sql]")
}


#' @export
#' @importFrom dplyr tbl
#' @importFrom dplyr sql_quote
tbl.src_datalake <- function(src, from, ...) {
  # Previously we did a visibility check here.
  # Omitted for now. (Need to rewrite R client to use REST API.)
  tbl_sql("datalake", src = src, from = from, ...)
}

#' @export'
db_query_fields.DatalakeConnection <- function(con, from, ...) {
  dbGetQuery(con,
      build_sql( "with schema_query as ("
               , "select nspname from pg_namespace n join pg_class c "
               , "on n.oid = c.relnamespace "
               , "where c.oid = ", sql_quote(as.character(from), "\""), "::regclass )"

               , "select column_name from information_schema.columns, "
               , "schema_query where "
               , "table_name = ", as.character(from), " AND table_schema = nspname"
               , con = con))[[1]]
}

#' Translates expressions from R to SQL containing json operators.
#'
#' The infix JSON operators are surrounded with \% in R (\%->\% translates to ->
#' in SQL, \%@>\% translates to @>, etc). We translate all the operators described
#' in https://www.postgresql.org/docs/9.5/static/functions-json.html
#'
#' jsonb('{...}') is translated to PostgreSQL syntax '{...}'::jsonb.
#'
#' @examples
#' \dontrun{
#'  # Test with
#'  dbConn <- src_datalake("localhost", "3000")
#'  translate_sql( jsonb('{"a":2}')%->%'a', con=dbConn[[1]])
#' }
#' @export
#' @import dplyr
#' @import dbplyr
sql_translate_env.DatalakeConnection <- function (con) {
  sql_variant (
    sql_translator(.parent = base_scalar
      , `%->%`  = sql_infix("->")
      , `%->>%` = sql_infix("->>")
      , `%#>%`  = sql_infix("#>")
      , `%#>>%` = sql_infix("#>>")
      , `%@>%`  = sql_infix("@>")
      , `%<@%`  = sql_infix("<@")
      , `%?%`   = sql_infix("?")
      , `%?&%`  = sql_infix("?&")
      , `%#-%`  = sql_infix("#-")
      , jsonb = function(x) build_sql( x, "::jsonb")
      )
  # copied from dplyr:::sql_translate_env.PostgreSQLConnection
  , sql_translator(.parent = base_agg
      , n = function() sql("count(*)")
      , cor = sql_prefix("corr")
      , cov = sql_prefix("covar_samp")
      , sd = sql_prefix("stddev_samp")
      , var = sql_prefix("var_samp")
      , all = sql_prefix("bool_and")
      , any = sql_prefix("bool_or")
      , paste = function(x, collapse) build_sql("string_agg("
           , x, ", ", collapse, ")")
       )
  , sql_translator(.parent = base_win,
      n = function() {
        win_over(sql("count(*)"), partition = win_current_group())
      },
      cor = win_recycled("corr"),
      cov = win_recycled("covar_samp"),
      sd =  win_recycled("stddev_samp"),
      var = win_recycled("var_samp"),
      all = win_recycled("bool_and"),
      any = win_recycled("bool_or"),
      paste = function(x, collapse) {
        win_over(
          build_sql("string_agg(", x, ", ", collapse, ")"),
          partition = win_current_group(),
          order = win_current_order()
        )
      }
    )
  )
}
