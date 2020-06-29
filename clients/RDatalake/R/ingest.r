
#' Creates a configuration with connection parameters
#'
#' @param pghost The host of the PostgreSQL database.
#' @param pgport The port of the database.
#' @param pgdatabase The name of the database.
#' @param pguser The name of the database user.
#' @param pgpassword The password of the database user.
#' @param datalake_host The host of the Datalake server.
#'        Defaults to "localhost" or the value of the environment
#'        variable DATALAKE_HOST if defined.
#' @param datalake_port The port of the Datalake server.
#'        Defaults to "22089" or the value of the environment
#'        variable DATALAKE_HOST if defined.
#' @param ingest_user The user of ingest. Defaults to the system user.
#' @export
createConfig <- function( pghost = NULL
                        , pgport = NULL
                        , pgdatabase = NULL
                        , pguser = NULL
                        , pgpassword = NULL
                        , datalake_host = "localhost"
                        , datalake_port = "22089"
                        , ingest_user = unname(Sys.info()["user"])
                        ) {
  test_env <- function(v, var) {
      if (is.null(v)) {
        val <- Sys.getenv(var);
        if (val == "") v else val
      } else v
  };
  list( PG_HOST = pghost
      , PG_PORT = pgport
      , PG_DATABASE = pgdatabase
      , PG_USER = pguser
      , PG_PASSWORD = pgpassword
      , DATALAKE_HOST = test_env(datalake_host, "DATALAKE_HOST")
      , DATALAKE_PORT = test_env(datalake_port, "DATALAKE_PORT")
      , INGEST_USER = ingest_user
      )
}

# An environment to keep the default configuration mutable after the package
# is loaded.
config.env<-new.env();

#' The default configuration to use for calls in this package.
#' @export
getDefaultConfig <- function() {
  get("defaultConfiguration", config.env);
}

#' Sets the default configuration
#'
#' See the documentation of createConfig.
#' @export
setDefaultConfig <- function(config) {
    assign("defaultConfiguration", config, config.env);
}

setDefaultConfig(createConfig());

test_null <- function(p, v) { if (is.null(v)) NULL else c(p, shQuote(v)) }

ingest_db_args <- function(config) {
    c( test_null("--pghost", config$PG_HOST)
     , test_null("--pgport", config$PG_PORT)
     , test_null("--pgdatabase", config$PG_DATABASE)
     , test_null("--pguser", config$PG_USER)
     , test_null("--pgpassword", config$PG_PASSWORD)
     )
}

#' Imports data into a PostgreSQL database
#'
#' @param source_name The name to give to the source.
#' @param source_path The location of the data to import. Can be a local file
#'                    or a url.
#' @param description Free-form description for this data source. If not
#'                    provided, the source name is used instead.
#' @param tags A character vector of tags to apply to the source.
#' @param json_path Ingest part of a JSON file. A path is either '_',
#'                  '[path]' or '{"field":path}'. For example, '{"groups":[_]}'
#'                  extracts all elements of the array of the "groups" field of
#'                  the top-level object as separate rows into the database.
#' @param config specifies the database connection details. Defaults to
#'               defaultConfiguration.
#' @export
ingest <- function( source_name
                  , source_path
                  , description = NULL
                  , tags = NULL
                  , json_path = NULL
                  , config = getDefaultConfig()) {
    tag_args <- if (is.null(tags)) NULL else
        unlist(lapply(tags,function(x) c("--tag", shQuote(x))))
    system2("ingest",c( ingest_db_args(config)
                      , test_null("--description", description)
                      , tag_args
                      , test_null("--json-path", json_path)
                      , "--user", config$INGEST_USER
                      , "--name", source_name
                      , source_path
                      ))
}

#' Prints the inferred type of JSON file
#' @export
infer_json_type <- function(source_path) {
    system2("ingest", c("infer-json-type", source_path))
}

#' Yields the version information of ingest.
#' @export
ingest_version <- function() {
    system2("ingest", "--version", stdout=TRUE)
}

#' Lists the users
#' @export
#' @import dplyr
getUsers <- function(config = getDefaultConfig()) {
    dbConn <- src_datalake(config$DATALAKE_HOST, config$DATALAKE_PORT);
    tbl(dbConn, "users") %>% select(name) %>% collect;
}

#' Lists the table names along with source names
#' @export
#' @import dplyr
getTableNames <- function(config = getDefaultConfig()) {
    dbConn <- src_datalake(config$DATALAKE_HOST, config$DATALAKE_PORT);
    sourcenames <- tbl(dbConn, "sourcenames");
    users <- tbl(dbConn, "users");
    username <- config$INGEST_USER
    tbl(dbConn, "sources") %>%
      left_join(sourcenames, c( "sourcename"="ix" )) %>%
      left_join(users, c( "addedby.x"="ix" )) %>%
      # Previously this also filtered out not-visible sources here
      # Omitted for now. (Need to rewrite R client to use REST API.)
      filter(!deprecated) %>%
      select(name.x, viewname) %>%
      collect
}

#' Lists the column names of a table and their types
#' @export
#' @import dplyr
getColumns <- function(table_name, config = getDefaultConfig()) {
    dbConn <- src_datalake(config$DATALAKE_HOST, config$DATALAKE_PORT);
    sources <- tbl(dbConn, "sources");
    tbl(dbConn, "sourcecolumns") %>% left_join(sources, c("source"="ix")) %>%
      filter(viewname == table_name) %>% select(columninview, type) %>% collect
}
