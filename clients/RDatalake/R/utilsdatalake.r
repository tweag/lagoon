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

## Some abstracted helper functions to query table metadata and retrieve tables
## without requiring detailed knowledge of schema relationships.


#' Lists all versions of public or personal datasets
#'
#' @param service The host of the datalake RESTful service.
#'
#' @export
#' @import dplyr
#' @import jsonlite
list_versions <- function(service) {
    if (class(service)[1] != "src_datalake") {
        cat("Argument needs to be a src_datalake object which links to the REST service\n")
        return(NULL)
    }
    userid <- attr(service, 'datalake_user')
    return(
        tbl(service, 'sources')
        %>% left_join((tbl(service, 'users')
                %>% rename(addedby = ix, user = name)), by = 'addedby')
        %>% left_join(tbl(service, 'sourcenames')
                      %>% rename(sourcename = ix)
                      %>% select(-addedby)
                     , by = c('sourcename'))
        # Previously we filtered out private sources.
        # Omitted for now. (Need to rewrite R client to use REST API.)
        %>% filter(!deprecated)
        %>% as.data.frame
        %>% select(-addedby, -sourcename, -created, -schema, -starts_with('cached_'))
    )
}

#' Lists versions of datasets - used by get_table() to allow
#' private data retrieval if name is known (not exported)
#'
#' @import dplyr
#' @import jsonlite
list_versions2 <- function(service) {
    if (class(service)[1] != "src_datalake") {
        cat("Argument needs to be a src_datalake object which links to the REST service\n")
        return(NULL)
    }
    userid <- attr(service, 'datalake_user')
    return(
        tbl(service, 'sources')
        %>% left_join((tbl(service, 'users')
                %>% rename(addedby = ix, user = name)), by = 'addedby')
        %>% left_join(tbl(service, 'sourcenames')
                      %>% rename(sourcename = ix)
                      %>% select(-addedby)
                     , by = c('sourcename'))
        %>% as.data.frame
        %>% select(-addedby, -sourcename, -created, -schema, -starts_with('cached_'))
    )
}

#' Lists all public or personal datasets
#'
#' @param service The host of the datalake RESTful service.
#'
#' @param verbose If TRUE, returns detailed metadata for datasets, if FALSE (default) only shows core features (url if remote data source, versions, dataset owner, and dataset name).
#'
#' @export
#' @import dplyr
#' @import jsonlite
list_tables <- function(service, verbose = FALSE) {
    if (class(service)[1] != "src_datalake") {
        cat("First argument needs to be a src_datalake object which links to the REST service\n")
        return(NULL)
    }
    meta <- (list_versions(service)
        %>% group_by(name)
        %>% do({
                tmp <- .
                (tmp %>% filter(version == max(version)))
            })
        %>% as.data.frame)
    if (verbose) {
        return(meta)
    } else {
        ## put visibility in once access is implemented
        return(meta %>% select(-description, -ix, -viewname, -tablename, -deprecated))
    }
}


#' Lists datasets - used by get_table() to allow
#' private data retrieval if name is known (not exported)
#'
#' @import dplyr
#' @import jsonlite
list_tables2 <- function(service, verbose = FALSE) {
    if (class(service)[1] != "src_datalake") {
        cat("First argument needs to be a src_datalake object which links to the REST service\n")
        return(NULL)
    }
    meta <- (list_versions2(service)
        %>% group_by(name)
        %>% do({
                tmp <- .
                (tmp %>% filter(version == max(version)))
            })
        %>% as.data.frame)
    if (verbose) {
        return(meta)
    } else {
        ## put visibility in once access is implemented
        return(meta %>% select(-description, -ix, -viewname, -tablename, -deprecated))
    }
}

#' @export
#' @import dplyr
#' @import jsonlite
query_tables <- function(service, query) {
    if (class(service)[1] != "src_datalake") {
        cat("First argument needs to be a src_datalake object which links to the REST service\n")
        return(NULL)
    }
    return(
        list_tables(service, verbose = TRUE) %>% filter(grepl(query, user)
                                                  | grepl(query, description)
                                                  | grepl(query, name))
    )
}

view_exists <- function(service, viewRequest) {
    (tbl(service, 'typedsources')
        %>% select(viewname)
        %>% filter(viewname == viewRequest)
        %>% as.data.frame %>% nrow) > 0
}

#' Retrieves a specific dataset by name.
#'
#' @param service The host of the datalake RESTful service.
#'
#' @param tableName The name of the datset to retrieve.
#'
#' @param version The version of the dataset to retrieve. If the value is NA, it
#'     retrieves the most recent version. For reproducible analyses, it is
#'     recommended to explicitly specify a dataset version
#'
#' @param verbose If TRUE, print details of the data retrieval, useful if there
#'     is a problem with a retrieval (FALSE by default).
#'
#' @export
#' @import dplyr
#' @import jsonlite
get_table <- function(service, tableName, version = NA, verbose = FALSE) {
    if (class(service)[1] != "src_datalake") {
        cat("First argument needs to be a src_datalake object which links to the REST service\n")
        return(NULL)
    }
    candidates <- list_tables2(service, verbose = TRUE) %>% filter(name == tableName)
    if (verbose) {
        cat('Dataset versions:\n')
        print(candidates)
    }
    if (is.na(version)) {
        view <- candidates %>% filter(version == max(version)) %>% .$viewname
    } else {
        view <- candidates %>% filter(version == version) %>% .$viewname
    }
    if (verbose) {
        cat('Retrieving :\n')
        print(view)
    }
    if (view_exists(service, paste0(view, '_typed'))) {
        return(tbl(service, paste0(view, '_typed')) %>% select(-ix))
    } else {
        cat('Retrieving a json / untyped table\n')
        return(tbl(service, view) %>% select(-ix))
    }
}

#' Raw sql fallback for when dplyr cannot express desired query
#'
#' @export
#' @import jsonlite
sql_direct <- function(sql, host, port) {
    req <- httr::POST(paste0("http://", host, ":", port, "/sql")
                    , httr::add_headers("Content-Type" = "application/json")
                    , body = paste0('{ "sql" : "', gsub('"', '\\\\"', sql), '" }')
                      );
    txt <- httr::content(req, as = "text", encoding = "UTF-8");
    d <- fromJSON(txt, simplifyDataFrame=FALSE);
    d <- do.call(rbind, d);
    d <- data.frame( d, stringsAsFactors=FALSE)
    is_vectorizeable <- function(x) is.atomic(x) && length(x) == 1 && !is.null(x);
    if (dim(d)[2] > 0) {
      for(i in c(1:dim(d)[2])) {
        if (all(sapply(d[[i]], is_vectorizeable)))
          d[[i]] <- do.call(c, d[[i]]);
      }
    }
    return(d)
}
