#' Crosswalk from one lake ID to another
#'
#' These are functions to be able to quickly and easily translate data.frames
#' of lake data in the Midwest from one identifier ot another. IDs are from
#' national, regianal, and state data.sets; these include National Hydrography
#' Dataset (NHD), NHD Plus High Resolution (NHDHR), LAke multi-scaled
#' GeOSpatial and temporal database (LAGOS), the Midwest Glacial Lakes
#' Partnership (MGLP), and state resource management and/or geography/mapping
#' agencies. Local state agency lake IDs are housed under the column
#' \code{local.id} in \code{\link{lake_id_xref}} and correspond to the state
#' in which they reside and the agency that provided the data in those
#' respective columns.
#'
#' @param data A data.set with a column containing a lake identifier
#' corresponding to one of the datasets listed above (also see lake_id_xref)
#'
#' @name crosswalk_lake_id
#'
#' @return A data.frame the same as \code{data}, but with an additional
#' lake identification column from lake_id_xref
#' @export
#'
#' @examples
#' local_to_nhdhr(wis_lakes, from_colname = "lake.id", states = "wi")
nhdhr_to_local <- function(data,
                           from_colname = "nhdhr.id",
                           states = NULL,
                           agency = NULL,
                           id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
local_to_nhdhr <- function(data,
                           from_colname,
                           states = NULL,
                           agency = NULL,
                           id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhdhr_to_comid <- function(data, from_colname = "nhdhr.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "comid",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
comid_to_nhdhr <- function(data, from_colname = "nhd.comid") {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "nhdhr",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhdhr_to_nhd <- function(data, from_colname = "nhdhr.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "nhd",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhd_to_nhdhr <- function(data, from_colname = "nhd.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "nhdhr",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhdhr_to_lagos <- function(data, from_colname = "nhdhr.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "lagos",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
lagos_to_nhdhr <- function(data, from_colname = "lagos.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "nhdhr",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhdhr_to_mglp <- function(data, from_colname = "nhdhr.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "mglp",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
mglp_to_nhdhr <- function(data, from_colname = "mglp.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "nhdhr",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
comid_to_nhd <- function(data, from_colname = "nhd.comid") {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "nhd",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhd_to_comid <- function(data, from_colname = "nhd.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "comid",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
comid_to_lagos <- function(data, from_colname = "nhd.comid") {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "lagos",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
lagos_to_comid <- function(data, from_colname = "lagos.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "comid",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
comid_to_mglp <- function(data, from_colname = "nhd.comid") {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "mglp",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
mglp_to_comid <- function(data, from_colname = "mglp.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "comid",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
comid_to_local <- function(data,
                           from_colname = "nhd.comid",
                           states = NULL,
                           agency = NULL,
                           id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
local_to_comid <- function(data,
                           from_colname,
                           states = NULL,
                           agency = NULL,
                           id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhd_to_lagos <- function(data, from_colname = "nhd.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "lagos",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
lagos_to_nhd <- function(data, from_colname = "lagos.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "nhd",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhd_to_mglp <- function(data, from_colname = "nhd.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "mglp",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
mglp_to_nhd <- function(data, from_colname = "mglp.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "nhd",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
nhd_to_local <- function(data,
                         from_colname = "nhd.id",
                         states = NULL,
                         agency = NULL,
                         id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
local_to_nhd <- function(data,
                         from_colname,
                         states = NULL,
                         agency = NULL,
                         id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
lagos_to_mglp <- function(data, from_colname = "lagos.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "mglp",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
mglp_to_lagos <- function(data, from_colname = "mglp.id") {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "lagos",
      from_colname = from_colname
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
lagos_to_local <- function(data,
                           from_colname = "lagos.id",
                           states = NULL,
                           agency = NULL,
                           id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
local_to_lagos <- function(data,
                           from_colname,
                           states = NULL,
                           agency = NULL,
                           id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
mglp_to_local <- function(data,
                          from_colname = "mglp.id",
                          states = NULL,
                          agency = NULL,
                          id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname crosswalk_lake_id
#' @export
local_to_mglp <- function(data,
                          from_colname,
                          states = NULL,
                          agency = NULL,
                          id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = states,
      agency = agency,
      id_field = id_field
    )
  return(out)
}



#' @rdname crosswalk_lake_id
#' @param from Character. The name of the lake ID field from lake_id_xref that
#' you want to reference from
#' @param to Character. The name of the lake ID field from lake_id_xref that
#' you want to reference to
#' @param from_colname Character. The column name in \code{data} that lake IDs
#' that are being converted from (e.g. \code{data} has a lake ID column that
#' you are trying to convert from -- what is the name of that column)
#' @param states Character. The two digit state abbreviation in lower case (
#' i.e. "wi", "mn", "mi", etc.). This will filter the crosswalk table to only
#' the states listed in this argument.
#' @param agency Optional character vector specifying the agency to use for
#' the local ID column. This is helpful if data exist in lake_id_xref for
#' which there are more than one agency in a state that provided data
#' @param id_field Optional character vector specifying the id.field to use
#' for the local ID column. Occasionally states use more than one identifier.
#' This allows explicit choice of which identifier to use.
#' @export
crosswalk_lake_id <- function(data,
                              from, to,
                              from_colname,
                              states = NULL,
                              agency = NULL,
                              id_field = NULL) {
  if (!grepl("\\.id$", from)) {
    from <- paste0(from, ".id")
  }
  if (!grepl("\\.id$", to)) {
    to <- paste0(to, ".id")
  }
  if (grepl("comid", from)) {
    from <- "nhd.comid"
  }
  if (grepl("comid", to)) {
    to <- "nhd.comid"
  }
  xwalk <- mwlaxeref::lake_id_xref
  if ((to == "local.id" | from == "local.id")) {
    if (is.null(states)) {
      # would like to create a heuristic here to match state in data
      # using built-in state.names and state.abbr
      stop(
        "If from or to are specified as local.id, then states must be specified"
      )
    } else {
      xwalk <- dplyr::filter(xwalk, .data$state %in% states)
    }
    if (!is.null(agency)) {
      xwalk <- dplyr::filter(xwalk, .data$agency == agency)
    }
    if (!is.null(id_field)) {
      xwalk <- dplyr::filter(xwalk, .data$id.field == id_field)
    }
  }
  select_cols <- c(from, to)
  xwalk <-
    xwalk |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::matches(from), as.character)) |>
    dplyr::distinct()
  join_vars <- structure(names = from_colname, .Data = from)
  out <-
    data |>
    dplyr::mutate(dplyr::across(dplyr::matches(from_colname), as.character)) |>
    dplyr::left_join(xwalk, by = join_vars, na_matches = "never") |>
    dplyr::relocate(
      dplyr::matches(to),
      .after = dplyr::matches(from_colname)
    )
  if (nrow(out) > nrow(data)) {
    if (sys.nframe() == 1) {
      fun_call <- paste(trimws(deparse(sys.calls()[[1]])), collapse = "")
    } else {
      fun_call <- paste(
        trimws(deparse(sys.calls()[[sys.nframe() - 1]])), collapse = ""
      )
    }
    warning_message <- paste0(
      "\n  Some of records in the output may be duplicated ",
      "due to one-to-many relationships among lake identifiers.\n",
      "  i.e. newdat <- ", fun_call, "\n",
      "  nrow(newdat) > ", "nrow(",
      gsub(",.*$", "", gsub(".*\\(", "", fun_call)), ")\n",
      "  This likely means duplicated data. Proceed with caution."
    )
    if (!is.null(states) || !is.null(agency)) {
      warning_message <- paste0(
        warning_message, "\n",
        "  Some states have multiple ID fields. ",
        "Consider using the id_field argument"
      )
    }
    warning(warning_message)
  }
  return(out)
}
