#' Crosswalk lake identifiers to/from specific states
#'
#' State specific functions to be used as handy shortcuts.
#' (i.e. you can call a state's function directly rather than specifying
#' \code{local_to_*} or \code{*_to_local} with the state as an argument.
#' One of these functions exists for all combinations of both states and lake
#' ID columns found in \code{lake_id_xref}. For a full list of these functions,
#' see ?all_state_crosswalks
#'
#' @inheritParams crosswalk_lake_id
#'
#' @name state_crosswalks
#'
#' @return A data.frame the same as \code{data}, but with an additional
#' lake identification column from lake_id_xref
#'
#' @examples
#' wi_to_nhdhr(wis_lakes, from_colname = "lake.id")
NULL

#' @rdname state_crosswalks
#' @export
nhdhr_to_wi <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname state_crosswalks
#' @export
lagos_to_mi <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL,
                        id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname state_crosswalks
#' @export
mn_to_mglp <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}


#' A full list of state crosswalk functions
#'
#' This help file is simply to serve as a full list of all state shortcut
#' crosswalk functions. For functionality and examples, see
#' \code{\link{state_crosswalks}}.
#'
#' @inheritParams crosswalk_lake_id
#'
#' @name all_state_crosswalks
#' @return A data.frame the same as \code{data}, but with an additional
#' lake identification column from lake_id_xref
NULL

#' @rdname all_state_crosswalks
#' @export
comid_to_ia <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL,
                        id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_in <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_mi <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL,
                        id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_mn <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_mo <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_nd <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_sd <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
comid_to_wi <- function(data,
                        from_colname = "nhd.comid",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "comid",
      to = "local",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
ia_to_comid <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
ia_to_lagos <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
ia_to_mglp <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
ia_to_nhd <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
ia_to_nhdhr <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
in_to_comid <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "in",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
in_to_lagos <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
in_to_mglp <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
in_to_nhd <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
in_to_nhdhr <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_ia <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL,
                        id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_in <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_mn <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_mo <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_nd <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_sd <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
lagos_to_wi <- function(data,
                        from_colname = "lagos.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "lagos",
      to = "local",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_ia <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL,
                       id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_in <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_mi <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL,
                       id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_mn <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_mo <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_nd <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_sd <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mglp_to_wi <- function(data,
                       from_colname = "mglp.id",
                       agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "mglp",
      to = "local",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mi_to_comid <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mi_to_lagos <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mi_to_mglp <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mi_to_nhd <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mi_to_nhdhr <- function(data, from_colname, agency = NULL, id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mn_to_comid <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mn_to_lagos <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mn_to_nhd <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mn_to_nhdhr <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mo_to_comid <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mo_to_lagos <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mo_to_mglp <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mo_to_nhd <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
mo_to_nhdhr <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nd_to_comid <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nd_to_lagos <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nd_to_mglp <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nd_to_nhd <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nd_to_nhdhr <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_ia <- function(data,
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
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_in <- function(data,
                      from_colname = "nhd.id",
                      states = NULL,
                      agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_mi <- function(data,
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
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_mn <- function(data,
                      from_colname = "nhd.id",
                      states = NULL,
                      agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_mo <- function(data,
                      from_colname = "nhd.id",
                      states = NULL,
                      agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_nd <- function(data,
                      from_colname = "nhd.id",
                      states = NULL,
                      agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_sd <- function(data,
                      from_colname = "nhd.id",
                      states = NULL,
                      agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhd_to_wi <- function(data,
                      from_colname = "nhd.id",
                      states = NULL,
                      agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhd",
      to = "local",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_ia <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL,
                        id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "ia",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_in <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "in",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_mi <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL,
                        id_field = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "mi",
      agency = agency,
      id_field = id_field
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_mn <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "mn",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_mo <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "mo",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_nd <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "nd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
nhdhr_to_sd <- function(data,
                        from_colname = "nhdhr.id",
                        agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "nhdhr",
      to = "local",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
sd_to_comid <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
sd_to_lagos <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
sd_to_mglp <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
sd_to_nhd <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
sd_to_nhdhr <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "sd",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
wi_to_comid <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "comid",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
wi_to_lagos <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "lagos",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
wi_to_mglp <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "mglp",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
wi_to_nhd <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhd",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

#' @rdname all_state_crosswalks
#' @export
wi_to_nhdhr <- function(data, from_colname, agency = NULL) {
  out <-
    crosswalk_lake_id(
      data,
      from = "local",
      to = "nhdhr",
      from_colname = from_colname,
      states = "wi",
      agency = agency
    )
  return(out)
}

