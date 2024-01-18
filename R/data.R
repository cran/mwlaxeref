#' Lake identification cross-reference table
#'
#' This table contains ID values for various different datasets that reference
#' these lakes. There are local (i.e. state ID fields), National Hydrography
#' Dataset (NHDHR), and NHD COMID. There is also a field for Winslow ID, which
#' relates to the original ID in the NHD dataset (i.e. pre-NHDHR)
#'
#' @format ## lake_id_xref
#' A data.frame with 489,557 rows and 8 columns
#' \describe{
#'  \item{nhdhr.id}{Permanent identifier from NHDHR Resolution 2}
#'  \item{nhd.comid}{COMID from NHDHR}
#'  \item{winslow.id}{Permanent identifier from original NHD dataset}
#'  \item{lagos.id}{Identifier from LAke multi-scaledGeOSpatial and temporal database}
#'  \item{mglp.id}{Identifier from the Midwest Glacial Lakes Partnership}
#'  \item{local.id}{The local identifier (usually state agency)}
#'  \item{state}{The state that the local identifier corresponds to}
#'  \item{agency}{The agency from which the local.id was taken}
#' }
"lake_id_xref"


#' Example of lake ID data from Wisconsin
#'
#' This data.frame contains the names and lake IDs for 10 lakes in
#' Wisconsin. These are used in examples throughout the package.
#'
#' @format ## wis_lakes
#' A data.frame with 10 rows and 4 columns
#' \describe{
#'  \item{state}{The state abbreviation (i.e. "wi")}
#'  \item{county}{The name of the county in which the lake is found}
#'  \item{lake.id}{A unique identifier for each lake}
#'  \item{lake.name}{The name of the lake}
#' }
"wis_lakes"
