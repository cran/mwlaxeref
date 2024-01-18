## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(mwlaxeref)

## ----example_data-------------------------------------------------------------
head(wis_lakes, n = 3)

## ----basic_use----------------------------------------------------------------
nhdhr_ids <- local_to_nhdhr(wis_lakes, from_colname = "lake.id", states = "wi")
head(nhdhr_ids, n = 3)

## ----lagos--------------------------------------------------------------------
nhdhr_ids <- nhdhr_ids[, "nhdhr.id"]
lagos_ids <- nhdhr_to_lagos(nhdhr_ids)
head(lagos_ids, n = 3)

## ----id_fields----------------------------------------------------------------
head(lake_id_xref, n = 3)

## ----wi_to_mglp---------------------------------------------------------------
lagos_id <- wi_to_lagos(wis_lakes, from_colname = "lake.id")
head(lagos_id, n = 3)

## ----mi_dups------------------------------------------------------------------
mi_nhdhr <- data.frame(nhdhr.id = "123397651")
nhdhr_to_mi(mi_nhdhr)

## ----mi_new_key---------------------------------------------------------------
nhdhr_to_mi(mi_nhdhr, id_field = "NEW_KEY")

