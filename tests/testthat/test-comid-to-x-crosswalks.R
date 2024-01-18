set.seed(270)
comid_sample <-
  lake_id_xref |>
  dplyr::filter(!is.na(state), !is.na(nhd.comid)) |>
  dplyr::group_by(state) |>
  dplyr::sample_n(2) |>
  dplyr::ungroup() |>
  dplyr::pull(nhd.comid)
test_data <-
  lake_id_xref |>
  dplyr::filter(nhd.comid %in% comid_sample) |>
  dplyr::filter(!is.na(state))

test_example <- function(ids, st = NULL) {
  out <-
    test_data |>
    dplyr::select(dplyr::all_of(ids)) |>
    dplyr::distinct() |>
    dplyr::mutate(nhd.comid = as.character(nhd.comid)) |>
    dplyr::arrange(!!!rlang::syms(ids))
  if (!is.null(st)) {
    out <- dplyr::filter(out, .data$state %in% st)
  }
  return(out)
}

# need to test each x.id_to_y.id function for each state example above
test_that("comid_to_local works correctly for each state", {
  states <- unique(test_data$state)
  for (i in states) {
    expect_equal(
      suppressWarnings(comid_to_local(
        test_example(c("nhd.comid", "state"), i),
        from_colname = "nhd.comid",
        state = i
      )) |> dplyr::arrange(nhd.comid, local.id),
      test_example(c("nhd.comid", "local.id", "state"), i)
    )
  }
  # expect a duplication warning, but only from some states
  dup_states <-
    test_data |>
    dplyr::distinct(state, local.id, nhd.comid) |>
    dplyr::count(state, nhd.comid) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(state) |>
    unique()
  for (i in dup_states) {
    expect_warning(
      comid_to_local(
        test_example(c("nhd.comid", "state"), i),
        state = i
      )
    )
  }
})

test_that("id_field argument works correctly", {
  mi_comid_dup <- data.frame(nhd.comid = "11949795")
  mi_new_key <- cbind(mi_comid_dup, data.frame(local.id = "27-265"))
  mi_unique_id <- cbind(mi_comid_dup, data.frame(local.id = "13524"))
  expect_warning(comid_to_local(mi_comid_dup, states = "mi"))
  # nhd.comid just has a lot of duplicates (maybe due to chains? not sure)
  expect_warning(
    comid_to_local(mi_comid_dup, states = "mi", id_field = "NEW_KEY")
  )
  expect_warning(
    comid_to_local(mi_comid_dup, states = "mi", id_field = "UNIQUE_ID")
  )
})

test_that("comid_to_nhdhr works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(comid_to_nhdhr(
      test_example(c("nhd.comid")),
    )) |> dplyr::arrange(nhd.comid, nhdhr.id),
    test_example(c("nhd.comid", "nhdhr.id"))
  )
})

test_that("comid_to_nhd works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(comid_to_nhd(
      test_example(c("nhd.comid")),
    )) |> dplyr::arrange(nhd.comid, nhd.id),
    test_example(c("nhd.comid", "nhd.id"))
  )
  expect_warning(
    comid_to_nhd(
      test_example(c("nhd.comid")),
    )
  )
})

test_that("comid_to_lagos works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(comid_to_lagos(
      test_example(c("nhd.comid")),
    )) |> dplyr::arrange(nhd.comid, lagos.id),
    test_example(c("nhd.comid", "lagos.id"))
  )
})

test_that("comid_to_mglp works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(comid_to_mglp(
      test_example(c("nhd.comid")),
    )) |> dplyr::arrange(nhd.comid, mglp.id),
    test_example(c("nhd.comid", "mglp.id"))
  )
  expect_warning(
    comid_to_mglp(
      test_example(c("nhd.comid"))
    )
  )
})
