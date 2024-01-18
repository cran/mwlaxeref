set.seed(270)
mglp_id_sample <-
  lake_id_xref |>
  dplyr::filter(!is.na(state), !is.na(mglp.id)) |>
  dplyr::group_by(state) |>
  dplyr::sample_n(2) |>
  dplyr::ungroup() |>
  dplyr::pull(mglp.id)
test_data <-
  lake_id_xref |>
  dplyr::filter(mglp.id %in% mglp_id_sample) |>
  dplyr::filter(!is.na(state))

test_example <- function(ids, st = NULL) {
  out <-
    test_data |>
    dplyr::select(dplyr::all_of(ids)) |>
    dplyr::distinct() |>
    dplyr::mutate(mglp.id = as.character(mglp.id)) |>
    dplyr::arrange(!!!rlang::syms(ids))
  if (!is.null(st)) {
    out <- dplyr::filter(out, .data$state %in% st)
  }
  return(out)
}

# need to test each x.id_to_y.id function for each state example above
test_that("mglp_to_local works correctly for each state", {
  states <- unique(test_data$state)
  for (i in states) {
    expect_equal(
      suppressWarnings(mglp_to_local(
        test_example(c("mglp.id", "state"), i),
        from_colname = "mglp.id",
        state = i
      )) |> dplyr::arrange(mglp.id, local.id),
      test_example(c("mglp.id", "local.id", "state"), i)
    )
  }
  # expect a duplication warning, but only from some states
  dup_states <-
    test_data |>
    dplyr::distinct(state, local.id, mglp.id) |>
    dplyr::count(state, mglp.id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(state) |>
    unique()
  for (i in dup_states) {
    expect_warning(
      mglp_to_local(
        test_example(c("mglp.id", "state"), i),
        state = i
      )
    )
  }
})

test_that("id_field argument works correctly", {
  mi_mglp_dup <- data.frame(mglp.id = "MIonton97651")
  mi_new_key <- cbind(mi_mglp_dup, data.frame(local.id = "27-265"))
  mi_unique_id <- cbind(mi_mglp_dup, data.frame(local.id = "13524"))
  expect_warning(mglp_to_local(mi_mglp_dup, states = "mi"))
  expect_equal(
    mglp_to_local(mi_mglp_dup, states = "mi", id_field = "NEW_KEY"),
    mi_new_key
  )
  expect_equal(
    mglp_to_local(mi_mglp_dup, states = "mi", id_field = "UNIQUE_ID"),
    mi_unique_id
  )
})

test_that("mglp_to_comid works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(mglp_to_comid(
      test_example(c("mglp.id"))
    )) |> dplyr::arrange(mglp.id, nhd.comid),
    test_example(c("mglp.id", "nhd.comid"))
  )
  expect_warning(
    mglp_to_comid(
      test_example(c("mglp.id")),
    )
  )
})

test_that("mglp_to_nhdhr works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(mglp_to_nhdhr(
      test_example(c("mglp.id")),
    )) |> dplyr::arrange(mglp.id, nhdhr.id),
    test_example(c("mglp.id", "nhdhr.id"))
  )
  # no warnings in mglp_to_nhdhr, but keeping for posterity
  # expect_warning(
  #   mglp_to_nhdhr(
  #     test_example(c("mglp.id")),
  #   )
  # )
})

test_that("mglp_to_lagos works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(mglp_to_lagos(
      test_example(c("mglp.id")),
    )) |> dplyr::arrange(mglp.id, lagos.id),
    test_example(c("mglp.id", "lagos.id"))
  )
  # no warnings in mglp_to_lagos, but keeping for posterity
  # expect_warning(
  #   mglp_to_lagos(
  #     test_example(c("mglp.id")),
  #   )
  # )
})

test_that("mglp_to_nhd works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(mglp_to_nhd(
      test_example(c("mglp.id")),
    )) |> dplyr::arrange(mglp.id, nhd.id),
    test_example(c("mglp.id", "nhd.id"))
  )
  # expect_warning(
  #   mglp_to_nhd(
  #     test_example(c("mglp.id")),
  #   )
  # )
})
