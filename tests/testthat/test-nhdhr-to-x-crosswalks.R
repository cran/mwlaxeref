set.seed(270)
nhdhr_id_sample <-
  lake_id_xref |>
  dplyr::filter(!is.na(state)) |>
  dplyr::group_by(state) |>
  dplyr::sample_n(2) |>
  dplyr::ungroup() |>
  dplyr::pull(nhdhr.id)
test_data <-
  lake_id_xref |>
  dplyr::filter(nhdhr.id %in% nhdhr_id_sample) |>
  dplyr::filter(!is.na(state))

test_example <- function(ids, st = NULL) {
  out <-
    test_data |>
    dplyr::select(dplyr::all_of(ids)) |>
    dplyr::distinct() |>
    dplyr::arrange(!!!rlang::syms(ids))
  if (!is.null(st)) {
    out <- dplyr::filter(out, .data$state %in% st)
  }
  return(out)
}

# need to test each x.id_to_y.id function for each state example above
test_that("nhdhr_to_local works correctly for each state", {
  states <- unique(test_data$state)
  for (i in states) {
    expect_equal(
      suppressWarnings(nhdhr_to_local(
        test_example(c("nhdhr.id", "state"), i),
        state = i
      )) |> dplyr::arrange(nhdhr.id, local.id),
      test_example(c("nhdhr.id", "local.id", "state"), i)
    )
  }
  # expect a duplication warning, but only from some states
  dup_states <-
    test_data |>
    dplyr::distinct(state, local.id, nhdhr.id) |>
    dplyr::count(state, nhdhr.id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(state) |>
    unique()
  for (i in dup_states) {
    expect_warning(
      nhdhr_to_local(
        test_example(c("nhdhr.id", "state"), i),
        state = i
      )
    )
  }
})

test_that("id_field argument works correctly", {
  mi_nhdhr_dup <- data.frame(nhdhr.id = "123397651")
  mi_new_key <- cbind(mi_nhdhr_dup, data.frame(local.id = "27-265"))
  mi_unique_id <- cbind(mi_nhdhr_dup, data.frame(local.id = "13524"))
  expect_warning(nhdhr_to_local(mi_nhdhr_dup, states = "mi"))
  expect_equal(
    nhdhr_to_local(mi_nhdhr_dup, states = "mi", id_field = "NEW_KEY"),
    mi_new_key
  )
  expect_equal(
    nhdhr_to_local(mi_nhdhr_dup, states = "mi", id_field = "UNIQUE_ID"),
    mi_unique_id
  )
})

test_that("nhdhr_to_comid works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhdhr_to_comid(
      test_example(c("nhdhr.id")),
    )) |> dplyr::arrange(nhdhr.id, nhd.comid),
    test_example(c("nhdhr.id", "nhd.comid"))
  )
  expect_warning(
    nhdhr_to_comid(
      test_example(c("nhdhr.id")),
    )
  )
})

test_that("nhdhr_to_nhd works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhdhr_to_nhd(
      test_example(c("nhdhr.id")),
    )) |> dplyr::arrange(nhdhr.id, nhd.id),
    test_example(c("nhdhr.id", "nhd.id"))
  )
  # no warnings in nhdhr_to_nhd, but keeping for posterity
  # expect_warning(
  #   nhdhr_to_nhd(
  #     test_example(c("nhdhr.id")),
  #   )
  # )
})

test_that("nhdhr_to_lagos works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhdhr_to_lagos(
      test_example(c("nhdhr.id")),
    )) |> dplyr::arrange(nhdhr.id, lagos.id),
    test_example(c("nhdhr.id", "lagos.id"))
  )
  # no warnings in nhdhr_to_lagos, but keeping for posterity
  # expect_warning(
  #   nhdhr_to_lagos(
  #     test_example(c("nhdhr.id")),
  #   )
  # )
})

test_that("nhdhr_to_mglp works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhdhr_to_mglp(
      test_example(c("nhdhr.id")),
    )) |> dplyr::arrange(nhdhr.id, mglp.id),
    test_example(c("nhdhr.id", "mglp.id"))
  )
  expect_warning(
    nhdhr_to_mglp(
      test_example(c("nhdhr.id")),
    )
  )
})
