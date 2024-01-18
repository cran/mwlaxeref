set.seed(270)
nhd_id_sample <-
  lake_id_xref |>
  dplyr::filter(!is.na(state), !is.na(nhd.id)) |>
  dplyr::group_by(state) |>
  dplyr::sample_n(2) |>
  dplyr::ungroup() |>
  dplyr::pull(nhd.id)
test_data <-
  lake_id_xref |>
  dplyr::filter(nhd.id %in% nhd_id_sample) |>
  dplyr::filter(!is.na(state))

test_example <- function(ids, st = NULL) {
  out <-
    test_data |>
    dplyr::select(dplyr::all_of(ids)) |>
    dplyr::distinct() |>
    dplyr::mutate(nhd.id = as.character(nhd.id)) |>
    dplyr::arrange(!!!rlang::syms(ids))
  if (!is.null(st)) {
    out <- dplyr::filter(out, .data$state %in% st)
  }
  return(out)
}

# need to test each x.id_to_y.id function for each state example above
test_that("nhd_to_local works correctly for each state", {
  states <- unique(test_data$state)
  for (i in states) {
    expect_equal(
      suppressWarnings(nhd_to_local(
        test_example(c("nhd.id", "state"), i),
        from_colname = "nhd.id",
        state = i
      )) |> dplyr::arrange(nhd.id, local.id),
      test_example(c("nhd.id", "local.id", "state"), i)
    )
  }
  # expect a duplication warning, but only from some states
  dup_states <-
    test_data |>
    dplyr::distinct(state, local.id, nhd.id) |>
    dplyr::count(state, nhd.id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(state) |>
    unique()
  for (i in dup_states) {
    expect_warning(
      nhd_to_local(
        test_example(c("nhd.id", "state"), i),
        state = i
      )
    )
  }
})

test_that("id_field argument works correctly", {
  mi_nhd_dup <- data.frame(nhd.id = "11949795")
  mi_new_key <- cbind(mi_nhd_dup, data.frame(local.id = "27-265"))
  mi_unique_id <- cbind(mi_nhd_dup, data.frame(local.id = "13524"))
  expect_warning(nhd_to_local(mi_nhd_dup, states = "mi"))
  expect_equal(
    nhd_to_local(mi_nhd_dup, states = "mi", id_field = "NEW_KEY"),
    mi_new_key
  )
  expect_equal(
    nhd_to_local(mi_nhd_dup, states = "mi", id_field = "UNIQUE_ID"),
    mi_unique_id
  )
})

test_that("nhd_to_comid works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhd_to_comid(
      test_example(c("nhd.id"))
    )) |> dplyr::arrange(nhd.id, nhd.comid),
    test_example(c("nhd.id", "nhd.comid"))
  )
  expect_warning(
    nhd_to_comid(
      test_example(c("nhd.id")),
    )
  )
})

test_that("nhd_to_nhdhr works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhd_to_nhdhr(
      test_example(c("nhd.id")),
    )) |> dplyr::arrange(nhd.id, nhdhr.id),
    test_example(c("nhd.id", "nhdhr.id"))
  )
  # no warnings in nhd_to_nhdhr, but keeping for posterity
  # expect_warning(
  #   nhd_to_nhdhr(
  #     test_example(c("nhd.id")),
  #   )
  # )
})

test_that("nhd_to_lagos works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhd_to_lagos(
      test_example(c("nhd.id")),
    )) |> dplyr::arrange(nhd.id, lagos.id),
    test_example(c("nhd.id", "lagos.id"))
  )
  # no warnings in nhd_to_lagos, but keeping for posterity
  # expect_warning(
  #   nhd_to_lagos(
  #     test_example(c("nhd.id")),
  #   )
  # )
})

test_that("nhd_to_mglp works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(nhd_to_mglp(
      test_example(c("nhd.id")),
    )) |> dplyr::arrange(nhd.id, mglp.id),
    test_example(c("nhd.id", "mglp.id"))
  )
  expect_warning(
    nhd_to_mglp(
      test_example(c("nhd.id")),
    )
  )
})
