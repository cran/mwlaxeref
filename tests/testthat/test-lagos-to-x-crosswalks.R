set.seed(270)
lagos_id_sample <-
  lake_id_xref |>
  dplyr::filter(!is.na(state), !is.na(lagos.id)) |>
  dplyr::group_by(state) |>
  dplyr::sample_n(2) |>
  dplyr::ungroup() |>
  dplyr::pull(lagos.id)
test_data <-
  lake_id_xref |>
  dplyr::filter(lagos.id %in% lagos_id_sample) |>
  dplyr::filter(!is.na(state))

test_example <- function(ids, st = NULL) {
  out <-
    test_data |>
    dplyr::select(dplyr::all_of(ids)) |>
    dplyr::distinct() |>
    dplyr::mutate(lagos.id = as.character(lagos.id)) |>
    dplyr::arrange(!!!rlang::syms(ids))
  if (!is.null(st)) {
    out <- dplyr::filter(out, .data$state %in% st)
  }
  return(out)
}

# need to test each x.id_to_y.id function for each state example above
test_that("lagos_to_local works correctly for each state", {
  states <- unique(test_data$state)
  for (i in states) {
    expect_equal(
      suppressWarnings(lagos_to_local(
        test_example(c("lagos.id", "state"), i),
        from_colname = "lagos.id",
        state = i
      )) |> dplyr::arrange(lagos.id, local.id),
      test_example(c("lagos.id", "local.id", "state"), i)
    )
  }
  # expect a duplication warning, but only from some states
  dup_states <-
    test_data |>
    dplyr::distinct(state, local.id, lagos.id) |>
    dplyr::count(state, lagos.id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(state) |>
    unique()
  for (i in dup_states) {
    expect_warning(
      lagos_to_local(
        test_example(c("lagos.id", "state"), i),
        state = i
      )
    )
  }
})

test_that("id_field argument works correctly", {
  mi_lagos_dup <- data.frame(lagos.id = "1040")
  mi_new_key <- cbind(mi_lagos_dup, data.frame(local.id = "27-265"))
  mi_unique_id <- cbind(mi_lagos_dup, data.frame(local.id = "13524"))
  expect_warning(lagos_to_local(mi_lagos_dup, states = "mi"))
  expect_equal(
    lagos_to_local(mi_lagos_dup, states = "mi", id_field = "NEW_KEY"),
    mi_new_key
  )
  expect_equal(
    lagos_to_local(mi_lagos_dup, states = "mi", id_field = "UNIQUE_ID"),
    mi_unique_id
  )
})

test_that("lagos_to_comid works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(lagos_to_comid(
      test_example(c("lagos.id"))
    )) |> dplyr::arrange(lagos.id, nhd.comid),
    test_example(c("lagos.id", "nhd.comid"))
  )
  expect_warning(
    lagos_to_comid(
      test_example(c("lagos.id")),
    )
  )
})

test_that("lagos_to_nhdhr works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(lagos_to_nhdhr(
      test_example(c("lagos.id")),
    )) |> dplyr::arrange(lagos.id, nhdhr.id),
    test_example(c("lagos.id", "nhdhr.id"))
  )
  # no warnings in lagos_to_nhdhr, but keeping for posterity
  # expect_warning(
  #   lagos_to_nhdhr(
  #     test_example(c("lagos.id")),
  #   )
  # )
})

test_that("lagos_to_nhd works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(lagos_to_nhd(
      test_example(c("lagos.id")),
    )) |> dplyr::arrange(lagos.id, nhd.id),
    test_example(c("lagos.id", "nhd.id"))
  )
  # no warnings in lagos_to_nhd, but keeping for posterity
  # expect_warning(
  #   lagos_to_nhd(
  #     test_example(c("lagos.id")),
  #   )
  # )
})

test_that("lagos_to_mglp works correctly for each state", {
  states <- unique(test_data$state)
  expect_equal(
    # suppress the duplicate warnings in this function
    suppressWarnings(lagos_to_mglp(
      test_example(c("lagos.id")),
    )) |> dplyr::arrange(lagos.id, mglp.id),
    test_example(c("lagos.id", "mglp.id"))
  )
  expect_warning(
    lagos_to_mglp(
      test_example(c("lagos.id")),
    )
  )
})
