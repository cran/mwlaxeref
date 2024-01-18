make_example_data <- function(states, lake.ids) {
  out <-
    lake_id_xref |>
    dplyr::filter(.data$state == states, .data$local.id %in% lake.ids)
  return(out)
}

ia_data <- make_example_data("ia", c("64", "106", "115"))
in_data <- make_example_data(
  "in", c("Big Chapman_Kosciusko", "Crystal_Greene", "Huntingburg City_Dubois")
)
mi_data <- make_example_data("mi", c("40-61", "68-142", "54-136"))
mn_data <- make_example_data("mn", c("51003900", "58001800", "38010300"))
mo_data <- make_example_data("mo", c("RA3", "RA8", "PT19"))
nd_data <- make_example_data("nd", c("254", "356", "315"))
sd_data <- make_example_data(
  "sd",
  c("SD-VM-L-SILVER_01", "SD-JA-L-CARTHAGE_01", "SD-JA-L-SOUTH_BUFFALO_01")
)
wi_data <- make_example_data("wi", c("805400", "2497000", "854300"))

test_data <- dplyr::bind_rows(
  ia_data, in_data, mi_data, mn_data, mo_data, nd_data, sd_data, wi_data
)

test_example <- function(ids, st = NULL) {
  out <- dplyr::select(test_data, dplyr::all_of(ids)) |> dplyr::distinct()
  if (!is.null(st)) {
    out <- dplyr::filter(out, .data$state %in% st)
  }
  return(out)
}


# need to test each x.id_to_y.id function for each state example above
test_that("local_to_nhdhr works correctly for each state", {
  states <- c("ia", "in", "mi", "mn", "mo", "nd", "sd", "wi")
  for (i in states) {
    expect_equal(
      local_to_nhdhr(
        test_example(c("local.id", "state"), i),
        from_colname = "local.id",
        state = i
      ),
      test_example(c("local.id", "nhdhr.id", "state"), i)
    )
  }
})

test_that("local_to_comid works correctly for each state", {
  states <- c("ia", "in", "mi", "mn", "mo", "nd", "sd", "wi")
  for (i in states) {
    expect_equal(
      # suppress the duplicate warnings in this function
      suppressWarnings(local_to_comid(
        test_example(c("local.id", "state"), i),
        from_colname = "local.id",
        state = i
      )),
      test_example(c("local.id", "nhd.comid", "state"), i)
    )
    # expect warnings due to duplicate records, but only for certain states
    if (i %in% c("ia", "in", "mo", "wi")) {
      expect_warning(
        local_to_comid(
          test_example(c("local.id", "state"), i),
          from_colname = "local.id",
          state = i
        )
      )
    }
  }
})

test_that("local_to_nhd works correctly for each state", {
  states <- c("ia", "in", "mi", "mn", "mo", "nd", "sd", "wi")
  for (i in states) {
    expect_equal(
      local_to_nhd(
        test_example(c("local.id", "state"), i),
        from_colname = "local.id",
        state = i
      ),
      test_example(c("local.id", "nhd.id", "state"), i)
    )
  }
})

test_that("local_to_lagos works correctly for each state", {
  states <- c("ia", "in", "mi", "mn", "mo", "nd", "sd", "wi")
  for (i in states) {
    expect_equal(
      local_to_lagos(
        test_example(c("local.id", "state"), i),
        from_colname = "local.id",
        state = i
      ),
      test_example(c("local.id", "lagos.id", "state"), i)
    )
  }
})

test_that("local_to_mglp works correctly for each state", {
  states <- c("ia", "in", "mi", "mn", "mo", "nd", "sd", "wi")
  for (i in states) {
    expect_equal(
      suppressWarnings(
        local_to_mglp(
          test_example(c("local.id", "state"), i),
          from_colname = "local.id",
          state = i
        )
      ),
      test_example(c("local.id", "mglp.id", "state"), i)
    )
    # expect warnings due to duplicate records, but only for certain states
    if (i %in% c("ia", "in")) {
      expect_warning(
        local_to_mglp(
          test_example(c("local.id", "state"), i),
          from_colname = "local.id",
          state = i
        )
      )
    }
  }
})

