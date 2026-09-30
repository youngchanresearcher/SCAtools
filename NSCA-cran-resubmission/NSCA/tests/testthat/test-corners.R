test_that("the necessity corner is always the diagonal opposite", {
  opposite <- c(`1` = 4L, `2` = 3L, `3` = 2L, `4` = 1L)
  for (direction in c("HH", "LH", "HL", "LL")) {
    corners <- nsca_corners(direction)
    expect_identical(
      corners$necessity_corner,
      unname(opposite[as.character(corners$sufficiency_corner)]),
      info = direction
    )
  }
})

test_that("the sufficiency corner agrees with SCAtools", {
  for (direction in c("HH", "LH", "HL", "LL")) {
    expect_identical(
      nsca_corners(direction)$sufficiency_corner,
      as.integer(SCAtools::sca_corner(direction)),
      info = direction
    )
  }
})

test_that("necessity and sufficiency always bound opposite sides", {
  # One frontier must bound the point cloud from above and the other from
  # below, otherwise there is no band between them and the geometry is wrong.
  for (direction in c("HH", "LH", "HL", "LL")) {
    corners <- nsca_corners(direction)
    expect_setequal(
      c(corners$necessity_bound, corners$sufficiency_bound),
      c("upper", "lower")
    )
  }
})

test_that("directions are validated", {
  expect_error(nsca_corners("XY"), "Invalid direction")
  expect_error(nsca_corners(character()), "must contain")
  expect_identical(nsca_corners(" hh ")$direction, "HH")
})

test_that("the direction map covers all four statements", {
  map <- nsca_direction_map()
  expect_equal(nrow(map), 4L)
  expect_setequal(map$direction, c("HH", "LH", "HL", "LL"))
  expect_identical(map$nsca_number, 1:4)
  expect_identical(
    map$equivalence_class,
    c("HH/LL", "LH/HL", "LH/HL", "HH/LL")
  )
  expect_match(
    map$biconditional[map$direction == "HH"],
    "High X is necessary and sufficient for High Y"
  )
})

# ---------------------------------------------------------------- 0.4.0 terms

test_that("the direction map reproduces Table 1", {
  map <- nsca_direction_map()
  expect_equal(
    map$relationship,
    c("Higher X for higher Y", "Lower X for higher Y",
      "Higher X for lower Y", "Lower X for lower Y")
  )
  expect_equal(
    map$necessity_empty_space,
    c("Low X with high Y", "High X with high Y",
      "Low X with low Y", "High X with low Y")
  )
  expect_equal(map$necessity_boundary,
               c("ceiling", "ceiling", "floor", "floor"))
  expect_equal(
    map$sufficiency_empty_space,
    c("High X with low Y", "Low X with low Y",
      "High X with high Y", "Low X with high Y")
  )
  expect_equal(map$sufficiency_boundary,
               c("floor", "floor", "ceiling", "ceiling"))
})

test_that("the two boundaries are always one ceiling and one floor", {
  map <- nsca_direction_map()
  # The corners are diagonally opposite, so one is in the upper half and one in
  # the lower half, whatever the direction.
  expect_true(all(map$necessity_boundary != map$sufficiency_boundary))
  expect_setequal(
    unique(paste(map$necessity_boundary, map$sufficiency_boundary)),
    c("ceiling floor", "floor ceiling")
  )
  # Boundary type follows the corner and nothing else.
  expect_equal(
    map$necessity_boundary,
    ifelse(map$necessity_corner %in% c(1L, 2L), "ceiling", "floor")
  )
})

test_that("the glossary and the legacy map are self-consistent", {
  terms <- nsca_terms()
  expect_equal(ncol(terms), 3L)
  expect_true(all(c("scope", "boundary", "frontier", "admissible region",
                    "necessity-sufficiency interval", "joint support") %in%
                    terms$term))

  legacy <- nsca_legacy_names()
  expect_true(is.character(legacy))
  # Both the 0.3.0 and the 0.4.0 retirements are reachable.
  expect_equal(unname(legacy[["d_nsca"]]), "weakest_effect")
  expect_equal(unname(legacy[["data_zone_share"]]), "admissible_region_share")
  expect_equal(unname(legacy[["data_zone_width"]]),
               "necessity_sufficiency_interval")
  expect_equal(unname(legacy[["nsca_supported"]]), "joint_support")
  expect_equal(unname(legacy[["boundary"]]), "inequality")
  expect_equal(unname(legacy[["region"]]), "threshold_status")

  # The map is the documented one, exactly. NEWS, both READMEs and the package
  # guide all print this table, and `region` was missing from the code through
  # 0.4.3 without any of them noticing, so pin the whole set rather than a
  # sample of it.
  expect_setequal(
    names(legacy),
    c("d_nsca", "j_nsca", "determinacy", "weaker_side", "p_nsca",
      "undetermined_share", "data_zone_share", "data_zone_width",
      "undetermined_width", "nsca_supported", "conjunction", "region",
      "boundary")
  )
})
