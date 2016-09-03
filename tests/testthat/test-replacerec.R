test_that("Recursive Replacements Work", {
  # This checks that all functions are modified and tha the target values are
  # the same as in the input.  We don't actually check the tracing is done
  # properly

  ## NOTE: TESTS NON FUNCTIONAL RIGHT NOW
  # l.funs <- list(
  #   function(a) {
  #     1 + 1
  #   },
  #   list(function(b) b, function(c) 3, list(function(d) 4)),
  #   1:10, NULL,
  #   list(list(1:5), NULL, 1)
  # )
  # res <- covr:::replacements_rec(l.funs)
  # l.funs.flat <- Filter(is.function, unlist(l.funs))
  # l.new.funs <- lapply(res, "[[", "new_value")
  # l.rep.funs <- lapply(res, "[[", "target_value")

  # # New values should all be modified versions of the originals
  # expect_true(!any(mapply(identical, l.funs.flat, l.new.funs)))
  # # Target values should all match
  # expect_true(all(mapply(identical, l.funs.flat, l.rep.funs)))
}
