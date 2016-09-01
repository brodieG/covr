test_that("Recursive Replacements Work", {
  # This checks that all functions are modified and tha the target values are
  # the same as in the input.  We don't actually check the tracing is done
  # properly

  l.funs <- list(
    function(a) 1,
    list(function(b) 2, function(3) 3, list(function(d) 4)),
    1:10, NULL,
    list(list(1:5), NULL, 1)
  )
  expect_identical(l.funs, l.ref)
  res <- replace_rec(l.funs)
  l.funs.flat <- Filter(is.function, unlist(l.funs))
  l.new.funs <- lapply(res, "[[", "new_value")
  l.rep.funs <- lapply(res, "[[", "target_value")

  expect_true(!any(mapply(identical, l.funs.flat, l.new.funs)))
  expect_true(all(mapply(identical, l.funs.flat, l.rep.funs)))



}
