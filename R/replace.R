## Recursively Generate Fun Replacement Data
##
## Note that `name` and `env` are not actually necessary or really used, so we
## just provide a dummy name and omit the environment.  Any changes to
## `replacement` may require modifying what we do here.
##
## Will recurse through lists, pair.lists, and attributes, and return a flat
## list of all the functions that need replacing with their traced replacement
## and the original value.  This does not actually execute the replacement.  For
## that to happen `replace` must be run on each element of the return value.
##
## Note we use \"identity\" as a dummy value for elements; doesn't seem to
## affect anything.

replacement_rec <- function(target_value) {
  c(
    if(is.list(target_value) || is.pairlist(target_value)) {
      unlist(
        lapply(
          target_value,
          function(x)
            if(is.recursive(x)) replacement_rec(x) else
              list(replacement(target_value=x, name="identity"))
        ),
        recursive=FALSE
      )
    } else if (is.environment(target_value)) {
      # In order to implement this we need an environment tracking registry to
      # avoid infinite loops when there are referential loops within
      # environments

      NULL
    } else {
      list(replacement(target_value=target_value, name="identity"))
    },
    if(
      length(attrs <- attributes(target_value)) &&
      length(attrs.not.names <- which(names(attrs) != "names"))
    ) {
      replacement_rec(attrs[attrs.not.names])
    }
  )
}

#' @useDynLib covr covr_duplicate_

NULL

## Generate a Replacement List
##
## Returns a list with a copy of the original function, traced function, and the
## SEXP that needs to be replaced.  Actual replacement is done by `replace`.
##
## @name character, presumably name of function but can seemingly be anything

replacement <- function(name, env = as.environment(-1), target_value = get(name, envir = env)) {

  if (is.function(target_value) && !is.primitive(target_value)) {
    if (is_vectorized(target_value)) {
      new_value <- target_value
      environment(new_value)$FUN <- trace_calls(environment(new_value)$FUN, name)
    } else if (is.function(target_value) && inherits(target_value, "memoised")) {
      new_value <- target_value
      environment(new_value)$`_f` <- trace_calls(environment(new_value)$`_f`, name)
    } else {
      new_value <- trace_calls(target_value, name)
    }
    attributes(new_value) <- attributes(target_value)

    if (isS4(target_value)) {
      new_value <- asS4(new_value)
    }

    list(
      env = env,
      name = as.name(name),
      orig_value = .Call(covr_duplicate_, target_value),
      target_value = target_value,
      new_value = new_value
    )
  }
}

#' @useDynLib covr covr_reassign_function
replace <- function(replacement) {
  .Call(covr_reassign_function, replacement$name, replacement$env, replacement$target_value, replacement$new_value)
}

#' @useDynLib covr covr_reassign_function
reset <- function(replacement) {
  .Call(covr_reassign_function, replacement$name, replacement$env, replacement$target_value, replacement$orig_value)
}
