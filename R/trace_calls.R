#' trace each call with a srcref attribute
#'
#' This function calls itself recursively so it can properly traverse the AST.
#' @param x the call
#' @param parent_functions the functions which this call is a child of.
#' @param parent_ref argument used to set the srcref of the current call when recursing
#' @seealso \url{http://adv-r.had.co.nz/Expressions.html}
#' @return a modified expression with count calls inserted before each previous
#' call.
trace_calls <- function (x, parent_functions = NULL, parent_ref = NULL) {
  if (is.null(parent_functions)) {
    parent_functions <- deparse(substitute(x))
  }
  recurse <- function(y) {
    lapply(y, trace_calls, parent_functions = parent_functions)
  }

  if (is.atomic(x) || is.name(x)) {
    if (length(x) == 0 || is.null(parent_ref)) {
      x
    }
    else {
      if ((!is.symbol(x) && is.na(x)) || as.character(x) == "{") {
        x
      } else {
        key <- new_counter(parent_ref, parent_functions) # nolint
        bquote(`{`(covr:::count(.(key)), .(x)))
      }
    }
  }
  else if (is.call(x)) {
    if ((identical(x[[1]], as.name("<-")) || identical(x[[1]], as.name("="))) &&
        (is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")))) {
      parent_functions <- c(parent_functions, as.character(x[[2]]))
    }
    if (inherits(x, "if")) {
      x <- fix_srcref(x, parent_ref)
    }
    src_ref <- attr(x, "srcref")
    if (!is.null(src_ref)) {
      as.call(Map(trace_calls, x, src_ref, MoreArgs = list(parent_functions = parent_functions)))
    } else if (!is.null(parent_ref)) {
      key <- new_counter(parent_ref, parent_functions)
      bquote(`{`(covr:::count(.(key)), .(as.call(recurse(x)))))
    } else {
      as.call(recurse(x))
    }
  }
  else if (is.function(x)) {
    fun_body <- body(x)

    if(!is.null(fun_body) && !is.null(attr(x, "srcref")) &&
       (is.symbol(fun_body) || !identical(fun_body[[1]], as.name("{")))) {
      src_ref <- attr(x, "srcref")
      key <- new_counter(src_ref, parent_functions)
      fun_body <- bquote(`{`(covr:::count(.(key)), .(trace_calls(fun_body, parent_functions))))
    } else {
      fun_body <- trace_calls(fun_body, parent_functions)
    }

    new_formals <- trace_calls(formals(x), parent_functions)
    if (is.null(new_formals)) new_formals <- list()
    formals(x) <- new_formals
    body(x) <- fun_body
    x
  }
  else if (is.pairlist(x)) {
    as.pairlist(recurse(x))
  }
  else if (is.expression(x)) {
    as.expression(recurse(x))
  }
  else if (is.list(x)) {
    recurse(x)
  }
  else {
    message("Unknown language class: ", paste(class(x), collapse = "/"),
      call. = FALSE)
    x
  }
}

fix_srcref <- function(x, parent_src_ref) {
  src_ref <- attr(x, "srcref")
  if (!is.null(src_ref)) {
    return(x)
  }
  src_ref <- parent_src_ref
  lines <- paste(as.character(src_ref, useSource = TRUE), collapse = "\n")

  # we need to blank "", ``, #... so parsing the if else block doesn't get
  # confused

  # http://stackoverflow.com/a/1016356/2055486
  # \"(\\.|[^\"])*\"
  double_quote_re <-
    rex::rex(
      double_quote,
      zero_or_more(
        or(rex::rex("\\", any),
          none_of(double_quote))),
      double_quote)

  single_quote_re <-
    rex::rex(
      single_quote,
      zero_or_more(
        or(rex::rex("\\", any),
          none_of(single_quote))),
      single_quote)

  backtick_re <-
    rex::rex(
      "`",
      zero_or_more(
        or(rex::rex("\\", any),
          none_of("`"))),
      "`")

  comment_re <- rex::rex("#", except_any_of("\n"), newline)

  stripped_lines <-
    blank_pattern(pattern = double_quote_re,
    blank_pattern(pattern = single_quote_re,
    blank_pattern(pattern = backtick_re,
    blank_pattern(pattern = comment_re,
    lines))))

  # helper functions for calculating the updated lines and columns
  newline_search <-
    rex::re_matches(lines,
               rex::rex(newline),
               locations = TRUE,
               global = TRUE)[[1]]$start

  newline_locs <- c(0,
                    if (!is.na(newline_search[1])) newline_search,
                    nchar(lines) + 1)

  find_line <- function(x) {
    src_ref[1] + (which(newline_locs >= x)[1] - 1) - 1
  }

  find_column <- function(x) {
    line_number <- which(newline_locs >= x)[1] - 1
    if (line_number == 1) {
      start <- src_ref[2]
    } else {
      start <- 1
    }
    start + (x - newline_locs[line_number]) - 1
  }

  # search regex
  re <- rex::rex("if", except_any_of("("),
    rex::regex("(?<condition>\\(([^()]++|(?-2))*\\))"),
    capture(name = "if", anything))

  # if an if/else add the else to the search
  has_else <- length(x) == 4
  if (has_else) {
    re <- paste0(re, rex::rex(any_spaces, "else", any_spaces,
                       capture(name = "else", anything)))
  }

  # we need to set options = "s" to allow the dot to match a newline as well
  res <- rex::re_matches(stripped_lines, re, locations = TRUE, options = "s")

  if (is.na(res$`condition`) || is.na(res$`if`)) {
    stop("error parsing if statement ", sQuote(lines), call. = FALSE)
  }

  # the condition locations need to be modified to remove the parenthesis
  condition_src_ref <- srcref(attr(src_ref, "srcfile"),
                                   c(find_line(res$`condition.start`),
                                     find_column(res$`condition.start`),
                                     find_line(res$`condition.end`),
                                     find_column(res$`condition.end`),
                                     find_column(res$`condition.start`),
                                     find_column(res$`condition.end`),
                                     src_ref[7],
                                     src_ref[8]))

  if_src_ref <- srcref(attr(src_ref, "srcfile"),
                                   c(find_line(res$`if.start`),
                                     find_column(res$`if.start`),
                                     find_line(res$`if.end`),
                                     find_column(res$`if.end`),
                                     find_column(res$`if.start`),
                                     find_column(res$`if.end`),
                                     src_ref[7],
                                     src_ref[8]))

  attr(x, "srcref") <- list(src_ref, condition_src_ref, if_src_ref)

  if (has_else) {
    if (is.na(res$`else`)) {
      stop("error parsing else clause ", sQuote(lines), call. = FALSE)
    }
    attr(x, "srcref")[[4]] <- srcref(attr(src_ref, "srcfile"),
                                   c(find_line(res$`else.start`),
                                     find_column(res$`else.start`),
                                     find_line(res$`else.end`),
                                     find_column(res$`else.end`),
                                     find_column(res$`else.start`),
                                     find_column(res$`else.end`),
                                     src_ref[7],
                                     src_ref[8]))

  }


  x
}
#s <- "if(method == \"linear\") {
        ### linear method (no iteration!)
        #lambda <- ginv(t(X * d * q) %*% X, tol=eps) %*% (totals - as.vector(t(d) %*% X))
        #g <- 1 + q * as.vector(X %*% lambda)  # g-weights
    #} else {
        ### multiplicative method (raking) or logit method
        #lambda <- matrix(0, nrow=p)  # initial values
        ## function to determine whether teh desired accuracy has
        ## not yet been reached (to be used in the 'while' loop)
        #tolNotReached <- function(X, w, totals, tol) {
            #max(abs(crossprod(X, w) - totals) / totals) >= tol
        #}
    #}"
        #if(method == \"raking\") {
            ### multiplicative method (raking)
            ## some initial values
            #g <- rep.int(1, n)  # g-weights
            #w <- d  # sample weights
            ### iterations
            #i <- 1
            #while(!any(is.na(g)) && tolNotReached(X, w, totals, tol) && i <= maxit) {
                ## here 'phi' describes mor"
f3 <- function() {
  if (TRUE) { print("else") ; print('hi')} #else 2
  else { message ( `else` ) } # else }
  #if (TRUE) { 3 } else { 4 }
}

.counters <- new.env(parent = emptyenv())

#' initialize a new counter
#'
#' @param src_ref a \code{\link[base]{srcref}}
#' @param parent_functions the functions that this srcref is contained in.
new_counter <- function(src_ref, parent_functions) {
  key <- key(src_ref)
  .counters[[key]]$value <- 0
  .counters[[key]]$srcref <- src_ref
  .counters[[key]]$functions <- parent_functions
  key
}

#' increment a given counter
#'
#' @param key generated with \code{\link{key}}
count <- function(key) {
  .counters[[key]]$value <- .counters[[key]]$value + 1
}

#' clear all previous counters
#'
clear_counters <- function() {
  rm(envir = .counters, list=ls(envir = .counters))
}

#' Generate a key for a  call
#'
#' @param x the srcref of the call to create a key for
key <- function(x) {
  src_file <- attr(x, "srcfile")
  paste(collapse = ":", c(address(src_file), x))
}

f1 <- function() {
  f2 <- function() {
    2
  }
  f2()
}
