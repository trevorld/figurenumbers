# Picks operands and an arithmetic operation (addition, subtraction, or
# multiplication) such that applying the operation to `operands` equals
# `target`.
#
# `op` is one of "auto" (default), "add", "subtract", or "multiply".
# "auto" picks subtraction when `target < 50`, and otherwise picks
# addition or multiplication at random (only considering multiplication
# when `target` actually has a suitable factor pair, e.g. never for a
# prime `target`).
#
# Returns a list with elements `op`, `operands`, and `target` such that
# `Reduce(op, operands) == target`.
generate_problem <- function(
	target,
	op = c("auto", "add", "subtract", "multiply"),
	max_factor = 12L
) {
	op <- match.arg(op)
	stopifnot(
		"`target` must be a whole number between 1 and 100" = target == as.integer(target) &&
			target >= 1 &&
			target <= 100
	)
	if (op == "auto") {
		op <- if (target < 50) {
			"subtract"
		} else if (is.null(valid_factors(target, max_factor))) {
			"add"
		} else {
			sample(c("add", "multiply"), 1L)
		}
	}
	switch(
		op,
		add = add_problem(target),
		subtract = subtract_problem(target),
		multiply = multiply_problem(target, max_factor)
	)
}

add_problem <- function(target) {
	lo <- max(10L, target - 99L)
	hi <- min(99L, target - 10L)
	stopifnot("no two-digit addends sum to `target`" = lo <= hi)
	a <- sample(seq.int(lo, hi), 1L)
	list(op = "add", operands = c(a, target - a), target = target)
}

subtract_problem <- function(target) {
	stopifnot("`target` must be at most 89 for two-digit subtraction" = target <= 89L)
	b <- sample(seq.int(10L, 99L - target), 1L)
	list(op = "subtract", operands = c(b + target, b), target = target)
}

multiply_problem <- function(target, max_factor = 12L) {
	factors <- valid_factors(target, max_factor)
	stopifnot("no factor pair (<= `max_factor`) multiplies to `target`" = !is.null(factors))
	i <- sample(length(factors$a), 1L)
	# larger operand first so long-form stacking puts it on top
	operands <- sort(c(factors$a[i], factors$b[i]), decreasing = TRUE)
	list(op = "multiply", operands = operands, target = target)
}

# Returns `NULL` if `target` has no factor pair a * b == target with
# 2 <= a <= max_factor and b <= 99 (e.g. `target` is prime).
valid_factors <- function(target, max_factor = 12L) {
	a <- seq_len(min(max_factor, target))
	a <- a[target %% a == 0L]
	b <- target %/% a
	keep <- a >= 2L & b <= 99L
	a <- a[keep]
	b <- b[keep]
	if (!length(a)) NULL else list(a = a, b = b)
}

#' Generate a numbered sequence of arithmetic problems
#'
#' `generate_math_problems()` generates one problem per target answer,
#' numbered in order.  This is intended to produce the left-hand
#' ("problems") panel matching a sequence of grid points on the
#' right-hand panel (see [numberGridGrob()]).
#'
#' @param targets Integer vector of desired answers (1-100), in the order
#'                the problems should be numbered.
#' @param op One of `"auto"` (default), `"add"`, `"subtract"`, or
#'           `"multiply"`.  `"auto"` picks subtraction when `target < 50`,
#'           and otherwise picks addition or multiplication at random
#'           (only considering multiplication when `target` actually has
#'           a suitable factor pair, e.g. never for a prime `target`).
#' @param max_factor Largest multiplication factor to consider.
#' @return A data frame with columns `problem` (1-based index), `op`,
#'         `operands` (a list column of numeric vectors, one per problem,
#'         allowing more than two operands per problem), and `target`.
#' @examples
#' set.seed(42)
#' generate_math_problems(c(34, 37, 67, 64, 34))
#' @export
generate_math_problems <- function(targets, op = "auto", max_factor = 12L) {
	problems <- lapply(targets, generate_problem, op = op, max_factor = max_factor)
	df <- data.frame(
		problem = seq_along(targets),
		op = vapply(problems, `[[`, character(1L), "op"),
		target = vapply(problems, `[[`, double(1L), "target")
	)
	df$operands <- lapply(problems, `[[`, "operands")
	df
}
