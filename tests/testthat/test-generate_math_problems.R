problem_value <- function(op, operands) {
	switch(op, add = sum(operands), subtract = Reduce(`-`, operands), multiply = prod(operands))
}

test_that("`generate_problem()` returns a valid problem for every target 1-100", {
	for (target in 1:100) {
		for (i in 1:5) {
			p <- generate_problem(target)
			expect_equal(problem_value(p$op, p$operands), target)
			expect_true(all(p$operands >= 1 & p$operands <= 99))
		}
	}
})

test_that("`generate_problem()` auto never picks multiply for a prime target", {
	is_prime <- function(n) n >= 2 && all(n %% 2:(n - 1L) != 0)
	primes <- Filter(is_prime, 50:100)
	for (target in primes) {
		for (i in 1:20) {
			expect_identical(generate_problem(target)$op, "add")
		}
	}
})

test_that("`generate_problem()` respects an explicit `op`", {
	expect_identical(generate_problem(34, op = "subtract")$op, "subtract")
	expect_identical(generate_problem(67, op = "add")$op, "add")
	expect_identical(generate_problem(64, op = "multiply")$op, "multiply")
	expect_error(generate_problem(97, op = "multiply"), "factor pair")
})

test_that("`generate_math_problems()` numbers problems in order", {
	targets <- c(34, 37, 67, 64, 34)
	problems <- generate_math_problems(targets)
	expect_identical(problems$problem, seq_along(targets))
	expect_identical(problems$target, as.double(targets))
	expect_true(is.list(problems$operands))
	for (i in seq_along(targets)) {
		expect_equal(problem_value(problems$op[i], problems$operands[[i]]), targets[i])
	}
})
