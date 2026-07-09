test_that("`problemsGrob()` picks sensible layout defaults", {
	problems <- generate_math_problems(rep(64, 18))
	grob <- problemsGrob(problems)
	expect_s3_class(grob, "problems_grob")

	problems <- generate_math_problems(rep(64, 19))
	expect_s3_class(problemsGrob(problems), "problems_grob")

	expect_error(problemsGrob(data.frame()), "columns")
	expect_error(
		problemsGrob(generate_math_problems(rep(64, 19)), ncol = 3, nrow = 6),
		"must fit"
	)
})

test_that("`problemsGrob()` renders as expected", {
	skip_if_not_installed("vdiffr")
	library("grid")

	set.seed(42)
	problems <- generate_math_problems(c(34, 37, 67, 64, 34, 42, 88, 15))
	draw_problems <- function() {
		grid.newpage()
		grid.draw(problemsGrob(problems))
	}
	vdiffr::expect_doppelganger("problems", draw_problems)

	set.seed(42)
	problems_4col <- generate_math_problems(sample(1:100, 22))
	draw_problems_4col <- function() {
		grid.newpage()
		grid.draw(problemsGrob(problems_4col))
	}
	vdiffr::expect_doppelganger("problems_4col", draw_problems_4col)
})
