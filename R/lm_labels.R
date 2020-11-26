#' lm_labels creates linear equation to be included in plot
#'
#' @param x dataframe
#'
#' @return labels
#' @export lm_labels
#'
lm_labels <- function (x) {
  mod <- stats::lm(std ~ conc, data = x)
  formula <- sprintf("italic(y) == %.2f + %.2f* italic(x)",
                     round(stats::coef(mod)[1], 2), round(stats::coef(mod)[2], 2))

  r <- stats::cor(x$conc, x$std)
# Hoppar över NA (8st i A) i R-uträkningen.
# %.Xf x bestämmer antalet decimaler!
  r2 <- sprintf("italic(R^2) == %.4f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)

}
