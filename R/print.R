# Impressao da classe 'leem'
#' @export
# print.leem <- function(x, ...) {
#   if (!is.null(attr(x, "table"))) {
#     print(x$table)
#   } else {
#     attributes(x) <- NULL
#     print(x)
#   }
# }
#' @export
print.leem <- function(x, ...) {
  aux <- attr(x, "output")
  # Print
  switch(aux,
         htest = output_htest(x),
         table = output_table(x),
         newleem = output_newleem(x),
         rprob = output_rprob(x))
}

output_htest <- function(x) {
  if (x$test == "ztest") {
    if(x$alternative == "two.sided"){
      cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
      # Step 1
      cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
          crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
      cat(crayon::bold(x$nullhyp), "\n")
      cat(crayon::bold(x$althyp), "\n\n")
      # Step 2
      cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
          crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
      cat(crayon::bold(x$signlevel), "\n\n")
      # Step 3
      cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
          crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
      cat(crayon::green$bold(gettext("   If |ztest| > |ztab| => Reject H0!", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("   ztest - test statistic; ztab - critical point", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision), "\n")
      cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision2), "\n\n")

      # Step 4
      cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
          crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
      cat(crayon::bold(x$conclusion))
    }

    if(any(x$alternative == c("less","l","L"))){

      cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
      # Step 1
      cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
          crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
      cat(crayon::bold(x$nullhyp), "\n")
      cat(crayon::bold(x$althyp), "\n\n")
      # Step 2
      cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
          crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
      cat(crayon::bold(x$signlevel), "\n\n")
      # Step 3
      cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
          crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
      cat(crayon::green$bold(gettext("   If |ztest| > |ztab| => Reject H0!", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("   ztest - test statistic; ztab - critical point", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision), "\n")
      cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision2), "\n\n")

      # Step 4
      cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
          crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
      cat(crayon::bold(x$conclusion))
    }

    if(any(x$alternative == c("greater","g","G"))){

      cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
      # Step 1
      cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
          crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
      cat(crayon::bold(x$nullhyp), "\n")
      cat(crayon::bold(x$althyp), "\n\n")
      # Step 2
      cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
          crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
      cat(crayon::bold(x$signlevel), "\n\n")
      # Step 3
      cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
          crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
      cat(crayon::green$bold(gettext("   If |ztest| > |ztab| => Reject H0!", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("   ztest - test statistic; ztab - critical point", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision), "\n")
      cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision2), "\n\n")

      # Step 4
      cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
          crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
      cat(crayon::bold(x$conclusion))
    }
  }
  if (x$test == "ttest") {
    if(x$alternative == "two.sided"){
      cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
      # Step 1
      cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
          crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
      cat(crayon::bold(x$nullhyp), "\n")
      cat(crayon::bold(x$althyp), "\n\n")
      # Step 2
      cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
          crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
      cat(crayon::bold(x$signlevel), "\n\n")
      # Step 3
      cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
          crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
      cat(crayon::green$bold(gettext("   If |ttest| > |ttab| => Reject H0!", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("   ttest - test statistic; ttab - critical point", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision), "\n")
      cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision2), "\n\n")

      # Step 4
      cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
          crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
      cat(crayon::bold(x$conclusion))
    }

    if(any(x$alternative == c("less","l","L"))){

      cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
      # Step 1
      cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
          crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
      cat(crayon::bold(x$nullhyp), "\n")
      cat(crayon::bold(x$althyp), "\n\n")
      # Step 2
      cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
          crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
      cat(crayon::bold(x$signlevel), "\n\n")
      # Step 3
      cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
          crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
      cat(crayon::green$bold(gettext("   If |ttest| > |ttab| => Reject H0!", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("   ttest - test statistic; ttab - critical point", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision), "\n")
      cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision2), "\n\n")

      # Step 4
      cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
          crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
      cat(crayon::bold(x$conclusion))
    }

    if(any(x$alternative == c("greater","g","G"))){

      cat("\n\n", crayon::bgGreen$bold(x$title), "\n")
      # Step 1
      cat(crayon::blue$underline$bold(gettext("Step 1:", domain = "R-leem")),
          crayon::blue(gettext("Hypothesis", domain = "R-leem")), "\n")
      cat(crayon::bold(x$nullhyp), "\n")
      cat(crayon::bold(x$althyp), "\n\n")
      # Step 2
      cat(crayon::blue$underline$bold(gettext("Step 2:", domain = "R-leem")),
          crayon::blue(gettext("Significance level", domain = "R-leem")), "\n")
      cat(crayon::bold(x$signlevel), "\n\n")
      # Step 3
      cat(crayon::blue$underline$bold(gettext("Step 3:", domain = "R-leem")),
          crayon::blue(gettext("Rule of decision", domain = "R-leem")), "\n")
      cat(crayon::green$bold(gettext("   If |ttest| > |ttab| => Reject H0!", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("   ttest - test statistic; ttab - critical point", domain = "R-leem")), "\n")
      cat(crayon::green(gettext("So...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision), "\n")
      cat(crayon::green(gettext("Otherside...", domain = "R-leem")), "\n")
      cat(crayon::bold(x$decision2), "\n\n")

      # Step 4
      cat(crayon::blue$underline$bold(gettext("Step 4:", domain = "R-leem")),
          crayon::blue(gettext("Conclusion", domain = "R-leem")), "\n")
      cat(crayon::bold(x$conclusion))
    }
  }
}
output_table <- function(x) {
  print(x$table)
}
output_newleem <- function(x) {
  attributes(x) <- NULL
  print(x)
}
output_rprob <- function(x) {
  attributes(x) <- NULL
  print(x)
}
