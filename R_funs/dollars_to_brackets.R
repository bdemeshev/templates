dollars_to_brackets <- function(output_string) {
  bracket_pairs <- stringr::str_count(output_string, pattern = "[$][$]") / 2
  for (index in 1:bracket_pairs) {
    output_string <- stringr::str_replace(output_string,
                                          pattern = "[$][$]", replacement = "\\\\[")
    output_string <- stringr::str_replace(output_string,
                                          pattern = "[$][$]", replacement = "\\\\]")
  }
  return(output_string)
}


brackets_to_dollars <- function(output_string) {
  output_string = stringr::str_replace_all(output_string, "\\\\\\]", replacement = "$$")
  output_string = stringr::str_replace_all(output_string, "\\\\\\[", replacement = "$$")
  return(output_string)
}





a <- readLines("~/Documents/gt201/games_pset/gt_problems_utf8.tex")
a
a0 <- paste0(a, collapse = "\n")
a0
a1 <- dollars_to_brackets(a0)
writeLines(a1, "~/Documents/gt201/games_pset/gt_problems_utf8.tex")


