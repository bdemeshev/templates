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
