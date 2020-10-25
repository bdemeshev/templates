# remove all latex comments with exception
# remove all lines that starts with "%"
# but not those that starts with "% !" or "%!"
# cut latex file at specific comment
# %! END FILE HERE
# trim_lines TRUE/FALSE — removes spaces at the beginning and at the end of the line

library("stringr")
library("tidyverse")

# detects presence of at least one of the specified patterns
str_or_detect = function(string, pattern, negate = FALSE) {
  detected = str_detect(string, pattern[1], negate = negate)
  if (length(pattern) > 1) {
    for (pattern_no in 2:length(pattern)) {
      detect_new = str_detect(string, pattern[pattern_no], negate = negate)
      detected = detected | detect_new
    }
  }
  return(detected)
}

# test:
# str_or_detect(c("aaa", "bbb", "ccc", "bbbbb"), c("aa", "bb"))


latex_remove_comments_and_cut = function(old_file, new_file = NULL, 
                              stop_at = c("^%! END FILE HERE", "^%!  END FILE HERE"),
                              comment_at = c("^%"),
                              do_not_remove_at = c("^% !", "^%!"),
                              trim_lines = TRUE) {
  if (is.null(new_file)) {
    new_file = paste0(str_replace(old_file, ".tex$", ""), "_new.tex")
  }
  
  cat("Old file: ", old_file, ", new file: ", new_file)
  
  text = tibble(line = read_lines(file = old_file))
  if (trim_lines) {
    text = mutate(text, line = str_trim(line))
  }
  
  text = mutate(text, 
                 commented_line = str_or_detect(line, comment_at),
                 important_line = str_or_detect(line, do_not_remove_at),
                 stop_line = str_or_detect(line, stop_at))
  
  text = mutate(text, stop_line_encountered = cumsum(stop_line))
  text = mutate(text, 
          remove_line = (commented_line & (!important_line)) | stop_line_encountered)
  
  text = filter(text, !remove_line)
  readr::write_lines(text$line, path = new_file)
  
  return(NULL)
}






