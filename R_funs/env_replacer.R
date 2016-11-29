# replace \env{ ... } by \begin{env} ... \end{env}
library("dplyr")
library("reshape2")
library("stringr")

# NON matching { } in tex! \left\{ .... \rigth. Achtung!  --> idea: not count \{
# commented tex

fname <- "~/Documents/gt201/games_pset/gt_problems_utf8.tex"
envir <- "problem"

con <- file(fname, "r")
h <- readLines(con)
close(con)

find_expression_with_bracket <- function(all_lines, expression_with_bracket) {
  delta <- str_length(expression_with_bracket) - 1 # (-1) for bracket itself
  m_exp <- gregexpr(expression_with_bracket, all_lines, fixed = TRUE)
  mm_exp <- melt(m_exp) %>% dplyr::filter(value > 0)
  mm_exp <- mutate(mm_exp, value = as.integer(value) + delta, L1 = as.integer(L1),
                   text = expression_with_bracket)
  mm_exp <- dplyr::rename(mm_exp, line_no = L1, bracket_pos = value)
  return(mm_exp)
}

mm_exp <- find_expression_with_bracket(h, paste0("\\", envir, "{"))

mm_open <- find_expression_with_bracket(h, "{")

mm_close <- find_expression_with_bracket(h, "}")

mm_slash_open <- find_expression_with_bracket(h, "\\{")

mm_slash_close <- find_expression_with_bracket(h, "\\}")



mm_close_cleaned <- anti_join(mm_close, mm_slash_close, by = c("bracket_pos", "line_no"))
mm_open_cleaned <- anti_join(mm_open, mm_slash_open, by = c("bracket_pos", "line_no"))
mm_open_cleaned <- anti_join(mm_open_cleaned, mm_exp, by = c("bracket_pos", "line_no"))

mm_all <- bind_rows(mm_exp, mm_open_cleaned, mm_close_cleaned)


# check matching!
check_matching <- group_by(mm_all, text) %>% summarise(n = n())



