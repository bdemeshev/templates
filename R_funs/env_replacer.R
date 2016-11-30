# replace \env{ ... } by \begin{env} ... \end{env}
library("tidyverse")
library("stringr")

# NON matching { } in tex! \left\{ .... \rigth. Achtung!  --> idea: not count \{
# commented tex -> to do!

fname <- "~/Documents/gt201/games_pset/gt_problems_utf8_new_new.tex"
new_fname <- str_replace(fname, "[.]", "_new.")

envir <- "problem"
exp_with_bracket <- paste0("\\", envir, "{")

open_text <- paste0("\n\\begin{", envir, "}\n")
close_text <- paste0("\n\\end{", envir, "}\n")


h <- read_lines(fname)

find_expression_with_bracket <- function(all_lines, expression_with_bracket) {
  delta <- str_length(expression_with_bracket) - 1 # (-1) for bracket itself
  m_exp <- gregexpr(expression_with_bracket, all_lines, fixed = TRUE)
  mm_exp <- melt(m_exp) %>% dplyr::filter(value > 0)
  mm_exp <- mutate(mm_exp, value = as.integer(value) + delta, L1 = as.integer(L1),
                   text = expression_with_bracket)
  mm_exp <- dplyr::rename(mm_exp, line_no = L1, bracket_pos = value)
  return(mm_exp)
}

mm_exp <- find_expression_with_bracket(h, exp_with_bracket)

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


mm_all <- mutate(mm_all, add = ifelse(text %in% c("{", exp_with_bracket), 1, -1))
mm_all <- arrange(mm_all, line_no, bracket_pos) %>% mutate(bracket_level = cumsum(add))

mm_all <- mutate(mm_all, add2 = ifelse(text == exp_with_bracket, 1, 0))
mm_all <- arrange(mm_all, line_no, bracket_pos) %>% mutate(env_no = cumsum(add2))

mm_all <- mutate(mm_all, closing = FALSE)

# match closing bracket!
inside_env <- FALSE
for (i in 1:nrow(mm_all)) {
  if (mm_all$text[i] == exp_with_bracket) {
    inside_env <- TRUE
    open_level <- mm_all$bracket_level[i]
  }
  if ((inside_env) & (mm_all$text[i] == "}")) {
    if (mm_all$bracket_level[i] == open_level - 1) {
      inside_env <- FALSE
      mm_all$closing[i] <- TRUE
    }
  }
}

mm_all <- mutate(mm_all, rep_text = ifelse(closing == TRUE, close_text,
                                      ifelse(text == exp_with_bracket, open_text, "")))
mm_all_to_replace <- dplyr::filter(mm_all, !rep_text == "")

process_line <- function(line_info, old_line) {
  line_info <- dplyr::arrange(line_info, bracket_pos)

  new_line <- ""
  n_subst <- nrow(line_info)
  last_pos <- 0

  for (j in 1:n_subst) {
    new_pos <- line_info$bracket_pos[j]
    old_text_len <- str_length(line_info$text[j])

    added_chunk <- str_sub(old_line, last_pos + 1, new_pos - old_text_len)
    new_line <- paste0(new_line, added_chunk, line_info$rep_text[j])

    last_pos <- new_pos
  }

  new_line <- paste0(new_line, str_sub(old_line,
                                       line_info$bracket_pos[n_subst] + 1))

  return(new_line)
}

# replace!
new_h <- h
for (i in 1:length(h)) {
  line_info <- dplyr::filter(mm_all_to_replace, line_no == i)
  if (nrow(line_info) > 0) {
    new_h[i] <- process_line(line_info, h[i])
  }
}


write_lines(new_h, path = new_fname)
