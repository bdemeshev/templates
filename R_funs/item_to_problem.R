# replace \item by
# \begin{problem}
#
#
# \begin{sol}
# \end{sol}
# \end{problem}

replaced_item_level <- 1 
start_rep <- "\\\\begin{problem}\n"
end_rep <- "\\\\begin{sol}\n\\\\end{sol}\n\\\\end{problem}"

file_name <- "~/Documents/pr201/tvims_notes_2013/tvims_notes_2013.Rnw"
new_file <- "~/Documents/pr201/tvims_notes_2013/tvims_notes_2013_env.Rnw"

# assumptions:
# max 1 \item per line
# \item inside \begin{comment} are left untouched :)
# \item inside \verb or \verbatim are not considered

library("stringr")
library("tidyverse")





text <- data_frame(line = read_lines(file = file_name))
text <- mutate(text, line_trim = str_trim(line))
text <- mutate(text, commented = str_detect(line_trim, "^%"))

text <- mutate(text, begin_enum = str_detect(line_trim, "^\\\\begin[{]enumerate[}]"))
text <- mutate(text, end_enum = str_detect(line_trim, "^\\\\end[{]enumerate[}]"))


text <- mutate(text, verb = str_detect(line_trim, "\\\\verb"))
text <- mutate(text, begin_verbatim = str_detect(line_trim, "^\\\\begin[{]verbatim[}]"))
text <- mutate(text, end_verbatim = str_detect(line_trim, "^\\\\end[{]verbatim[}]"))

if (sum(text$verb, text$begin_verbatim) > 0) {
  warning("Text contains \\verb or \\begin{verbatim}. The result may be wrong!!!")
}

text <- mutate(text, item = str_detect(line_trim, "^\\\\item"))

text <- mutate(text, begin_comment = str_detect(line_trim, "^\\\\begin[{]comment[}]"))
text <- mutate(text, end_comment = str_detect(line_trim, "^\\\\end[{]comment[}]"))

text <- mutate(text, item_level_delta = ifelse(begin_enum, +1, ifelse(end_enum, -1, 0)))
text <- mutate(text, long_comment_delta = ifelse(begin_comment, +1, ifelse(end_comment, -1, 0)))

text <- mutate(text, item_level = cumsum(item_level_delta))
text <- mutate(text, long_comment = cumsum(long_comment_delta))

text <- mutate(text, item_to_replace = 
  item & (item_level == replaced_item_level) & (!commented) & (!long_comment) )
                                                                          
text <- mutate(text, begin_to_replace = 
  begin_enum & (item_level == replaced_item_level) & (!commented) & (!long_comment) )

text <- mutate(text, end_to_replace = 
  end_enum & (item_level == (replaced_item_level - 1)) & (!commented) & (!long_comment) )


text <- mutate(text, item_no = cumsum(item_to_replace))
text <- mutate(text, begin_no = cumsum(begin_to_replace))

text <- group_by(text, (begin_no * item_to_replace)) %>% mutate(first_item = item_to_replace & (item_no == min(item_no)) )
text <- ungroup(text)

text <- mutate(text, new_line = line)

for (i in 1:nrow(text)) { 
  if (text$first_item[i]) {
    text$new_line[i] <- str_replace(text$line[i], pattern = "\\\\item", replacement = start_rep)
  }

  if (text$end_to_replace[i]) {
    text$new_line[i] <- str_replace(text$line[i], pattern = "\\\\end[{]enumerate[}]", replacement = end_rep)
  }
  
  if (text$begin_to_replace[i]) {
    text$new_line[i] <- str_replace(text$line[i], pattern = "\\\\begin[{]enumerate[}]", replacement = "")
  }
  
  if ((!text$first_item[i]) & (text$item_to_replace[i])) {
    text$new_line[i] <- str_replace(text$line[i], pattern = "\\\\item", 
                                    replacement = paste0(end_rep, "\n", start_rep))
  }
}


readr::write_lines(text$new_line, path = new_file)
