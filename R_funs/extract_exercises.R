library(tools)
library(xml2)

setwd("~/Downloads/qti/")

tdir <- tempfile()
dir.create(tdir)
owd <- getwd()
dir.create("Rmd")

zips <- dir()
zips <- zips[file_ext(zips) == "zip"]
dropped <- list()

N <- 0

j = zips


xml = read_xml("ge3bf9d2d183d15863a72877fccbd1869.xml")

library(tidyverse)
xml=
xml_find_all(xml, xpath = ".//item")

xml_children(xml) %>% xml_children()

for(j in zips) {
  file.copy(file.path(owd, j), file.path(tdir, j))

  dropped[[j]] <- NULL

  cdir <- file.path(owd, "Rmd", file_path_sans_ext(j))
  dir.create(cdir)

  setwd(tdir)
  unzip(j)

  xml <- readLines("questions.xml")

  starts <- grep("<item ident", xml, fixed = TRUE)
  ends <- grep("</item>", xml, fixed = TRUE)

  if(length(starts) != length(ends)) {
    stop("problem with items!")
  }

  questions <- list()
  for(i in seq_along(starts)) {
    qu <- xml[starts[i]:ends[i]]

    if(any(grepl('.PNG"', qu, fixed = TRUE)))
      stopped <- TRUE
    else
      stopped <- FALSE

    title <- grep("<item ident", qu, value = TRUE)
    title <- strsplit(title, 'title="', fixed = TRUE)[[1]][2]
    title <- gsub('">', '', title, fixed = TRUE)
    title <- gsub(" ", "_", title)
    rmc <- c("(", ")", "+", "-", ":", "[", "]", "{", "}", "=", "!",
      "$", "%", "?", ".", ",", ";")
    for(char in rmc)
      title <- gsub(char, "", title, fixed = TRUE)

    ii <- grep("<presentation>", qu, fixed = TRUE)
    jj <- grep("</presentation>", qu, fixed = TRUE)

    body <- read_xml(paste(qu[ii:jj], collapse = "\n"))

    text <- xml_find_all(body, ".//mattext")
    text2 <- list()
    for(jj in 1:length(text)) {
      tmp <- xml_text(text[jj])
      tmp <- gsub("&nbsp;", " ", tmp, fixed = TRUE)
      if(!any(grepl("<", tmp, fixed = TRUE))) {
        text2[[jj]] <- tmp
      } else {
        tmp <- strsplit(tmp, "")[[1]]
        n <- length(tmp)
        if(paste(tmp[(n-3):n], collapse = "") == "</p>")
          tmp <- tmp[1:(n-4)]
        tmp <- paste(tmp, collapse = "")
        tmp <- try(read_xml(tmp, options = "RECOVER"))
        if(!inherits(tmp, "try-error")) {
          text2[[jj]] <- xml_text(tmp)
        } else {
          stopped <- TRUE
          break
        }
      }
    }

    if(stopped) {
      dropped[[j]] <- c(dropped[[j]], title)
      next
    }

    ids <- grep("<response_label", qu, fixed = TRUE, value = TRUE)
    ids <- sapply(strsplit(ids, 'ident="', ids), function(x) x[2])
    ids <- sapply(strsplit(ids, '" rshuffle="Yes">', ids), function(x) x[1])

    correct <- grep('action="Set">1</setvar>', qu, fixed = TRUE)
    correct <- qu[correct - 2]
    correct <- strsplit(correct, '>', fixed = TRUE)
    correct <- sapply(correct, function(x) x[2])
    correct <- strsplit(correct, "<", fixed = TRUE)
    correct <- sapply(correct, function(x) x[1])

    solution <- rep(FALSE, length(ids))
    for(jj in seq_along(ids)) {
      if(ids[[jj]] %in% correct)
        solution[jj] <- TRUE
    }

    foo <- function(x) ifelse(x, "True.", "False.")

    rmd <- c("Question", "========",
      text2[[1]], "",
      "Answerlist", "----------",
      paste("*", unlist(text2[-1])),
      "",
      "Solution", "========",
      "",
      "Answerlist", "----------",
      paste("*", foo(solution)),
      "",
      "Meta-information",
      "================",
      paste("exname:", title),
      paste("extype:", if(sum(solution) < 2) "schoice" else "mchoice"),
      paste("exsolution:", exams::mchoice2string(solution))
    )

    writeLines(rmd, file.path(cdir, paste0(title, ".Rmd")))

    N <- N + 1
  }

  file.remove(dir(tdir))
}

setwd(owd)



xml_child(xml_child(xml_child(xml, 1),2),3)



a = read_qti("ge3bf9d2d183d15863a72877fccbd1869.xml")
a

xml2 = xml_ns_strip(xml)
xml2
qti:::qti_item_id
xml_find_first(xml2, "//assessment")
xml_find_first(xml, "//mattext") %>% xml_name()

a = xml_find_first(xml, "//mattext") %>% xml_text()

a  %>% read_xml()

a_h = read_html(a)

a_h %>% xml_find_all("//img") %>% xml_attr("title")

xml_remove(a_h, "//img")
?xml_remove

library(rvest)


a_h %>% html_nodes("img")

b = as_list(a_h)

bdiv = b$html$body$div
str(bdiv, 1)
bdiv[[2]]
