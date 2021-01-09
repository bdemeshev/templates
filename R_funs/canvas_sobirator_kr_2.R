library(tidyverse)
library(rio)
library(stringr)
library(tools)

rooster = import("~/Downloads/2020-12-12T0120_Grades-pr201.csv")
glimpse(rooster)
rooster = select(rooster, Student, ID, Section)
head(rooster, 1)
tail(rooster, 1)

rooster = filter(rooster, Student != "Student, Test")
rooster = filter(rooster, !is.na(ID))

good_names = import("~/Downloads/hse_probability_2020_2021 - studs.csv")
glimpse(good_names)
good_names = select(good_names, canvas_ID, first_name, last_name, hse_mail, k1_sum, min_2_reregrade)
good_names = mutate(good_names, fi = paste0(first_name, "_", last_name),
                    canvas_ID = as.numeric(canvas_ID))

head(good_names)

good_names = filter(good_names, !is.na(canvas_ID))
good_names %>% arrange(fi) %>% head()
good_names %>% arrange(fi) %>% tail()

rooster = left_join(rooster, good_names, by = c("ID" = "canvas_ID"))
rooster %>% filter(is.na(fi))

head(rooster)

cnvs_get_stud_id = function(filenames) {
  ids = as.numeric(str_match(filenames, "_([0-9]{8})_")[, 2])
  return(ids)
}

cnvs_sortirator = function(path, out_path = path, ID, Section, file_prefix = rep("", length(ID))) {
  dir.create(out_path, showWarnings = FALSE)
  if (length(ID) != length(unique(ID))) {
    stop("Indices in ID are not unique")
  }
  if (length(ID) != length(Section)) {
    stop("Lenghts of ID and Section do not match")
  }
  if (length(ID) != length(file_prefix)) {
    stop("Lenghts of ID and file_prefix do not match")
  }

  filenames = list.files(path)
  n_files = length(filenames)
  ids = cnvs_get_stud_id(filenames)

  for (file_no in 1:n_files) {
    from = paste0(path, "/", filenames[file_no])
    stud_no = which(ids[file_no] == ID)

    to_folders_string = Section[stud_no]
    to_folders = str_split(to_folders_string, " and ")[[1]]
    print(paste0("From: ", from, " to folders: ", to_folders))
    for (to_folder in to_folders) {
      dir.create(paste0(out_path, "/", to_folder), showWarnings = FALSE)
      to = paste0(out_path, "/", to_folder, "/", file_prefix[stud_no], "_", filenames[file_no])
      file.copy(from, to)
    }
  }

  return(n_files)
}
strings_2_pdfs = function(comments, out_comments_path = "./comment_pdfs/", pad_lenght=3) {
  dir.create(out_comments_path, showWarnings = FALSE)

  n_comments = length(comments)

  comment_file_pdf = paste0("comment_", str_pad(1:n_comments, pad_lenght, pad = "0"), ".pdf")
  comment_file_pdf_full = paste0(out_comments_path, comment_file_pdf)

  temp_file = tempfile()
  for (comment_no in 1:n_comments) {
    file_connection = file(temp_file)
    writeLines(comments[comment_no], file_connection)
    close(file_connection)

    from = temp_file
    to = paste0("'", comment_file_pdf_full[comment_no], "'")

    command = paste0("paps ", from, " | ps2pdf - ", to)
    # alternatives:
    # enscript + ps2pdf: I failed to set up cyrillic font
    # pandoc: takes a lot of time because of latex

    print(command)
    system(command)
  }
  return(invisible(comment_file_pdf_full))
}
pdf_unifier = function(filenames, comments = filenames, out_path = "./", out_name = "merged.pdf", pad_lenght=3) {
  n_files = length(filenames)
  if (n_files == 0) {
    return(character(0))
  }
  if (n_files != length(comments)) {
    stop("Lengths of filenames and comments do not match")
  }
  extensions = unique(str_to_lower(file_ext(filenames)))

  if ((length(extensions) > 1) | (extensions != "pdf")) {
    stop("problem with file extensions: ", extensions)
  }

  comment_pdfs = strings_2_pdfs(comments, out_comments_path = paste0(out_path, "/comment_pdfs/"), pad_lenght = pad_lenght)

  mixed = c(t(matrix(c(comment_pdfs, filenames), ncol = 2)))
  mixed = paste0("'", mixed, "'")

  mixed_string = paste0(mixed, collapse = " ")


  to = paste0("'", out_path, "/", out_name, "'")
  # command = paste0("pdftk ", mixed_string, " cat output ", to)
  command = paste0("gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dAutoRotatePages=/None -sOutputFile=", to, " ", mixed_string)
  print(command)
  system(command)

  return(invisible(command))
}

cnvs_comment_creator = function(filenames, rooster) {
  # rooster table should have ID field
  n_files = length(filenames)
  if (n_files == 0) {
    return(character(0))
  }
  ids = cnvs_get_stud_id(filenames)
  comments = rep("", n_files)
  field_names = colnames(rooster)
  field_names = paste0(field_names, ":\n")
  field_matrix = matrix(c(field_names, field_names), ncol = 2)
  for (file_no in 1:n_files) {
    comment = paste0("filename:\n", filenames[file_no], "\n\n")
    one_line = unlist(filter(rooster, ID == ids[file_no]))
    field_matrix[, 2] = paste0(one_line, "\n\n")
    fields = paste0(c(t(field_matrix)), collapse = "")
    comments[file_no] = paste0(comment, fields)
  }
  return(comments)
}


filenames = list.files("~/Downloads/kr2_upload_test/", full.names = TRUE)
comments = cnvs_comment_creator(filenames, rooster)

pdf_unifier(filenames, cnvs_comment_creator(filenames, rooster))

dir.create("~/Downloads/kr2_upload_test/sorted/")

cnvs_sortirator("~/Downloads/kr2_upload_test/", "~/Downloads/kr2_upload_test/sorted/", rooster$ID, rooster$Section, rooster$fi)



# strings_2_pdfs("тестик")

path = "output_1"
cnvs_folder_merger = function(path = ".", out_path = "out_merged/") {
  dir.create(out_path, showWarnings = FALSE)
  folders = list.dirs(path)
  for (folder in folders) {
    filenames = list.files(folder, full.names = TRUE, recursive = FALSE)
    filenames = filenames[!file.info(filenames)$isdir]
    comments = cnvs_comment_creator(filenames, rooster)
    cat("\n\n------------------------")
    cat(folder, "\n")
    cat("------------------------\n")
    folder_as_filename = str_replace(folder, "/", "___")
    out_name = paste0(folder_as_filename, "___merged.pdf")
    pdf_unifier(filenames, comments, out_path = out_path, out_name = out_name)
  }
  return(invisible(folders))
}

cnvs_folder_merger("output_1", out_path = "out_merged_1")
cnvs_folder_merger("output_2", out_path = "out_merged_2")
cnvs_folder_merger("output_3", out_path = "out_merged_3")

