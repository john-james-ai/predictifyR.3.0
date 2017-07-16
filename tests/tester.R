td <- c("shouldnt", "ive gone home & you")
key <- paste0("\\b", corrections$key, "\\b")
for (i in 1:length(key)) {
  td <- gsub(key[i], corrections$value[i], td, ignore.case = TRUE)
}
td <- gsub("\\&", 'and', td)
