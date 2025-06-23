# EPSA 2025 Schedule Scraper
# Downloads and parses the EPSA 2025 schedule from the official JS file
# Saves the result as a CSV file

# 1. Download the JS file
url <- "https://coms.events/epsa2025/data/auxiliary/zoom_pageinfo_en.js"
js_text <- readLines(url, warn = FALSE)
js_text <- paste(js_text, collapse = "\n")

# 2. Extract pageinfo
csv_text1 <- sub("^.*pageinfo\\s*=\\s*\\[\\[", "", js_text)
csv_text1 <- sub("\\]\\];.*$", "", csv_text1)
csv_text1 <- gsub("null", "", csv_text1)
csv_text1 <- gsub("\\],\\[", "\n", csv_text1)
csv_text1 <- gsub("\\[|\\]", "", csv_text1)
tmpfile1 <- tempfile(fileext = ".csv")
writeLines(csv_text1, tmpfile1)
df_info <- read.csv(tmpfile1, header = FALSE, stringsAsFactors = FALSE)
colnames(df_info)[1:16] <- c(
  "unknown1", "unknown2", "unknown3", "unknown4", "unknown5",
  "session_id", "title", "author", "unknown6", "room", "unknown7",
  "time", "start", "end", "lang", "timezone"
)

library(stringr)
# 3. Extract pagedata
df_info$links <- str_extract_all(js_text, "../data/abstracts/en/[^,\"]*")[[1]]
df_info$links <- str_remove(df_info$links, "\\.\\.")
df_info$links <- str_c("https://coms.events/epsa2025", df_info$links)

# 4. Download each abstract HTML if not already downloaded
abstract_dir <- "abstract_htmls"
if (!dir.exists(abstract_dir)) dir.create(abstract_dir)

for (i in seq_along(df_info$links)) {
  link <- df_info$links[i]
  if (is.na(link) || link == "") next
  # Use the last part of the URL as filename
  file_name <- basename(link)
  file_path <- file.path(abstract_dir, file_name)
  if (!file.exists(file_path)) {
    cat(sprintf("Downloading %s...\n", file_name))
    tryCatch({
      download.file(link, file_path, quiet = TRUE)
      # Sys.sleep(0.5) # Be polite to the server
    }, error = function(e) {
      cat(sprintf("Failed to download %s: %s\n", link, e$message))
    })
  } else {
    cat(sprintf("Already downloaded: %s\n", file_name))
  }
}


