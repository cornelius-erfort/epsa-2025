# EPSA 2025 Schedule Scraper
# Downloads and parses the EPSA 2025 schedule from the official JS file
# Saves the result as a CSV file

if (!require("rvest")) install.packages("rvest")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)

# authors_raw <- "Ramin Shirali 1,2,3, Victor Lapuente 2, Kohei Suzuki 3"
# authors_raw <-"Prisca Jöst 1, 2, Ellen Lust 1, 2, 3, Erica Metheney 2, 3, Dave Namusanya 2, 3"
# affils_raw <- "1 Cornell University2 Governance and Local Development Institute3 University of Gothenburg"
# parse_authors_affils(authors_raw, affils_raw)
# authors_raw <- authors
# affils_raw <- affiliations
parse_authors_affils <- function(authors_raw, affils_raw) {
  # Clean up
  authors_raw <- gsub("\\s+", " ", authors_raw)
  affils_raw <- gsub("\\s+", " ", affils_raw)
  
  # Check if this is numbered format (contains numbers after author names)
  has_numbers <- grepl("\\d", authors_raw)
  
  author_names <- character(10)
  author_affils <- character(10)
  affil_list <- character(10)
  
  if (has_numbers) {
    # Format 1: Numbered format (like in JS data)
    affil_matches <- str_match_all(affils_raw, "(\\d+)\\s*([^\\d]+)")
    affil_list <- if (length(affil_matches[[1]]) > 0) trimws(affil_matches[[1]][,3]) else if (nchar(affils_raw) > 0) affils_raw else ""
    author_parts <- str_extract_all(authors_raw, "[^,]+\\d+(?:, *\\d+)*")[[1]]
    for (i in seq_along(author_parts)) {
      m <- str_match(author_parts[i], "^(.+?)(\\d[\\d, ]*)?$")
      name <- trimws(m[2])
      nums <- trimws(m[3])
      nums <- unlist(strsplit(nums, ","))
      nums <- nums[nums != ""]
      author_names[i] <- name %>% str_trim("both")
      if (length(nums) > 0 && length(affil_list) > 1) {
        affils <- affil_list[as.integer(nums)]
        author_affils[i] <- paste(affils, collapse = "; ")
      } else if (length(affil_list) == 1) {
        author_affils[i] <- affil_list[1]
      } else {
        author_affils[i] <- NA
      }
    }
    # Fill up to 10
    author_names <- c(author_names, rep(NA, 10 - length(author_names)))
    author_affils <- c(author_affils, rep(NA, 10 - length(author_affils)))
    affil_list <- c(affil_list, rep(NA, 10 - length(affil_list)))
  } else {
    # Format 2: HTML format without numbers
    author_parts <- unlist(strsplit(authors_raw, ",\\s*"))
    for (i in seq_along(author_parts)) {
      author_names[i] <- trimws(author_parts[i])
    }
    author_names <- c(author_names, rep(NA, 10 - length(author_names)))
    affiliations_clean <- trimws(affils_raw)
    affil_list[1] <- affiliations_clean
    affil_list[2:10] <- NA
    # By default, set all author_affils to NA (will be fixed below if needed)
    author_affils <- rep(NA, 10)
  }
  
  # If there is more than one author, but only affil1 is not NA, assign affil1 to all authors
  num_authors <- sum(!is.na(author_names) & author_names != "")
  if (num_authors > 1 && !is.na(affil_list[1]) && all(is.na(affil_list[2:10]))) {
    for (i in 1:num_authors) {
      author_affils[i] <- affil_list[1]
    }
  }
  
  # Return as a named list
  res <- list()
  for (i in 1:10) {
    res[[paste0("author", i)]] <- author_names[i]
    res[[paste0("author", i, "_affil")]] <- author_affils[i]
  }
  for (i in 1:10) {
    res[[paste0("affil", i)]] <- affil_list[i]
  }
  res
}

# file_path = "abstract_htmls/abstract_0032_0925_0328.html"
parse_abstract_html <- function(file_path) {
  html <- tryCatch(read_html(file_path), error = function(e) return(NULL))
  if (is.null(html)) return(NULL)
  
  title <- html %>% html_node(".abstracttitle") %>% html_text(trim = TRUE)
  authors <- html %>% html_node(".authors") %>% html_text(trim = TRUE)
  affiliations <- html %>% html_node(".affiliations") %>% html_text(trim = TRUE)
  abstract <- html %>% html_node(".abstracttext") %>% html_text(trim = TRUE)
  keywords <- html %>% html_node(".keywords") %>% html_text(trim = TRUE)
  
  # Extract presenter information
  presenter <- NA
  presentation_section <- html %>% html_node(".presentation_section")
  if (!is.null(presentation_section)) {
    presenter_text <- presentation_section %>% html_text()
    presenter_match <- str_match(presenter_text, "Presented by:\\s*([^\\n]+)")
    if (!is.na(presenter_match[2])) {
      presenter <- trimws(presenter_match[2])
    }
  }
  
  # Parse authors and affiliations
  parsed <- parse_authors_affils(authors, affiliations)
  
  data.frame(
    file = basename(file_path),
    title = title,
    abstract = abstract,
    keywords = keywords,
    presenter_abstract = presenter,
    as.list(parsed),
    stringsAsFactors = FALSE
  )
}


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
  "session_id", "title", "presenter", "unknown6", "room", "unknown7",
  "time", "start", "end", "lang", "timezone"
)


# 3. Extract pagedata
df_info$links <- str_extract_all(js_text, "../data/abstracts/en/[^,\"]*")[[1]]
df_info$links <- str_remove(df_info$links, "\\.\\.")
df_info$links <- str_c("https://coms.events/epsa2025", df_info$links)

df_info$links[1]

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

# If you want to resume, check for an existing CSV
output_csv <- "epsa2025_abstracts.csv"
parsed_files <- character(0)
if (file.exists(output_csv)) {
  existing <- read.csv(output_csv, stringsAsFactors = FALSE)
  parsed_files <- existing$file
}

# Only parse files not already parsed
to_parse <- setdiff(basename(list.files(abstract_dir, pattern = "\\.html$", full.names = TRUE)), parsed_files)
html_files_to_parse <- file.path(abstract_dir, to_parse)

results <- list()
for (file_path in html_files_to_parse) {
  cat("Parsing", file_path, "...\n")
  res <- parse_abstract_html(file_path)
  if (!is.null(res)) results[[length(results) + 1]] <- res
}

# Combine with existing if resuming
if (file.exists(output_csv)) {
  all_results <- bind_rows(existing, bind_rows(results))
} else {
  all_results <- bind_rows(results)
}

# Save results
write.csv(all_results, output_csv, row.names = FALSE)
print(head(all_results))
