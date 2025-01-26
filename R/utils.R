#' Collapse Strings into a TiddlyWiki-Compatible Format
#'
#' Combines a list of strings into a single string with each element enclosed in double square brackets.
#'
#' @param strlist A character vector of strings to be collapsed.
#' @return A single string with each element wrapped in `[[...]]`.
#' @export
tid_collapse <- function(strlist) {
  strlist <- strlist[!is.na(strlist)]
  paste0(
    "[[",
    paste0(strlist, collapse = "]] [["),
    "]]"
  )
}

#' Extract Tags from Block Content
#'
#' Searches for hashtags in the `content` column of a data.table and assigns them to the `tags` column.
#'
#' @param blocks A data.table containing blocks with a `content` column.
#' @return The updated `blocks` data.table with tags extracted and added to the `tags` column.
#' @export
extract_tags <- function(blocks) {
  for (i in seq_len(nrow(blocks))) {
    if (grepl("#", blocks[i, content], fixed = TRUE)) {
      tags <- unlist(regmatches(blocks[i, content], gregexpr("#\\w+", blocks[i, content])))
      tags <- gsub("#", "", tags, fixed = TRUE)
      tags <- tid_collapse(tags)

      if (!is.null(blocks$tags)) {
        if (!is.na(blocks[i, tags])) {
          tags <- paste(tags, blocks[i, tags])
        }
      }

      set(blocks, i = i, j = "tags", value = tags)
    }
  }
  invisible(blocks)
}

#' Extract Task Keywords from Block Content
#'
#' Identifies task-related keywords (e.g., TODO, DONE) in the `content` column and adds them to the `lq_task` column.
#'
#' @param blocks A data.table containing blocks with a `content` column.
#' @param keywords_of_interest A character vector of task keywords to search for. Defaults to `c("TODO", "DONE", "CANCELLED", "CANCELED")`.
#' @param remove_keywords Logical. If TRUE, removes keywords from the `content` column after extraction. Defaults to TRUE.
#' @return The updated `blocks` data.table with task keywords extracted and added to the `lq_task` column.
#' @export
extract_task_keywords <- function(blocks, keywords_of_interest = c("TODO", "DONE", "CANCELLED", "CANCELED"), remove_keywords = TRUE) {
  keywords_of_interest_collapsed <- paste0(keywords_of_interest, collapse = " |")
  for (i in seq_len(nrow(blocks))) {
    if (grepl(keywords_of_interest_collapsed, blocks[i, content])) {

      lq_task <- tid_collapse(keywords_of_interest[grep(paste(keywords_of_interest, collapse = "|"), blocks[i, content])])

      if (!is.null(blocks$lq_task)) {
        if (!is.na(blocks[i, lq_task])) {
          lq_task <- paste(lq_task, blocks[i, lq_task])
        }
      }

      set(blocks, i = i, j = "lq_task", value = lq_task)
      if (remove_keywords) {
        set(blocks, i = i, j = "content", value = gsub(keywords_of_interest_collapsed, "", blocks[i, content]))
      }
    }
  }
  invisible(blocks)
}

#' Extract Custom Keywords from Block Content
#'
#' Searches for specified keywords in the `content` column and adds them to the `lq_keywords` column.
#'
#' @param blocks A data.table containing blocks with a `content` column.
#' @param keywords_of_interest A character vector of keywords to search for.
#' @param remove_keywords Logical. If TRUE, removes keywords from the `content` column after extraction. Defaults to FALSE.
#' @return The updated `blocks` data.table with keywords extracted and added to the `lq_keywords` column.
#' @export
extract_other_keywords <- function(blocks, keywords_of_interest, remove_keywords = FALSE) {
  keywords_of_interest_collapsed <- paste0(keywords_of_interest, collapse = " |")
  for (i in seq_len(nrow(blocks))) {
    if (grepl(keywords_of_interest_collapsed, blocks[i, content])) {

      lq_keywords <- tid_collapse(keywords_of_interest[grep(paste(keywords_of_interest, collapse = "|"), blocks[i, content])])

      if (!is.null(blocks$lq_keywords)) {
        if (!is.na(blocks[i, lq_keywords])) {
          lq_keywords <- paste(lq_keywords, blocks[i, lq_keywords])
        }
      }

      set(blocks, i = i, j = "lq_keywords", value = lq_keywords)
      if (remove_keywords) {
        set(blocks, i = i, j = "content", value = gsub(keywords_of_interest_collapsed, "", blocks[i, content]))
      }
    }
  }
  invisible(blocks)
}

#' Transform Links in Block Content
#'
#' Replaces links to page names in the `content` column with links to page IDs.
#'
#' @param blocks A data.table containing blocks with a `content` column.
#' @param pages A data.table containing `page_name` and `id` columns for mapping links.
#' @return The updated `blocks` data.table with links transformed.
#' @export
transform_links <- function(blocks, pages) {

  for (i in 1:nrow(pages)) {
    old_link <- paste0("[[", pages$page_name[i], "]]")
    new_link <- paste0("[[", pages$page_name[i], "|", pages$id[i], "]]")

    blocks[, content := gsub(old_link, new_link, content, fixed = TRUE)]
  }

  invisible(blocks)
}
