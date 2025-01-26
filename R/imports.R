#' Import Data from an HTTP Endpoint
#'
#' This function is a placeholder for importing data from an HTTP endpoint.
#'
#' @return Placeholder return value.
#' @export
import_from_http <- function() {

}

#' Import Data from a JSON File
#'
#' Reads a JSON file and extracts pages and blocks, converting them to tiddler-compatible formats.
#'
#' @param file_name The name of the JSON file to import.
#' @return A list containing:
#'   \describe{
#'     \item{pages}{A data.table with extracted pages.}
#'     \item{blocks}{A data.table with extracted blocks.}
#'   }
#' @export
import_from_json <- function(file_name, keywords = NA) {
  df_raw <- jsonlite::read_json(file_name)$blocks
  data <- list(
    pages  = json_pages_to_tiddler(df_raw),
    blocks = json_blocks_to_tiddler(df_raw)
  )
  return(
    process_raw_blocks(data, keywords)
    )
}

#' Import Data from a SQLite Database
#'
#' This function is a placeholder for importing data from a SQLite database.
#'
#' @return Placeholder return value.
#' @export
import_from_sqlite <- function() {

}

#' Process Raw Blocks Data
#'
#' Processes raw block data by extracting tags, task-related keywords, and transforming links.
#'
#' @param data A list containing `blocks` and `pages` data.tables.
#' @return The input data, invisibly modified in place.
#' @export
process_raw_blocks <- function(data, keywords = NA) {

  data$blocks[is.na(parent_id), lq_ancestors := NA]
  data$blocks[is.na(parent_id), parent_id := page_id]

  data$blocks <- transform_markup(data$blocks)
  data$blocks <- extract_tags(data$blocks)
  data$blocks <- extract_task_keywords(data$blocks)
  if (any(!is.na(keywords))) {
    data$blocks <- extract_other_keywords(data$blocks, keywords_of_interest = keywords)
  }
  # data$blocks <- transform_links(data$blocks, data$pages)

  invisible(data)
}

#' Convert a JSON Page to Tiddler Format
#'
#' Converts a single JSON page entry into a tiddler-compatible data.table.
#'
#' @param data A JSON object representing a page.
#' @return A data.table containing the page's `id`, `page_name`, and properties (if any).
#' @export
json_page_to_tiddler <- function(data) {

  df_out <- data.table(
    id = data[["id"]],
    page_name = data[["page-name"]],
    lq_children = tid_collapse(vapply(data$children, function(x) {x$id}, FUN.VALUE = character(1)))
  )

  if (!is.null(data[["properties"]])) {
    df_out <- cbind(
      df_out,
      as.data.table(data$properties)
    )
  }

  return(df_out)
}

#' Convert JSON Pages to Tiddler Format
#'
#' Converts a list of JSON page entries into a combined data.table.
#'
#' @param data A list of JSON objects representing pages.
#' @return A data.table containing all pages in tiddler-compatible format.
#' @export
json_pages_to_tiddler <- function(data) {
  return(
    rbindlist(lapply(data, function(data_item) {
      json_page_to_tiddler(data_item)
    }), fill = TRUE)
  )
}

#' Convert a JSON Block to Tiddler Format
#'
#' Recursively converts a JSON block and its children into a tiddler-compatible data.table.
#'
#' @param data A JSON object representing a block.
#' @param page_id The ID of the page the block belongs to.
#' @param block_id (Optional) The ID of the parent block.
#' @param ancestors (Optional) A vector of ancestor block IDs.
#' @return A data.table containing the block's metadata, content, and properties.
#' @export
json_block_to_tiddler <- function(data, page_id, block_id = NA, ancestors = NA) {
  if ( length(data$children) > 0 ) {
    df_out <- rbindlist(lapply(data$children, function(k) {
      json_block_to_tiddler(k, page_id = page_id, block_id = data$id, ancestors = c(ancestors, data$id))
    }), fill = TRUE)

    df_out <- rbind(
      df_out,
      data.table(
        page_id = page_id,
        parent_id = block_id,
        block_id = data$id,
        content = data$content,
        lq_ancestors = tid_collapse(ancestors),
        lq_children  = tid_collapse(vapply(data$children, function(x) {x$id}, FUN.VALUE = character(1))),
        as.data.table(data$properties)
      ),
      fill = TRUE)

  } else {

    data.table(
      page_id = page_id,
      parent_id = block_id,
      block_id = data$id,
      content = data$content,
      lq_ancestors = tid_collapse(ancestors),
      as.data.table(data$properties)
    )

  }
}

#' Convert JSON Blocks to Tiddler Format
#'
#' Converts a list of JSON block entries into a combined data.table.
#'
#' @param data A list of JSON objects representing blocks.
#' @return A data.table containing all blocks in tiddler-compatible format.
#' @export
json_blocks_to_tiddler <- function(data) {
  return(
    rbindlist(lapply(data, function(data_item) {
      json_block_to_tiddler(data_item, page_id = data_item[["id"]])
    }), fill = TRUE)
  )
}
