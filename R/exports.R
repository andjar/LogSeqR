#' Export Data to a Server
#'
#' This function is a placeholder for exporting data to a server.
#'
#' @param data A list containing the data to be exported.
#' @return Placeholder return value.
#' @export
export_to_server <- function(data) {

}

#' Export Data to a JSON File
#'
#' Converts pages and blocks to a JSON-compatible format and writes the data to a file.
#'
#' @param data A list containing `pages` and `blocks` data.tables.
#' @param file_name The name of the JSON file to write to. Defaults to "tiddlers.json".
#' @return NULL. The JSON file is written to disk as a side effect.
#' @details The function processes `pages` and `blocks` to ensure compatibility with the JSON format.
#'   Column names are modified to match the required format, and the combined data is written to the specified file.
#' @export
export_to_json <- function(data, file_name = "tiddlers.json", use_streams = FALSE) {
  jsonlite::write_json(
    prepare_export(data = data, use_streams = use_streams),
    file_name
  )
}

prepare_export <- function(data, use_streams = FALSE) {
  pages <- data$pages
  pages[, `lq-type` := "page"]
  colnames(pages)[colnames(pages) == "title"] <- "lq-title"
  colnames(pages)[colnames(pages) == "page_name"] <- "title"
  colnames(pages)[colnames(pages) == "id"] <- "lq-page-id"

  blocks <- data$blocks
  blocks[, `lq-type` := "block"]
  colnames(blocks)[colnames(blocks) == "title"] <- "lq-title"
  colnames(blocks)[colnames(blocks) == "lq_ancestors"] <- "lq-ancestors"
  colnames(blocks)[colnames(blocks) == "lq_task"] <- "lq-task"
  colnames(blocks)[colnames(blocks) == "lq_keywords"] <- "lq-keywords"
  colnames(blocks)[colnames(blocks) == "block_id"] <- "title"
  colnames(blocks)[colnames(blocks) == "text"] <- "lq_text"
  colnames(blocks)[colnames(blocks) == "content"] <- "text"

  if ( use_streams ) {
    pages[, `stream-type` := "default"]
    blocks[, `stream-type` := "default"]

    colnames(blocks)[colnames(blocks) == "parent_id"] <- "parent"

    colnames(pages)[colnames(pages) == "lq_children"] <- "stream-list"
    colnames(blocks)[colnames(blocks) == "lq_children"] <- "stream-list"

    blocks <- blocks[parent != title]
  }

  return(rbind(pages, blocks, fill = TRUE))
}

#' Export Data to Tiddler Files
#'
#' This function is a placeholder for exporting data to individual tiddler files.
#'
#' @param data A list containing `pages` and `blocks` data.tables to be exported.
#' @return Placeholder return value.
#' @export
export_to_tids <- function(data) {

}
