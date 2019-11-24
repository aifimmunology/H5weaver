#' Render a nicely-formatted table for QC reports
#'
#' @param df A data.frame to format
#'
#' @return an HTML/JavaScript DT object.
#' @export
#'
qc_table <- function(df) {
  DT::datatable(df_wide,
                class = "compact table-striped",
                extensions = "Buttons",
                style = "bootstrap",
                options = list(dom = "<'row'<'col-md-6'l><'col-md-6'fr>><'row'<'col-md-12't>><'row'<'col-md-12'i>><'row'<'col-md-6'B><'col-md-6'p>>",
                               buttons = c('copy', 'csv', 'excel'),
                               lengthMenu = c(15, 25, 50),
                               scrollX = TRUE),
                rownames = FALSE)
}
