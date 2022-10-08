#' A collapsible box
#'
#' A custom collapsible box with Shiny inputs upon collapse, more or less stolen from \pkg{shinyBS}.
#'
#' @param id String specifying the identifier for this object, to use as a field of the Shiny input.
#' @param title String specifying the title of the box for use in the UI.
#' @param ... Additional UI elements to show inside the box.
#' @param open Logical scalar indicating whether this box should be open upon initialization.
#' @param style String specifying the box style, defaults to \code{"default"}.
#'
#' @return A HTML tag object containing a collapsible box.
#'
#' @section Comments on \pkg{shinyBS}:
#' We would have preferred to use \code{bsCollapse} from \pkg{shinyBS}.
#' However, that package does not seem to be under active maintenance, and there are several aspects that make it difficult to use.
#' Specifically, it does not seem to behave well with conditional elements inside the box,
#' and it also does needs a \code{Depends:} relationship with \pkg{shinyBS}.
#'
#' For these reasons, we created our own collapsible box, taking code from \code{shinyBS} where appropriate.
#' The underlying Javascript code for this object is present in \code{inst/www} and is attached to the search path for Shiny resources upon loading \pkg{iSEE}.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#' @seealso
#' \pkg{shinyBS}, from which the Javascript code was derived.
#'
#' @examples
#' library(shiny)
#' ui <- fluidPage(
#'     fluidRow(
#'         column(width=6L, collapseBox(
#'             id="collapseBox_001",
#'             title="Collapsible box",
#'             open=FALSE,
#'             p("A simple paragraph."),
#'             selectInput("selectInput_001",
#'                 label="Select a letter",
#'                 choices=LETTERS, selected="A"
#'             )
#'         )),
#'         column(width=6L, p(
#'             "On the left is an example of a", code("collapseBox"), ".", br(),
#'             "Click on the title of the box to expand or collapse its contents."
#'         ))
#'     )
#' )
#' 
#' shinyApp(ui, server = function(input, output) { })
#'
#' @export
#' @importFrom shiny h4 tagList tags singleton
collapseBox <- function(id, title, ..., open = FALSE, style = NULL) {
    if(is.null(style)) {
        style <- "default"
    }

    sub.id <- paste0("collapse_", id)
    bsTag <- tags$div(class = paste0("isee-collapse-box panel panel-", style),
        id=id,
        value = title,
        tags$div(class = "panel-heading",
            role = "tab",
            id = paste0("heading_", sub.id),
            tags$h4(class = "panel-title",
                tags$a("data-toggle" = "collapse",
                    href = paste0("#", sub.id),
                    title
                )
            )
        ),
        tags$div(
            id = sub.id,
            class = sprintf("panel-collapse %s", ifelse(open, "collapse in", "collapse")),
            role = "tabpanel",
            tags$div(class = "panel-body", list(...))
        )
    )

    tagList(singleton(tags$head(tags$script(src = "iSEE/collapseBox.js"))), bsTag)
}
