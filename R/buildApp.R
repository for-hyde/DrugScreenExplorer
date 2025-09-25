
#' Build the DrugScreenExplorer Shiny Application
#'
#' This function constructs a Shiny app object by combining a user interface (UI)
#' component and a server function.
#'
#' This interactive web interface provides comprehensive access to all package
#' functionality through an intuitive graphical user interface. Designed for users
#' of all skill levels, the application enables complete analysis of drug screening
#' experiments - from data import to response effect calculation - through a
#' streamlined point-and-click workflow.
#' @param ... Additional parameters passed to both `.build_ui()` and
#'  `.build_server()` functions. The specific parameters available depend on
#'  the implementation of these internal builder functions.
#'
#' @return A Shiny application object (`shiny.appobj`) that can be run using
#'   `shiny::runApp()`. This object contains both the UI definition and server
#'   logic required to execute the application.
#'
#' @export
#' @examples
#' # Build the Shiny application
#' app <- buildApp()
#' # Run the application
#' if (interactive()) {
#'   shiny::runApp(app)
#' }

buildApp <- function(...) {
    ui <- .buildUI(...)
    server <- .buildServer(...)
    app <- shinyApp(ui = ui, server = server)
    return(app)
}

