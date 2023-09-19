#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = "url",
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      controlbar_width="40%",
      last_KG_update="2023-08-25",
      last_twitterDB_update="2023-08-25",
      twitter_data_from = as.Date("30-05-2020","%d-%m-%Y"),# start date to be used in the dashboard input UI
      twitter_data_to  = as.Date("27-08-2020","%d-%m-%Y"),#end date to be used in the dashboard input UI
      API_KEY = Sys.getenv("KG_KEY") # you need to set an environment variable named KG_KEY. It can be done from R using the following command Sys.setenv(KG_KEY = “the key used to access the KG of the MUHAI inequality observatory”)
      )
  )
}
