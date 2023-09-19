# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
#options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
Hermione::run_app()


#Run application with options

# Hermione::run_app <- function(
#     name = "example",
#     time = Sys.time(),
#     port = 2811
# ) {
#   with_golem_options(
#     app = shinyApp(ui = app_ui,
#                    server = app_server,
#                    options = list(port = port)),
#     golem_opts = list(name = name, time = time)
#   )
# }


