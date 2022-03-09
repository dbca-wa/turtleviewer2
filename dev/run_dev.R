# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
golem::document_and_reload()
run_app()

# -----------------------------------------------------------------------------#
# Release package
# -----------------------------------------------------------------------------#
styler::style_pkg()
spelling::spell_check_package()
spelling::update_wordlist()

# Code and docs tested, working, committed
usethis::use_version()
usethis::edit_file("NEWS.md")
usethis::edit_file("inst/CITATION")

# Git commit, then tag and push
v <- packageVersion("turtleviewer2")
system(glue::glue("git tag -a v{v} -m 'v{v}'"))
system(glue::glue("git push origin v{v}"))
