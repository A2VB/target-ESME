usethis::use_git()
usethis::use_git_config(user.name = "A2VB", user.email = "alison.antoine@icloud.com")

# git config --global --add safe.directory "*"

## usethis::use_logo("../logo/monlogo.png")
usethis::use_readme_rmd()
usethis::use_pkgdown()

##usethis::use_data(mondataframe, overwrite = TRUE)

usethis::use_build_ignore("devtools_history.R")
usethis::use_git_ignore("devtools_history.R")

#pkgdown::build_site()

usethis::use_pkgdown_github_pages()
usethis::use_version()

devtools::build_readme()
