# this script downloads the flag icons from https://github.com/lipis/flag-icons
# if only does so, if the currently downloaded version is outdated.

# the flags have been created by koppi (https://github.com/koppi) and are provide
# by lipis (https://github.com/lipis). They are licensed under the MIT license
# as is this package.

library(devtools)
library(jsonlite)
library(dplyr)
library(cli)

# load package information from github
repo <- "https://raw.githubusercontent.com/lipis/flag-icons"
flags_info <- paste0(repo, "/refs/heads/main/package.json") %>% 
  fromJSON() %>% 
  `[`(c("name", "version", "author", "license", "repository"))
available_version <- as.numeric_version(flags_info$version)

# load the package information that has been stored locally if it exists
local_flags_info_file <- package_file("inst", "flags", "package.json")
if (file.exists(local_flags_info_file)) {
  local_flags_info <- local_flags_info_file %>% 
    fromJSON() %>% 
    `[`(c("name", "version", "author", "license", "repository"))
  installed_version <- as.numeric_version(local_flags_info$version)
} else {
  installed_version <- as.numeric_version("0.0.0")
}


# if the available version is newer, download the flags
if (available_version > installed_version) {
  cli_alert_info(
    c("The available version ({available_version}) is newer than ",
      "installed version ({installed_version}).")
  )
  cli_alert_info("The flags are downloaded from github")

  # download repo as a zip file and unzip
  zip_file <- tempfile("flags", fileext = "zip")
  download.file(
    "https://github.com/lipis/flag-icons/archive/refs/heads/main.zip",
    zip_file
  )
  unzip(zip_file, exdir = tempdir())

  # delete the old flags and copy the new ones
  svg_target <- package_file("inst", "flags", "svg")
  unlink(svg_target, recursive = TRUE)
  dir.create(svg_target)
  svg_source <- file.path(tempdir(), "flag-icons-main", "flags", "4x3")
  file.copy(list.files(svg_source, "\\.svg$", full.names = TRUE), svg_target)

  # write the file with country info
  file.path(tempdir(), "flag-icons-main", "country.json") %>% 
    fromJSON() %>% 
    select(name, continent, code, capital) %>% 
    write_json(package_file("inst", "flags", "country.json"), pretty = TRUE)

  # write the file with info about the repository
  write_json(flags_info, local_flags_info_file, pretty = TRUE, auto_unbox = TRUE)

  cli_alert_success(
    "Flags have been downloaded and prepared. Version: {available_version}"
  )

} else {
  cli_alert_info(
    c("The installed version ({installed_version}) is the ",
      "current version.")
  )
  cli_alert_info("No download is needed.")
}
