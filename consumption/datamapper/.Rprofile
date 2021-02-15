# set up the CRAN repository to download from
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/"
  options(repos = r)
})

# set up the libPaths to include the library in the R installation
# and the local library, but to exclude the defauilt one set up by RStudio
local.lib.path <- file.path(getwd(), "Rlib")
.libPaths(local.lib.path)

current.lib.paths <- .libPaths()

new.libs <- c()
for (i in 1:length(current.lib.paths)) {
  if (!grepl("win-library", current.lib.paths[i])) {
    new.libs <- c(new.libs, current.lib.paths[[i]])
  }
}

.libPaths(new.libs)

final.libs <- c(new.libs, local.lib.path)

.libPaths(final.libs)

print(.libPaths())

library("utils")

# download libraries that are required
packages <- c("dplyr","moments","plyr","haven","tidyr","jsonlite","plotly")

for (i in 1:length(packages)) {
  if (packages[i] %in% rownames(installed.packages(local.lib.path)) == FALSE) {
    print(sprintf("Loading library: %s", packages[i]))
    install.packages(packages[i], local.lib.path, quiet = T)
  } else {
    print(sprintf("Library %s already loaded", packages[i]))
  }
}


# assign home directory to where this .Rprofile is stored
home.directory <- getwd()
options(stringsAsFactors = FALSE)

rm(i)
rm(final.libs)
rm(new.libs)
rm(current.lib.paths)
# rm(packages)
rm(local.lib.path)
