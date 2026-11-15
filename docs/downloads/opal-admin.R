# Optional administrator script, run from book-work after mounting both databases.
# This changes the selected Opal: installs/publishes packages, creates resources,
# and grants a named analyst access. It does not change disclosure settings.
library(opalr)
source("tools/book/connection.R")
stopifnot(book_driver == "Opal", nzchar(opal_url))
admin_user <- Sys.getenv("BOOK_OPAL_ADMIN_USER", "administrator")
admin_password <- Sys.getenv("BOOK_OPAL_ADMIN_PASSWORD")
analyst <- Sys.getenv("BOOK_OPAL_USER")
analyst_password <- Sys.getenv("BOOK_OPAL_PASSWORD")
stopifnot(nzchar(admin_password), nzchar(analyst), nzchar(analyst_password),
          analyst != admin_user)
local({
  opal <- opal.login(admin_user, admin_password, url = opal_url)
  on.exit(opal.logout(opal))
  # Allow the administrator to install R packages, as in Opal's General settings.
  if (!isTRUE(opal.get(opal, "system", "conf", "general")$allowRPackageManagement))
    invisible(opal.put(opal, "system", "conf", "general", "_rPackage"))
  stopifnot(isTRUE(opal.get(opal, "system", "conf", "general")$allowRPackageManagement))
  if (!dsadmin.profile_exists(opal, opal_profile))
    dsadmin.profile_create(opal, opal_profile, cluster = "default")
  stopifnot(dsadmin.install_github_package(opal, "dsBase", username = "datashield",
    ref = "6.3.5", profile = opal_profile))
  stopifnot(dsadmin.install_github_package(opal, "dsOMOP", username = "isglobal-brge",
    ref = "2.10.5", profile = opal_profile))
  dsadmin.publish_package(opal, "dsBase", profile = opal_profile)
  dsadmin.publish_package(opal, "dsOMOP", profile = opal_profile)
  dsadmin.profile_enable(opal, opal_profile)
  print(dsadmin.package_descriptions(opal, fields = c("Package", "Version"),
                                    profile = opal_profile))
  if (!oadmin.user_exists(opal, analyst))
    invisible(oadmin.user_add(opal, analyst, password = analyst_password))
  dsadmin.perm_add(opal, analyst, permission = "use")
  dsadmin.profile_perm_add(opal, opal_profile, analyst, permission = "use")
  if (!opal.project_exists(opal, opal_project))
    opal.project_create(opal, opal_project)
  for (key in names(opal_resources)) {
    resource <- opal_resources[[key]]
    url <- paste0("omop+dbi:sqlite:///data/practice/", key, ".sqlite")
    if (!opal.resource_exists(opal, opal_project, resource))
      opal.resource_create(opal, opal_project, resource, url = url,
        format = "omop.dbi.db", package = "dsOMOP")
    ref <- opal.resource(opal, opal_project, resource)
    # Never silently accept an existing resource pointing to different data.
    stopifnot(identical(ref$resource$url, url),
              identical(ref$resource$format, "omop.dbi.db"))
    opal.resource_perm_add(opal, opal_project, resource, analyst, permission = "view")
  }
  print(opal.resources(opal, opal_project)[c("name", "project", "url", "format")])
})
