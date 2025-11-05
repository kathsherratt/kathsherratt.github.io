library(here)
library(gh)
owner <- "kathsherratt"
repo <- "kathsherratt"
issues <- gh("/repos/:owner/:repo/issues",
             owner = owner, repo = repo,
             state = "open", .limit = Inf)

for (i in issues) {
  date <- strsplit(issues[[1]][["updated_at"]], "T")[[1]][1]
  title <- issues[[1]][["title"]]
  body <- issues[[1]][["body"]]
  writeLines(text = c(paste("#", title,  "\n"), body),
             con = here("blog", paste0(date, ".md")))
}


