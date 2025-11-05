# TODO: extract DOIs from external-id col; use rcrossref pkg to get abstract/full text
library(dplyr)
library(tidyr)
library(purrr)
library(googlesheets4)
library(rorcid)
library(stringr)
library(tidytext)
# setting up access to orcid public API: https://ciakovx.github.io/rorcid.html#Setting_up_rorcid

# get orcids
ids <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Tx2HiKUDo-erHHSg8ahUgBJqDVLoPXwp7VluHo1M8Sg/edit?gid=261682304#gid=261682304") |>
  rename(person_orcid = "ORCID", person = "Slack member") |>
  drop_na(person_orcid)

# get works' DOIs
dois <- map(ids$person_orcid,
         ~ works(orcid_id(.x)) |>
           identifiers(type = "doi"))
names(dois) <- ids$person_orcid
dois <- dois |>
  compact() |>
  map_dfr(~ tibble("doi" = .x), .id = "person_orcid")




#---- Keywords
equiv <- c("sarscov2", "covid19", "covid")

person_pubs <- pubs |>
  group_by(person) |>
  unnest_tokens(input = title, output = "token") |>
  anti_join(get_stopwords(), by = c("token" = "word")) |>
  count(token, sort = TRUE) |>
  filter(is.na(as.numeric(token))) |>
  slice_max(order_by = n, n = 3, with_ties = FALSE)

# ---
library(tidytext)
library(topicmodels)
ap_lda <- LDA(AssociatedPress, k=2)
ap_lda <- tidy(ap_lda, matrix = "beta")


# openalex ----------------------------------------------------------------
library(openalexR)
openalex <- openalexR::oa_fetch(
  search = query,
  pages = 1,
  per_page = 10,
  options = list(sort = "relevance_score:desc"),
  verbose = TRUE
)

auid <- "[auid] OR "
pubmed <- clipr::write_clip(cat(ids |> filter(Active=="TRUE") |> pull(person_orcid), sep = auid))

