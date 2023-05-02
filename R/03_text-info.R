# ==============================================================================
# * Set up workspace and load libraries
# ==============================================================================
options(scipen = 999)

library(dplyr)
library(ggforce)
library(gtsummary)
library(stringr)
library(DT)


# * Helper function for searching text
# ==============================================================================
search_fun = function(search_term, text) {
    text_extract = str_extract_all(
      text,
      regex(paste0("(\\w+\\s+)?(\\b\\w*", search_term, "\\w*\\b)(\\s+\\w+)?"),
        ignore_case=TRUE)) |>
      unlist() |>
      unique() |>
      paste0(collapse = "; ")
    gsub("\\s+", " ", text_extract)
}


# * Load data and update for publication ready results
# ==============================================================================
out_dir = "results"
if( !dir.exists(out_dir) ) dir.create(out_dir)

# Note: the text data hasn't been shared in the repository for privacy reasons
vl = readRDS("data/Coleman_et_al_SI_3_unique-listings-text.RDS") |>
  filter(species %in% "K. picta")


# ==============================================================================
# * Number of bat listings mentioning K. picta and associated names
# ==============================================================================
species_terms = c("kerivoula picta", "painted bat",
  "butterfly bat", "asiatic yellow painted bat", "asiatic painted bat", "asian painted bat",
  "painted woolly bat", "orange fire bat", "red orange fire bat", "fire bat")
observed_typos = c("painted wooly bat")
search_terms_ind = c(species_terms, observed_typos)
search_terms = paste0(search_terms_ind, collapse = "|")
# Note: kerivoula pellucida matches "kerivoula"


names_df = vl |>
  select(shop, listing, seller, species, title, description, description_tbl) |> 
  mutate(
    description = ifelse(is.na(description), "", description),
    description_tbl = ifelse(is.na(description_tbl), "", description_tbl)
  ) |>
  mutate(
    desc = sprintf(
      "DESCRIPTION: \n %s; \n DESCRIPTION TBL: \n %s",
      description, description_tbl)
  ) |>
  mutate(
    text = sprintf(
      "TITLE: \n %s; \n DESCRIPTION TEXT: \n %s",
      tolower(title), tolower(desc))
  ) |>
  mutate(
    kp_title = grepl( search_terms, title, ignore.case = TRUE),
    kp_desc = grepl( search_terms, text, ignore.case = TRUE),
    kp_text = kp_title | kp_desc
  ) |>
  select(-description) |>
  data.frame()


names_df |>
  select(kp_title, kp_desc, kp_text) |>
  tbl_summary(
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    sort = all_categorical() ~ "frequency"
  )|>
  bold_labels() |>
  modify_header(
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) |>
  modify_caption("Breakdown of whether listing title/text contained K. picta associated names") |>
  as_kable()

# |**Characteristic** | **Overall**<br>N = 215 (100.0%) |
# |:------------------|:-------------------------------:|
# |__kp_title__       |            161 (75%)            |
# |__kp_desc__        |            186 (87%)            |
# |__kp_text__        |            186 (87%)            |
  

text_search = lapply(names_df$text, function(text, search_terms_ind) {
  term_info = sapply(search_terms_ind, search_fun, text = text)
  term_df = data.frame(t(term_info))
  term_df[ term_df %in% "" ] = NA
  term_df
  }, search_terms_ind) |>
  bind_rows()

names(text_search) = gsub("\\. | ", "_", search_terms_ind)

datatable(text_search, options = list(pageLength = 215))

text_search_n = lapply(text_search, function(text_col) ifelse(is.na(text_col), 0, 1)) |>
  do.call(what = "cbind") |>
  data.frame() |>
  # filter_all(any_vars(. != 0)) |>
  mutate(
    # painted_bat = ifelse(painted_bat == asian_painted_bat, 0, painted_bat),
    # Not doing the above exclusion because painted bat generally occurred on its own as
    # well as in addition to asian painted bat or other similar terms
    orange_fire_bat = ifelse(orange_fire_bat == red_orange_fire_bat, 0, orange_fire_bat),
    fire_bat = ifelse(fire_bat == orange_fire_bat | fire_bat == red_orange_fire_bat, 0, fire_bat))

comm_names = sort(sapply(text_search_n, function(search_n) sum(search_n)/186 * 100), decreasing = TRUE)
names(comm_names)

cat(sprintf('"%s" (%s%%)', gsub("_", " ", names(comm_names)), round(comm_names)), sep = ", ")



# ==============================================================================
# * Search for goth/halloween/christmas decor, etc
# ==============================================================================

addl_search_terms = c("goth", "halloween", "christmas")

addl_search = lapply(names_df$text, function(text, addl_search_terms) {
  term_info = sapply(addl_search_terms, search_fun, text = text)
  term_df = data.frame(t(term_info))
  term_df[ term_df %in% "" ] = NA
  term_df
}, addl_search_terms) |>
  bind_rows()

names(addl_search) = gsub("\\. | ", "_", addl_search_terms)

addl_search_df = cbind(names_df[ , c("listing", "shop", "seller")], addl_search)
addl_search_df = addl_search_df[ !(is.na(addl_search_df$halloween) & is.na(addl_search_df$goth) & is.na(addl_search_df$christmas)), ]

datatable(addl_search_df, options = list(pageLength = 215))

dim(addl_search_df)

addl_search_df |>
  mutate(goth = !is.na(goth),
    halloween = !is.na(halloween),
    christmas = !is.na(christmas)) |>
  group_by(seller) |>
  summarise(goth = sum(goth),
    halloween = sum(halloween),
    christmas = sum(christmas)) |>
  data.frame() |>
  adorn_totals(where = "col") |>
  arrange(-Total) |>
  adorn_totals()

table(addl_search_df$shop)  
sort(table(tolower(addl_search_df$seller)))
prop.table(sort(table(tolower(addl_search_df$seller))))

addl_search_df |>
  filter(!seller %in% "seller_006") |>
  mutate(goth = !is.na(goth),
    halloween = !is.na(halloween),
    christmas = !is.na(christmas)) |>
  group_by(seller) |>
  summarise(goth = sum(goth),
    halloween = sum(halloween),
    christmas = sum(christmas)) |>
  data.frame() |>
  adorn_totals(where = "col") |>
  arrange(-Total) |>
  adorn_totals()



# ==============================================================================
# * Ethics
# ==============================================================================
vl$ethics = (grepl("ethic|sustain|responsibl|conserv", vl$description, ignore.case = TRUE) |
                   grepl("ethic|sustain|responsibl|conserv", vl$title, ignore.case = TRUE))

ethics_df = vl |>
  filter(ethics) |>
  select(listing, seller, title, description, bat, species, ethics) |>
  arrange(species)

# The text was read to manually verify for the presence of terms referencing
# ethical sourcing (however inaccurate they may be).


