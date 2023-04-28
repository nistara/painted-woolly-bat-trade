options(scipen = 999)


# * Libraries
# ==============================================================================
library(DT)
library(conflicted)
library(cowplot)
library(data.table)
library(dplyr)
library(extrafont)
library(flextable)
library(ggforce)
library(ggpattern)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(gtsummary)
library(hrbrthemes)
library(janitor)
library(lubridate)
# library(maps)
# library(maptools)
library(stringr)
library(tidyr)
# extrafont::font_import()

extrafont::loadfonts(quiet = TRUE)
  
conflicts_prefer(lubridate::week)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)



# * Load data and update for publication ready results
# ==============================================================================
out_dir = "results"
if( !dir.exists(out_dir) ) dir.create(out_dir)

vl = readRDS("data/verified_listing_data.RDS") # verified, unique listings
bat_df = readRDS("data/bat-listings.RDS") # all bat listings



# ==============================================================================
# * Search result dates
# ==============================================================================
search_dates = sort(unique(bat_df$date[ bat_df$shop %in% c("Etsy", "eBay") ]))
search_dates[1]
search_dates[length(search_dates)]



# ==============================================================================
# * Number of identified bat listings
# ==============================================================================
# Verified bat listings	N = 1,3661
#     FALSE	505 (37.0%)
#     TRUE	856 (62.7%)
#     UNSURE	5 (0.37%)



# ==============================================================================
# * Tbl 1. Numbers and characteristics of identified bat listings
# ==============================================================================
tbl_1 = vl |>
  select(species, shop, format, part, price_mean) |>
  tbl_summary(
    by = species,
    label = list(
      shop ~ "Shop",
      format ~ "Format",
      part ~ "Bat part",
      price_mean ~ "Price ($)"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      price_mean ~ "{median} ({IQR})"
    ),
    sort = all_categorical() ~ "frequency"
  )|>
  bold_labels() |>
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) %>%
  modify_caption("Characteristics of identified bat listings") |>
  add_overall(
    last = TRUE,
    col_label = "**All bat listings**<br>N = {N}"
  )

tbl_1

tbl_1 |>
  as_flex_table() |>
  save_as_docx(path = "results/tbl-1_characteristics-bat-listings.docx")



# ==============================================================================
# * Tbl S1. Comparison between the listings of K. picta and other bat species on Etsy"
# ==============================================================================
tbl_s1 = vl |>
  filter(species %in% c("K. picta", "Other bat spp.") & shop %in% "Etsy") |>
  select(species, format, part) |>
  tbl_summary(
    by = species,
    label = list(
      format ~ "Format",
      part ~ "Bat part"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    sort = all_categorical() ~ "frequency")|>
  bold_labels() |>
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) %>%
  modify_caption("Comparison between the listings of K. picta and other bat species on Etsy") |>
  add_overall(
    last = TRUE,
    col_label = "**Total**<br>N = {N}"
  ) |>
  add_p()

tbl_s1

tbl_s1 |>
  as_flex_table() |>
  save_as_docx(path = "results/tbl-s1_k-picta-vs-other-bats-etsy.docx")



# ==============================================================================
# * Tbl S2. Comparison between K. picta listings on ebay and etsy
# ==============================================================================
tbl_s2 = vl |>
  filter(species %in% "K. picta" & shop %in% c("eBay", "Etsy")) |>
  select(format, part, shop) |>
  tbl_summary(
    by = shop,
    label = list(
      format ~ "Format",
      part ~ "Bat part"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    sort = all_categorical() ~ "frequency")|>
  bold_labels() |>
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p, digits=1)}%)"
  ) %>%
  modify_caption("Comparison between the listings of K. picta across Etsy and eBay") |>
  add_overall(
    last = TRUE,
    col_label = "**Total**<br>N = {N}"
  ) |>
  add_p()

tbl_s2

tbl_s2 |>
  as_flex_table() |>
  save_as_docx(path = "results/tbl-2_k-picta-etsy-vs-ebay.docx")



# ==============================================================================
# Fig 2. Plots for bat formats
# ==============================================================================
format_order = vl |>
  filter(species %in% c("K. picta", "Other bat spp.") ) |>
  group_by(format) |>
  tally() |>
  arrange(-n) |>
  pull(format)


format_bar_plot = vl |>
  filter(species %in% c("K. picta", "Other bat spp.") & shop %in% c("eBay", "Etsy")) |>
  mutate(species = factor(species, levels = c("Other bat spp.", "K. picta"))) |>
  group_by(species, shop, format) |>
  tally() |>
  arrange(n) |>
  mutate(format = factor(format, levels = format_order)) |>
  mutate(shop = factor(shop, levels = c("Etsy", "eBay"))) |>
  ggplot( aes(x = shop, y = n, fill = species, alpha = species)) +
  geom_bar(stat="identity", width = 0.5) +
  theme_ipsum() +
  ylab("Number of listings") +
  ylim(c(0, 400)) +
  xlab("") +
  facet_wrap(~ format, nrow = 1, strip.position = "bottom") +
  scale_fill_manual("", values = c("#F0C19C", "#E88024"),
    labels = c("Other bats species", expression(italic("K. picta")))) +
  scale_alpha_discrete(guide = "none", range = c(0.55, 0.9)) +
  theme(
    strip.placement = "outside",
    strip.text = element_text(face = "bold", hjust = 0.5, vjust = 0, margin = margin(0, 0, 0, 0, "pt")),
    axis.text.x = element_text(size = 12, vjust=0.5, margin=margin(-15,0,0,0)),
    axis.title.y = element_text(size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    strip.text.x = element_text(size = 14),
    legend.text=element_text(size=14),
    ) +
  guides(fill = guide_legend(reverse = TRUE))

format_bar_plot

ggsave("results/plot_format-bar-plot.jpg", format_bar_plot, width = 9)



# ==============================================================================
# * Fig 3. Price plots: line graph
# ==============================================================================
pr_weeks = bat_df |>
  select(date, listing) |>
  mutate(week = week(date)) |>
  select(week, listing) |>
  group_by(listing, week) |>
  unique() |>
  data.frame() |>
  group_by(listing) |>
  tally() |>
  arrange(-n)

pr_weeks |>
  mutate(
    nweek = ifelse(n >= 8, ">= 8","<= 7" )
  ) |>
  group_by(nweek) |>
  tally()

wk8_listings = pr_weeks$listing[ pr_weeks$n >= 8 ] # 256 listings

price_df = bat_df |>
  select(shop, species, listing, price, date) |>
  filter(species %in% c("K. picta", "Other bat spp.")) |>
  filter( !(shop %in% "eBay" & species !="K. picta") ) |>
  filter(listing %in% wk8_listings & shop %in% c("eBay", "Etsy")) |>
  mutate(week = floor_date(date, "week")) |>
  mutate(bat_shop = paste0(shop, " ", species))

setDT(price_df)[, count := uniqueN(listing), by = bat_shop] 
setDT(price_df)[, mean_price := mean(price), by = bat_shop]

plot_df = price_df |>
  mutate(mean_price = round(mean_price, digits = 2)) |>
  select(bat_shop, week, price) |>
  group_by(bat_shop, week) |>
  summarize(weekly_shop_price = mean(price),
    min_price = min(price),
    max_price = max(price),
    std_error = sd(price)) |>
  data.frame()

plot_labels = plot_df |>
  data.frame() |>
  filter(week == max(week)) |>
  mutate(shop = str_split_fixed(bat_shop, " ", 2)[, 1]) |>
  mutate(label = case_when(
    str_detect(bat_shop, "picta") ~ paste0("<i>K. picta</i> on ", shop),
    .default = paste0("Other species on ", shop)))

priceline_plot = plot_df |>
  ggplot( aes(x = week, y = weekly_shop_price, group = bat_shop, color = bat_shop)) +
  geom_line() +
  geom_point(aes(color = bat_shop), size = 2) +
  # ggtitle("Average weekly price of bat listings by shop") +
  theme_ipsum(
  subtitle_family = "Arial") +
  ylab("Average weekly price (USD)") +
  ylim(c(45, 90)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_color_discrete(guide = FALSE) +
  scale_color_tableau() +
  theme(legend.position = "none",
    axis.title.y = element_text(size = 14)) +
  theme(plot.margin = unit(c(1,7,1,1), "lines")) +
  scale_x_date(
    limits = c(min(plot_df$week) - 7, max(plot_df$week) + 13),
    date_labels = "%b\n%Y") +
  xlab("") +
  geom_richtext(data = plot_labels, aes(x = week, y = weekly_shop_price, label = label,
    hjust = -0.1, label.size = 0.5, vjust = 0.7))

priceline_plot

ggsave("results/fig-3_mean-weekly-price.jpg", priceline_plot, width = 7)


# Figure caption
price_df |>
  select(bat_shop, mean_price, count) |>
  unique()

#              bat_shop  mean_price  count
# 1:       eBay K. picta   69.00238    42
# 2:       Etsy K. picta   74.34407    60
# 3: Etsy Other bat spp.   66.76129   130

# https://stackoverflow.com/a/68357304/5443003



# ==============================================================================
# * Differences in average weekly prices for k. picta vs other bats on etsy
# ==============================================================================
etsy_price_df = plot_df |>
  select(bat_shop, week, weekly_shop_price) |>
  filter(grepl("Etsy", bat_shop)) |>
  spread(bat_shop, weekly_shop_price) |>
  setNames( c("week", "Etsy_Kpicta", "Etsy_OtherBats")) |>
  mutate(diff_price = Etsy_Kpicta - Etsy_OtherBats)

mean_price_kpicta = round(mean(etsy_price_df$Etsy_Kpicta), 2)
mean_price_others = round(mean(etsy_price_df$Etsy_OtherBats), 2)
mean_price_diff = mean_price_kpicta - mean_price_others



# ==============================================================================
# * Differences in average weekly prices compared to second week of Dec
# ==============================================================================
peak_prices = plot_df |>
  select(bat_shop, week, weekly_shop_price) |>
  mutate(weekly_shop_price = round(weekly_shop_price, 2)) |>
  group_by(bat_shop) |>
  top_n(1, weekly_shop_price)
  
peak_week = unique(peak_prices$week)

pre_peak_prices = plot_df |>
  filter(week < peak_week) |>
  group_by(bat_shop) |>
  summarise(pre_peak_price = mean(weekly_shop_price)) |>
  mutate(pre_peak_price = round(pre_peak_price, 2))
 
peak_prices |>
  left_join(pre_peak_prices, by = "bat_shop") |>
  mutate(price_diff = weekly_shop_price - pre_peak_price) |>
  data.frame()



# ==============================================================================
# * Fig other. Price plots: K. picta by shop
# ==============================================================================
shop_sample_size =
  vl |>
  filter(species %in% "K. picta") |>
  group_by(shop) |>
  summarize(num = n())

shop_price_plot = vl %>%
  filter(species %in% "K. picta") |>
  left_join(shop_sample_size) |>
  mutate(myaxis = paste0(shop, "\n", "n=", num)) |>
  ggplot( aes(x=reorder(myaxis, price_mean, mean), y=price_mean, color = shop, fill = shop)) +
  geom_violin(width=0.75, alpha = 0.2) +
  geom_boxplot(width=0.1, alpha=0.4) +
  geom_sina(alpha = .5, size = 0.5) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Price for K. picta listings by shop") +
  xlab("") +
  ylab("Price ($)") +
  coord_flip() 

# shop_price_plot

# ggsave("results/fig-z_kpicta-price-by-shop.jpg", shop_price_plot, width = 7)
# 
# # Caption text
# theme_gtsummary_eda()
# 
# vl |>
#   filter(species %in% "K. picta") |>
#   select(shop, price_mean) |>
#   tbl_summary(by = shop)



# ==============================================================================
# * Fig other. Price violin plots: K. picta by format
# ==============================================================================
format_sample_size =
  vl |>
  filter(species %in% "K. picta") |>
  group_by(format) |>
  summarize(num = n())

format_price_plot = vl %>%
  filter(species %in% "K. picta") |>
  left_join(format_sample_size) |>
  mutate(myaxis = paste0(format, "\n", "n=", num)) |>
  ggplot( aes(x=reorder(myaxis, price_mean, mean), y=price_mean, color = format,
    fill = format)) +
  geom_violin(width=0.75, alpha = 0.2) +
  geom_boxplot(width=0.1, alpha=0.4) +
  geom_sina(alpha = .5, size = 0.5) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Price for K. picta listings by format type") +
  xlab("") +
  ylab("Price ($)") +
  coord_flip() 

# format_price_plot

# ggsave("results/plot-3_species-price-by-format.jpg", format_price_plot, width = 7)
# 
# vl |>
#   filter(species %in% "K. picta") |>
#   select(format, price_mean) |>
#   tbl_summary(by = format)



# ==============================================================================
# * Price for specimen/part type
# ==============================================================================
vl |>
  filter(species %in% "K. picta") |>
  select(part, price_mean) |>
  tbl_summary(by = part)


  
# ==============================================================================
# * Fig 5. Shipping country plots
# ==============================================================================

ship_join = bat_df |>
  select(ship_country, ship_state, listing) |>
  unique()

ship_df = vl |>
  filter(species %in% c("K. picta", "Other bat spp.") & shop %in% c("eBay", "Etsy")) |>
  left_join(ship_join, by = "listing") |>
  select(listing, ship_country, ship_state, bat, species, shop) |>
  mutate(country = ship_country,
    state = ship_state)
  
country_order = ship_df |>
  group_by(country) |>
  tally() |>
  arrange(n) |>
  pull(country)


# Bar plot----------------------------------------------------------------------
country_bar_plot = ship_df %>%
  group_by(species, country) |>
  tally() |>
  mutate(country = factor(country, levels = country_order)) |>
  mutate(species = factor(species, levels = c("Other bat spp.", "K. picta"))) |>
  ggplot( aes(x=country, y=n, fill = species, alpha = species)) +
  geom_bar(stat="identity", width = 0.5) +
    coord_flip() +
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="top",
      legend.title=element_blank(),
      axis.title.x = element_text(size = 10),
      legend.text=element_text(size=10),
    ) +
    xlab("") +
  ylab("Frequency") +
  scale_fill_manual("", values = c("#F0C19C", "#E88024"),
    labels = c("Other bats", expression(italic("K. picta")))) +
  scale_alpha_discrete(guide = "none", range = c(0.55, 0.9)) +
  ylim(c(0, 600)) +
  guides(fill = guide_legend(reverse = TRUE))

country_bar_plot

# Numbers behind figure
ship_df |>
  group_by(species, country) |>
  tally() |>
  spread(species, n) |>
  adorn_totals("col") |>
  arrange(-Total)

if( FALSE ) {

  ship_df |>
    select(species, country) |>
    tbl_summary(
      by = species,
      statistic = list(
        all_categorical() ~ "{n} ({p}%)"
      ),
      sort = all_categorical() ~ "frequency"
    )|>
    bold_labels() |>
    add_overall(
      last = TRUE
    )

}


# * Map-------------------------------------------------------------------------
kpicta_countries = unique(ship_df$country[ ship_df$species %in% "K. picta" ])
all_countries = unique(ship_df$country)
other_bat_countries = setdiff(all_countries, kpicta_countries)

kp_level1 = "*K. picta* and/or other species"
kp_level2 = "Other species only"

country_join = data.frame(
  country = c(kpicta_countries, other_bat_countries),
  kpicta = c(
    rep(kp_level1, length(kpicta_countries)),
    rep(kp_level2, length(other_bat_countries))))

kpicta_origin = c("Bangladesh", "Cambodia", "China", "India", "Indonesia",
  "Laos", "Myanmar", "Nepal", "Sri Lanka", "Thailand", "Vietnam")

countries = map_data("world")
countries$country = countries$region

countries = countries |>
  filter(country != "Antarctica") |>
  left_join(country_join, by = "country") |>
  mutate(kpicta = factor(kpicta, levels = c(kp_level1, kp_level2)))

kpicta_countries_map = countries |>
  filter(country %in% kpicta_origin)

country_map_plot = ggplot(data = countries) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#E3E1E0", color = "white") + 
  geom_polygon(aes(x = long, y = lat, fill = kpicta, group = group), color = "white") +
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual("", values = c("#E88024", "#F0C19C"), na.translate = FALSE) +
  theme(
    legend.position='top',
    legend.text = element_markdown()) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))

country_origin_plot = country_map_plot +
  geom_polygon_pattern(data = kpicta_countries_map,
    aes(x = long, y = lat, group = group),
    pattern = 'stripe',
    pattern_color = "#844914",
    pattern_fill = "#844914",
    fill = NA,
    pattern_density = 0.005, pattern_spacing = 0.025,
    pattern_key_scale_factor = 0.6
  ) +
  geom_polygon(data = kpicta_countries_map,
    aes(x = long, y = lat, fill = NA, group = group), color = "white")

country_origin_plot

# Combine bar and map------------------------------------------------------
country_plot = plot_grid(country_bar_plot,
  country_origin_plot, ncol = 1, rel_heights = c(1.5, 1),
  labels = c("A.", "B."))

country_plot

ggsave("results/fig-5_ship-country.jpg", country_plot, width = 7)
ggsave("results/fig-5_ship-country.svg", country_plot, width = 7)



# ==============================================================================
# * Shipping state plots
# ==============================================================================
state_tbl = data.frame(state_abb = state.abb, state = state.name)

# Bar plot----------------------------------------------------------------------
state_order = ship_df |>
  left_join(state_tbl, by = "state") |>
  mutate(state = sprintf("%s (%s)", state, state_abb)) |>
  filter( !is.na(state) ) |>
  group_by(state) |>
  tally() |>
  arrange(n) |>
  pull(state)

state_bar_plot = ship_df %>%
  filter( !is.na(state) ) |>
  left_join(state_tbl, by = "state") |>
  mutate(state = sprintf("%s (%s)", state, state_abb)) |>
  mutate(state = factor(state, levels = state_order)) |>
  group_by(species, state) |>
  tally() |>
  mutate(species = factor(species, levels = c("Other bat spp.", "K. picta"))) |>
  ggplot( aes(x=state, y=n, fill = species, alpha = species)) +
  geom_bar(stat="identity", width = 0.5) +
    coord_flip() +
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="top",
      axis.title.x = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 6),
      legend.text=element_text(size=13)
    ) +
    xlab("") +
  ylab("Frequency") +
  ylim(0, 150) +
  scale_fill_manual("", values = c("#F0C19C", "#E88024"),
            labels = c("Other bats", expression(italic("K. picta")))) +
  scale_alpha_discrete(guide = "none", range = c(0.55, 0.9)) +
  guides(fill = guide_legend(reverse = TRUE))

state_bar_plot


# * Map plot--------------------------------------------------------------------
states = map_data("state")
states$state = str_to_title(states$region)

state_df = ship_df[ !is.na(ship_df$state), ]

kpicta_states = unique(state_df$state[ state_df$species %in% "K. picta" ])
all_states = unique(state_df$state)
other_bat_states = setdiff(all_states, kpicta_states)

state_join = data.frame(
  state = c(kpicta_states, other_bat_states),
  species = c(
    rep("*K. picta* and/or other species", length(kpicta_states)),
    rep("Other species only", length(other_bat_states))
    )
  )
  
states = states |>
  left_join(state_join, by = "state") |>
  mutate(species = factor(species, levels = c("Other species only", "*K. picta* and/or other species")))

state_map_plot = ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#E3E1E0", color = "white") + 
  geom_polygon(aes(x = long, y = lat, fill = species, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual("",
    values = c("#F0C19C", "#E88024"),
    na.translate = FALSE) +
  theme(legend.position='top',
    legend.text = element_markdown(),
    ) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  guides(fill = guide_legend(reverse = TRUE))

state_map_plot


# Combine bar and map------------------------------------------------------
state_plot = plot_grid(state_bar_plot,
  state_map_plot, ncol = 1,
  labels = c("A.", "B."),
  rel_heights = c(2, 1))

state_plot

ggsave("results/fig-6_ship-state.jpg", state_plot, width = 7)


# Numbers behind figure
state_df |>
  group_by(species, state) |>
  tally() |>
  spread(species, n) |>
  adorn_totals("col") |>
  arrange(-Total)

if( FALSE ) {
  state_df |>
    filter( !is.na(state) ) |>
    select(species, state) |>
    tbl_summary(
      by = species,
      statistic = list(
        all_categorical() ~ "{n} ({p}%)"
      ),
      sort = all_categorical() ~ "frequency"
    )|>
    bold_labels() |>
    add_overall(
      last = TRUE
    )
}

