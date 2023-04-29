##===========================================================================##
## Set up workspace and load libraries
##===========================================================================##
library(tidyverse)
library(ggplot2)
library(dplyr)
library(chisq.posthoc.test)
library(car)
library(lme4)
library(report)
library(lmerTest)
library(glmm.hp)
library(emmeans)
library(performance)

conflicts_prefer(stats::chisq.test)
conflicts_prefer(lme4::lmer)

## Import datasets for analysis
## -----------------------------------------------------------------------------
all_listings = read.csv("data/Coleman_et_al_SI_1_all-bat-listings.csv")
unique_listings = read.csv("data/Coleman_et_al_SI_2_unique-listings-info.csv")


##===========================================================================##
## ANALYSIS 1. Does the count of individual Kpicta differ between eBay & Etsy?##
##===========================================================================##

sum(unique_listings$individuals[ unique_listings$species %in% "K. picta" ]) # 284

## Create a df with only Kpicta listings & no Amazon
K_picta_individuals <- unique_listings |>
  filter(species=="K. picta" & shop!="Amazon")

## Get some summary statistics
summary(K_picta_individuals)

K_picta_individuals |>
  group_by(shop) |>
  summarise(
    meanCount = mean(individuals),
    sdCount = sd(individuals),
    minCount = min(individuals),
    maxCount = max(individuals))

## Build Poisson regression model
model.Kpicta.count = glm(individuals ~ shop,
                         data=K_picta_individuals,
                         family="poisson")

## Print the Analysis of deviance table
Anova(model.Kpicta.count,
      type="II",
      test="LR")
summary(model.Kpicta.count)

## Check if model fits the data (P-value < 0.05 indicates poor fit).
with(model.Kpicta.count, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

## Exponentiate Etsy coefficient (per unit ??? in the ref level (eBay), how much ??? on Etsy?)
exp(-0.21309)


##================================================================================##
## ANALYSIS 2. Is there an association betw species (Kpicta vs other) and format? ##
##================================================================================##

## For Etsy and eBay ##---------------------------------------------------------
## Create a df with 3 variables: format (4 levels), species (2 levels; exclude undetermined), listing.count (total number)
bat_format_all <- unique_listings |>
  filter(species!="Undetermined" & shop != "Amazon") |>
  group_by(species, format) |>
  summarise(unique_listings = n())

## Perform a two-way contingency test
# tibble(bat_format_all) # First, generate matrix of  total frequencies
bat.format <- xtabs(unique_listings ~ format + species, data = bat_format_all)
chisq.test(bat.format)
# Get row & column totals, observed & expected values
bat.chi <- chisq.test(bat.format)
names(bat.chi)
bat.chi$expected
# Post-hoc analysis to determine which groups differ
chisq.posthoc.test(bat.format, method = "bonferroni")


## Repeat for Etsy only ##------------------------------------------------------

## Create df with 3 variables: format (4 levels), species (2 levels; exclude undetermined), listing.count (total number)
bat_format_Etsy <- unique_listings |>
  filter(species!="Undetermined" & shop %in% "Etsy") |>
  group_by(species, format) |>
  summarise(unique_listings = n())

## Perform a two-way contingency test as above
# tibble(bat_format_Etsy) # First, give R a matrix of the total frequencies
bat.format.Etsy <- xtabs (unique_listings ~ format + species, data = bat_format_Etsy)
bat.format.Etsy
chisq.test(bat.format.Etsy)
# Get row & column totals, observed & expected values
bat.chi.Etsy <- chisq.test(bat.format.Etsy)
names(bat.chi.Etsy)
bat.chi.Etsy$expected
# Post-hoc analysis to determine which groups differ
chisq.posthoc.test(bat.format.Etsy, method = "bonferroni")


##=================================================================================##
## ANALYSIS 2. Does mean listing price vary by platform, format & species? ##
##=================================================================================##

##=========================================================##
## Model 1. Does pricing on Etsy vary by species & format? ##
##=========================================================##

## Restrict to Etsy listings for entire bats & exclude undetermined species, keep only needed column & one observation per listing
Etsy.price <- unique_listings |>
  filter(shop=="Etsy" & part=="Entire bat" & species!="Undetermined") |>
  select(listing, shop, seller, species, format, part, mean_price)

str(Etsy.price)

## 1.1 Fit saturated GLMM (DV = price; fixed effects = species, format; random effect = seller)
# ------------------------------------------------------------------------------
Etsy.price.full <- lmer(mean_price ~ format * species + (1|seller), data=Etsy.price)
summary(Etsy.price.full)
# Get easy to understand report
report(Etsy.price.full)

## Test the effect of seller
ranova(Etsy.price.full)

## Perform model diagnostics
check_singularity(Etsy.price.full) 
check_heteroscedasticity(Etsy.price.full)
# Comprehensive diagnostic plots
check_model(Etsy.price.full)

## 1.2 Remove interaction from above model
## -----------------------------------------------------------------------------
Etsy.price.reduced <- lmer(mean_price ~ format + species + (1|seller), data=Etsy.price)
summary(Etsy.price.reduced)

## Get easy to understand report
report(Etsy.price.reduced)

## Test the effect of seller
ranova(Etsy.price.reduced)

## Perform model diagnostics
check_singularity(Etsy.price.reduced)
check_heteroscedasticity(Etsy.price.reduced)
# Comprehensive diagnostic plots
check_model(Etsy.price.reduced)

## 1.3 Rerun with sqrt-transformed price
## -----------------------------------------------------------------------------
Etsy.price.reduced.sqrt <- lmer(sqrt(mean_price) ~ format + species + (1|seller), data=Etsy.price)
summary(Etsy.price.reduced.sqrt)

## Get easy to understand report
report(Etsy.price.reduced.sqrt)

## Test the effect of seller
ranova(Etsy.price.reduced.sqrt)

## Perform model diagnostics
check_singularity(Etsy.price.reduced.sqrt)
check_heteroscedasticity(Etsy.price.reduced.sqrt)
# Comprehensive diagnostic plots
check_model(Etsy.price.reduced.sqrt)

## Compute influence of each fixed effect (see doi.org/10.1093/jpe/rtac096)
glmm.hp(Etsy.price.reduced.sqrt)

## Perform post hoc contrasts and generate back-transformed means & CIs
emmeans(Etsy.price.reduced.sqrt, specs = pairwise ~ format, type = "response")   
emmeans(Etsy.price.reduced.sqrt, specs = pairwise ~ species, type = "response") 

## Compare model performance
compare_performance(Etsy.price.full, Etsy.price.reduced, Etsy.price.reduced.sqrt,
  rank = TRUE, verbose = FALSE)


##==========================================================##
## Model 2. Does K_picta pricing vary by shop and format? ##
##==========================================================##

## Restrict to K. picta listings for entire bats, keep only needed column & one observation per listing
Kpicta.price <- unique_listings |>
  filter(species=="K. picta" & part=="Entire bat") |>
  select(listing, shop, seller, species, format, part, mean_price)

str(Kpicta.price)

## 1.1 Fit saturated GLMM (DV = price; fixed effects = platform (aka shop), format; random effect = seller)
## -----------------------------------------------------------------------------
Kpicta.price.full <- lmer(mean_price ~ format * shop + (1|seller), data=Kpicta.price)
summary(Kpicta.price.full)
## Get easy to understand report
report(Kpicta.price.full)

## Test the effect of seller
ranova(Kpicta.price.full)

## Perform model diagnostics
check_singularity(Kpicta.price.full)
check_heteroscedasticity(Kpicta.price.full)
# Comprehensive diagnostic plots
check_model(Kpicta.price.full)

## Remove interaction term & retry
Kpicta.price.reduced <- lmer(mean_price ~ format + shop + (1|seller), data=Kpicta.price)
summary(Kpicta.price.reduced)

## Get easy to understand report
report(Kpicta.price.reduced)

## Test the effect of seller
ranova(Kpicta.price.reduced)

## Perform model diagnostics
check_singularity(Kpicta.price.reduced)
check_heteroscedasticity(Kpicta.price.reduced)
# Comprehensive diagnostic plots
check_model(Kpicta.price.reduced)

## 1.2 Rerun with sqrt-transformed price
## -----------------------------------------------------------------------------
Kpicta.price.reduced.sqrt <- lmer(sqrt(mean_price) ~ format + shop + (1|seller), data=Kpicta.price)
summary(Kpicta.price.reduced.sqrt)

## Get easy to understand report
report(Kpicta.price.reduced.sqrt)

## Test the effect of seller
ranova(Kpicta.price.reduced.sqrt)

## Perform model diagnostics
check_singularity(Kpicta.price.reduced.sqrt)
check_heteroscedasticity(Kpicta.price.reduced.sqrt)
# Comprehensive diagnostic plots
check_model(Kpicta.price.reduced.sqrt)

## Compute influence of each fixed effect (see Lai et al 2022, doi.org/10.1093/jpe/rtac096)
glmm.hp(Kpicta.price.reduced.sqrt)

## Compare model performance
compare_performance(Kpicta.price.full, Kpicta.price.reduced, Kpicta.price.reduced.sqrt, rank = TRUE, verbose = FALSE)

## Perform post hoc contrasts and generate back-transformed means & CIs
emmeans(Kpicta.price.reduced.sqrt, specs = pairwise ~ format, type = "response")   
emmeans(Kpicta.price.reduced.sqrt, specs = pairwise ~ shop, type = "response") 

## Figure 4. Jitter boxplot with mean listing price (not estimated means)
ggplot(Kpicta.price, aes(x=format, y=mean_price, fill=shop)) +
  geom_boxplot(position=position_dodge(width=0.8)) +
  coord_cartesian(ylim=c(0,250)) +
  xlab("Format") + ylab("Mean weekly listing price (USD)") +
  scale_fill_manual(values = c(Amazon = '#BBCC33', eBay = '#EEDD88', Etsy = '#77AADD'),
    name="Platform") +
  geom_point(position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 0.8),
             aes(fill=shop),
             pch=21) +
  theme_bw()
