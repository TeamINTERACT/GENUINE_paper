####
#### IMPLEMENTATION OF GENTRIFICATION MEASURES FOR CANADIAN CITIES ####
####

#### Remarks and limitations ####
# - Using the online database of Canadian Census -> https://mountainmath.github.io/cancensus/index.html
# - Joining CT 2006 to CT 2016: no LUT available, some CT have been removed or/added,
#   hence a full match between both census years cannot be done without resorting to
#   a spatial join approach. In this condition, some CTs' status cannot be evaluated
#   due to some missing data for the 2006 year.
#   EDIT: this has been solved with commit c63d6e9. We now use a chained LUT 
#   (DA2016 > DA2011 > DA2006) to infer the best candidate to link CT2016 to CT2006.
# - Rania's gentrification measures: the map produced for the gentrification grant is
#   quite different from the map produced here, although the same method is used.
#   Explanation is two-fold: 1) the z-scores were not computed over the CMA, as in
#   Steinmetz-Wood & Wasfi's paper (DOI 10.1186/s12942-017-0096-6), but on the all
#   Canadian CTs and 2) Higest education level was considering Bachelor degree only,
#   whereas all university degrees are considered below.
# - Preferred aggregation level is the census division (see 
#   https://www150.statcan.gc.ca/n1/pub/92-195-x/92-195-x2016001-eng.htm)
# - All aggregated computations are unweighted
#   EDIT: this has changed with commit c9de7ef -> median, z-score are now weighted
# - Adding a new level of aggregation : Census Consolidated Subdivision 
#   (see https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/geo007-eng.cfm)

#### session info ####
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] cancensus_0.2.1 Hmisc_4.3-1     Formula_1.2-3   survival_3.1-8  lattice_0.20-38 janitor_1.2.1   cowplot_1.0.0   ggplot2_3.3.0  
# [9] sf_0.9-4        stringr_1.4.0   tidyr_1.1.0     dplyr_1.0.0    
# 
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.3          png_0.1-7           class_7.3-15        assertthat_0.2.1    utf8_1.1.4          digest_0.6.23       R6_2.4.1           
#  [8] backports_1.1.5     acepack_1.4.1       evaluate_0.14       e1071_1.7-3         httr_1.4.1          pillar_1.4.2        rlang_0.4.6        
# [15] curl_4.3            rstudioapi_0.11     data.table_1.12.8   rpart_4.1-15        Matrix_1.2-17       checkmate_2.0.0     rmarkdown_2.1      
# [22] splines_3.6.1       readr_1.3.1         foreign_0.8-71      htmlwidgets_1.5.1   munsell_0.5.0       tinytex_0.19        compiler_3.6.1     
# [29] xfun_0.12           pkgconfig_2.0.3     base64enc_0.1-3     htmltools_0.4.0     nnet_7.3-12         tidyselect_1.1.0    tibble_2.1.3       
# [36] gridExtra_2.3       htmlTable_1.13.3    fansi_0.4.0         crayon_1.3.4        withr_2.1.2         grid_3.6.1          jsonlite_1.6       
# [43] gtable_0.3.0        lifecycle_0.2.0     DBI_1.1.0           magrittr_1.5        units_0.6-5         scales_1.1.0        KernSmooth_2.23-15 
# [50] cli_1.1.0           stringi_1.4.3       farver_2.0.1        snakecase_0.11.0    latticeExtra_0.6-29 ellipsis_0.3.0      generics_0.0.2     
# [57] vctrs_0.3.0         RColorBrewer_1.1-2  tools_3.6.1         glue_1.4.1          purrr_0.3.3         hms_0.5.3           jpeg_0.1-8.1       
# [64] yaml_2.2.1          colorspace_1.4-1    cluster_2.1.0       classInt_0.4-2      knitr_1.28         


## ---- chunk-census-get-data ----
#### Setup ####
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(cowplot)
library(janitor)
library(Hmisc)
library(cancensus)
source('cancensus_apiKey.R')
options(cancensus.cache_path = 'cancensus_cache')


#### Get StatCan data at the CT and DA levels, using CanCensus library ####
# Select region codes for the 25 largest cities by population
cities_list25 <- list_census_regions('CA16') %>% 
  filter(level=="CD") %>% 
  top_n(25,pop) %>% 
  as_census_region_list

# Select region codes for the 10 largest CMAs by population
regions_list10 <- list_census_regions('CA16') %>% 
  filter(level=="CMA") %>% 
  top_n(10,pop) %>% 
  as_census_region_list


# Extract required variables at the CT level (2016)
# ---
#  G (Grube-Cavers), R (Rania), F (Freeman) and D (Ding) flags indicate which variables 
#  are used for which measures.
# ---
# - Education
#   - Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data | v_CA16_5051 | G,R,F,D
#   - University certificate, diploma or degree at bachelor level or above | v_CA16_5078 | G,R,F,D
# - Age
#   - Total - Age | v_CA16_1 | R
#   - 30 to 34 years | v_CA16_118 | R
#   - 35 to 39 years | v_CA16_136 | R
#   - 40 to 44 years | v_CA16_154 | R
# - Housing
#   - Total - Private households by tenure - 25% sample data | v_CA16_4836 | G,R,D
#   - Owner | v_CA16_4837 | G,R
#   - Renter | v_CA16_4838
#   - Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data | v_CA16_4897 | G,R,D
#   - Median monthly shelter costs for rented dwellings ($) | v_CA16_4900 | NB. should be used for Ding but no match in 2006
#   - Average monthly shelter costs for rented dwellings ($) | v_CA16_4901 | G,R,D
#   - Total - Occupied private dwellings by period of construction - 25% sample data | v_CA16_4862 | F
#   - 1960 or before ... 2011 to 2016 | v_CA16_4863 ... v_CA16_4869 | F
#   - Total - Owner households in non-farm, non-reserve private dwellings - 25% sample data | v_CA16_4890 | F,D
#   - Median value of dwellings ($) | v_CA16_4895
#   - Average value of dwellings ($) | v_CA16_4896 | F,D
# - Income
#   - Total - Income statistics in 2015 for private households by household size - 100% data | v_CA16_2396 | R,F,D
#   - Median total income of households in 2015 ($) | v_CA16_2397 | R,F,D
#   - Median after-tax income of households in 2015 ($) | v_CA16_2398
#   - Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data | v_CA16_2510 | R
#   - Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%) | v_CA16_2540
#   - Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%) | v_CA16_2570 | R
#   - Total - Income statistics in 2015 for economic families in private households - 100% data | v_CA16_2446 | G
#   - Median total income of economic families in 2015 ($) | v_CA16_2447
#   - Median after-tax income of economic families in 2015 ($) | v_CA16_2448
#   - Average family size of economic families | v_CA16_2449
#   - Total - Income statistics in 2015 for economic families in private households - 25% sample data | v_CA16_4993 | G
#   - Average total income of economic families in 2015 ($) | v_CA16_4994 | G
#   - Average after-tax income of economic families in 2015 ($) | v_CA16_4995
# - Occupation | NB: No match for sub-categories by occupation, such as in 2006. Need to switch to another classification...
#   - Industry | v_CA16_5693 | G
#   - 54 Professional, scientific and technical services | v_CA16_5735 | G
census_vars_16 <- c("v_CA16_5051", "v_CA16_5078", #education
                   "v_CA16_1", "v_CA16_118", "v_CA16_136", "v_CA16_154", #age
                   "v_CA16_4836", "v_CA16_4837", "v_CA16_4838", "v_CA16_4897", "v_CA16_4900", "v_CA16_4901", #housing
                   "v_CA16_4862", "v_CA16_4863", "v_CA16_4864", "v_CA16_4865", "v_CA16_4866", "v_CA16_4867", "v_CA16_4868", "v_CA16_4869",#construction dates
                   "v_CA16_4890", "v_CA16_4895", "v_CA16_4896", #housing
                   "v_CA16_2396", "v_CA16_2397", "v_CA16_2398", "v_CA16_2510", "v_CA16_2540", "v_CA16_2570", "v_CA16_4993", "v_CA16_4994", #income
                   "v_CA16_5693", "v_CA16_5735") #occupation
#census_data_16 <- get_census(dataset='CA16', regions=cities_list25, vectors=census_vars_16, level='CT', geo_format = "sf")
#census_data_16 <- get_census(dataset='CA16', regions=regions_list10, vectors=census_vars_16, level='CT', geo_format = "sf")
census_data_16 <- get_census(dataset='CA16', regions=list(C='1'), vectors=census_vars_16, level='CT', geo_format = "sf")
census_data_16 <- filter(census_data_16, Type == "CT")


# Extract required variables at the CT level (2006)
# ---
#  G (Grube-Cavers), R (Rania), F (Freeman) and D (Ding) flags indicate which variables 
#  are used for which measures.
# ---
# - Education
#   - Total population 15 to 24 years by highest certificate, diploma or degree | v_CA06_1234 | G,R,F,D
#   - University certificate, diploma or degree | v_CA06_1240 | G,R,F,D
#   - Total population 25 to 64 years by highest certificate, diploma or degree | v_CA06_1248 | G,R,F,D
#   - University certificate, diploma or degree | v_CA06_1254 | G,R,F,D
#   - Total population 65 years and over by highest certificate, diploma or degree | v_CA06_1262 | G,R,F,D
#   - University certificate, diploma or degree | v_CA06_1268 | G,R,F,D
# - Age
#   - Total population by sex and age groups | v_CA06_2 | R
#   - [Male] 30 to 34 years | v_CA06_10 | R
#   - [Male] 35 to 39 years | v_CA06_11 | R
#   - [Male] 40 to 44 years | v_CA06_12 | R
#   - [Female] 30 to 34 years | v_CA06_29 | R
#   - [Female] 35 to 39 years | v_CA06_30 | R
#   - [Female] 40 to 44 years | v_CA06_31 | R
# - Housing
#   - Total number of occupied private dwellings by housing tenure | v_CA06_101 | G,R
#   - Owned | v_CA06_102 | G,R
#   - Rented | v_CA06_103
#   - Tenant-occupied private non-farm, non-reserve dwellings | v_CA06_2049 | G,R,D
#   - Average gross rent $ | v_CA06_2050 | G,R,D
#   - Total number of occupied private dwellings by period of construction | v_CA06_109 | F
#   - Period of construction, before 1946 ... Period of construction, 2001 to 2006 | v_CA06_110 ... v_CA06_118 | F
#   - Owner-occupied private non-farm, non-reserve dwellings | v_CA06_2053 | F,D
#   - Average value of dwelling $ | v_CA06_2054 | F,D
# - Income
#   - Household income in 2005 of private households | v_CA06_1988 | R,F,D
#   - Median household income $ | v_CA06_2000 | R,F,D
#   - Median after-tax household income $ | v_CA06_2030
#   - [Low Income] Total persons in private households | v_CA06_1979 | R
#   - Prevalence of low income after tax in 2005 % | v_CA06_1981 | R
#   - Family income in 2005 of economic families | v_CA06_1729 | G
#   - Median family income $ | v_CA06_1741
#   - Average family income $ | v_CA06_1742 | G
# - Occupation
#   - Total labour force 15 years and over by occupation - National Occupational Classification for Statistics 2006 | v_CA06_827
#   - B0 Professional occupations in business and finance | v_CA06_836
#   - C0 Professional occupations in natural and applied sciences | v_CA06_843
#   - D0 Professional occupations in health | v_CA06_846
#   - F0 Professional occupations in art and culture | v_CA06_855
#   - Total labour force 15 years and over by industry - North American Industry Classification System 2002 | v_CA06_1007 | G
#   - 54 Professional, scientific and technical services | v_CA06_1021 | G
census_vars_06 <- c("v_CA06_1234", "v_CA06_1240", "v_CA06_1248", "v_CA06_1254", "v_CA06_1262", "v_CA06_1268", #education
                    "v_CA06_2", "v_CA06_10", "v_CA06_11", "v_CA06_12", "v_CA06_29", "v_CA06_30", "v_CA06_31", #age
                    "v_CA06_101", "v_CA06_102", "v_CA06_103", "v_CA06_2049", "v_CA06_2050", #housing
                    "v_CA06_109", "v_CA06_111", "v_CA06_112", "v_CA06_113", "v_CA06_114", "v_CA06_115", "v_CA06_116", "v_CA06_117", "v_CA06_118", #construction dates
                    "v_CA06_2053", "v_CA06_2054", #housing
                    "v_CA06_1988", "v_CA06_2000", "v_CA06_2030", "v_CA06_1979", "v_CA06_1981", "v_CA06_1729", "v_CA06_1742", #income
                    "v_CA06_827", "v_CA06_836", "v_CA06_843", "v_CA06_846", "v_CA06_855", "v_CA06_1007", "v_CA06_1021") #occupation
#census_data_06 <- get_census(dataset='CA06', regions=cities_list25, vectors=census_vars_06, level='CT', geo_format = "sf")
#census_data_06 <- get_census(dataset='CA06', regions=regions_list10, vectors=census_vars_06, level='CT', geo_format = "sf")
census_data_06 <- get_census(dataset='CA06', regions=list(C='1'), vectors=census_vars_06, level='CT', geo_format = "sf")
census_data_06 <- filter(census_data_06, Type == "CT")

# BUILD CORRESPONDANCE TABLE CT2016 -> CT2006
# ---
# Get DA level data to link CT2006 to CT2016 through look-up table
da2016 <- get_census(dataset='CA16', regions=list(C='1'), vectors=c("v_CA16_1"), level='DA')
da2011 <- get_census(dataset='CA11', regions=list(C='1'), vectors=c("v_CA11F_1"), level='DA')
da2006 <- get_census(dataset='CA06', regions=list(C='1'), vectors=c("v_CA06_1"), level='DA')
# Get correspondance tables (from StatCan -> https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/cor-eng.cfm)
lut2016_2011 <- read.csv("lut/LUT_DA_2016_2011/2016_92-156_DA_AD.txt", colClasses = c(rep("character", 3), "numeric"))
lut2011_2006 <- read.csv("lut/LUT_DA_2011_2006/2011_92-156_DA_AD.txt", colClasses = c(rep("character", 3), "numeric"))
# Join correspondance tables to get DA2016 <-> DA2006
lut2016_2006 <- lut2016_2011 %>%
  full_join(lut2011_2006, by="DAUID2011", suffix=c(".16.11", ".11.06")) %>%
  select(starts_with('DAU'), starts_with(('DA_'))) %>%
  distinct()
# Join DA<->CT for each census year
lut2016_2006 <- lut2016_2006 %>%
  left_join(da2016, by=c("DAUID2016" = "GeoUID")) %>%
  left_join(da2011, by=c("DAUID2011" = "GeoUID"), suffix=c(".2016", ".2011")) %>%
  left_join(da2006, by=c("DAUID2006" = "GeoUID"), suffix=c("", ".2006")) %>%
  select(starts_with('DAU'), starts_with('DA_'), starts_with("CT_"), starts_with("Population")) %>%
  rename(CT_UID.2006 = CT_UID, Population.2006 = Population) %>%
  filter(!is.na(na_if(CT_UID.2006, "")) & !is.na(na_if(CT_UID.2016, "")))
# Aggregation rule -> when several 2006 CTs correspond to one 2016 CT, the most populated is chosen
lut_ct2016_2006 <- lut2016_2006 %>%
  group_by(CT_UID.2016, CT_UID.2006) %>%
  summarise(n_DAs = n(),
            PopTotale.2016 = sum(Population.2016),
            PopTotale.2006 = sum(Population.2006)) %>%
  group_by(CT_UID.2016) %>%
  summarise(CT_UID.2006 = last(CT_UID.2006, order_by = PopTotale.2006)) %>%
  ungroup()

# ADD CENSUS CONSOLIDATED SUBDIVISIONS
# ---
# Get CCS LUT, which comes from StatCan 2016 census data (see https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/geo007-eng.cfm)
lut_csd2ccs2016 <- read.csv("lut/csd2016.csv", colClasses = c("NULL", rep("character", 18)), fileEncoding = "UTF-8")
census_data_16 <- census_data_16 %>%
  left_join(transmute(lut_csd2ccs2016,
                      CSD_UID = CSDUID,
                      CCS_UID = CCSUID), by="CSD_UID")
# Same for 2006, this time from the DAs attrubute table
lut_csd2ccs2006 <- read.csv("lut/ccs2006.csv", colClasses = c(rep("NULL", 2), rep("character", 2)))
census_data_06 <- census_data_06 %>%
  left_join(transmute(lut_csd2ccs2006,
                      CSD_UID = CSDUID,
                      CCS_UID = CCSUID), by="CSD_UID")

## ---- chunk-rania-metric ----
# ---
# Steinmtez-Wood, M., Wasfi, R. et al. 2017. Is gentrification all bad? ####
# Positive association between gentrification and individual’s perceived neighborhood collective efficacy in Montreal, Canada
# Int J Health Geogr 16:24, 178–194. https://ij-healthgeographics.biomedcentral.com/articles/10.1186/s12942-017-0096-6
# ---
# Indicators used to identify gentrifying tracts included: 
# median household income, proportion of the population with 
# a bachelor’s degree, average rent, proportion of the 
# population with low income, and proportion of the population 
# aged 30–44. Z scores for each measure in 1996 and 2006 were 
# calculated using the Montreal census metropolitan area average
# and standard deviation
#
# Potential to undergo gentrification:
#   negative Z scores in 1996 for median household income, 
#   proportion of the population with a bachelor’s degree, 
#   and average rent and had a positive Z score for the 
#   proportion of population with low income.
# Gentrified:
#   difference between the 2006 and 1996 Z scores was positive 
#   for all indicators except for the proportion of low income, 
#   which needed to be negative

# Define weighted scale function
wtd.scale <- function(x, w) {
  (x - wtd.mean(x, w)) / sqrt(wtd.var(x, w))
}

#
# Main routine to compute Rania's gentrification metric at one specific aggregation level
#
compute_rania_metric <- function(census_data_16, census_data_06, agg_level) {
  agg_level <- enquo(agg_level)

  # Extract Census 2016 variables for Rania's measures
  gentrif_rania16 <- census_data_16 %>% 
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA16_5051"),starts_with("v_CA16_5078"), #education
           starts_with("v_CA16_1"),starts_with("v_CA16_118"), starts_with("v_CA16_136"),starts_with("v_CA16_154"), #age
           starts_with("v_CA16_4836"),starts_with("v_CA16_4837"),starts_with("v_CA16_4897"),starts_with("v_CA16_4901"), #housing
           starts_with("v_CA16_2396"),starts_with("v_CA16_2397"),starts_with("v_CA16_2398"),starts_with("v_CA16_2510"),starts_with("v_CA16_2570")) #income
  
  # Compute derived variables for 2016
  # ---
  # New variables:
  # - propbach15_2016: proportion of high educ attainment in 2016 (%)
  # - propage30_44_2016: proportion of population aged 30-44 yrs in 2016 (%)
  gentrif_rania16 <- gentrif_rania16 %>% 
    mutate(propbach15_2016 = `v_CA16_5078: University certificate, diploma or degree at bachelor level or above` / `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data` * 100) %>%
    mutate(propage30_44_2016 = (`v_CA16_118: 30 to 34 years` + `v_CA16_136: 35 to 39 years` + `v_CA16_154: 40 to 44 years`) / `v_CA16_1: Age Stats` * 100) %>%
    filter(!is.na(CD_UID))
  
  # Extract Census 2006 variables
  gentrif_rania06 <- census_data_06 %>% 
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA06_1234"),starts_with("v_CA06_1240"),starts_with("v_CA06_1248"),starts_with("v_CA06_1254"),starts_with("v_CA06_1262"),starts_with("v_CA06_1268"), #education
           starts_with("v_CA06_2"),starts_with("v_CA06_10"),starts_with("v_CA06_11"),starts_with("v_CA06_12"),starts_with("v_CA06_29"),starts_with("v_CA06_30"),starts_with("v_CA06_31"), #age
           starts_with("v_CA06_101"),starts_with("v_CA06_102"),starts_with("v_CA06_2049"),starts_with("v_CA06_2050"), #housing
           starts_with("v_CA06_1988"),starts_with("v_CA06_2000"),starts_with("v_CA06_1979"),starts_with("v_CA06_1981")) %>% #income
    filter(!is.na(CD_UID))
  # Compute derived variables for 2006
  # ---
  # New variables:
  # - propbach15_2006: proportion of high educ attainment in 2006 (%)
  # - propage30_44_2006: proportion of population aged 30-44 yrs in 2006 (%)
  gentrif_rania06 <- gentrif_rania06 %>% 
    mutate(propbach15_2006 = (`v_CA06_1240: University certificate, diploma or degree` + 
                                `v_CA06_1254: University certificate, diploma or degree` + 
                                `v_CA06_1268: University certificate, diploma or degree`) / 
             (`v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data`) * 100) %>%
    mutate(propage30_44_2006 = (`v_CA06_29: 30 to 34 years` + `v_CA06_10: 30 to 34 years` + `v_CA06_11: 35 to 39 years` + `v_CA06_30: 35 to 39 years` + `v_CA06_12: 40 to 44 years` + `v_CA06_31: 40 to 44 years`) / `v_CA06_2: Total population by sex and age groups - 100% data` * 100)
  
  # Compute zscores at the specified aggregation level
  # ---
  # New variables:
  # - propbach15_2016z: z-score of proportion of high educ attainment in 2016, ref level = census division or census metropolitan area
  # - propage30_44_2016z: z-score of proportion of population aged 30-44 yrs in 2016, ref level = census division or census metropolitan area
  # - incmedhous_2016z: z-score of median household income in 2016, ref level = census division or census metropolitan area
  # - proplowinc_2016z: z-score of prrevalence of low income in 2016, ref level = census division or census metropolitan area
  # - avgrent_2016z: z-score of average monthly renting costs in 2016, ref level = census division or census metropolitan area
  gentrif_rania16 <- gentrif_rania16 %>% 
    group_by(!!agg_level) %>%
    mutate(propbach15_2016z = wtd.scale(propbach15_2016, `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`)) %>%
    mutate(propage30_44_2016z = wtd.scale(propage30_44_2016, `v_CA16_1: Age Stats`)) %>%
    mutate(incmedhous_2016z = wtd.scale(`v_CA16_2397: Median total income of households in 2015 ($)`, `v_CA16_2396: Total - Income statistics in 2015 for private households by household size - 100% data`)) %>%
    mutate(proplowinc_2016z = wtd.scale(`v_CA16_2570: Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`, `v_CA16_2510: Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data`)) %>%
    mutate(avgrent_2016z = wtd.scale(`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)`, `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`)) %>%
    ungroup() %>%
    filter(!is.na(!!agg_level))
  
  # Compute zscores at the Census Division level
  # ---
  # New variables:
  # - propbach15_2006z: z-score of proportion of high educ attainment in 2006, ref level = census division or census metropolitan area
  # - propage30_44_2006z: z-score of proportion of population aged 30-44 yrs in 2006, ref level = census division or census metropolitan area
  # - incmedhous_2006z: z-score of median household income in 2006, ref level = census division or census metropolitan area
  # - proplowinc_2006z: z-score of prrevalence of low income in 2006, ref level = census division or census metropolitan area
  # - avgrent_2006z: z-score of average monthly renting costs in 2006, ref level = census division or census metropolitan area
  gentrif_rania06 <- gentrif_rania06 %>% 
    group_by(!!agg_level) %>%
    mutate(propbach15_2006z = wtd.scale(propbach15_2006, `v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + 
                                          `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + 
                                          `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data`)) %>%
    mutate(propage30_44_2006z = wtd.scale(propage30_44_2006, `v_CA06_2: Total population by sex and age groups - 100% data`)) %>%
    mutate(incmedhous_2006z = wtd.scale(`v_CA06_2000: Median household income $`, `v_CA06_1988: Household income in 2005 of private households - 20% sample data`)) %>%
    mutate(proplowinc_2006z = wtd.scale(`v_CA06_1981: Prevalence of low income after tax in 2005 %`, `v_CA06_1979: Total persons in private households - 20% sample data`)) %>%
    mutate(avgrent_2006z = wtd.scale(`v_CA06_2050: Average gross rent $`, `v_CA06_2049: Tenant-occupied private non-farm, non-reserve dwellings`)) %>%
    ungroup() %>%
    filter(!is.na(!!agg_level))
  
  # Merge and cleanup function:
  # ---
  # New variables:
  # - propbach15diff: difference between 2006 and 2016 in higher educ attainment
  # - propage30_44diff: difference between 2006 and 2016 in population aged 30-44 yrs
  # - incmedhousdiff: difference between 2006 and 2016 in household income
  # - proplowincdiff: difference between 2006 and 2016 in prevalence of low income
  # - avgrentdiff: difference between 2006 and 2016 in average renting costs
  # - potentially_gentrifying: candidate CTs to gentrification in 2006
  # - gentrified: actually gentrified CTs in 2016
  .gentrif_rania06_nogeom <- gentrif_rania06 %>%
    select(GeoUID, Population, ends_with("z"))
  st_geometry(.gentrif_rania06_nogeom) <- NULL
  
  return (gentrif_rania16 %>%
            select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population, ends_with("z")) %>%
            left_join(lut_ct2016_2006, by=c("GeoUID" = "CT_UID.2016")) %>%
            left_join(.gentrif_rania06_nogeom, by=c("CT_UID.2006" = "GeoUID"), suffix=c("2016", "2006")) %>%
            select(-CT_UID.2006) %>%
            # Compute differences between 2006 and 2016
            mutate(propbach15diff = propbach15_2016z - propbach15_2006z) %>%
            mutate(propage30_44diff = propage30_44_2016z - propage30_44_2006z) %>%
            mutate(incmedhousdiff = incmedhous_2016z - incmedhous_2006z) %>%
            mutate(proplowincdiff = proplowinc_2016z - proplowinc_2006z) %>%
            mutate(avgrentdiff = avgrent_2016z - avgrent_2006z) %>%
            # Flag potentially gentrifying CTs in 2006
            mutate(potentially_gentrifying = incmedhous_2006z < 0 & propbach15_2006z < 0 & avgrent_2006z < 0 & proplowinc_2006z > 0) %>%
            #Flag actually gentrified CTs in 2016
            mutate(gentrified = potentially_gentrifying &
                     incmedhousdiff > 0 & propbach15diff > 0 & avgrentdiff > 0 & propage30_44diff > 0 & proplowincdiff < 0)
  )
}

# Compute Rania's metrics for all three levels CMS | CD | CCS
gentrif_rania06_16_CD <- compute_rania_metric(census_data_16, census_data_06, CD_UID)
gentrif_rania06_16_CCS <- compute_rania_metric(census_data_16, census_data_06, CCS_UID)
gentrif_rania06_16_CMA <- compute_rania_metric(census_data_16, census_data_06, CMA_UID)

## ---- chunk-grube-metric ----
# ---
# Grube-Cavers, A., Patterson, Z., 2015. Urban rapid rail transit and gentrification in Canadian urban centres ####
# A survival analysis approach. 
# Urban Stud. 52, 178–194. https://doi.org/10.1177/0042098014524287
# ---
# NB: Adaptation of some variables, which no longer exist in 2016 (professional occupation)
# and number of degrees (~highest education level)
# NB: Grube-Cavers compares to CMA (like Rania) instead of CD (as Ding and Freeman do)
# ---
# Gentrifiable:
#   - average family income and number of degrees per capita lower than the CMA average
# Gentrified: all indicators below have to have experienced an improvement in the CT greater than the 
#   average change experienced in those indicators for the CMA
#   - average monthly rent;
#   - proportion of people in professional occupations;
#   - percentage of owner-occupied dwellings;
#   - average family income;
#   - number of degrees per capita

#
# Main routine to compute Grube-Caver's gentrification metric at one specific aggregation level
#
compute_grube_metric <- function(census_data_16, census_data_06, agg_level) {
  agg_level <- enquo(agg_level)
  
  # Extract Census 2016 variables for Grube-Cavers' measures
  gentrif_grube16 <- census_data_16 %>% 
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA16_5051"),starts_with("v_CA16_5078"), #education
           starts_with("v_CA16_4836"),starts_with("v_CA16_4837"),starts_with("v_CA16_4897"),starts_with("v_CA16_4901"), #housing
           starts_with("v_CA16_4993"),starts_with("v_CA16_4994"), #income
           starts_with("v_CA16_5693"),starts_with("v_CA16_5735")) %>% #professional occupation
    mutate(`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)` = na_if(`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)`, 0)) #set 0$ rent to NA
  
  # Compute derived variables for 2016
  # ---
  # New variables:
  # - propbach15_2016: proportion of high educ attainment in 2016 (%)
  # - propprof_2016: proportion of people in professional occupations in 2016 (%)
  # - propowner_2016: percentage of owner-occupied dwellings in 2016 (%)
  gentrif_grube16 <- gentrif_grube16 %>% 
    mutate(propbach15_2016 = `v_CA16_5078: University certificate, diploma or degree at bachelor level or above` / `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data` * 100,
           propprof_2016 = `v_CA16_5735: 54 Professional, scientific and technical services` / `v_CA16_5693: Total Labour Force population aged 15 years and over by Industry - North American Industry Classification System (NAICS) 2012 - 25% sample data` * 100,
           propowner_2016 = `v_CA16_4837: Owner` / `v_CA16_4836: Total - Private households by tenure - 25% sample data` * 100) %>%
    filter(!is.na(!!agg_level))
  
  # Extract Census 2006 variables
  gentrif_grube06 <- census_data_06 %>% 
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA06_1234"),starts_with("v_CA06_1240"),starts_with("v_CA06_1248"),starts_with("v_CA06_1254"),starts_with("v_CA06_1262"),starts_with("v_CA06_1268"), #education
           starts_with("v_CA06_101"),starts_with("v_CA06_102"),starts_with("v_CA06_2049"),starts_with("v_CA06_2050"), #housing
           starts_with("v_CA06_1729"),starts_with("v_CA06_1742"), #income
           starts_with("v_CA06_1007"), starts_with("v_CA06_1021")) %>% #occupation
    mutate(`v_CA06_2050: Average gross rent $` = na_if(`v_CA06_2050: Average gross rent $`, 0)) %>% #set 0$ rent to NA
    filter(!is.na(!!agg_level))
  
  # Compute derived variables for 2006
  # ---
  # New variables:
  # - propbach15_2006: proportion of high educ attainment in 2006 (%)
  # - propprof_2006: proportion of people in professional occupations in 2006 (%)
  # - propowner_2006: percentage of owner-occupied dwellings in 2006 (%)
  # - famincome_2006_mean: CMA average of average family income
  # - propbach15_2006_mean: CMA average of proportion of high educ attainment
  gentrif_grube06 <- gentrif_grube06 %>% 
    mutate(propbach15_2006 = (`v_CA06_1240: University certificate, diploma or degree` + 
                                `v_CA06_1254: University certificate, diploma or degree` + 
                                `v_CA06_1268: University certificate, diploma or degree`) / 
             (`v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data`) * 100,
           propprof_2006 = `v_CA06_1021: 54 Professional, scientific and technical services` / `v_CA06_1007: Total labour force 15 years and over by industry - North American Industry Classification System 2002  - 20% sample data` * 100,
           propowner_2006 = `v_CA06_102: Owned` / `v_CA06_101: Total number of occupied private dwellings by housing tenure - 20% sample data` * 100) %>%
    group_by(!!agg_level) %>%
    mutate(famincome_2006_mean = wtd.mean(`v_CA06_1742: Average family income $`, 
                                          `v_CA06_1729: Family income in 2005 of economic families - 20% sample data`, na.rm = TRUE),
           propbach15_2006_mean = wtd.mean(propbach15_2006, 
                                           `v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + 
                                             `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + 
                                             `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data` , na.rm = TRUE)) %>%
    ungroup()
  
  # Join Census 2006 to 2016 and compute differences, etc.
  # ---
  # New variables:
  # - propbach15_change: change in proportion of high educ attainment (propbach15_2016 - propbach15_2006)
  # - propprof_change: change in proportion of people in professional occupations (propprof_2016 - propprof_2006)
  # - propowner_change: change in percentage of owner-occupied dwellings (propowner_2016 - propowner_2006)
  # - famincome_change: change in average family income between 2006 and 2016
  # - avgrent_change: change in average monthly rent between 2006 and 2016
  .gentrif_grube06_nogeom <- gentrif_grube06 %>%
    select(GeoUID, Population, starts_with("v_CA06_1742"), starts_with("v_CA06_2050"), 
           propbach15_2006, propprof_2006, propowner_2006, famincome_2006_mean, propbach15_2006_mean)
  st_geometry(.gentrif_grube06_nogeom) <- NULL
  
  gentrif_grube06_16 <- gentrif_grube16 %>%
    select(GeoUID, Population, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`,
           starts_with("v_CA16_4897"), starts_with("v_CA16_4901"), 
           starts_with("v_CA16_4993"), starts_with("v_CA16_4994"),
           starts_with("v_CA16_5051"), propbach15_2016,
           starts_with("v_CA16_5693"), propprof_2016, 
           starts_with("v_CA16_4836"), propowner_2016) %>%
    left_join(lut_ct2016_2006, by=c("GeoUID" = "CT_UID.2016")) %>%
    left_join(.gentrif_grube06_nogeom, by=c("CT_UID.2006" = "GeoUID"), suffix=c("2016", "2006")) %>%
    select(-CT_UID.2006) %>%
    mutate(propbach15_change = propbach15_2016 - propbach15_2006,
           propprof_change = propprof_2016 - propprof_2006,
           propowner_change = propowner_2016 - propowner_2006,
           famincome_change = (`v_CA16_4994: Average total income of economic families in 2015 ($)` - `v_CA06_1742: Average family income $`) / 
             `v_CA06_1742: Average family income $` * 100,
           avgrent_change = (`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)` - `v_CA06_2050: Average gross rent $`) / 
             `v_CA06_2050: Average gross rent $` * 100) %>%
    group_by(!!agg_level) %>%
    mutate(propbach15_change_mean = wtd.mean(propbach15_change, 
                                             `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`, na.rm = TRUE),
           propprof_change_mean = wtd.mean(propprof_change,
                                           `v_CA16_5693: Total Labour Force population aged 15 years and over by Industry - North American Industry Classification System (NAICS) 2012 - 25% sample data`, na.rm = TRUE),
           propowner_change_mean = wtd.mean(propowner_change,
                                            `v_CA16_4836: Total - Private households by tenure - 25% sample data`, na.rm = TRUE),
           famincome_change_mean = wtd.mean(famincome_change,
                                            `v_CA16_4993: Total - Income statistics in 2015 for economic families in private households - 25% sample data`, na.rm = TRUE),
           avgrent_change_mean = wtd.mean(avgrent_change,
                                          `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`, na.rm = TRUE)) %>%
    ungroup()
  
  rm(.gentrif_grube06_nogeom) #cleanup
  
  # Flag gentrifiable and gentrified CTs
  # ---
  # New variables:
  # - gentrifiable: CTs candidate to gentrification in 2006
  # - gentrified: CTs catually gentrified in 2016
  return(gentrif_grube06_16 %>%
    mutate(gentrifiable = `v_CA06_1742: Average family income $` < famincome_2006_mean & propbach15_2006 < propbach15_2006_mean,
           gentrified = gentrifiable & 
             propbach15_change > propbach15_change_mean &
             propprof_change > propprof_change_mean &
             propowner_change > propowner_change_mean &
             famincome_change > famincome_change_mean &
             avgrent_change > avgrent_change_mean)
  )
}

# Compute Grube-Caver's metrics for all three levels CMA | CD | CCS
gentrif_grube06_16_CMA <- compute_grube_metric(census_data_16, census_data_06, CMA_UID)
gentrif_grube06_16_CD <- compute_grube_metric(census_data_16, census_data_06, CD_UID)
gentrif_grube06_16_CCS <- compute_grube_metric(census_data_16, census_data_06, CCS_UID)

## ---- chunk-freeman-metric ----
# ---
# Freeman, L., 2005. Displacement or Succession?: Residential Mobility in Gentrifying Neighborhoods. ####
# Urban Aff. Rev. 40, 463–491.
# ---
# Criteria:
# 1. Be located in the central city at the beginning of the intercensal period.
# 2. Have a median income less than the median (40th percentile) for that metropolitan
# area at the beginning of the intercensal period.
# 3. Have a proportion of housing built within the past 20 years lower than the proportion
# found at the median (40th percentile) for the respective metropolitan area.
# 4. Have a percentage increase in educational attainment greater than the median
# increase in educational attainment for that metropolitan area.
# 5. Have an increase in real housing prices during the intercensal period.
# ---

#
# Main routine to compute Freeman's gentrification metric at one specific aggregation level
#
compute_freeman_metric <- function(census_data_16, census_data_06, agg_level) {
  agg_level <- enquo(agg_level)
  
  # Extract census variables for Freeman's measures
  gentrif_freeman16 <- census_data_16  %>% 
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA16_5051"),starts_with("v_CA16_5078"), #education
           starts_with("v_CA16_4862"),starts_with("v_CA16_4866"),starts_with("v_CA16_4867"),starts_with("v_CA16_4868"),starts_with("v_CA16_4869"), #construction year
           starts_with("v_CA16_4890"), starts_with("v_CA16_4895"),starts_with("v_CA16_4896")) #housing prices
  
  gentrif_freeman06 <- census_data_06 %>%
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA06_1234"),starts_with("v_CA06_1240"),starts_with("v_CA06_1248"),starts_with("v_CA06_1254"),starts_with("v_CA06_1262"),starts_with("v_CA06_1268"), #education
           starts_with("v_CA06_1988"),starts_with("v_CA06_2000"), #income
           starts_with("v_CA06_2053"),starts_with("v_CA06_2054")) #housing prices
  
  # Compute status of CT compared to median of income and construction year variables
  # ---
  # New variables:
  # - pct_built_after_2000: proportion of private dwellings built from 2001 to 2016 (%)
  # - pct_built_after_1990: proportion of private dwellings built from 1991 to 2016 (%)
  # - pct_built_after_2000_median: median of the proportion of private dwellings built from 2001 to 2016 at the Census Division level (%)
  # - pct_built_after_1990_median: median of the proportion of private dwellings built from 1991 to 2016 at the Census Division level (%)
  # - pct_built_after_2000_40thpctile: 40th percentile of the proportion of private dwellings built from 2001 to 2016 at the Census Division level (%)
  # - pct_built_after_1990_40thpctile: 40th percentile of the proportion of private dwellings built from 1991 to 2016 at the Census Division level (%)
  # - pct_educ_attainment2016: proportion of university degrees in 2016 among population aged 15yrs and over (%)
  # - pct_educ_attainment2006: proportion of university degrees in 2006 among population aged 15yrs and over (%)
  # - med_household_income2006_median: median of the median household income in 2006 at the Census Division level ($)
  # - med_household_income2006_40thpctile: 40th percentile of the median household income in 2006 at the Census Division level ($)
  gentrif_freeman16 <- gentrif_freeman16 %>%
    mutate(pct_built_after_2000 = (`v_CA16_4867: 2001 to 2005` + `v_CA16_4868: 2006 to 2010` + `v_CA16_4869: 2011 to 2016`) / `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data` * 100,
           pct_built_after_1990 = (`v_CA16_4866: 1991 to 2000` + `v_CA16_4867: 2001 to 2005` + `v_CA16_4868: 2006 to 2010` + `v_CA16_4869: 2011 to 2016`) / `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data` * 100,
           pct_educ_attainment2016 = `v_CA16_5078: University certificate, diploma or degree at bachelor level or above` / `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data` * 100) %>%
    group_by(!!agg_level) %>%
    mutate(pct_built_after_2000_median = wtd.quantile(pct_built_after_2000,
                                                      `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data`,
                                                      probs = c(.5), na.rm = TRUE),
           pct_built_after_2000_40thpctile = wtd.quantile(pct_built_after_2000, 
                                                          `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data`,
                                                          probs = c(.4), na.rm = TRUE),
           pct_built_after_1990_median = wtd.quantile(pct_built_after_1990,
                                                      `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data`,
                                                      probs = c(.5), na.rm = TRUE),
           pct_built_after_1990_40thpctile = wtd.quantile(pct_built_after_1990, 
                                                          `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data`,
                                                          probs = c(.4), na.rm = TRUE)) %>%
    ungroup()
  
  gentrif_freeman06 <- gentrif_freeman06 %>%
    group_by(!!agg_level) %>%
    mutate(pct_educ_attainment2006 = (`v_CA06_1240: University certificate, diploma or degree` + 
                                        `v_CA06_1254: University certificate, diploma or degree` + 
                                        `v_CA06_1268: University certificate, diploma or degree`) / 
             (`v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data`) * 100) %>%
    mutate(sum_non_NA = sum(!is.na(`v_CA06_2000: Median household income $`)), # add condition to avoid error while computing the weighted median for CD with no non NA value
           med_household_income2006_median = ifelse(sum_non_NA != 0,
                                                    wtd.quantile(`v_CA06_2000: Median household income $`, 
                                                                 `v_CA06_1988: Household income in 2005 of private households - 20% sample data`,
                                                                 probs = c(.5), na.rm = TRUE),
                                                    NA),
           med_household_income2006_40thpctile = ifelse(sum_non_NA != 0,
                                                        wtd.quantile(`v_CA06_2000: Median household income $`,
                                                                     `v_CA06_1988: Household income in 2005 of private households - 20% sample data`,
                                                                     probs = c(.4), na.rm = TRUE),
                                                        NA)) %>%
    ungroup() %>%
    select(-sum_non_NA)
  
  # Join Census 2006 to 2016 and compute differences, etc.
  # ---
  # New variables:
  # - pct_educ_attainment_change: increase in educational attainment (pct_educ_attainment2016 - pct_educ_attainment2006)
  # - pct_house_value_change: increase in house values (in $ 2006 and $ 2016 respectively)
  # - pct_educ_attainment_change_median: median of the increase in educational attainment between 2006 and 2016 at the Census Division level
  # - pct_house_value_change_median: median of the house value between 2006 and 2016 at the Census Division level
  .gentrif_freeman06_nogeom <- gentrif_freeman06 %>%
    select(GeoUID, Population, starts_with("v_CA06_2054"), starts_with("v_CA06_2000"), 
           pct_educ_attainment2006, med_household_income2006_median, med_household_income2006_40thpctile)
  st_geometry(.gentrif_freeman06_nogeom) <- NULL
  
  gentrif_freeman06_16 <- gentrif_freeman16 %>%
    select(GeoUID, Population, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`,
          starts_with("v_CA16_4890"), starts_with("v_CA16_4896"),
           starts_with("v_CA16_5051"), pct_educ_attainment2016, 
          starts_with("v_CA16_4862"), pct_built_after_1990, pct_built_after_1990_median, pct_built_after_1990_40thpctile,
           pct_built_after_2000, pct_built_after_2000_median, pct_built_after_2000_40thpctile) %>%
    left_join(lut_ct2016_2006, by=c("GeoUID" = "CT_UID.2016")) %>%
    left_join(.gentrif_freeman06_nogeom, by=c("CT_UID.2006" = "GeoUID"), suffix=c("2016", "2006")) %>%
    select(-CT_UID.2006) %>%
    mutate(pct_educ_attainment_change = pct_educ_attainment2016 - pct_educ_attainment2006,
           pct_house_value_change = (`v_CA16_4896: Average value of dwellings ($)` - `v_CA06_2054: Average value of dwelling $`) / 
             `v_CA06_2054: Average value of dwelling $` * 100) %>%
    group_by(!!agg_level) %>%
    mutate(sum_non_NA1 = sum(!is.na(pct_educ_attainment_change)), # add condition to avoid error while computing the weighted median for CD with no non NA value
           sum_non_NA2 = sum(!is.na(pct_house_value_change)),
            pct_educ_attainment_change_median = ifelse(sum_non_NA1 != 0,
                                                       wtd.quantile(pct_educ_attainment_change,
                                                            `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`,
                                                            probs = c(.5), na.rm = TRUE),
                                                       NA),
           pct_house_value_change_median = ifelse(sum_non_NA2 != 0,
                                                  wtd.quantile(pct_house_value_change, 
                                                  `v_CA16_4862: Total - Occupied private dwellings by period of construction - 25% sample data`,
                                                  probs = c(.5), na.rm = TRUE),
                                                  NA)) %>%
    ungroup() %>%
    select(-starts_with("sum_non_NA"))
  
  rm(.gentrif_freeman06_nogeom) #cleanup
  
  # Flag candidate CTs
  # NB. Condition 1. (CT in central city not considered here)
  # ---
  # New variables:
  # - candidate_built1990_median: candidate based on conditions 2. and 3., median reference, built year ref = 1990
  # - candidate_built2000_median: candidate based on conditions 2. and 3., median reference, built year ref = 2000
  # - candidate_built1990_40thpctile: candidate based on conditions 2. and 3., 40th percentile reference, built year ref = 1990
  # - candidate_built2000_40thpctile: candidate based on conditions 2. and 3., 40th percentile reference, built year ref = 2000
  gentrif_freeman06_16 <- gentrif_freeman06_16 %>%
    mutate(candidate_built1990_median = `v_CA06_2000: Median household income $` < med_household_income2006_median & pct_built_after_1990 < pct_built_after_1990_median,
           candidate_built2000_median = `v_CA06_2000: Median household income $` < med_household_income2006_median & pct_built_after_2000 < pct_built_after_2000_median,
           candidate_built1990_40thpctile = `v_CA06_2000: Median household income $` < med_household_income2006_median & pct_built_after_1990 < pct_built_after_1990_40thpctile,
           candidate_built2000_40thpctile = `v_CA06_2000: Median household income $` < med_household_income2006_median & pct_built_after_2000 < pct_built_after_2000_40thpctile)
 
  # Flag gentrified CTs (original Freeman's method)
  # ---
  # New variables:
  # - gentrified_built1990_median: gentrified CT based on candidate_built1990_median and conditions 4. and 5.
  # - gentrified_built2000_median: gentrified CT based on candidate_built2000_median and conditions 4. and 5.
  # - gentrified_built1990_40thpctile: gentrified CT based on candidate_built1990_40thpctile and conditions 4. and 5.
  # - gentrified_built1990_40thpctile: gentrified CT based on candidate_built1990_40thpctile and conditions 4. and 5.
  gentrif_freeman06_16 <- gentrif_freeman06_16 %>%
    mutate(gentrified_built1990_median = candidate_built1990_median & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > 17.77, #comparing to inflation (17.77%) between 2006 and 2016. See https://www.bankofcanada.ca/rates/related/inflation-calculator/
           gentrified_built2000_median = candidate_built2000_median & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > 17.77,
           gentrified_built1990_40thpctile = candidate_built1990_40thpctile & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > 17.77,
           gentrified_built2000_40thpctile = candidate_built2000_40thpctile & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > 17.77)
  
  # Flag gentrified CTs (tweaked Freeman's method: we no longer compare house value increase to inflation but to
  # median house value increase in each census division, i.e. same as for higher educ attainment condition)
  # ---
  # New variables:
  # - gentrified_built1990_median: gentrified CT based on candidate_built1990_median and conditions 4. and 5.
  # - gentrified_built2000_median: gentrified CT based on candidate_built2000_median and conditions 4. and 5.
  # - gentrified_built1990_40thpctile: gentrified CT based on candidate_built1990_40thpctile and conditions 4. and 5.
  # - gentrified_built1990_40thpctile: gentrified CT based on candidate_built1990_40thpctile and conditions 4. and 5.
  gentrif_freeman06_16 <- gentrif_freeman06_16 %>%
    mutate(gentrified_tweaked_built1990_median = candidate_built1990_median & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > pct_house_value_change_median,
           gentrified_tweaked_built2000_median = candidate_built2000_median & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > pct_house_value_change_median,
           gentrified_tweaked_built1990_40thpctile = candidate_built1990_40thpctile & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > pct_house_value_change_median,
           gentrified_tweaked_built2000_40thpctile = candidate_built2000_40thpctile & pct_educ_attainment_change > pct_educ_attainment_change_median &
             pct_house_value_change > pct_house_value_change_median)
  
  return(gentrif_freeman06_16)
}

# Compute Freeman's metrics for all three levels CMA | CD | CCS
gentrif_freeman06_16_CD <- compute_freeman_metric(census_data_16, census_data_06, CD_UID)
gentrif_freeman06_16_CMA <- compute_freeman_metric(census_data_16, census_data_06, CMA_UID)
gentrif_freeman06_16_CCS <- compute_freeman_metric(census_data_16, census_data_06, CCS_UID)
  
## ---- chunk-ding-metric ----
# ---
# Ding, L., Hwang, J., 2016. The Consequences of Gentrification: A Focus on Residents’ Financial Health in Philadelphia ####
# Cityscape, Vol. 18, No. 3, Gentrification (2016), pp. 27-56
# https://www.jstor.org/stable/10.2307/26328272
# ---
# Criteria:
# - Gentrifiable -> Have a median income less than the median for that metropolitan
# area at the beginning of the intercensal period.
# - Gentrifying -> Gentrifiable + 
#     - percentage increase above the citywide median increase in either its median gross rent or median home value AND
#     - increase above the citywide median increase in its share of college-educated residents
#
# NB: Canadian census give average rent costs and house values instead of median
# 
# Graded gentrification levels:
#  - Continued gentrification: not implemented (would require to load census data from 1996, or even from 1986)
#  - Weak gentrification: both median rent prices and home values in the bottom quartile of gentrifying CTs
#  - Intense gentrification: either median rent prices or median home values in the top quartile of gentrifying CTs
#  - Moderate gentrification: remaining CTs not categorized as weak or intense gentrification
# ---

compute_ding_metric <- function(census_data_16, census_data_06, agg_level) {
  agg_level <- enquo(agg_level)
  
  # Extract census variables for Ding's measures
  gentrif_ding16 <- census_data_16  %>% 
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA16_5051"),starts_with("v_CA16_5078"), #education
           starts_with("v_CA16_4897"),starts_with("v_CA16_4901"), #renting costs
           starts_with("v_CA16_4890"), starts_with("v_CA16_4895"),starts_with("v_CA16_4896")) #housing prices
  
  gentrif_ding06 <- census_data_06 %>%
    select(GeoUID, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`, Population,
           starts_with("v_CA06_1234"),starts_with("v_CA06_1240"),starts_with("v_CA06_1248"),starts_with("v_CA06_1254"),starts_with("v_CA06_1262"),starts_with("v_CA06_1268"), #education
           starts_with("v_CA06_1988"),starts_with("v_CA06_2000"), #income
           starts_with("v_CA06_2049"),starts_with("v_CA06_2050"), #renting costs
           starts_with("v_CA06_2053"),starts_with("v_CA06_2054")) #housing prices
  
  # Compute status of CT compared to median of income
  # ---
  # New variables:
  # - pct_educ_attainment2016: proportion of university degrees in 2016 among population aged 15yrs and over (%)
  # - pct_educ_attainment2006: proportion of university degrees in 2006 among population aged 15yrs and over (%)
  # - med_household_income2006_median: median of the median household income in 2006 at the Census Division level ($)
  gentrif_ding16 <- gentrif_ding16 %>%
    mutate(pct_educ_attainment2016 = `v_CA16_5078: University certificate, diploma or degree at bachelor level or above` / 
             `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data` * 100)
  
  gentrif_ding06 <- gentrif_ding06 %>%
    mutate(pct_educ_attainment2006 = (`v_CA06_1240: University certificate, diploma or degree` + 
                                        `v_CA06_1254: University certificate, diploma or degree` + 
                                        `v_CA06_1268: University certificate, diploma or degree`) / 
             (`v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + 
                `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data`) * 100) %>%
    group_by(!!agg_level) %>%
    mutate(sum_non_NA = sum(!is.na(`v_CA06_2000: Median household income $`)), # add condition to avoid error while computing the weighted median for agg levels with no non NA value
           med_household_income2006_median = ifelse(sum_non_NA, wtd.quantile(`v_CA06_2000: Median household income $`,
                                                                             `v_CA06_1988: Household income in 2005 of private households - 20% sample data`,
                                                                             probs = c(.5), na.rm = TRUE),
                                                    NA)) %>%
    ungroup() %>%
    select(-sum_non_NA)
  
  # Join Census 2006 to 2016 and compute differences, etc.
  # ---
  # New variables:
  # - pct_educ_attainment_change: increase in educational attainment (pct_educ_attainment2016 - pct_educ_attainment2006)
  # - pct_house_value_change: increase in house values (in $ 2006 and $ 2016 respectively)
  # - pct_rent_cost_change: increase in rent costs (in $ 2006 and $ 2016 respectively)
  # - pct_educ_attainment_change_median: median of the increase in educational attainment between 2006 and 2016 at the Census Division level
  # - pct_house_value_change_median: median of the home value increase between 2006 and 2016 at the Census Division level
  # - pct_rent_cost_change_median: median of the rent cost increase between 2006 and 2016 at the Census Division level
  .gentrif_ding06_nogeom <- gentrif_ding06 %>%
    select(GeoUID, Population, starts_with("v_CA06_2050"), starts_with("v_CA06_2054"), starts_with("v_CA06_2000"), 
           pct_educ_attainment2006, med_household_income2006_median)
  st_geometry(.gentrif_ding06_nogeom) <- NULL
  
  gentrif_ding06_16 <- gentrif_ding16 %>%
    mutate(`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)` = na_if(`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)`, 0),
           `v_CA16_4896: Average value of dwellings ($)` = na_if(`v_CA16_4896: Average value of dwellings ($)`, 0)) %>% #set 0$ home value to NA
    select(GeoUID, Population, CMA_UID, CD_UID, CCS_UID, CSD_UID, `Region Name`,
          starts_with("v_CA16_4890"), starts_with("v_CA16_4896"),
          starts_with("v_CA16_4897"), starts_with("v_CA16_4901"),
          starts_with("v_CA16_5051"), pct_educ_attainment2016) %>%
    left_join(lut_ct2016_2006, by=c("GeoUID" = "CT_UID.2016")) %>%
    left_join(.gentrif_ding06_nogeom, by=c("CT_UID.2006" = "GeoUID"), suffix=c("2016", "2006")) %>%
    select(-CT_UID.2006) %>%
    mutate(pct_educ_attainment_change = pct_educ_attainment2016 - pct_educ_attainment2006,
           pct_house_value_change = (`v_CA16_4896: Average value of dwellings ($)` - `v_CA06_2054: Average value of dwelling $`) / 
             `v_CA06_2054: Average value of dwelling $` * 100,
           pct_rent_cost_change = (`v_CA16_4901: Average monthly shelter costs for rented dwellings ($)` - `v_CA06_2050: Average gross rent $`) /
             `v_CA06_2050: Average gross rent $` * 100) %>%
    group_by(!!agg_level) %>%
    mutate(sum_non_NA1 = sum(!is.na(pct_educ_attainment_change)), # add condition to avoid error while computing the weighted median for agg levels with no non NA value
           sum_non_NA2 = sum(!is.na(pct_house_value_change)),
           sum_non_NA3 = sum(!is.na(pct_rent_cost_change)),
           pct_educ_attainment_change_median = ifelse(sum_non_NA1 != 0,
                                                      wtd.quantile(pct_educ_attainment_change,
                                                                   `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`,
                                                                   probs = c(.5), na.rm = TRUE),
                                                      NA),
           pct_house_value_change_median = ifelse(sum_non_NA2 != 0,
                                                  wtd.quantile(pct_house_value_change,
                                                               `v_CA16_4890: Total - Owner households in non-farm, non-reserve private dwellings - 25% sample data`,
                                                               probs = c(.5), na.rm = TRUE),
                                                  NA),
           pct_rent_cost_change_median = ifelse(sum_non_NA3 != 0,
                                                wtd.quantile(pct_rent_cost_change,
                                                             `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`,
                                                             probs = c(.5), na.rm = TRUE),
                                                NA)) %>%
    ungroup() %>%
    select(-starts_with("sum_non_NA"))
  
  rm(.gentrif_ding06_nogeom) #cleanup
  
  # Flag candidate CTs: have a median income less than the median for that metropolitan
  # ---
  # New variables:
  # - Gentrifiable: 2006 candidate CTs to gentrification in 2016
  gentrif_ding06_16 <- gentrif_ding06_16 %>%
    mutate(gentrifiable = `v_CA06_2000: Median household income $` < med_household_income2006_median)
  
  # Flag gentrified CTs
  # ---
  # New variables:
  # - gentrified: gentrified CT in 2016
  gentrif_ding06_16 <- gentrif_ding06_16 %>%
    mutate(gentrified = gentrifiable & 
             (pct_house_value_change > pct_house_value_change_median | pct_rent_cost_change > pct_rent_cost_change_median) &
             pct_educ_attainment_change > pct_educ_attainment_change_median)
  
  # Categorize gentrified CTs into weak|moderate|intense gentrification
  # ---
  # New variables:
  # - pct_house_value_change_bottomQuartile: bottom quartile of home value increase for gentrifying CTs
  # - pct_house_value_change_topQuartile: top quartile of home value increase for gentrifying CTs
  # - pct_rent_value_change_bottomQuartile: bottom quartile of rent cost increase for gentrifying CTs
  # - pct_rent_value_change_topQuartile: top quartile of rent cost increase for gentrifying CTs
  # - gentrified_category: weak|moderate|intense gentrification (continuous not computed)
  gentrif_ding06_16 <- gentrif_ding06_16 %>%
    group_by(!!agg_level, gentrified) %>%
    mutate(sum_non_NA1 = sum(!is.na(pct_house_value_change)), # add condition to avoid error while computing the weighted median for CD with no non NA value
           sum_non_NA2 = sum(!is.na(pct_rent_cost_change)),
           pct_house_value_change_bottomQuartile = case_when(gentrified ~ ifelse(sum_non_NA1, wtd.quantile(pct_house_value_change, 
                                                                                       `v_CA16_4890: Total - Owner households in non-farm, non-reserve private dwellings - 25% sample data`,
                                                                                       probs = c(.25), na.rm = TRUE),
                                                                                 NA)),
           pct_house_value_change_topQuartile = case_when(gentrified ~ ifelse(sum_non_NA1, wtd.quantile(pct_house_value_change,
                                                                                    `v_CA16_4890: Total - Owner households in non-farm, non-reserve private dwellings - 25% sample data`,
                                                                                    probs = c(.75), na.rm = TRUE),
                                                                              NA)),
           pct_rent_cost_change_bottomQuartile = case_when(gentrified ~ ifelse(sum_non_NA2, wtd.quantile(pct_rent_cost_change,
                                                                                     `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`,
                                                                                     probs = c(.25), na.rm = TRUE),
                                                                               NA)),
           pct_rent_cost_change_topQuartile = case_when(gentrified ~ ifelse(sum_non_NA2, wtd.quantile(pct_rent_cost_change, 
                                                                                  `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`,
                                                                                  probs = c(.75), na.rm = TRUE),
                                                                            NA))) %>%
    ungroup() %>%
    select(-starts_with("sum_non_NA")) %>%
    mutate(gentrified_category = case_when(
      pct_house_value_change < pct_house_value_change_bottomQuartile & pct_rent_cost_change < pct_rent_cost_change_bottomQuartile ~ "weak",
      pct_house_value_change > pct_house_value_change_topQuartile | pct_rent_cost_change > pct_rent_cost_change_topQuartile ~ "intense",
      gentrified ~ "moderate"
    ))
  
  return(gentrif_ding06_16)
}

# Compute Ding's metrics for all three levels CMA | CD | CCS
gentrif_ding06_16_CD <- compute_ding_metric(census_data_16, census_data_06, CD_UID)
gentrif_ding06_16_CCS <- compute_ding_metric(census_data_16, census_data_06, CCS_UID)
gentrif_ding06_16_CMA <- compute_ding_metric(census_data_16, census_data_06, CMA_UID)

## ---- chunk-export-data ----
# Export all gentrification related metrics in the same geopackage + GeoJSON for selected cities ----
.gentrif_freeman_CD_nogeom <- transmute(gentrif_freeman06_16_CD, 
                                         GeoUID = GeoUID,
                                         freeman_cd_gentrifiable = candidate_built1990_median,
                                         freeman_cd_gentrified = gentrified_tweaked_built1990_median)
st_geometry(.gentrif_freeman_CD_nogeom) <- NULL

.gentrif_freeman_CCS_nogeom <- transmute(gentrif_freeman06_16_CCS, 
                                         GeoUID = GeoUID,
                                         freeman_ccs_gentrifiable = candidate_built1990_median,
                                         freeman_ccs_gentrified = gentrified_tweaked_built1990_median)
st_geometry(.gentrif_freeman_CCS_nogeom) <- NULL

.gentrif_ding_CMA_nogeom <- transmute(gentrif_ding06_16_CMA, 
                                     GeoUID = GeoUID,
                                     ding_cma_gentrifiable = gentrifiable,
                                     ding_cma_gentrified = gentrified,
                                     ding_cma_gentrified_category = gentrified_category)
st_geometry(.gentrif_ding_CMA_nogeom) <- NULL

.gentrif_ding_CD_nogeom <- transmute(gentrif_ding06_16_CD, 
                                     GeoUID = GeoUID,
                                     ding_cd_gentrifiable = gentrifiable,
                                     ding_cd_gentrified = gentrified,
                                     ding_cd_gentrified_category = gentrified_category)
st_geometry(.gentrif_ding_CD_nogeom) <- NULL

.gentrif_ding_CCS_nogeom <- transmute(gentrif_ding06_16_CCS, 
                                  GeoUID = GeoUID,
                                  ding_ccs_gentrifiable = gentrifiable,
                                  ding_ccs_gentrified = gentrified,
                                  ding_ccs_gentrified_category = gentrified_category)
st_geometry(.gentrif_ding_CCS_nogeom) <- NULL

.gentrif_grube_CMA_nogeom <- transmute(gentrif_grube06_16_CMA,
                                       GeoUID = GeoUID,
                                       grube_cma_gentrifiable = gentrifiable,
                                       grube_cma_gentrified = gentrified)
st_geometry(.gentrif_grube_CMA_nogeom) <- NULL

.gentrif_grube_CD_nogeom <- transmute(gentrif_grube06_16_CD,
                                       GeoUID = GeoUID,
                                       grube_cd_gentrifiable = gentrifiable,
                                       grube_cd_gentrified = gentrified)
st_geometry(.gentrif_grube_CD_nogeom) <- NULL

.gentrif_grube_CCS_nogeom <- transmute(gentrif_grube06_16_CCS,
                                       GeoUID = GeoUID,
                                       grube_ccs_gentrifiable = gentrifiable,
                                       grube_ccs_gentrified = gentrified)
st_geometry(.gentrif_grube_CCS_nogeom) <- NULL

.gentrif_rania_CMA_nogeom <- transmute(gentrif_rania06_16_CMA,
                                      GeoUID = GeoUID,
                                      steinmetz_cma_gentrifiable = potentially_gentrifying,
                                      steinmetz_cma_gentrified = gentrified)
st_geometry(.gentrif_rania_CMA_nogeom) <- NULL

.gentrif_rania_CD_nogeom <- transmute(gentrif_rania06_16_CD,
                                      GeoUID = GeoUID,
                                      steinmetz_cd_gentrifiable = potentially_gentrifying,
                                      steinmetz_cd_gentrified = gentrified)
st_geometry(.gentrif_rania_CD_nogeom) <- NULL

.gentrif_rania_CCS_nogeom <- transmute(gentrif_rania06_16_CCS,
                                      GeoUID = GeoUID,
                                      steinmetz_ccs_gentrifiable = potentially_gentrifying,
                                      steinmetz_ccs_gentrified = gentrified)
st_geometry(.gentrif_rania_CCS_nogeom) <- NULL

gentrif_combined <- gentrif_freeman06_16_CMA %>%
  transmute(GeoUID = GeoUID, CMA_UID = CMA_UID, CD_UID = CD_UID, CCS_UID = CCS_UID, CSD_UID = CSD_UID, Population2016 = Population2016, Region_name = `Region Name`, geometry = geometry, 
            freeman_cma_gentrifiable = candidate_built1990_median,
            freeman_cma_gentrified = gentrified_tweaked_built1990_median) %>%
  inner_join(.gentrif_freeman_CD_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_freeman_CCS_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_ding_CMA_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_ding_CD_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_ding_CCS_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_grube_CMA_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_grube_CD_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_grube_CCS_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_rania_CMA_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_rania_CD_nogeom, by="GeoUID") %>%
  inner_join(.gentrif_rania_CCS_nogeom, by="GeoUID")

st_write(clean_names(gentrif_combined), dsn = "data/gentrif_combined.gpkg", delete_dsn = TRUE)


# SAVE COMBINED DATASET TO CSV TOO
# (Drop spatial component + replace TRUE/FALSE by 0/1)
.gentrif_combined <- gentrif_combined
st_geometry(.gentrif_combined) <- NULL
to.replace <- names(which(sapply(.gentrif_combined, is.logical)))
.gentrif_combined <- .gentrif_combined %>% mutate_at(to.replace, as.integer)
write.csv(.gentrif_combined, file = "data/gentrif_combined.csv", na = "", row.names = FALSE)
rm(.gentrif_combined)
