---
title: "CMA stats for paper"
author: "B. Thierry, Spherelab"
date: "04/06/2020"
output:
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    code_folding: hide
---





## CMA stats for 2006 and 2016

- median income
- rental housing
- owner occupied housing
- age 30-44yrs
- professional occupation
- low income
- university degree

By CMA + mean/median/sd/min/max, computed at the CT level, all stats weighted


```r
# Extract and harmonize variable names
ct2006_stats <- census_data_06 %>%
  transmute(year = 2006,
            GeoUID = GeoUID,
            PR_UID = PR_UID,
            CMAP_UID = CMA_UID, #sync with StatCan notation
            CMA_UID = case_when(str_length(CMA_UID) == 3 ~ CMA_UID,
                                str_length(CMA_UID) == 5 ~ str_sub(CMA_UID, 3, -1)),
            median_income.pop = `v_CA06_1988: Household income in 2005 of private households - 20% sample data`,
            median_income = case_when(median_income.pop > 0 ~ `v_CA06_2000: Median household income $`,
                                      TRUE ~ NA_real_),
            rental_housing.pop = `v_CA06_101: Total number of occupied private dwellings by housing tenure - 20% sample data`,
            rental_housing = `v_CA06_103: Rented` / rental_housing.pop * 100,
            owner_occupied_housing.pop = `v_CA06_101: Total number of occupied private dwellings by housing tenure - 20% sample data`,
            owner_occupied_housing = `v_CA06_102: Owned` / owner_occupied_housing.pop * 100,
            age_30_44yrs.pop = `v_CA06_2: Total population by sex and age groups - 100% data`,
            age_30_44yrs = (`v_CA06_10: 30 to 34 years` + `v_CA06_11: 35 to 39 years` + `v_CA06_12: 40 to 44 years` + 
                              `v_CA06_29: 30 to 34 years` + `v_CA06_30: 35 to 39 years` + `v_CA06_31: 40 to 44 years`) / age_30_44yrs.pop * 100,
            prof_occup.pop = `v_CA06_1007: Total labour force 15 years and over by industry - North American Industry Classification System 2002  - 20% sample data`,
            prof_occup = `v_CA06_1021: 54 Professional, scientific and technical services` / prof_occup.pop * 100,
            low_income.pop = `v_CA06_1979: Total persons in private households - 20% sample data`,
            low_income = `v_CA06_1981: Prevalence of low income after tax in 2005 %`,
            university_degree.pop = `v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data` + `v_CA06_1248: Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data` + `v_CA06_1262: Total population 65 years and over by highest certificate, diploma or degree - 20% sample data`,
            university_degree = (`v_CA06_1240: University certificate, diploma or degree` + `v_CA06_1254: University certificate, diploma or degree` + `v_CA06_1268: University certificate, diploma or degree`) / university_degree.pop * 100,
            rental_housing_price.pop = `v_CA06_2049: Tenant-occupied private non-farm, non-reserve dwellings`,
            rental_housing_price = `v_CA06_2050: Average gross rent $`)
st_geometry(ct2006_stats) <- NULL

ct2016_stats <- census_data_16 %>%
  transmute(year = 2016,
            GeoUID = GeoUID,
            PR_UID = PR_UID,
            CMAP_UID = case_when(str_length(CMA_UID) == 3 ~ paste0(PR_UID, CMA_UID),
                                str_length(CMA_UID) == 5 ~ CMA_UID), #sync with StatCan notation
            CMA_UID = case_when(str_length(CMA_UID) == 3 ~ CMA_UID,
                                str_length(CMA_UID) == 5 ~ str_sub(CMA_UID, 3, -1)),
            median_income.pop = `v_CA16_2396: Total - Income statistics in 2015 for private households by household size - 100% data`,
            median_income = case_when(median_income.pop > 0 ~ `v_CA16_2397: Median total income of households in 2015 ($)`,
                                      TRUE ~ NA_real_),
            rental_housing.pop = `v_CA16_4836: Total - Private households by tenure - 25% sample data`,
            rental_housing = `v_CA16_4838: Renter` / rental_housing.pop * 100,
            owner_occupied_housing.pop = `v_CA16_4836: Total - Private households by tenure - 25% sample data`,
            owner_occupied_housing = `v_CA16_4837: Owner` / owner_occupied_housing.pop * 100,
            age_30_44yrs.pop = `v_CA16_1: Age Stats`,
            age_30_44yrs = (`v_CA16_118: 30 to 34 years` + `v_CA16_136: 35 to 39 years` + `v_CA16_154: 40 to 44 years`) / age_30_44yrs.pop * 100,
            prof_occup.pop = `v_CA16_5693: Total Labour Force population aged 15 years and over by Industry - North American Industry Classification System (NAICS) 2012 - 25% sample data`,
            prof_occup = `v_CA16_5735: 54 Professional, scientific and technical services` / prof_occup.pop * 100,
            low_income.pop = `v_CA16_2510: Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data`,
            low_income = `v_CA16_2570: Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`,
            university_degree.pop = `v_CA16_5051: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`,
            university_degree = `v_CA16_5078: University certificate, diploma or degree at bachelor level or above` / university_degree.pop * 100,
            rental_housing_price.pop = `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`,
            rental_housing_price = `v_CA16_4900: Median monthly shelter costs for rented dwellings ($)`
            )
st_geometry(ct2016_stats) <- NULL

cma_stats <- bind_rows(ct2006_stats, ct2016_stats)

# Add general CMA attributes
geo_attrib <- read.csv('data/Geographic_attribute_file/2016_92-151_XBB.csv', encoding = "latin1") # Import CMA/CA attributes from StatCan | Catalogue ref.: 92-151-G | Geographic attribute file
cma_B <- geo_attrib %>% 
  group_by(PRname.PRnom, CMAPuid.RMRPidu, CMAuid.RMRidu, CMAname.RMRnom, CMAtype.RMRgenre) %>%
  summarise(pop_total = sum(DBpop2016.IDpop2016, na.rm = TRUE),
            dwell_total = sum(DBtdwell2016.IDtlog2016, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(CMAtype.RMRgenre == 'B') %>%
  transmute(PRname = PRname.PRnom,
            CMAP_UID = as.character(CMAPuid.RMRPidu),
            CMAname = CMAname.RMRnom,
            pop_total = pop_total,
            dwell_total = dwell_total)
```

```
## `summarise()` regrouping output by 'PRname.PRnom', 'CMAPuid.RMRPidu', 'CMAuid.RMRidu', 'CMAname.RMRnom' (override with `.groups` argument)
```

```r
# Control
# cma_stats_ctrl <- cma_stats %>% group_by(year) %>%
#   summarise(n = n(),
#             mean_median_income.pop = sum(median_income.pop, na.rm=T),
#             mean_median_income = round(wtd.mean(median_income, median_income.pop, na.rm=T), digits=1),
#             mean_rental_housing.pop = sum(rental_housing.pop, na.rm=T),
#             mean_rental_housing = round(wtd.mean(rental_housing, rental_housing.pop, na.rm=T), digits=1),
#             mean_owner_occupied_housing.pop = sum(owner_occupied_housing.pop, na.rm=T),
#             mean_owner_occupied_housing = round(wtd.mean(owner_occupied_housing, owner_occupied_housing.pop, na.rm=T), digits=1),
#             mean_age_30_44yrs.pop = sum(age_30_44yrs.pop, na.rm=T),
#             mean_age_30_44yrs = round(wtd.mean(age_30_44yrs, age_30_44yrs.pop, na.rm=T), digits=1),
#             mean_prof_occup.pop = sum(prof_occup.pop, na.rm=T),
#             mean_prof_occup = round(wtd.mean(prof_occup, prof_occup.pop, na.rm=T), digits=1),
#             mean_low_income.pop = sum(low_income.pop, na.rm=T),
#             mean_low_income = round(wtd.mean(low_income, low_income.pop, na.rm=T), digits=1),
#             mean_university_degree.pop = sum(university_degree.pop, na.rm=T),
#             mean_university_degree = round(wtd.mean(university_degree, university_degree.pop, na.rm=T), digits=1)) %>%
#   pivot_longer(-year) %>%
#   pivot_wider(names_from=year, values_from=value)
# knitr::kable(cma_stats_ctrl, caption = "Control | Basic stats, 2006 vs. 2016, weighted") %>%
#   kable_styling(bootstrap_options = "striped")

# Output global stats for all CMAs (-> table 2 in paper)
cma_stats_pop <- cma_stats %>%
  select(year, GeoUID, ends_with(".pop")) %>%
  pivot_longer(-c(year, GeoUID), names_to = "var", names_transform = list(var = ~ str_replace(.x, ".pop", "")), values_to = "pop")

cma_stats_var <- cma_stats %>%
  select(-ends_with(".pop")) %>%
  pivot_longer(-c(year, GeoUID, PR_UID, CMA_UID, CMAP_UID), names_to = "var") %>%
  inner_join(cma_stats_pop, by=c("year", "GeoUID", "var"))


cma_stats_global <- cma_stats_var %>%
  group_by(year, var) %>%
  summarise(N = n(),
            Median = wtd.quantile(value, pop, probs = .5, na.rm = T),
            IQR = wtd.quantile(value, pop, probs = .75, na.rm = T) - wtd.quantile(value, pop, probs = .25, na.rm = T),
            Range = max(value, na.rm = T) - min(value, na.rm = T)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = c(N, Median, IQR, Range)) %>%
    select(var, ends_with('_2006'), ends_with('_2016'))
```

```
## `summarise()` regrouping output by 'year' (override with `.groups` argument)
```

```r
knitr::kable(cma_stats_global,
            digits = 1,
            col.names = c("Variable",
                          "N CTs", "Median", "IQR", "Range",
                          "N CTs", "Median", "IQR", "Range")) %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "2006" = 4, "2016" = 4))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:right;"> N CTs </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> IQR </th>
   <th style="text-align:right;"> Range </th>
   <th style="text-align:right;"> N CTs </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> IQR </th>
   <th style="text-align:right;"> Range </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> age_30_44yrs </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 46.2 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 48.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> low_income </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 10.2 </td>
   <td style="text-align:right;"> 11.9 </td>
   <td style="text-align:right;"> 85.4 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 66.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> median_income </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 56603.0 </td>
   <td style="text-align:right;"> 29749.0 </td>
   <td style="text-align:right;"> 251025.0 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 74002.0 </td>
   <td style="text-align:right;"> 38494.0 </td>
   <td style="text-align:right;"> 317114.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> owner_occupied_housing </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 70.5 </td>
   <td style="text-align:right;"> 39.3 </td>
   <td style="text-align:right;"> 110.0 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 70.4 </td>
   <td style="text-align:right;"> 37.2 </td>
   <td style="text-align:right;"> 125.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prof_occup </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 40.0 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 27.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rental_housing </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 29.5 </td>
   <td style="text-align:right;"> 39.3 </td>
   <td style="text-align:right;"> 101.2 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 29.6 </td>
   <td style="text-align:right;"> 37.2 </td>
   <td style="text-align:right;"> 107.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rental_housing_price </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 729.0 </td>
   <td style="text-align:right;"> 263.0 </td>
   <td style="text-align:right;"> 2601.0 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 961.0 </td>
   <td style="text-align:right;"> 422.0 </td>
   <td style="text-align:right;"> 3971.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> university_degree </td>
   <td style="text-align:right;"> 5076 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 73.0 </td>
   <td style="text-align:right;"> 5721 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 18.8 </td>
   <td style="text-align:right;"> 74.2 </td>
  </tr>
</tbody>
</table>




```r
compute_stats_by_CMA <- function(v, w) {
  # Compute weighted stats mean/med/sd/min/max for var V by year and CMA and output table
  digits = 1
  v_group <- cma_stats %>%
    group_by(year, CMAP_UID) %>%
    summarise(N = n(),
              Mean = round(wtd.mean({{ v }}, {{ w }}, na.rm = TRUE), digits = digits),
              Median = round(wtd.quantile({{ v }}, {{ w }}, probs=.5, na.rm = TRUE), digits = digits),
              Std = round(sqrt(wtd.var({{ v }}, {{ w }}, na.rm = TRUE)), digits = digits),
              Min = round(min({{ v }}, na.rm = TRUE), digits = digits),
              Max = round(max({{ v }}, na.rm = TRUE),  digits = digits)) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = c(N, Mean, Median, Std, Min, Max)) %>%
    select(CMAP_UID, ends_with('_2006'), ends_with('_2016'))
    
  v_group <- cma_B %>% 
    mutate(PRname = str_replace(PRname,"(\\(.* / .*| / .*)", ""),
           CMAname = str_replace(CMAname,"(\\(.* / .*| / .*)", "")) %>%
    select(-dwell_total) %>% 
    inner_join(v_group, by="CMAP_UID")
  
  return(v_group)
    
}
```

## Median income ($)

2006: `v_CA06_1988: Household income in 2005 of private households - 20% sample data`  
2016: `v_CA16_2397: Median total income of households in 2015 ($)`


```r
agg_stats <- compute_stats_by_CMA(median_income, median_income.pop)
```

```
## `summarise()` regrouping output by 'year' (override with `.groups` argument)
```

```r
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 53790.3 </td>
   <td style="text-align:right;"> 52603 </td>
   <td style="text-align:right;"> 13880.9 </td>
   <td style="text-align:right;"> 30313 </td>
   <td style="text-align:right;"> 88530 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 77383.1 </td>
   <td style="text-align:right;"> 76757 </td>
   <td style="text-align:right;"> 20958.9 </td>
   <td style="text-align:right;"> 26560 </td>
   <td style="text-align:right;"> 118626 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 71850.5 </td>
   <td style="text-align:right;"> 68079 </td>
   <td style="text-align:right;"> 23868.9 </td>
   <td style="text-align:right;"> 16655 </td>
   <td style="text-align:right;"> 174363 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 103953.1 </td>
   <td style="text-align:right;"> 100352 </td>
   <td style="text-align:right;"> 32240.1 </td>
   <td style="text-align:right;"> 37344 </td>
   <td style="text-align:right;"> 264192 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 65290.3 </td>
   <td style="text-align:right;"> 62168 </td>
   <td style="text-align:right;"> 21896.4 </td>
   <td style="text-align:right;"> 25489 </td>
   <td style="text-align:right;"> 139028 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 97322.6 </td>
   <td style="text-align:right;"> 98176 </td>
   <td style="text-align:right;"> 28095.0 </td>
   <td style="text-align:right;"> 32384 </td>
   <td style="text-align:right;"> 182912 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 53177.7 </td>
   <td style="text-align:right;"> 51352 </td>
   <td style="text-align:right;"> 15414.4 </td>
   <td style="text-align:right;"> 30559 </td>
   <td style="text-align:right;"> 91328 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 74254.6 </td>
   <td style="text-align:right;"> 72294 </td>
   <td style="text-align:right;"> 21078.5 </td>
   <td style="text-align:right;"> 41888 </td>
   <td style="text-align:right;"> 126025 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 56851.0 </td>
   <td style="text-align:right;"> 61639 </td>
   <td style="text-align:right;"> 14867.2 </td>
   <td style="text-align:right;"> 28371 </td>
   <td style="text-align:right;"> 82260 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 75938.1 </td>
   <td style="text-align:right;"> 78738 </td>
   <td style="text-align:right;"> 21426.0 </td>
   <td style="text-align:right;"> 38528 </td>
   <td style="text-align:right;"> 117120 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 57848.1 </td>
   <td style="text-align:right;"> 55333 </td>
   <td style="text-align:right;"> 17600.6 </td>
   <td style="text-align:right;"> 11433 </td>
   <td style="text-align:right;"> 149881 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 75443.9 </td>
   <td style="text-align:right;"> 73045 </td>
   <td style="text-align:right;"> 21054.0 </td>
   <td style="text-align:right;"> 17051 </td>
   <td style="text-align:right;"> 145481 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 55963.3 </td>
   <td style="text-align:right;"> 55719 </td>
   <td style="text-align:right;"> 16224.6 </td>
   <td style="text-align:right;"> 27521 </td>
   <td style="text-align:right;"> 102780 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 72551.9 </td>
   <td style="text-align:right;"> 71351 </td>
   <td style="text-align:right;"> 19330.4 </td>
   <td style="text-align:right;"> 36224 </td>
   <td style="text-align:right;"> 139349 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 54375.9 </td>
   <td style="text-align:right;"> 50072 </td>
   <td style="text-align:right;"> 20448.1 </td>
   <td style="text-align:right;"> 18561 </td>
   <td style="text-align:right;"> 104581 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 74603.3 </td>
   <td style="text-align:right;"> 69952 </td>
   <td style="text-align:right;"> 25376.2 </td>
   <td style="text-align:right;"> 24619 </td>
   <td style="text-align:right;"> 147285 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 50881.2 </td>
   <td style="text-align:right;"> 50596 </td>
   <td style="text-align:right;"> 13285.5 </td>
   <td style="text-align:right;"> 25728 </td>
   <td style="text-align:right;"> 77972 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 64347.7 </td>
   <td style="text-align:right;"> 67698 </td>
   <td style="text-align:right;"> 17146.2 </td>
   <td style="text-align:right;"> 31168 </td>
   <td style="text-align:right;"> 96256 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 51749.2 </td>
   <td style="text-align:right;"> 51559 </td>
   <td style="text-align:right;"> 17332.3 </td>
   <td style="text-align:right;"> 16679 </td>
   <td style="text-align:right;"> 93177 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 67537.9 </td>
   <td style="text-align:right;"> 68771 </td>
   <td style="text-align:right;"> 23546.9 </td>
   <td style="text-align:right;"> 21717 </td>
   <td style="text-align:right;"> 121664 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 53867.5 </td>
   <td style="text-align:right;"> 52327 </td>
   <td style="text-align:right;"> 14559.7 </td>
   <td style="text-align:right;"> 26159 </td>
   <td style="text-align:right;"> 97487 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 81549.7 </td>
   <td style="text-align:right;"> 82261 </td>
   <td style="text-align:right;"> 22133.1 </td>
   <td style="text-align:right;"> 37056 </td>
   <td style="text-align:right;"> 127040 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 56043.6 </td>
   <td style="text-align:right;"> 57538 </td>
   <td style="text-align:right;"> 17740.9 </td>
   <td style="text-align:right;"> 24784 </td>
   <td style="text-align:right;"> 117738 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 71668.1 </td>
   <td style="text-align:right;"> 71522 </td>
   <td style="text-align:right;"> 22309.7 </td>
   <td style="text-align:right;"> 32333 </td>
   <td style="text-align:right;"> 154112 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 71844.5 </td>
   <td style="text-align:right;"> 72830 </td>
   <td style="text-align:right;"> 24677.9 </td>
   <td style="text-align:right;"> 23191 </td>
   <td style="text-align:right;"> 185528 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 88838.4 </td>
   <td style="text-align:right;"> 91312 </td>
   <td style="text-align:right;"> 28538.6 </td>
   <td style="text-align:right;"> 26944 </td>
   <td style="text-align:right;"> 232960 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 58240.6 </td>
   <td style="text-align:right;"> 60953 </td>
   <td style="text-align:right;"> 19465.6 </td>
   <td style="text-align:right;"> 26053 </td>
   <td style="text-align:right;"> 103620 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 73247.4 </td>
   <td style="text-align:right;"> 77461 </td>
   <td style="text-align:right;"> 23555.3 </td>
   <td style="text-align:right;"> 31616 </td>
   <td style="text-align:right;"> 122912 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 51876.4 </td>
   <td style="text-align:right;"> 51584 </td>
   <td style="text-align:right;"> 11697.9 </td>
   <td style="text-align:right;"> 27876 </td>
   <td style="text-align:right;"> 75061 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 64697.1 </td>
   <td style="text-align:right;"> 63104 </td>
   <td style="text-align:right;"> 15933.7 </td>
   <td style="text-align:right;"> 29355 </td>
   <td style="text-align:right;"> 92695 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 54695.7 </td>
   <td style="text-align:right;"> 57730 </td>
   <td style="text-align:right;"> 14247.4 </td>
   <td style="text-align:right;"> 23336 </td>
   <td style="text-align:right;"> 81520 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 66788.0 </td>
   <td style="text-align:right;"> 65097 </td>
   <td style="text-align:right;"> 19254.2 </td>
   <td style="text-align:right;"> 27920 </td>
   <td style="text-align:right;"> 104883 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 72974.1 </td>
   <td style="text-align:right;"> 77082 </td>
   <td style="text-align:right;"> 19062.1 </td>
   <td style="text-align:right;"> 30792 </td>
   <td style="text-align:right;"> 106475 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 88395.5 </td>
   <td style="text-align:right;"> 92629 </td>
   <td style="text-align:right;"> 25588.6 </td>
   <td style="text-align:right;"> 31328 </td>
   <td style="text-align:right;"> 135613 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 67383.3 </td>
   <td style="text-align:right;"> 63713 </td>
   <td style="text-align:right;"> 23726.4 </td>
   <td style="text-align:right;"> 20673 </td>
   <td style="text-align:right;"> 246341 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 82553.1 </td>
   <td style="text-align:right;"> 78677 </td>
   <td style="text-align:right;"> 28650.0 </td>
   <td style="text-align:right;"> 22208 </td>
   <td style="text-align:right;"> 289792 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 63755.0 </td>
   <td style="text-align:right;"> 61891 </td>
   <td style="text-align:right;"> 22087.7 </td>
   <td style="text-align:right;"> 18368 </td>
   <td style="text-align:right;"> 135675 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 79644.7 </td>
   <td style="text-align:right;"> 78196 </td>
   <td style="text-align:right;"> 28457.8 </td>
   <td style="text-align:right;"> 20552 </td>
   <td style="text-align:right;"> 157440 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 54818.6 </td>
   <td style="text-align:right;"> 54244 </td>
   <td style="text-align:right;"> 14288.7 </td>
   <td style="text-align:right;"> 26312 </td>
   <td style="text-align:right;"> 95128 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 65404.1 </td>
   <td style="text-align:right;"> 64256 </td>
   <td style="text-align:right;"> 17616.9 </td>
   <td style="text-align:right;"> 29952 </td>
   <td style="text-align:right;"> 108117 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 65256.7 </td>
   <td style="text-align:right;"> 61484 </td>
   <td style="text-align:right;"> 18686.6 </td>
   <td style="text-align:right;"> 17446 </td>
   <td style="text-align:right;"> 122360 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 79903.0 </td>
   <td style="text-align:right;"> 76014 </td>
   <td style="text-align:right;"> 24859.5 </td>
   <td style="text-align:right;"> 19712 </td>
   <td style="text-align:right;"> 173696 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 57765.0 </td>
   <td style="text-align:right;"> 59713 </td>
   <td style="text-align:right;"> 14943.2 </td>
   <td style="text-align:right;"> 21741 </td>
   <td style="text-align:right;"> 124337 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 71290.0 </td>
   <td style="text-align:right;"> 72521 </td>
   <td style="text-align:right;"> 21012.2 </td>
   <td style="text-align:right;"> 26714 </td>
   <td style="text-align:right;"> 143360 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 67604.5 </td>
   <td style="text-align:right;"> 66887 </td>
   <td style="text-align:right;"> 17973.5 </td>
   <td style="text-align:right;"> 26187 </td>
   <td style="text-align:right;"> 100952 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 83864.6 </td>
   <td style="text-align:right;"> 88537 </td>
   <td style="text-align:right;"> 20837.3 </td>
   <td style="text-align:right;"> 45517 </td>
   <td style="text-align:right;"> 112085 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 58186.0 </td>
   <td style="text-align:right;"> 55770 </td>
   <td style="text-align:right;"> 19088.5 </td>
   <td style="text-align:right;"> 24818 </td>
   <td style="text-align:right;"> 118536 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 68590.3 </td>
   <td style="text-align:right;"> 64512 </td>
   <td style="text-align:right;"> 24089.1 </td>
   <td style="text-align:right;"> 31472 </td>
   <td style="text-align:right;"> 131021 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 62592.2 </td>
   <td style="text-align:right;"> 62696 </td>
   <td style="text-align:right;"> 22785.8 </td>
   <td style="text-align:right;"> 19660 </td>
   <td style="text-align:right;"> 103964 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 70383.6 </td>
   <td style="text-align:right;"> 70752 </td>
   <td style="text-align:right;"> 26931.3 </td>
   <td style="text-align:right;"> 21559 </td>
   <td style="text-align:right;"> 131258 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 66482.2 </td>
   <td style="text-align:right;"> 66385 </td>
   <td style="text-align:right;"> 13766.3 </td>
   <td style="text-align:right;"> 32603 </td>
   <td style="text-align:right;"> 100755 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 82082.8 </td>
   <td style="text-align:right;"> 81510 </td>
   <td style="text-align:right;"> 18515.5 </td>
   <td style="text-align:right;"> 46640 </td>
   <td style="text-align:right;"> 123392 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 56611.9 </td>
   <td style="text-align:right;"> 62624 </td>
   <td style="text-align:right;"> 16955.1 </td>
   <td style="text-align:right;"> 21072 </td>
   <td style="text-align:right;"> 91774 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 74714.8 </td>
   <td style="text-align:right;"> 81152 </td>
   <td style="text-align:right;"> 22417.4 </td>
   <td style="text-align:right;"> 28192 </td>
   <td style="text-align:right;"> 132535 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 55538.0 </td>
   <td style="text-align:right;"> 52730 </td>
   <td style="text-align:right;"> 17305.3 </td>
   <td style="text-align:right;"> 16187 </td>
   <td style="text-align:right;"> 98205 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 71278.2 </td>
   <td style="text-align:right;"> 65749 </td>
   <td style="text-align:right;"> 23863.7 </td>
   <td style="text-align:right;"> 23200 </td>
   <td style="text-align:right;"> 129728 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 47110.1 </td>
   <td style="text-align:right;"> 46859 </td>
   <td style="text-align:right;"> 10714.4 </td>
   <td style="text-align:right;"> 20258 </td>
   <td style="text-align:right;"> 69221 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 59884.1 </td>
   <td style="text-align:right;"> 59093 </td>
   <td style="text-align:right;"> 13508.8 </td>
   <td style="text-align:right;"> 23674 </td>
   <td style="text-align:right;"> 83232 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 51815.0 </td>
   <td style="text-align:right;"> 51478 </td>
   <td style="text-align:right;"> 17103.5 </td>
   <td style="text-align:right;"> 18344 </td>
   <td style="text-align:right;"> 105372 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 68180.3 </td>
   <td style="text-align:right;"> 68224 </td>
   <td style="text-align:right;"> 21033.6 </td>
   <td style="text-align:right;"> 30950 </td>
   <td style="text-align:right;"> 144981 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 43640.4 </td>
   <td style="text-align:right;"> 42546 </td>
   <td style="text-align:right;"> 12824.2 </td>
   <td style="text-align:right;"> 18238 </td>
   <td style="text-align:right;"> 68071 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 56069.4 </td>
   <td style="text-align:right;"> 58752 </td>
   <td style="text-align:right;"> 17426.1 </td>
   <td style="text-align:right;"> 18912 </td>
   <td style="text-align:right;"> 90027 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 43347.0 </td>
   <td style="text-align:right;"> 37862 </td>
   <td style="text-align:right;"> 15828.8 </td>
   <td style="text-align:right;"> 18787 </td>
   <td style="text-align:right;"> 90869 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 54508.5 </td>
   <td style="text-align:right;"> 54624 </td>
   <td style="text-align:right;"> 17637.5 </td>
   <td style="text-align:right;"> 23488 </td>
   <td style="text-align:right;"> 100576 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 51105.0 </td>
   <td style="text-align:right;"> 46327 </td>
   <td style="text-align:right;"> 19889.1 </td>
   <td style="text-align:right;"> 13024 </td>
   <td style="text-align:right;"> 262458 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 65508.0 </td>
   <td style="text-align:right;"> 60774 </td>
   <td style="text-align:right;"> 23939.9 </td>
   <td style="text-align:right;"> 18784 </td>
   <td style="text-align:right;"> 334165 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 60800.6 </td>
   <td style="text-align:right;"> 56750 </td>
   <td style="text-align:right;"> 19881.2 </td>
   <td style="text-align:right;"> 26424 </td>
   <td style="text-align:right;"> 116260 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 73829.4 </td>
   <td style="text-align:right;"> 72558 </td>
   <td style="text-align:right;"> 23078.9 </td>
   <td style="text-align:right;"> 33504 </td>
   <td style="text-align:right;"> 116443 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 58399.4 </td>
   <td style="text-align:right;"> 58560 </td>
   <td style="text-align:right;"> 20228.6 </td>
   <td style="text-align:right;"> 18723 </td>
   <td style="text-align:right;"> 128451 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 87551.7 </td>
   <td style="text-align:right;"> 85931 </td>
   <td style="text-align:right;"> 27876.7 </td>
   <td style="text-align:right;"> 25504 </td>
   <td style="text-align:right;"> 157408 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 52692.5 </td>
   <td style="text-align:right;"> 50081 </td>
   <td style="text-align:right;"> 17104.1 </td>
   <td style="text-align:right;"> 25385 </td>
   <td style="text-align:right;"> 92351 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 84374.2 </td>
   <td style="text-align:right;"> 82449 </td>
   <td style="text-align:right;"> 22179.4 </td>
   <td style="text-align:right;"> 20288 </td>
   <td style="text-align:right;"> 146651 </td>
  </tr>
</tbody>
</table>

## Rental housing (%)

2006: `v_CA06_101: Total number of occupied private dwellings by housing tenure - 20% sample data` & `v_CA06_103: Rented`  
2016: `v_CA16_4836: Total - Private households by tenure - 25% sample data` & `v_CA16_4838: Renter`


```r
agg_stats <- compute_stats_by_CMA(rental_housing, rental_housing.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 26.0 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 54.6 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 27.6 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 59.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 25.9 </td>
   <td style="text-align:right;"> 20.2 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 85.5 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 27.0 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 76.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 30.4 </td>
   <td style="text-align:right;"> 25.1 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 92.6 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 30.2 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 89.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 22.4 </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 70.4 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 26.6 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 70.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 26.5 </td>
   <td style="text-align:right;"> 21.6 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 66.9 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 28.4 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 14.2 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 69.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 34.9 </td>
   <td style="text-align:right;"> 32.8 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 95.6 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 36.3 </td>
   <td style="text-align:right;"> 34.8 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 91.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 35.1 </td>
   <td style="text-align:right;"> 27.4 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 88.0 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 37.3 </td>
   <td style="text-align:right;"> 31.7 </td>
   <td style="text-align:right;"> 19.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 86.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 32.8 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 98.4 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 32.7 </td>
   <td style="text-align:right;"> 26.6 </td>
   <td style="text-align:right;"> 24.3 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 107.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 29.8 </td>
   <td style="text-align:right;"> 23.3 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 82.9 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 31.0 </td>
   <td style="text-align:right;"> 25.5 </td>
   <td style="text-align:right;"> 21.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 86.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 30.0 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 26.5 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 29.7 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 26.7 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 100.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 28.4 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 15.3 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 60.6 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 30.0 </td>
   <td style="text-align:right;"> 28.6 </td>
   <td style="text-align:right;"> 15.0 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 59.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 36.0 </td>
   <td style="text-align:right;"> 32.6 </td>
   <td style="text-align:right;"> 27.2 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 39.9 </td>
   <td style="text-align:right;"> 35.0 </td>
   <td style="text-align:right;"> 27.5 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 98.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 33.3 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 26.9 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 94.8 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 33.3 </td>
   <td style="text-align:right;"> 24.2 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 93.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 32.6 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 25.7 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 101.2 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 34.4 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 25.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 99.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 32.1 </td>
   <td style="text-align:right;"> 31.1 </td>
   <td style="text-align:right;"> 20.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 99.0 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 30.8 </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 100.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 27.3 </td>
   <td style="text-align:right;"> 22.6 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 90.8 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 29.2 </td>
   <td style="text-align:right;"> 24.3 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 88.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 14.3 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 0.9 </td>
   <td style="text-align:right;"> 85.4 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 13.5 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 88.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 32.3 </td>
   <td style="text-align:right;"> 26.2 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 33.5 </td>
   <td style="text-align:right;"> 28.3 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 102.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 28.1 </td>
   <td style="text-align:right;"> 21.6 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 91.1 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 29.6 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 89.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 79.4 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 27.1 </td>
   <td style="text-align:right;"> 24.8 </td>
   <td style="text-align:right;"> 16.3 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 81.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 30.2 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 19.9 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 89.0 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 32.1 </td>
   <td style="text-align:right;"> 28.3 </td>
   <td style="text-align:right;"> 20.4 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 94.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 26.3 </td>
   <td style="text-align:right;"> 27.3 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 94.1 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 27.7 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 17.7 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 96.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 28.9 </td>
   <td style="text-align:right;"> 29.4 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 93.4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 29.8 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 86.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 34.0 </td>
   <td style="text-align:right;"> 30.5 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 83.6 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 36.0 </td>
   <td style="text-align:right;"> 33.4 </td>
   <td style="text-align:right;"> 22.7 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 82.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 25.7 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 23.3 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 83.3 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 28.3 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 85.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 17.0 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 59.8 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 14.8 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 63.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 33.0 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:right;"> 22.4 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 92.2 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 33.8 </td>
   <td style="text-align:right;"> 30.2 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 91.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 27.1 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 17.3 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 86.2 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 26.7 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 83.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 36.7 </td>
   <td style="text-align:right;"> 35.0 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 78.3 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 35.7 </td>
   <td style="text-align:right;"> 35.2 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 87.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 41.4 </td>
   <td style="text-align:right;"> 37.5 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 87.5 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 39.8 </td>
   <td style="text-align:right;"> 33.5 </td>
   <td style="text-align:right;"> 24.5 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 87.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 46.5 </td>
   <td style="text-align:right;"> 45.0 </td>
   <td style="text-align:right;"> 22.8 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 94.9 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 43.7 </td>
   <td style="text-align:right;"> 43.7 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 94.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 42.3 </td>
   <td style="text-align:right;"> 44.4 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 89.1 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 41.6 </td>
   <td style="text-align:right;"> 37.7 </td>
   <td style="text-align:right;"> 22.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 89.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 46.6 </td>
   <td style="text-align:right;"> 49.3 </td>
   <td style="text-align:right;"> 27.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 99.3 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 44.3 </td>
   <td style="text-align:right;"> 45.1 </td>
   <td style="text-align:right;"> 25.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 99.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 32.4 </td>
   <td style="text-align:right;"> 26.0 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 90.8 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 33.6 </td>
   <td style="text-align:right;"> 29.3 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 88.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 29.8 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 84.5 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 30.1 </td>
   <td style="text-align:right;"> 27.3 </td>
   <td style="text-align:right;"> 18.9 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 82.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 31.8 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 83.6 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 29.7 </td>
   <td style="text-align:right;"> 27.9 </td>
   <td style="text-align:right;"> 16.6 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 100.0 </td>
  </tr>
</tbody>
</table>

## Owner occupied housing (%)

2006: `v_CA06_101: Total number of occupied private dwellings by housing tenure - 20% sample data` & `v_CA06_102: Owned`  
2016: `v_CA16_4836: Total - Private households by tenure - 25% sample data` & `v_CA16_4837: Owner`


```r
agg_stats <- compute_stats_by_CMA(owner_occupied_housing, owner_occupied_housing.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 74.0 </td>
   <td style="text-align:right;"> 77.7 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 45.1 </td>
   <td style="text-align:right;"> 97.8 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 70.9 </td>
   <td style="text-align:right;"> 72.4 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 41.0 </td>
   <td style="text-align:right;"> 92.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 74.1 </td>
   <td style="text-align:right;"> 80.0 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 14.5 </td>
   <td style="text-align:right;"> 99.3 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 73.0 </td>
   <td style="text-align:right;"> 79.5 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:right;"> 98.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 69.4 </td>
   <td style="text-align:right;"> 74.8 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 99.2 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 69.6 </td>
   <td style="text-align:right;"> 75.7 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 98.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 77.7 </td>
   <td style="text-align:right;"> 81.9 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 29.4 </td>
   <td style="text-align:right;"> 95.1 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 73.3 </td>
   <td style="text-align:right;"> 76.5 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 29.4 </td>
   <td style="text-align:right;"> 92.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 73.5 </td>
   <td style="text-align:right;"> 78.4 </td>
   <td style="text-align:right;"> 13.7 </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 91.4 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 71.6 </td>
   <td style="text-align:right;"> 76.3 </td>
   <td style="text-align:right;"> 14.2 </td>
   <td style="text-align:right;"> 30.2 </td>
   <td style="text-align:right;"> 94.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 65.1 </td>
   <td style="text-align:right;"> 67.2 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 97.8 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 63.7 </td>
   <td style="text-align:right;"> 65.2 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 96.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 64.7 </td>
   <td style="text-align:right;"> 72.4 </td>
   <td style="text-align:right;"> 20.7 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 95.0 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 62.6 </td>
   <td style="text-align:right;"> 68.2 </td>
   <td style="text-align:right;"> 19.4 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 93.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 67.1 </td>
   <td style="text-align:right;"> 74.4 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 67.3 </td>
   <td style="text-align:right;"> 73.5 </td>
   <td style="text-align:right;"> 24.4 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 98.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 70.2 </td>
   <td style="text-align:right;"> 76.7 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 94.9 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 68.9 </td>
   <td style="text-align:right;"> 74.5 </td>
   <td style="text-align:right;"> 21.6 </td>
   <td style="text-align:right;"> 13.6 </td>
   <td style="text-align:right;"> 93.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 70.0 </td>
   <td style="text-align:right;"> 82.2 </td>
   <td style="text-align:right;"> 26.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 97.3 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 70.3 </td>
   <td style="text-align:right;"> 82.7 </td>
   <td style="text-align:right;"> 26.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 96.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 71.5 </td>
   <td style="text-align:right;"> 74.9 </td>
   <td style="text-align:right;"> 15.3 </td>
   <td style="text-align:right;"> 39.4 </td>
   <td style="text-align:right;"> 95.9 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 70.0 </td>
   <td style="text-align:right;"> 71.4 </td>
   <td style="text-align:right;"> 15.0 </td>
   <td style="text-align:right;"> 40.5 </td>
   <td style="text-align:right;"> 93.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 64.0 </td>
   <td style="text-align:right;"> 67.1 </td>
   <td style="text-align:right;"> 27.1 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 98.3 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 60.1 </td>
   <td style="text-align:right;"> 65.3 </td>
   <td style="text-align:right;"> 27.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 95.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 66.7 </td>
   <td style="text-align:right;"> 75.2 </td>
   <td style="text-align:right;"> 26.9 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 99.2 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 66.7 </td>
   <td style="text-align:right;"> 75.5 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 99.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 67.4 </td>
   <td style="text-align:right;"> 79.1 </td>
   <td style="text-align:right;"> 25.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 98.1 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 65.5 </td>
   <td style="text-align:right;"> 76.1 </td>
   <td style="text-align:right;"> 25.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 96.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 67.9 </td>
   <td style="text-align:right;"> 69.2 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 69.3 </td>
   <td style="text-align:right;"> 68.4 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 97.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 72.6 </td>
   <td style="text-align:right;"> 77.4 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 96.8 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 70.8 </td>
   <td style="text-align:right;"> 75.7 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 11.1 </td>
   <td style="text-align:right;"> 95.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 78.6 </td>
   <td style="text-align:right;"> 85.9 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 14.8 </td>
   <td style="text-align:right;"> 99.3 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 77.8 </td>
   <td style="text-align:right;"> 86.6 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 11.8 </td>
   <td style="text-align:right;"> 96.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 67.7 </td>
   <td style="text-align:right;"> 73.8 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 66.5 </td>
   <td style="text-align:right;"> 71.8 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 125.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 71.9 </td>
   <td style="text-align:right;"> 78.1 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 70.4 </td>
   <td style="text-align:right;"> 78.5 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 100.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 74.6 </td>
   <td style="text-align:right;"> 78.9 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 97.5 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 72.9 </td>
   <td style="text-align:right;"> 75.2 </td>
   <td style="text-align:right;"> 16.3 </td>
   <td style="text-align:right;"> 18.1 </td>
   <td style="text-align:right;"> 96.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 69.8 </td>
   <td style="text-align:right;"> 70.9 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 98.6 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 67.9 </td>
   <td style="text-align:right;"> 71.7 </td>
   <td style="text-align:right;"> 20.4 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 97.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 73.7 </td>
   <td style="text-align:right;"> 72.9 </td>
   <td style="text-align:right;"> 16.3 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 97.1 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 72.4 </td>
   <td style="text-align:right;"> 79.2 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 96.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 71.1 </td>
   <td style="text-align:right;"> 70.6 </td>
   <td style="text-align:right;"> 20.2 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 96.3 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 70.2 </td>
   <td style="text-align:right;"> 76.4 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 13.5 </td>
   <td style="text-align:right;"> 94.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 65.9 </td>
   <td style="text-align:right;"> 69.2 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 97.7 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 64.0 </td>
   <td style="text-align:right;"> 66.6 </td>
   <td style="text-align:right;"> 22.7 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> 99.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 74.3 </td>
   <td style="text-align:right;"> 81.9 </td>
   <td style="text-align:right;"> 23.3 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:right;"> 98.0 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 71.7 </td>
   <td style="text-align:right;"> 78.8 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 14.4 </td>
   <td style="text-align:right;"> 97.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 80.7 </td>
   <td style="text-align:right;"> 83.1 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> 40.2 </td>
   <td style="text-align:right;"> 97.3 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 76.1 </td>
   <td style="text-align:right;"> 78.5 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> 36.8 </td>
   <td style="text-align:right;"> 95.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 66.9 </td>
   <td style="text-align:right;"> 74.4 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 94.1 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 66.1 </td>
   <td style="text-align:right;"> 69.8 </td>
   <td style="text-align:right;"> 21.8 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 95.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 72.9 </td>
   <td style="text-align:right;"> 76.3 </td>
   <td style="text-align:right;"> 17.3 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 98.0 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 71.9 </td>
   <td style="text-align:right;"> 73.3 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:right;"> 97.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 63.3 </td>
   <td style="text-align:right;"> 64.7 </td>
   <td style="text-align:right;"> 16.3 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 88.0 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 64.4 </td>
   <td style="text-align:right;"> 64.7 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:right;"> 12.9 </td>
   <td style="text-align:right;"> 95.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 58.6 </td>
   <td style="text-align:right;"> 62.5 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:right;"> 12.5 </td>
   <td style="text-align:right;"> 96.6 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 60.2 </td>
   <td style="text-align:right;"> 66.7 </td>
   <td style="text-align:right;"> 24.5 </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> 96.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 53.5 </td>
   <td style="text-align:right;"> 55.0 </td>
   <td style="text-align:right;"> 22.8 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 92.5 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 56.3 </td>
   <td style="text-align:right;"> 56.2 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 91.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 57.6 </td>
   <td style="text-align:right;"> 55.3 </td>
   <td style="text-align:right;"> 24.0 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 58.4 </td>
   <td style="text-align:right;"> 62.3 </td>
   <td style="text-align:right;"> 22.3 </td>
   <td style="text-align:right;"> 10.1 </td>
   <td style="text-align:right;"> 100.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 53.4 </td>
   <td style="text-align:right;"> 50.7 </td>
   <td style="text-align:right;"> 27.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 99.6 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 55.7 </td>
   <td style="text-align:right;"> 54.8 </td>
   <td style="text-align:right;"> 25.7 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 100.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 67.6 </td>
   <td style="text-align:right;"> 74.0 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:right;"> 97.6 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 66.4 </td>
   <td style="text-align:right;"> 70.9 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 93.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 70.2 </td>
   <td style="text-align:right;"> 70.9 </td>
   <td style="text-align:right;"> 20.7 </td>
   <td style="text-align:right;"> 15.5 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 69.9 </td>
   <td style="text-align:right;"> 72.7 </td>
   <td style="text-align:right;"> 18.9 </td>
   <td style="text-align:right;"> 17.4 </td>
   <td style="text-align:right;"> 98.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 66.9 </td>
   <td style="text-align:right;"> 68.4 </td>
   <td style="text-align:right;"> 18.5 </td>
   <td style="text-align:right;"> 16.7 </td>
   <td style="text-align:right;"> 96.7 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 70.1 </td>
   <td style="text-align:right;"> 71.9 </td>
   <td style="text-align:right;"> 16.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 97.6 </td>
  </tr>
</tbody>
</table>

## Age 30-44 years (%)

2006: `v_CA06_2: Total population by sex and age groups - 100% data`, etc.  
2016: `v_CA16_1: Age Stats`, etc.


```r
agg_stats <- compute_stats_by_CMA(age_30_44yrs, age_30_44yrs.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 18.8 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 19.4 </td>
   <td style="text-align:right;"> 18.8 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 27.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 12.6 </td>
   <td style="text-align:right;"> 36.6 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 24.4 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 10.1 </td>
   <td style="text-align:right;"> 37.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 22.1 </td>
   <td style="text-align:right;"> 22.1 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 13.6 </td>
   <td style="text-align:right;"> 31.6 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 22.9 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 13.3 </td>
   <td style="text-align:right;"> 39.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 19.0 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 10.4 </td>
   <td style="text-align:right;"> 26.1 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 23.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 31.7 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 23.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:right;"> 22.8 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 39.5 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 41.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 19.7 </td>
   <td style="text-align:right;"> 20.3 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 34.8 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 18.7 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 31.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 36.2 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 20.3 </td>
   <td style="text-align:right;"> 20.4 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 31.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 22.6 </td>
   <td style="text-align:right;"> 22.4 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 29.6 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 14.6 </td>
   <td style="text-align:right;"> 26.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 8.7 </td>
   <td style="text-align:right;"> 25.9 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 24.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 12.9 </td>
   <td style="text-align:right;"> 33.8 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 28.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 22.6 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 32.2 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 30.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 22.9 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 36.7 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 18.8 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 8.5 </td>
   <td style="text-align:right;"> 37.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 20.7 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 46.2 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 30.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 19.9 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 11.1 </td>
   <td style="text-align:right;"> 36.4 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 17.1 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 30.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 13.5 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 16.2 </td>
   <td style="text-align:right;"> 15.9 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:right;"> 20.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 23.6 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 35.1 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 19.7 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 30.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 24.2 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 46.1 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 48.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 14.1 </td>
   <td style="text-align:right;"> 41.0 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 18.8 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 8.7 </td>
   <td style="text-align:right;"> 32.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 19.8 </td>
   <td style="text-align:right;"> 19.7 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 25.9 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 16.0 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 25.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 22.4 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 34.4 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 20.2 </td>
   <td style="text-align:right;"> 19.7 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 29.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 15.3 </td>
   <td style="text-align:right;"> 24.4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 18.5 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 11.6 </td>
   <td style="text-align:right;"> 26.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 22.3 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 10.2 </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 19.9 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 12.7 </td>
   <td style="text-align:right;"> 27.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 11.7 </td>
   <td style="text-align:right;"> 28.7 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 18.7 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 27.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 22.7 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 15.4 </td>
   <td style="text-align:right;"> 28.8 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 12.4 </td>
   <td style="text-align:right;"> 23.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 24.2 </td>
   <td style="text-align:right;"> 22.9 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 13.3 </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 27.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 21.2 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 13.9 </td>
   <td style="text-align:right;"> 25.5 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 18.3 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 21.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 19.9 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 16.1 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 17.0 </td>
   <td style="text-align:right;"> 16.8 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 12.9 </td>
   <td style="text-align:right;"> 20.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> 19.0 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 13.3 </td>
   <td style="text-align:right;"> 26.4 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 11.8 </td>
   <td style="text-align:right;"> 23.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 20.3 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 33.7 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 32.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 19.4 </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 12.5 </td>
   <td style="text-align:right;"> 26.9 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 18.1 </td>
   <td style="text-align:right;"> 18.1 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 11.0 </td>
   <td style="text-align:right;"> 26.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> 18.8 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 16.9 </td>
   <td style="text-align:right;"> 17.0 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 10.7 </td>
   <td style="text-align:right;"> 33.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 22.3 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 37.6 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 20.4 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 39.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 23.2 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 15.0 </td>
   <td style="text-align:right;"> 30.7 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 33.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 13.6 </td>
   <td style="text-align:right;"> 28.3 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 21.7 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 29.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 20.4 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 12.2 </td>
   <td style="text-align:right;"> 31.2 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 21.7 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 11.1 </td>
   <td style="text-align:right;"> 28.2 </td>
  </tr>
</tbody>
</table>

## Professional occupation (%)

2006: `v_CA06_1021: 54 Professional, scientific and technical services`  
2016: `v_CA16_5735: 54 Professional, scientific and technical services`


```r
agg_stats <- compute_stats_by_CMA(prof_occup, prof_occup.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 7.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 11.5 </td>
   <td style="text-align:right;"> 11.6 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 10.4 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 19.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 28.6 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 15.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 11.6 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 10.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 40.0 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 8.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 23.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 15.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 12.1 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 13.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 7.5 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 7.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 13.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 14.1 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 15.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 6.1 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 7.5 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 18.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 10.7 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 20.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 12.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 10.9 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 6.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 9.4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 7.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 0.9 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 20.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 28.7 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 27.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 13.5 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 13.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 12.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 16.2 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 18.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 13.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 11.3 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 11.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 14.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 10.7 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 10.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 10.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 8.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 16.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 10.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 31.6 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 8.7 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 25.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 12.7 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 12.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 9.9 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 5.5 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 12.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 13.7 </td>
  </tr>
</tbody>
</table>

## Low income cut-off (%)

2006: `v_CA06_1981: Prevalence of low income after tax in 2005 %`  
2016: `v_CA16_2570: Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`


```r
agg_stats <- compute_stats_by_CMA(low_income, low_income.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 8.5 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 24.0 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 19.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 52.4 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 24.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 38.2 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 35.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 16.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 9.9 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 31.8 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 3.7 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 17.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:right;"> 15.7 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 67.9 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 13.9 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 65.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 30.1 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 23.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 14.6 </td>
   <td style="text-align:right;"> 12.2 </td>
   <td style="text-align:right;"> 12.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 64.0 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 53.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 28.5 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 27.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 10.7 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 47.0 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 8.5 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 42.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 12.1 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 8.7 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 39.4 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 7.5 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 32.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 37.7 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 36.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 56.9 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 9.3 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 46.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 9.4 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 45.1 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 49.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 9.4 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 25.2 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 22.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 37.9 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 7.3 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 36.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 0.9 </td>
   <td style="text-align:right;"> 26.5 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 26.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 14.3 </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 61.9 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 12.8 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 52.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 9.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 48.0 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 10.4 </td>
   <td style="text-align:right;"> 7.5 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 47.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 29.8 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 28.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 48.8 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 66.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 34.8 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 32.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 29.4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 18.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 28.6 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 40.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 6.1 </td>
   <td style="text-align:right;"> 9.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 42.0 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 53.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 20.3 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> 19.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 9.4 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 37.5 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 29.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 44.1 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 6.2 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 29.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 9.9 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 34.1 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 5.2 </td>
   <td style="text-align:right;"> 0.9 </td>
   <td style="text-align:right;"> 27.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 48.0 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 35.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 11.8 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 41.5 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 8.7 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 43.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 12.7 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 40.7 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 7.5 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 32.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 16.0 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 85.4 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 9.4 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 58.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 10.7 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 44.5 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 34.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 7.3 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 44.5 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 22.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 12.2 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 8.5 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 44.8 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 18.3 </td>
  </tr>
</tbody>
</table>

## University degree (%)

2006: `v_CA06_1234: Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data`, `v_CA06_1240: University certificate, diploma or degree`, etc.  
2016: `v_CA16_5078: University certificate, diploma or degree at bachelor level or above`


```r
agg_stats <- compute_stats_by_CMA(university_degree, university_degree.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 15.6 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 32.7 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 18.7 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 35.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 29.7 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 63.8 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 32.1 </td>
   <td style="text-align:right;"> 30.7 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 68.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 11.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 61.2 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 24.2 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 12.1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 61.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 6.1 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 31.6 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 16.8 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 31.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 15.5 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 27.4 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 15.2 </td>
   <td style="text-align:right;"> 13.9 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 28.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 31.0 </td>
   <td style="text-align:right;"> 28.9 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 71.0 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 30.6 </td>
   <td style="text-align:right;"> 29.3 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 58.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 28.6 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 55.4 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 29.2 </td>
   <td style="text-align:right;"> 28.4 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 56.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 23.2 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 56.2 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 25.3 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 10.5 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 54.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 19.4 </td>
   <td style="text-align:right;"> 18.5 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 4.0 </td>
   <td style="text-align:right;"> 35.6 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 33.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 46.7 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 18.7 </td>
   <td style="text-align:right;"> 15.7 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 50.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:right;"> 20.2 </td>
   <td style="text-align:right;"> 9.7 </td>
   <td style="text-align:right;"> 6.1 </td>
   <td style="text-align:right;"> 57.9 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 24.3 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 10.2 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 61.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 28.4 </td>
   <td style="text-align:right;"> 26.7 </td>
   <td style="text-align:right;"> 12.8 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:right;"> 72.2 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 28.8 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 66.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 35.8 </td>
   <td style="text-align:right;"> 33.6 </td>
   <td style="text-align:right;"> 12.6 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 73.0 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 36.5 </td>
   <td style="text-align:right;"> 35.8 </td>
   <td style="text-align:right;"> 12.4 </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> 72.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 12.5 </td>
   <td style="text-align:right;"> 6.5 </td>
   <td style="text-align:right;"> 65.5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 25.2 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 11.7 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 61.9 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 12.7 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 27.2 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 11.9 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 26.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 17.1 </td>
   <td style="text-align:right;"> 15.1 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:right;"> 29.2 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 19.3 </td>
   <td style="text-align:right;"> 18.2 </td>
   <td style="text-align:right;"> 6.0 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 33.8 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 16.1 </td>
   <td style="text-align:right;"> 15.5 </td>
   <td style="text-align:right;"> 6.9 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 32.8 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 32.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 32.4 </td>
   <td style="text-align:right;"> 30.1 </td>
   <td style="text-align:right;"> 12.9 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 72.1 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 33.3 </td>
   <td style="text-align:right;"> 31.3 </td>
   <td style="text-align:right;"> 13.7 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 74.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 19.0 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 54.2 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 21.7 </td>
   <td style="text-align:right;"> 10.7 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 55.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 15.9 </td>
   <td style="text-align:right;"> 14.2 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 35.6 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 16.8 </td>
   <td style="text-align:right;"> 15.0 </td>
   <td style="text-align:right;"> 7.5 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 40.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 55.6 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 23.6 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 10.9 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 56.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 15.9 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> 36.5 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 14.7 </td>
   <td style="text-align:right;"> 5.7 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 40.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 27.4 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 9.5 </td>
   <td style="text-align:right;"> 14.1 </td>
   <td style="text-align:right;"> 45.2 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 28.6 </td>
   <td style="text-align:right;"> 26.0 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 46.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 17.7 </td>
   <td style="text-align:right;"> 12.1 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 55.1 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 22.9 </td>
   <td style="text-align:right;"> 19.0 </td>
   <td style="text-align:right;"> 12.7 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 59.5 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 20.7 </td>
   <td style="text-align:right;"> 7.9 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 38.4 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 4.9 </td>
   <td style="text-align:right;"> 50.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 16.0 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 27.5 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 5.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 29.7 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 15.4 </td>
   <td style="text-align:right;"> 13.2 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 36.9 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 16.9 </td>
   <td style="text-align:right;"> 14.5 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 4.5 </td>
   <td style="text-align:right;"> 39.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 17.8 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 37.4 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 19.5 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 7.2 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 37.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 17.1 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 8.4 </td>
   <td style="text-align:right;"> 38.8 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 15.4 </td>
   <td style="text-align:right;"> 13.9 </td>
   <td style="text-align:right;"> 5.8 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 31.2 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 63.0 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 11.6 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 61.4 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 19.1 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 42.7 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 6.7 </td>
   <td style="text-align:right;"> 38.0 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 18.3 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 37.5 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> 16.8 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 34.3 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 26.6 </td>
   <td style="text-align:right;"> 22.6 </td>
   <td style="text-align:right;"> 13.8 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 71.2 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 25.5 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 68.6 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 24.1 </td>
   <td style="text-align:right;"> 20.9 </td>
   <td style="text-align:right;"> 11.9 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 50.2 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:right;"> 12.1 </td>
   <td style="text-align:right;"> 4.3 </td>
   <td style="text-align:right;"> 50.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:right;"> 4.6 </td>
   <td style="text-align:right;"> 45.5 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 24.7 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 43.1 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:right;"> 23.6 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 51.5 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 25.9 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 9.9 </td>
   <td style="text-align:right;"> 6.6 </td>
   <td style="text-align:right;"> 53.4 </td>
  </tr>
</tbody>
</table>

## Rental housing price ($)

2006: `v_CA06_2050: Average gross rent $`  
2016: `v_CA16_4900: Median monthly shelter costs for rented dwellings ($)`


```r
agg_stats <- compute_stats_by_CMA(rental_housing_price, rental_housing_price.pop)
knitr::kable(agg_stats,
           col.names = c("Province", "CMA Id", "Name", "Population",
                         "N", "Mean", "Median", "Std", "Min", "Max",
                         "N", "Mean", "Median", "Std", "Min", "Max")) %>%
kable_styling("striped") %>%
add_header_above(c(" " = 4, "2006" = 6, "2016" = 6)) %>%
collapse_rows(columns = 1, valign = "top")
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="4"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2006</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">2016</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> Province </th>
   <th style="text-align:left;"> CMA Id </th>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Population </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Std </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="3"> Alberta </td>
   <td style="text-align:left;"> 48810 </td>
   <td style="text-align:left;"> Lethbridge </td>
   <td style="text-align:right;"> 117394 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 688.1 </td>
   <td style="text-align:right;"> 692 </td>
   <td style="text-align:right;"> 95.5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 912 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 1030.2 </td>
   <td style="text-align:right;"> 1002 </td>
   <td style="text-align:right;"> 170.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1827 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48825 </td>
   <td style="text-align:left;"> Calgary </td>
   <td style="text-align:right;"> 1392609 </td>
   <td style="text-align:right;"> 203 </td>
   <td style="text-align:right;"> 873.4 </td>
   <td style="text-align:right;"> 840 </td>
   <td style="text-align:right;"> 163.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2037 </td>
   <td style="text-align:right;"> 253 </td>
   <td style="text-align:right;"> 1340.4 </td>
   <td style="text-align:right;"> 1300 </td>
   <td style="text-align:right;"> 240.5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2625 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 48835 </td>
   <td style="text-align:left;"> Edmonton </td>
   <td style="text-align:right;"> 1321426 </td>
   <td style="text-align:right;"> 229 </td>
   <td style="text-align:right;"> 773.9 </td>
   <td style="text-align:right;"> 749 </td>
   <td style="text-align:right;"> 123.9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1480 </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 1263.3 </td>
   <td style="text-align:right;"> 1244 </td>
   <td style="text-align:right;"> 225.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2496 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="4"> British Columbia </td>
   <td style="text-align:left;"> 59915 </td>
   <td style="text-align:left;"> Kelowna </td>
   <td style="text-align:right;"> 194882 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 881.4 </td>
   <td style="text-align:right;"> 876 </td>
   <td style="text-align:right;"> 134.9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1223 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 1143.4 </td>
   <td style="text-align:right;"> 1146 </td>
   <td style="text-align:right;"> 168.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1724 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59932 </td>
   <td style="text-align:left;"> Abbotsford - Mission </td>
   <td style="text-align:right;"> 180518 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 766.8 </td>
   <td style="text-align:right;"> 733 </td>
   <td style="text-align:right;"> 76.8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1040 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 895.8 </td>
   <td style="text-align:right;"> 867 </td>
   <td style="text-align:right;"> 124.9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1460 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59933 </td>
   <td style="text-align:left;"> Vancouver </td>
   <td style="text-align:right;"> 2463431 </td>
   <td style="text-align:right;"> 410 </td>
   <td style="text-align:right;"> 892.7 </td>
   <td style="text-align:right;"> 868 </td>
   <td style="text-align:right;"> 201.8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2276 </td>
   <td style="text-align:right;"> 478 </td>
   <td style="text-align:right;"> 1160.6 </td>
   <td style="text-align:right;"> 1128 </td>
   <td style="text-align:right;"> 289.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3790 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 59935 </td>
   <td style="text-align:left;"> Victoria </td>
   <td style="text-align:right;"> 367770 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 821.8 </td>
   <td style="text-align:right;"> 803 </td>
   <td style="text-align:right;"> 127.7 </td>
   <td style="text-align:right;"> 644 </td>
   <td style="text-align:right;"> 1361 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 1035.6 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 135.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Manitoba </td>
   <td style="text-align:left;"> 46602 </td>
   <td style="text-align:left;"> Winnipeg </td>
   <td style="text-align:right;"> 778489 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> 616.4 </td>
   <td style="text-align:right;"> 602 </td>
   <td style="text-align:right;"> 121.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1295 </td>
   <td style="text-align:right;"> 174 </td>
   <td style="text-align:right;"> 912.8 </td>
   <td style="text-align:right;"> 893 </td>
   <td style="text-align:right;"> 194.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2121 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> New Brunswick </td>
   <td style="text-align:left;"> 13305 </td>
   <td style="text-align:left;"> Moncton </td>
   <td style="text-align:right;"> 144810 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 659.4 </td>
   <td style="text-align:right;"> 665 </td>
   <td style="text-align:right;"> 87.1 </td>
   <td style="text-align:right;"> 433 </td>
   <td style="text-align:right;"> 1096 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 810.5 </td>
   <td style="text-align:right;"> 802 </td>
   <td style="text-align:right;"> 106.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1114 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 13310 </td>
   <td style="text-align:left;"> Saint John </td>
   <td style="text-align:right;"> 126202 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 553.7 </td>
   <td style="text-align:right;"> 564 </td>
   <td style="text-align:right;"> 55.8 </td>
   <td style="text-align:right;"> 397 </td>
   <td style="text-align:right;"> 793 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 704.6 </td>
   <td style="text-align:right;"> 711 </td>
   <td style="text-align:right;"> 93.9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1137 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Newfoundland and Labrador </td>
   <td style="text-align:left;"> 10001 </td>
   <td style="text-align:left;"> St. John's </td>
   <td style="text-align:right;"> 205955 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 614.8 </td>
   <td style="text-align:right;"> 603 </td>
   <td style="text-align:right;"> 79.1 </td>
   <td style="text-align:right;"> 465 </td>
   <td style="text-align:right;"> 990 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 892.8 </td>
   <td style="text-align:right;"> 876 </td>
   <td style="text-align:right;"> 102.0 </td>
   <td style="text-align:right;"> 576 </td>
   <td style="text-align:right;"> 1127 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nova Scotia </td>
   <td style="text-align:left;"> 12205 </td>
   <td style="text-align:left;"> Halifax </td>
   <td style="text-align:right;"> 403390 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 754.4 </td>
   <td style="text-align:right;"> 730 </td>
   <td style="text-align:right;"> 134.0 </td>
   <td style="text-align:right;"> 504 </td>
   <td style="text-align:right;"> 1130 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 992.1 </td>
   <td style="text-align:right;"> 982 </td>
   <td style="text-align:right;"> 187.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1494 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="16"> Ontario </td>
   <td style="text-align:left;"> 35505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 991726 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 871.9 </td>
   <td style="text-align:right;"> 870 </td>
   <td style="text-align:right;"> 162.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1973 </td>
   <td style="text-align:right;"> 204 </td>
   <td style="text-align:right;"> 1138.7 </td>
   <td style="text-align:right;"> 1091 </td>
   <td style="text-align:right;"> 222.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1849 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35521 </td>
   <td style="text-align:left;"> Kingston </td>
   <td style="text-align:right;"> 161175 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 772.7 </td>
   <td style="text-align:right;"> 743 </td>
   <td style="text-align:right;"> 120.8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1367 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 1030.1 </td>
   <td style="text-align:right;"> 974 </td>
   <td style="text-align:right;"> 211.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1639 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35522 </td>
   <td style="text-align:left;"> Belleville </td>
   <td style="text-align:right;"> 103472 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 696.0 </td>
   <td style="text-align:right;"> 690 </td>
   <td style="text-align:right;"> 59.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 826 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 923.2 </td>
   <td style="text-align:right;"> 933 </td>
   <td style="text-align:right;"> 94.2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1768 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35529 </td>
   <td style="text-align:left;"> Peterborough </td>
   <td style="text-align:right;"> 121721 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 753.7 </td>
   <td style="text-align:right;"> 727 </td>
   <td style="text-align:right;"> 109.5 </td>
   <td style="text-align:right;"> 578 </td>
   <td style="text-align:right;"> 1540 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 939.0 </td>
   <td style="text-align:right;"> 917 </td>
   <td style="text-align:right;"> 96.8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1367 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35532 </td>
   <td style="text-align:left;"> Oshawa </td>
   <td style="text-align:right;"> 379848 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 853.9 </td>
   <td style="text-align:right;"> 815 </td>
   <td style="text-align:right;"> 184.1 </td>
   <td style="text-align:right;"> 559 </td>
   <td style="text-align:right;"> 2023 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 1112.9 </td>
   <td style="text-align:right;"> 1033 </td>
   <td style="text-align:right;"> 270.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2363 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35535 </td>
   <td style="text-align:left;"> Toronto </td>
   <td style="text-align:right;"> 5928040 </td>
   <td style="text-align:right;"> 1003 </td>
   <td style="text-align:right;"> 949.2 </td>
   <td style="text-align:right;"> 927 </td>
   <td style="text-align:right;"> 193.5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2601 </td>
   <td style="text-align:right;"> 1151 </td>
   <td style="text-align:right;"> 1240.0 </td>
   <td style="text-align:right;"> 1203 </td>
   <td style="text-align:right;"> 302.5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3971 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35537 </td>
   <td style="text-align:left;"> Hamilton </td>
   <td style="text-align:right;"> 747545 </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 766.6 </td>
   <td style="text-align:right;"> 733 </td>
   <td style="text-align:right;"> 147.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1804 </td>
   <td style="text-align:right;"> 191 </td>
   <td style="text-align:right;"> 999.8 </td>
   <td style="text-align:right;"> 918 </td>
   <td style="text-align:right;"> 254.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2097 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35539 </td>
   <td style="text-align:left;"> St. Catharines - Niagara </td>
   <td style="text-align:right;"> 406074 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 718.3 </td>
   <td style="text-align:right;"> 713 </td>
   <td style="text-align:right;"> 92.0 </td>
   <td style="text-align:right;"> 563 </td>
   <td style="text-align:right;"> 1158 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 878.4 </td>
   <td style="text-align:right;"> 853 </td>
   <td style="text-align:right;"> 133.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1854 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35541 </td>
   <td style="text-align:left;"> Kitchener - Cambridge - Waterloo </td>
   <td style="text-align:right;"> 523894 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 789.3 </td>
   <td style="text-align:right;"> 756 </td>
   <td style="text-align:right;"> 106.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1388 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 1025.4 </td>
   <td style="text-align:right;"> 969 </td>
   <td style="text-align:right;"> 191.5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1793 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35543 </td>
   <td style="text-align:left;"> Brantford </td>
   <td style="text-align:right;"> 134203 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 715.1 </td>
   <td style="text-align:right;"> 708 </td>
   <td style="text-align:right;"> 91.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1164 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 905.1 </td>
   <td style="text-align:right;"> 874 </td>
   <td style="text-align:right;"> 156.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1596 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35550 </td>
   <td style="text-align:left;"> Guelph </td>
   <td style="text-align:right;"> 151984 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 807.0 </td>
   <td style="text-align:right;"> 784 </td>
   <td style="text-align:right;"> 93.7 </td>
   <td style="text-align:right;"> 578 </td>
   <td style="text-align:right;"> 1113 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 1046.8 </td>
   <td style="text-align:right;"> 1001 </td>
   <td style="text-align:right;"> 162.5 </td>
   <td style="text-align:right;"> 782 </td>
   <td style="text-align:right;"> 1674 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35555 </td>
   <td style="text-align:left;"> London </td>
   <td style="text-align:right;"> 494069 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 726.8 </td>
   <td style="text-align:right;"> 717 </td>
   <td style="text-align:right;"> 101.7 </td>
   <td style="text-align:right;"> 411 </td>
   <td style="text-align:right;"> 1185 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 894.6 </td>
   <td style="text-align:right;"> 857 </td>
   <td style="text-align:right;"> 176.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1749 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35559 </td>
   <td style="text-align:left;"> Windsor </td>
   <td style="text-align:right;"> 329144 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 706.2 </td>
   <td style="text-align:right;"> 692 </td>
   <td style="text-align:right;"> 121.8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1258 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 786.5 </td>
   <td style="text-align:right;"> 766 </td>
   <td style="text-align:right;"> 168.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2022 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35568 </td>
   <td style="text-align:left;"> Barrie </td>
   <td style="text-align:right;"> 197059 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 915.7 </td>
   <td style="text-align:right;"> 919 </td>
   <td style="text-align:right;"> 147.2 </td>
   <td style="text-align:right;"> 667 </td>
   <td style="text-align:right;"> 1267 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 1242.0 </td>
   <td style="text-align:right;"> 1206 </td>
   <td style="text-align:right;"> 221.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1711 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35580 </td>
   <td style="text-align:left;"> Greater Sudbury </td>
   <td style="text-align:right;"> 164689 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 626.4 </td>
   <td style="text-align:right;"> 632 </td>
   <td style="text-align:right;"> 70.3 </td>
   <td style="text-align:right;"> 502 </td>
   <td style="text-align:right;"> 841 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 892.4 </td>
   <td style="text-align:right;"> 885 </td>
   <td style="text-align:right;"> 132.2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1603 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 35595 </td>
   <td style="text-align:left;"> Thunder Bay </td>
   <td style="text-align:right;"> 121621 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 625.7 </td>
   <td style="text-align:right;"> 609 </td>
   <td style="text-align:right;"> 83.2 </td>
   <td style="text-align:right;"> 362 </td>
   <td style="text-align:right;"> 964 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 824.9 </td>
   <td style="text-align:right;"> 827 </td>
   <td style="text-align:right;"> 116.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1060 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="6"> Quebec </td>
   <td style="text-align:left;"> 24408 </td>
   <td style="text-align:left;"> Saguenay </td>
   <td style="text-align:right;"> 160980 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 506.0 </td>
   <td style="text-align:right;"> 480 </td>
   <td style="text-align:right;"> 72.3 </td>
   <td style="text-align:right;"> 352 </td>
   <td style="text-align:right;"> 701 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 587.7 </td>
   <td style="text-align:right;"> 568 </td>
   <td style="text-align:right;"> 58.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 716 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24421 </td>
   <td style="text-align:left;"> Québec </td>
   <td style="text-align:right;"> 800296 </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 602.1 </td>
   <td style="text-align:right;"> 588 </td>
   <td style="text-align:right;"> 95.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 899 </td>
   <td style="text-align:right;"> 181 </td>
   <td style="text-align:right;"> 761.5 </td>
   <td style="text-align:right;"> 747 </td>
   <td style="text-align:right;"> 117.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1696 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24433 </td>
   <td style="text-align:left;"> Sherbrooke </td>
   <td style="text-align:right;"> 212105 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 550.0 </td>
   <td style="text-align:right;"> 550 </td>
   <td style="text-align:right;"> 58.3 </td>
   <td style="text-align:right;"> 443 </td>
   <td style="text-align:right;"> 708 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 639.1 </td>
   <td style="text-align:right;"> 605 </td>
   <td style="text-align:right;"> 77.1 </td>
   <td style="text-align:right;"> 501 </td>
   <td style="text-align:right;"> 826 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24442 </td>
   <td style="text-align:left;"> Trois-Rivières </td>
   <td style="text-align:right;"> 156042 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 508.3 </td>
   <td style="text-align:right;"> 492 </td>
   <td style="text-align:right;"> 64.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 696 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 587.4 </td>
   <td style="text-align:right;"> 581 </td>
   <td style="text-align:right;"> 73.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 809 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24462 </td>
   <td style="text-align:left;"> Montréal </td>
   <td style="text-align:right;"> 4098927 </td>
   <td style="text-align:right;"> 878 </td>
   <td style="text-align:right;"> 656.2 </td>
   <td style="text-align:right;"> 622 </td>
   <td style="text-align:right;"> 122.9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2177 </td>
   <td style="text-align:right;"> 970 </td>
   <td style="text-align:right;"> 798.3 </td>
   <td style="text-align:right;"> 762 </td>
   <td style="text-align:right;"> 145.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2250 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 24505 </td>
   <td style="text-align:left;"> Ottawa - Gatineau </td>
   <td style="text-align:right;"> 332057 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 654.0 </td>
   <td style="text-align:right;"> 640 </td>
   <td style="text-align:right;"> 84.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 915 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 807.7 </td>
   <td style="text-align:right;"> 769 </td>
   <td style="text-align:right;"> 130.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1430 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: top !important;" rowspan="2"> Saskatchewan </td>
   <td style="text-align:left;"> 47705 </td>
   <td style="text-align:left;"> Regina </td>
   <td style="text-align:right;"> 236481 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 655.1 </td>
   <td style="text-align:right;"> 630 </td>
   <td style="text-align:right;"> 114.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1122 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 1109.2 </td>
   <td style="text-align:right;"> 1104 </td>
   <td style="text-align:right;"> 189.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2499 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> 47725 </td>
   <td style="text-align:left;"> Saskatoon </td>
   <td style="text-align:right;"> 295095 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 658.4 </td>
   <td style="text-align:right;"> 652 </td>
   <td style="text-align:right;"> 81.7 </td>
   <td style="text-align:right;"> 324 </td>
   <td style="text-align:right;"> 1048 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 1086.7 </td>
   <td style="text-align:right;"> 1095 </td>
   <td style="text-align:right;"> 175.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1702 </td>
  </tr>
</tbody>
</table>
