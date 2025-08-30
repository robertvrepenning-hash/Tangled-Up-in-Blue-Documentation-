
library(writexl)
library(readxl)
library(tidyverse)

chalfin_dat <- read_xlsx("Final_Data.xlsx") |>  as_tibble() |> filter(year >= 1995)

weighted_years <- read_csv("post-Chalfin - only the population weighting factors with ori.csv") |> as_tibble() |> filter(year >= 1990)

groupings <- read_csv("post-Chalfin - only the groupings of cities for different coefficients by group.csv")

chalfin_dat <- chalfin_dat |> left_join(weighted_years |> select(ori, year, w1980factor, w2018factor), by = c("ori", "year"))

chalfin_dat <- chalfin_dat |> left_join(groupings, by = "xid")

chalfin_dat <- chalfin_dat |> mutate(
  Exp_Per_Capita = totalexpenditure / population_census,
  pop100k = population_census / 100000)

chalfin_dat <- chalfin_dat |>
  mutate(year = as.numeric(year)) |>          
  arrange(ori, year) |>                       
  group_by(ori) |>                            
  mutate(
    S_d = S - dplyr::lag(S),
    S_pc = S / pop100k,
    S_pc_sq = S_pc^2,
    S_pc_sq_d = S_pc_sq - dplyr::lag(S_pc_sq),
    S_pc_d = S_pc - dplyr::lag(S_pc),
    S_sq = S^2,
    S_sq_d = S_sq - dplyr::lag(S_sq),
    copseligible_hiring_pc = copseligible_hiring / pop100k,
    copseligible_hiring_pc_lag = dplyr::lag(copseligible_hiring_pc),
    copseligible_hiring_lag = dplyr::lag(copseligible_hiring),
    award_nonhiring_pc = award_nonhiring / pop100k,
    award_nonhiring_lag = dplyr::lag(award_nonhiring),
    award_nonhiring_pc_lag = dplyr::lag(award_nonhiring_pc),
    apply_hiring_lag = dplyr::lag(apply_hiring),
    apply_nonhiring_lag = dplyr::lag(apply_nonhiring)) |>
  ungroup()

chalfin_dat <- chalfin_dat |>
  mutate(year = as.numeric(year)) |>          
  arrange(ori, year) |>                       
  group_by(ori) |>                            
  mutate(
    murder_total_d = murder_tot_total - dplyr::lag(murder_tot_total),
    murder_total_pc = murder_tot_total / pop100k,
    murder_total_pc_d = murder_total_pc - dplyr::lag(murder_total_pc),
    clearance_total_d = clearance - dplyr::lag(clearance),
    clearance_total_pc = clearance / pop100k,
    clearance_total_pc_d = clearance_total_pc - dplyr::lag(clearance_total_pc),
    iarrest_total_d = iarrest_tot_total - dplyr::lag(iarrest_tot_total),
    iarrest_total_pc = iarrest_tot_total / pop100k,
    iarrest_total_pc_d = iarrest_total_pc / dplyr::lag(iarrest_total_pc),
    niarrest_total_d = niarrest_tot_total - dplyr::lag(niarrest_tot_total),
    niarrest_total_pc = niarrest_tot_total / pop100k,
    niarrest_total_pc_d = niarrest_total_pc - dplyr::lag(niarrest_total_pc)
  ) |>
  ungroup()

chalfin_dat <- chalfin_dat |>
  mutate(year = as.numeric(year)) |>          
  arrange(ori, year) |>                       
  group_by(ori) |>                            
  mutate(
      S_lag = dplyr::lag(S),
      S_pop = S_lag * pop100k,
      S_black_pct = S_lag * NH_Black_Pct,
      S_d_pop = S_d * pop100k,
      S_d_black_pct = S_d * NH_Black_Pct,
      S_lag_pc = S_lag / pop100k,
      S_pc_pop = S_lag_pc * pop100k,
      S_pc_black_pct = S_lag_pc * NH_Black_Pct
    ) |>
  ungroup()

write_xlsx(chalfin_dat, "Tangled_Up_In_Blue_Data.xlsx")


