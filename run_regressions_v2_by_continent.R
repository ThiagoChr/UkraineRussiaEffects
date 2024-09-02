library(tidyverse)
library(data.table)
library(fixest)
library(lubridate)
library(ggtext)
source("plot_utils.R")

# Russia invaded Ukraine on 24 February 2022
RUSSIA_INVASION_DATE = ymd(20220224)

UKRAINE_2DIGIT_ISOCODE = "UA"
RUSSIA_2DIGIT_ISOCODE = "RU"
BELARUS_2DIGIT_ISOCODE = "BY"

WAR_PARTICIPANTS = c(UKRAINE_2DIGIT_ISOCODE, RUSSIA_2DIGIT_ISOCODE, BELARUS_2DIGIT_ISOCODE)
# WAR_PARTICIPANTS = c(UKRAINE_2DIGIT_ISOCODE)

iso_country_name =
  fread("./data/iso_country_codes.csv")

geo =
  fread("./data/geo.csv") %>%
  mutate(discrete_dist_to_ukr = ntile(dist_to_ukr, 3))

exchange_rate =
  fread("./data/fx.csv") %>%
  filter(iso2 != "US") # anchored in the US

weo =
  fread("./data/weo.csv")

stock_index =
  fread("./data/stock_indexes_adj.csv")

sovereign_bonds =
  fread("./data/sb_adj.csv")

military =
  fread("./data/gfp.csv")

imports =
  fread("./data/imp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  rename(iso2 = origin) %>%
  mutate(date = ymd(sprintf("%s-01", date))) %>%
  group_by(., date, iso2) %>%
  summarize(
    total_imports = sum(imp_value, na.rm = T),
    total_imports_from_war = sum(ifelse(destiny %in% WAR_PARTICIPANTS, imp_value, 0.0), na.rm = T)
  ) %>%
  ungroup() %>%
  bind_rows(
    fread("./data/exp_seg.csv") %>%
      mutate(
        across(
          .cols = ends_with("_value"),
          .fns = ~ coalesce(., 0)
        )
      ) %>%
      rename(iso2 = destiny) %>%
      mutate(date = ymd(sprintf("%s-01", date))) %>%
      group_by(., date, iso2) %>%
      summarize(
        total_imports  = sum(exp_value, na.rm = T),
        total_imports_from_war = sum(ifelse(origin %in% WAR_PARTICIPANTS, exp_value, 0.0), na.rm = T)
      ) %>%
      ungroup()
  ) %>%
  group_by(date, iso2) %>%
  summarize(
    total_imports = sum(total_imports, na.rm = T),
    total_imports_from_war = sum(total_imports_from_war, na.rm = T)
  ) %>%
  ungroup()


exports =
  fread("./data/imp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  rename(iso2 = destiny) %>%
  mutate(date = ymd(sprintf("%s-01", date))) %>%
  group_by(., date, iso2) %>%
  summarize(
    total_exports = sum(imp_value, na.rm = T),
    total_exports_from_war = sum(ifelse(origin %in% WAR_PARTICIPANTS, imp_value, 0.0), na.rm = T)
  ) %>%
  ungroup() %>%
  bind_rows(
    fread("./data/exp_seg.csv") %>%
      mutate(
        across(
          .cols = ends_with("_value"),
          .fns = ~ coalesce(., 0)
        )
      ) %>%
      rename(iso2 = origin) %>%
      mutate(date = ymd(sprintf("%s-01", date))) %>%
      group_by(., date, iso2) %>%
      summarize(
        total_exports  = sum(exp_value, na.rm = T),
        total_exports_from_war = sum(ifelse(destiny %in% WAR_PARTICIPANTS, exp_value, 0.0), na.rm = T)
      ) %>%
      ungroup()
  ) %>%
  group_by(date, iso2) %>%
  summarize(
    total_exports = sum(total_exports, na.rm = T),
    total_exports_from_war = sum(total_exports_from_war, na.rm = T)
  ) %>%
  ungroup()

international_trade =
  imports %>%
  full_join(
    exports
  )

international_trade_2021 =
  international_trade %>%
  filter(year(date) == 2021) %>%
  rename_with(
    .cols = starts_with("total_"),
    .fn = ~ paste0(., "_2021")
  ) %>%
  select(-date) %>%
  group_by(iso2) %>%
  summarize_all(
    .funs = ~ sum(., na.rm = T)
  ) %>%
  mutate(import_exposure_to_war = 100 * total_imports_from_war_2021 / total_imports_2021) %>%
  mutate(export_exposure_to_war = 100 * total_exports_from_war_2021 / total_exports_2021) %>%
  mutate(openness_exposure_to_war = 100 * (total_imports_from_war_2021 + total_exports_from_war_2021) / (total_imports_2021 + total_exports_2021))

international_trade_2021 %>%
  ggplot(aes(x = import_exposure_to_war)) +
  geom_histogram()


# Merge datasets ----------------------------------------------------------

to_regress =
  stock_index %>%
  mutate(date = as_date(date)) %>%
  left_join(
    international_trade_2021
  ) %>%
  mutate(
    across(
      .cols = ends_with("exposure_to_war"),
      .fns = ~ coalesce(., 0.0)
    )
  ) %>%
  left_join(
    geo %>%
      mutate(discrete_distance_to_ukr = ntile(dist_to_ukr, 3))
  ) %>%
  left_join(
    weo %>%
      mutate(discrete_per_capita_gdp = ntile(gdppc, 3)) %>%
      mutate(discrete_pop = ntile(pop, 3)) %>%
      mutate(discrete_gdebt = ntile(gdebt, 3))
  ) %>%
  left_join(
    military
  ) %>%
  full_join(
    sovereign_bonds %>%
      mutate(date = as_date(date))
  ) %>%
  full_join(
    exchange_rate %>%
      mutate(date = as_date(date))
  ) %>%
  left_join(
    exchange_rate %>%
      mutate(date = as_date(date)) %>%
      filter(year(date) == 2021) %>%
      group_by(iso2) %>%
      summarize(mean_fx_2021 = mean(fx, na.rm = T)) %>%
      ungroup()
  ) %>%
  mutate(post = date >= RUSSIA_INVASION_DATE) %>%
  filter(!(iso2 %in% WAR_PARTICIPANTS)) %>%
  mutate(z_export_exposure_to_war = scale(export_exposure_to_war)) %>%
  mutate(z_import_exposure_to_war = scale(import_exposure_to_war)) %>%
  mutate(z_openness_exposure_to_war = scale(openness_exposure_to_war))


model_event_study =
  feols(
    data =
      to_regress %>%
      filter(date >= ymd(20210901)) %>%
      filter(!is.na(export_exposure_to_war)) %>%
      filter(continent != "Pacific") %>%
      # group_by(date, discrete_distance_to_ukr) %>%
      # mutate(stock_index = psych::winsor(stock_index, 0.025)) %>%
      # mutate(export_exposure_to_war = psych::winsor(export_exposure_to_war, 0.025)) %>%
      # mutate(import_exposure_to_war = psych::winsor(import_exposure_to_war, 0.025)) %>%
      # ungroup() %>%
      as.data.table()
    ,fml =
      log(stock_index) ~ i(date, z_export_exposure_to_war, ref = factor(ymd(20220223)))
    + post*scale(cab)
    # + post*gdpg
    + post*scale(gdppc)
    + post*scale(pop)
    # + post*scale(unp)
    + post*scale(pop/area)
    + post*scale(gdebt)
    + post*scale(gbor)
    + post*scale(powrindx) +
      + post*scale(dist_to_ukr)
    + post*scale(total_imports_2021/pop)
    + post*scale(total_exports_2021/pop)
    + post*scale(dis_int)
    | iso2 + date
    ,cluster =
      ~ iso2
    ,fixef.rm = "singleton"
    ,split=~continent
  )

iplot(model_event_study)



# Event study (stock_index - baseline) --------------------------------------------------

for(dependent_variable in c("stock_index", "sb_yield", "fx")) {

  for(variable in c("z_export_exposure_to_war", "z_import_exposure_to_war", "z_openness_exposure_to_war")) {

    for(current_continent in c("all_sample", setdiff(unique(geo$continent), c("Pacific", "No")))) {

      to_regress_event_study =
        to_regress %>%
        mutate(chosen_y_variable = to_regress[[dependent_variable]]) %>%
        mutate(chosen_measurement = to_regress[[variable]])

      if(current_continent != "all_sample") {
        to_regress_event_study =
          to_regress_event_study %>%
          filter(continent == current_continent)
      }

      model_event_study =
        feols(
          data =
            to_regress_event_study %>%
            filter(date >= ymd(20210901)) %>%
            mutate(stock_index = psych::winsor(stock_index, 0)) %>%
            filter(!is.na(chosen_measurement)) %>%
            as.data.table()
          ,fml =
            log(chosen_y_variable) ~ i(date, chosen_measurement, ref = factor(ymd(20220223)))
          + post*scale(cab)
          # + post*gdpg
          + post*scale(gdppc)
          + post*scale(pop)
          # + post*scale(unp)
          + post*scale(pop/area)
          + post*scale(gdebt)
          + post*scale(gbor)
          + post*scale(powrindx) +
            + post*scale(dist_to_ukr)
          + post*scale(total_imports_2021/pop)
          + post*scale(total_exports_2021/pop)
          + post*scale(dis_int)
          | iso2 + date
          ,cluster =
            ~ iso2
          ,fixef.rm = "singleton"
        )

      plot_coefplot(
        model_event_study = model_event_study,
        x_reference = RUSSIA_INVASION_DATE
      )

      ggsave(
        filename = sprintf("%s_%s_%s.png", dependent_variable, variable, current_continent),
        device = "png",
        path = "./results",
        width = 11,
        height = 6
      )

      rm(to_regress_event_study)

    }

  }
}


# Baseline regressions ----------------------------------------------------

model_baseline =
  feols(
    data =
      to_regress %>%
      filter(date >= ymd(20210901)) %>%
      # filter(continent != "Pacific") %>%
      mutate(stock_index = psych::winsor(stock_index, 0)) %>%
      as.data.table()
    ,fml =
      log(stock_index) ~ sw(
        post*scale(export_exposure_to_war)#,
        # post*scale(import_exposure_to_war)
        # post*scale(openness_exposure_to_war)
      )
    + post*scale(cab)
    # + post*gdpg
    + post*scale(gdppc)
    + post*scale(pop)
    # + post*scale(unp)
    + post*scale(pop/area)
    + post*scale(gdebt)
    + post*scale(gbor)
    + post*scale(powrindx) +
      + post*scale(dist_to_ukr)
    + post*scale(total_imports_2021/pop)
    + post*scale(total_exports_2021/pop)
    + post*scale(dis_int)
    | iso2 + date
    ,cluster =
      ~ iso2
    ,fixef.rm = "singleton"
    # ,fsplit = ~ co.ntinent
  )

etable(
  model_baseline
)

model_baseline_with_controls =
  feols(
    data =
      to_regress %>%
      filter(date >= ymd(20210901)) %>%
      mutate(stock_index = psych::winsor(stock_index, 0)) %>%
      as.data.table()
    ,fml =
      sw(log(stock_index), sb_yield) ~ sw(
        post*log(export_exposure_to_war),
        post*log(import_exposure_to_war),
        post*log(openness_exposure_to_war)
      )
    + post*scale(cab)
    + post*log(gdppc)
    + post*log(cpi)
    + post*log(pop)
    + post*log(unp)
    + post*log(powrindx) +
      + post*log(dist_to_ukr)
    + post*log(total_imports_2021/gdp)
    + post*log(total_exports_2021/gdp)
    | iso2 + date
    ,cluster =
      ~ iso2
    ,fixef.rm = "singleton"
  )

etable(
  model_baseline_with_controls
)

