library(tidyverse)
library(data.table)
library(fixest)
library(lubridate)
library(ggtext)

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

international_trade =
  fread("./data/imp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  mutate(date = ymd(sprintf("%s-01", date))) %>%
  inner_join(
    filter(., year(date) == 2021) %>%
      group_by(origin) %>%
      summarize(
        total_imports_2021 = sum(imp_value),
        total_imports_from_war_2021 = sum(ifelse(destiny %in% c(UKRAINE_2DIGIT_ISOCODE, RUSSIA_2DIGIT_ISOCODE, BELARUS_2DIGIT_ISOCODE), imp_value, 0.0))
      ) %>%
      ungroup()
  )%>%
  inner_join(
    filter(., year(date) == 2021) %>%
      group_by(destiny) %>%
      summarize(
        total_exports_2021 = sum(imp_value),
        total_exports_from_war_2021 = sum(ifelse(origin %in% c(UKRAINE_2DIGIT_ISOCODE, RUSSIA_2DIGIT_ISOCODE, BELARUS_2DIGIT_ISOCODE), imp_value, 0.0))
      ) %>%
      ungroup()
  ) %>%
  bind_rows(
    fread("./data/exp_seg.csv") %>%
      mutate(
        across(
          .cols = ends_with("_value"),
          .fns = ~ coalesce(., 0)
        )
      ) %>%
      mutate(date = ymd(sprintf("%s-01", date))) %>%
      inner_join(
        filter(., year(date) == 2021) %>%
          group_by(origin) %>%
          summarize(
            total_exports_2021 = sum(exp_value),
            total_exports_from_war_2021 = sum(ifelse(destiny %in% c(UKRAINE_2DIGIT_ISOCODE, RUSSIA_2DIGIT_ISOCODE, BELARUS_2DIGIT_ISOCODE), exp_value, 0.0))
          ) %>%
          ungroup()
      )%>%
      inner_join(
        filter(., year(date) == 2021) %>%
          group_by(destiny) %>%
          summarize(
            total_imports_2021 = sum(exp_value),
            total_imports_from_war_2021 = sum(ifelse(origin %in% c(UKRAINE_2DIGIT_ISOCODE, RUSSIA_2DIGIT_ISOCODE, BELARUS_2DIGIT_ISOCODE), exp_value, 0.0))
          ) %>%
          ungroup()
      )
  )

trade_exposure_to_war_2021 =
  international_trade %>%
  distinct(iso2 = origin,
           total_imports_2021, total_imports_from_war_2021,
           total_exports_2021, total_exports_from_war_2021) %>%
  group_by(iso2) %>%
  summarize(
    total_imports_2021 = sum(total_imports_2021, na.rm = T),
    total_imports_from_war_2021 = sum(total_imports_from_war_2021, na.rm = T),
    total_exports_2021 = sum(total_exports_2021, na.rm = T),
    total_exports_from_war_2021 = sum(total_exports_from_war_2021, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(import_exposure_to_war = 100 * total_imports_from_war_2021 / total_imports_2021) %>%
  mutate(export_exposure_to_war = 100 * total_exports_from_war_2021 / total_exports_2021) %>%
  mutate(openness_exposure_to_war = 100 * (total_imports_from_war_2021 + total_exports_from_war_2021) / (total_imports_2021 + total_exports_2021))


import_exposure_to_ukraine_2021 %>%
  ggplot(aes(x = openness_exposure_to_war)) +
  geom_histogram()



# Merge datasets ----------------------------------------------------------

to_regress =
  stock_index %>%
  mutate(date = as_date(date)) %>%
  left_join(
    trade_exposure_to_war_2021
  ) %>%
  mutate(
    across(
      .cols = ends_with("exposure_to_war"),
      .fns = ~ coalesce(., 0.0)
    )
  ) %>%
  inner_join(
    geo
  ) %>%
  left_join(
    weo %>%
      mutate(discrete_per_capita_gdp = ntile(gdppc, 3))
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
  mutate(post = date >= RUSSIA_INVASION_DATE)


model_event_study =
  feols(
    data =
      to_regress %>%
      filter(date >= ymd(20210901)) %>%
      mutate(log_export_exposure_to_war = log(export_exposure_to_war)) %>%
      filter(!is.na(sb_yield), !is.na(date), !is.na(export_exposure_to_war)) %>%
      as.data.table()
    ,fml =
      log(fx) ~ i(date, export_exposure_to_war, ref = factor(ymd(20220223)))
    + date*cab
    + date*log(gdppc)
    + date*log(cpi)
    + date*log(pop)
    + date*log(unp)
    + date*log(powrindx) +
      + date*log(dist_to_ukr)
    + date*log(total_imports_2021/gdp)
    + date*log(total_exports_2021/gdp)
    | iso2 + date
    ,cluster =
      ~ iso2
    # ,fixef.rm = "singleton"
  )

iplot(model_event_study)



# Event study (stock_index - baseline) --------------------------------------------------

for(dependent_variable in c("stock_index", "sb_yield", "fx", "total_exports", "total_imports")) {

  for(variable in c("export_exposure_to_war", "import_exposure_to_war", "openness_exposure_to_war")) {

    to_regress_event_study =
      to_regress %>%
      mutate(chosen_y_variable = to_regress[[dependent_variable]]) %>%
      mutate(chosen_measurement = to_regress[[variable]])

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
        + post*cab
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

    to_plot =
      iplot(model_event_study, only.params = T)$prms %>%
        as.data.frame() %>%
        rename(date = estimate_names) %>%
        mutate(weekday = wday(date)) %>%
        mutate(month_year = year(date) * 100 + month(date)) %>%
        group_by(month_year) %>%
          mutate(last_day = max(day(date))) %>%
          mutate(max_date = max(date)) %>%
        ungroup() %>%
        mutate(weekday_max_date = wday(max_date)) %>%
        mutate(date_string = case_when(
          day(date) == last_day ~ format(date, "<br/>%b<br/>%Y"),
          weekday %in% c(2) ~ format(date, "<i style='font-size:17px;'>%d</i>"),
          TRUE ~ ""
        ))

    to_plot %>%
      ggplot(
        aes(x = factor(date), y = estimate)
      ) +
      geom_errorbar(
        aes(ymin = ci_low, ymax = ci_high), width = 0.5, size = 0.5
      ) +
      geom_point(size = 1.5) +
      ggthemes::theme_clean(base_size = 22) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks()) +
      scale_x_discrete(breaks = factor(to_plot$date), labels = to_plot$date_string) +
      geom_vline(xintercept = factor(ymd(RUSSIA_INVASION_DATE - 1)), linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = "Date",
        y = "Estimate (Date x Exposure to War)"
      ) +
      theme(
        axis.text.x = element_markdown(),
        plot.background = element_blank(),
        aspect.ratio = 1/2,
        axis.line = element_line(colour = 'black', size = 55)
      )

    ggsave(
      filename = sprintf("%s_%s.pdf", dependent_variable, variable),
      device = "pdf",
      path = "./results",
      width = 11,
      height = 6
    )

    rm(to_regress_event_study)

  }
}


# Baseline regressions ----------------------------------------------------

model_baseline =
  feols(
    data =
      to_regress %>%
      filter(date >= ymd(20210901)) %>%
      mutate(stock_index = psych::winsor(stock_index, 0)) %>%
      as.data.table()
    ,fml =
      log(stock_index) ~ sw(
        post*scale(export_exposure_to_war),
        post*scale(import_exposure_to_war),
        post*scale(openness_exposure_to_war)
      )
    + post*cab + post*log(gdppc) + post*log(cpi) + post*log(pop) + post*log(unp)
    + post*log(powrindx) + post*log(dis_int)
    + post*log(dist_to_ukr)
    + post*log(total_imports_2021/gdp)
    + post*log(total_exports_2021/gdp)
    | iso2 + date
    ,cluster =
      ~ iso2
    ,fixef.rm = "singleton"
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

