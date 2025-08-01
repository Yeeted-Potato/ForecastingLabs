library(fpp3)
library(readr)
library(dplyr)
tute <- read_csv("tute111.csv")

autoplot(gafa_stock)
autoplot(vic_elec)
autoplot(pelt)

interval(gafa_stock)
gafa_stock
interval(vic_elec)
interval(pelt)

gafa_stock |> 
  group_by(Symbol) |>
  filter(Close == max(Close))

timeseries <- tute111 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  duplicates(index = Quarter)

timeseries

timeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) + geom_line() +
  facet_grid(name ~ ., scales = "free_y")

timeseries

us_employment
USEMPLOY <- us_employment |> filter(Title == "Total Private")
autoplot(USEMPLOY)
gg_season(USEMPLOY)
gg_subseries(USEMPLOY)
gg_lag(USEMPLOY)
ACF(USEMPLOY)

aus_production
AUSPRO <- aus_production |> select(Quarter, Bricks)
autoplot(AUSPRO)
gg_season(AUSPRO)
gg_subseries(AUSPRO)
gg_lag(AUSPRO)
ACF(AUSPRO)

pelt
PELTHARE <- pelt |> select(Year, Hare)
autoplot(PELTHARE)
gg_season(PELTHARE)
gg_subseries(PELTHARE)
gg_lag(PELTHARE)
ACF(PELTHARE)

PBS
HO2COST <- PBS |> filter(ATC2 == "H02") |> select(Month, Cost)
HO2COST
autoplot(HO2COST)
gg_season(HO2COST)
gg_subseries(HO2COST)
gg_lag(HO2COST)
ACF(HO2COST)

us_gasoline
autoplot(us_gasoline)
gg_season(us_gasoline)
gg_subseries(us_gasoline, y = Barrels)
gg_lag(us_gasoline, y = Barrels)
ACF(HO2COST)

