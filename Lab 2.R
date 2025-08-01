library(fpp3)

USGDP <- filter(global_economy, Country == 'United States')

autoplot(USGDP)
gg_season(USGDP)
gg_subseries(USGDP)
gg_lag(USGDP)
ACF(USGDP)

global_economy
print(global_economy, n = 15000)

aus_livestock
AUSKILLS <- filter(aus_livestock, State == "Victoria", Animal == "Bulls, bullocks and steers")
print(aus_livestock, n = 29000)

autoplot(AUSKILLS, y = Count)
gg_season(AUSKILLS, y = Count)
gg_subseries(AUSKILLS, y = Count)
gg_lag(AUSKILLS, y = Count)
ACF(AUSKILLS, y = Count)

us_gasoline |>
  features(Barrels, features = guerrero)

aus_arrivals |>
  autoplot()

aus_arrivals |>
  gg_season()

aus_arrivals |>
  gg_subseries()

aus_arrivals |>
  ACF()



aus_arrivals |>
  group_by(Origin) |>
  features(Arrivals, features = guerrero)

aus_production |>
  autoplot(Beer)

aus_production |>
  gg_season(Beer)

aus_production |>
  gg_subseries(Beer)

aus_production |>
  autoplot(Tobacco)

aus_production |>
  gg_season(Tobacco)

aus_production |>
  gg_subseries(Tobacco)


aus_production |>
  autoplot(Bricks)

aus_production |>
  gg_season(Bricks)

aus_production |>
  gg_subseries(Bricks)


aus_production |>
  autoplot(Cement)

aus_production |>
  gg_season(Cement)

aus_production |>
  gg_subseries(Cement)

dcmp <- us_change |>
  model(us_change = STL(Income))
  components(dcmp) |> 
    autoplot()


