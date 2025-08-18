library(readxl)
library(plm)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
options(scipen = 999)
data <- read_excel("C:\\Users\\Dilip\\Desktop\\data.xlsx")
p.data <- pdata.frame(data, index = c("id", "year"))
#---------------------------------------------------------------------------------------------------------------------------
#section 2
#graphical variations
#graph1 land and forest
ggplot(p.data, aes(x = log(land_area), y = log(forest_area))) +
  geom_point(color = "dark green") +
  labs(title = "comparison between logged values of land and forest area",
       x = "log(land_area)",
       y = "log(forest_area)") +
  geom_abline()

#graph2 mean values over time
mean_stats <- data %>%
  group_by(year) %>%
  summarise(
    mean_fa = mean(log(forest_area)),
    mean_fl = mean(fl),
    mean_co = mean(log(total_co2)),
    mean_rp = mean(rural_pop),
    mean_le = mean(life_exp), 
    mean_pd = mean(pop_dens)
  ) %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "variable", 
               values_to = "value")

ggplot(mean_stats, aes(x = year, y = value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Overall trend of variables over Years",
    x = "Year",
    y = "Value"
  ) 

#graph3 ekc
ekc <- data %>%
  group_by(year) %>%
  summarise(mean_co2 = mean(log(total_co2)),
            mean_gdp = mean(log(gdp_constant)))

ggplot(ekc, aes(x = mean_gdp, y = mean_co2)) +
  geom_line() +
  labs(title = "Environmental Kuznets Curve")

#graph4 gdp vs co2 colored by fl
ggplot(data, aes(x = log(gdp_constant), y = log(total_co2),color = fl)) +
  scale_color_viridis(discrete = FALSE) +
  geom_point() +
  labs(title = "co2 vs gdp filled by forest %",
       x = "GDP",
       y = "co2")

#graph5 agriculture_va vs co2
ggplot(data, aes(x = log(agriculture_va), y = log(total_co2))) +
  geom_point() +
  labs(title = "comparison between agriculture_va and co2",
       x = "agriculture",
       y = "co2")


#summary statistics
print(describe(p.data)[c("forest_area","land_area","pop_dens","rural_pop","life_exp","total_co2"), ]
      [, c("mean", "sd", "min", "max")])

#----------------------------------------------------------------------------------------------------------------------------------
#section 3.1
#pooled
pooled_model <- plm(log(forest_area) ~ log(land_area) + pop_dens + rural_pop + life_exp + 
                      log(total_co2), model = "pooling", data = p.data)
summary(pooled_model)
#pooled with time dummy
pooled_time_model <- plm(log(forest_area) ~ log(land_area) + pop_dens + rural_pop + life_exp +
                           log(total_co2) + year, effect = "individual", model = "pooling", data = p.data)
summary(pooled_time_model)


#fixed effects
fe_model <- plm(log(forest_area) ~ log(land_area) + pop_dens + rural_pop + life_exp +
                  log(total_co2), data = p.data)
summary(fe_model)
#fixed effects with time dummy
fe_time_model <- plm(log(forest_area) ~ log(land_area) + pop_dens + rural_pop + life_exp + 
                       log(total_co2) + year, effect = "individual", data = p.data)
summary(fe_time_model)


#random effects
re_model <- plm(log(forest_area) ~ log(land_area) + pop_dens + rural_pop + life_exp +
                  log(total_co2), model = "random", data = p.data)
summary(re_model)
#random effects with time dummy
re_time_model <- plm(log(forest_area) ~ log(land_area) + pop_dens + rural_pop + life_exp +
                       log(total_co2) + year, effect = "individual", model = "random", data = p.data)
summary(re_time_model)

#----------------------------------------------------------------------------------------------------------------------------------
#3.2 hypothesis testing
#without time dummy
# pooled vs FE using F-test
pFtest(fe_model, pooled_model)

# pooled vs RE using BPLM test
plmtest(pooled_model, type = "bp")

# FE vs RE using Hausman Test
phtest(fe_model, re_model)

#with time dummy
# pooled vs FE using F-test
pFtest(fe_time_model, pooled_time_model)

# pooled vs RE using BPLM test
plmtest(pooled_time_model, type = "bp")

# FE vs RE using Hausman Test
phtest(fe_time_model, re_time_model)

#------------------------------------------------------------------------------------------------------------------------------------
