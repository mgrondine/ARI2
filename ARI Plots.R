library(readr)
library(readxl)
library(ggplot2)

load("data/ari_base_char/working/installation_data_product.csv")
all <- read_csv("data/ari_base_char/working/installation_data_product.csv")

#Plot histograms of some variables
ggplot(data = all) +
  geom_histogram(aes(excess_drink), bins = 20) +
  geom_vline(aes(xintercept=18), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("2016 % Adults reporting binge/excessive drinking") +
  ylab("Count of Counties") +
  ggtitle("2016 Histogram of Excessive Drinking")

ggplot(data = all) +
  geom_histogram(aes(alc_drive_deaths_rate), bins = 20) +
  geom_vline(aes(xintercept=29), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("% Driving deaths with alcohol involvement") +
  ylab("Count of Counties") +
  ggtitle("2012-2016 Histogram of Alcohol Driving Deaths")

ggplot(data = all) +
  geom_histogram(aes(long_drive_alone), bins = 20) +
  geom_vline(aes(xintercept=35), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("% [Adults reporting commutes over 30 min. alone]/[all alone commuters]") +
  ylab("Count of Counties") +
  ggtitle("2012 - 2016 Histogram of Long Commutes Alone")

ggplot(data = all) +
  geom_histogram(aes(clap_rate), bins = 20) +
  geom_vline(aes(xintercept=478.8), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("Number of New Chlamydia Cases per 100,000") +
  ylab("Count of Counties") +
  ggtitle("2015 Histogram of New Chlamydia Cases")

#find someone to help you add a legend for the red line
ggplot(data = all, aes(teen_birth)) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept=27), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("Number of Teen Births per 1,000 (age 15-19)") +
  ylab("Count of Counties") +
  ggtitle("2010-2016 Histogram of Teen Births") #+
#legend(x = 5, legend = "National Average = 27", col = "red", lty = 1)

## Graphs for H+T Data
ggplot(data = all) +
  geom_histogram(aes(ht_ami), bins = 20) +
  geom_vline(aes(xintercept=53), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("H+T Costs % of Income for Regional Typical Household") +
  ylab("Count of Counties") +
  ggtitle("2014-15 Histogram of H+T Costs % of Income")

# Scatterplot
# Excess drinking rate + Alcohol driving deaths rate. Does not tell much.
gg <- ggplot(all, aes(x=excess_drink, y=alc_drive_deaths_rate)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(10, 30)) +
  ylim(c(10, 50)) +
  labs(subtitle=" Excess Drinking vs. Alcohol Driving Deaths",
       y="Alcohol Driving Deaths (%)",
       x="Excess Drinking (%)",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)

# HT Costs + Owner Occupied Housing
gg <- ggplot(all, aes(x=ht_ami, y=pct_owner_occupied_hu)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(25, 75)) +
  ylim(c(25, 75)) +
  labs(subtitle="H+T Costs % Income vs. % Owner Occupied Housing Units",
       y=" Owner Occupied Housing Units (%)",
       x="H+T Costs (%)",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)

#HT Costs + Renter Occupied Housing
gg <- ggplot(all, aes(x=ht_ami, y=pct_renter_occupied_hu)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(30, 70)) +
  ylim(c(10, 75)) +
  labs(subtitle="H+T Costs % Income vs. % Renter Occupied Housing Units",
       y=" Renter Occupied Housing Units (%)",
       x="H+T Costs (%)",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)


## Compact Nieghborhood Score + Job Access Score
gg <- ggplot(all, aes(x=emp_ovrll_ndx, y=compact_ndx)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(0, 10)) +
  ylim(c(0, 10)) +
  labs(subtitle="Job Access Score + Compact Neighborhood Score",
       y="Compact Neighborhood Score",
       x="Job Access Score",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)

gg <- ggplot(all, aes(x=h_ami, y=t_ami)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(20, 50)) +
  ylim(c(5, 40)) +
  labs(subtitle="Housing Costs + Transporation Costs
       % Income for the Regional Typical Household",
       y="Transportation Costs (% Income)",
       x="Housing Costs (% Income)",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)


gg <- ggplot(all, aes(x=mental_rate, y=social)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(0, 510)) +
  ylim(c(0, 30)) +
  labs(subtitle="Social + Mental Rate",
       y="Social Attractions",
       x="Mental Rate",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)


ggplot(data = all) +
  geom_histogram(aes(social), bins = 20) +
  geom_vline(aes(xintercept=9.94), color = "red") +
  theme(text = element_text(size = 15), #size of x-axis label text
        axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = .5)) +
  xlab("Social Attractions") +
  ylab("Count of Counties") +
  ggtitle("2012-2016 Histogram of Mental Rate")


gg <- ggplot(all, aes(x=phys_inactive, y=obese)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(10, 40)) +
  ylim(c(10, 50)) +
  labs(subtitle="Physically Inactive + Obese",
       y="Obese",
       x="Physically Inactive",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)


gg <- ggplot(all, aes(x=house_problem, y=child_pov)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(5, 40)) +
  ylim(c(0, 40)) +
  labs(subtitle="House Problem + Child Poverty",
       y="Child Poverty",
       x="House Problem",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)



gg <- ggplot(all, aes(x=mental_rate, y=violence)) +
  geom_point(aes(col = "base", size=population)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(0, 500)) +
  ylim(c(0, 1300)) +
  labs(subtitle="Mental Rate & Violence",
       y="Violence",
       x="Mental Rate",
       title="Scatterplot",
       caption = "Source: all")

plot(gg)



# devtools::install_github("kassambara/ggcorrplot")
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)

# Correlation matrix
data(all)
corr <- round(cor(all), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of mtcars",
           ggtheme=theme_bw)