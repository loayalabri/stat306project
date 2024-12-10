## Import required libraries 
library(tidyverse)
library(tidymodels)
library(dplyr)
library(cowplot)
library(ggplot2)
library(broom)
library(gridExtra)
library(car)
library(leaps)


## Read and wrangle the data

data <- read.csv("listings.csv", header = TRUE) %>%
  select(-id, -name, -host_id, -host_name, -host_since, -neighbourhood_cleansed, -property_type, -license, -latitude, -longitude,-host_response_time)
data <- na.omit(data)
data <- data %>%
  filter(beds != 0)

listings <- data %>%
  mutate(host_response_rate = as.numeric(host_response_rate), 
         host_is_superhost = as.factor(host_is_superhost), 
         neighbourhood_group_cleansed = as.factor(neighbourhood_group_cleansed), 
         room_type = as.factor(room_type),
         instant_bookable = as.factor(instant_bookable))

data["log_price"] = log(data$price)

## Price distribution plot
price_dist <- data %>%
    ggplot(aes(price)) + 
    geom_histogram(binwidth = 500, color = "black", fill ="#bcbbbb") +
    labs(title = "Distribution of Price",
        x = "Price", y = "Count") +
    theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),    # Title size
    axis.title = element_text(size = 15),                  # Axis titles size
    axis.text = element_text(size = 12)                    # Axis labels size
  )

price_dist2 <- data %>%
    ggplot(aes(log_price)) + 
    geom_histogram(binwidth = 0.3, color = "black", fill ="#bcbbbb") +
    labs(title = "Distribution of Log Price",
        x = "Log Price", y = "Count") +
    theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),    # Title size
    axis.title = element_text(size = 15),                  # Axis titles size
    axis.text = element_text(size = 12)                    # Axis labels size
  )
price_dist_combined <- grid.arrange(price_dist, price_dist2, ncol = 2)
ggsave("price_dist.png", plot = price_dist_combined, width = 15, height = 7, units = "in", dpi = 300)


## Neighbourhood boxplot
neighborhood_boxplot <- data %>%
    ggplot(aes(x=neighbourhood_group_cleansed,y = price, fill=neighbourhood_group_cleansed)) +
    geom_boxplot() +
    ylim(c(0, 2000)) +
    xlab("neighborhood cleansed") +
    ylab("price (limited to 2000)") +
    labs(fill = "neighborhood", title = "Nighborhood Cleansed VS. Price") +
    theme(
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),    # Title size
        axis.title = element_text(size = 15),                                # Axis titles size
        axis.text = element_text(size = 12))


neighborhood_boxplot2 <- data %>%
    ggplot(aes(x=neighbourhood_group_cleansed,y = log_price, fill=neighbourhood_group_cleansed)) +
    geom_boxplot() +
    xlab("neighborhood cleansed") +
    ylab("log price") +
    labs(fill = "neighborhood", title = "Nighborhood Cleansed VS. Log Price") +
    theme(
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),    # Title size
        axis.title = element_text(size = 15),                                # Axis titles size
        axis.text = element_text(size = 12))

ne_combined <- grid.arrange(neighborhood_boxplot, neighborhood_boxplot2, ncol = 2)
ggsave("neighborhood_boxplot.png", plot = ne_combined, width = 20, height = 10, units = "in", dpi = 600)

## Response Time boxplot
host_response_boxplot <- data %>%
    ggplot(aes(x=host_response_time,y = price, fill=host_response_time)) +
    geom_boxplot() +
    ylim(c(0, 1000)) +
    xlab("host response time") +
    ylab("price") +
    labs(fill = "neighborhood") +
    ggtitle("Price Vs. Host Response Time") +
    theme(
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),    # Title size
        axis.title = element_text(size = 15),                                # Axis titles size
        axis.text = element_text(size = 12))

host_response_boxplot2 <- data %>%
    ggplot(aes(x=host_response_time,y = log_price, fill=host_response_time)) +
    geom_boxplot() +
    xlab("host response time") +
    ylab("log price") +
    labs(fill = "neighborhood") +
    ggtitle("Log Price Vs. Host Response Time") +
    theme(
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),    # Title size
        axis.title = element_text(size = 15),                                # Axis titles size
        axis.text = element_text(size = 12))

combined_plot <- grid.arrange(
  host_response_boxplot, 
  host_response_boxplot2, 
  ncol = 2
)
ggsave("response_time.png", plot = combined_plot, width = 20, height = 10, unit = "in", dpi = 600)


## Scatter plots of bedrooms, beds, bathrooms, accomodates
bathrooms_log_price <- data %>% 
    ggplot(aes(bathrooms, log_price)) +
    geom_point() +
    xlim(c(0, 15)) +
    labs(x = "Number of Bathrooms", y = "Log Price",
    title = "Log Price VS. Bathrooms") +
    theme(plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15))

beds_log_price <-  data %>% 
    ggplot(aes(beds, log_price)) +
    geom_point() +
    xlim(c(0, 30)) +
    labs(x = "Number of Beds", y = "Log Price",
    title = "Log Price VS. Beds") +
    theme(plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15))

bedrooms_log_price <-  data %>% 
    ggplot(aes(bedrooms, log_price)) +
    geom_point() +
    xlim(c(0, 15)) +
    labs(x = "Number of Bedrooms", y = "Log Price",
    title = "Log Price VS. Bedrooms") +
    theme(plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15))

acco_log_price <-  data %>% 
    ggplot(aes(accommodates, log_price)) +
    geom_point() +
    #xlim(c(0, 15)) +
    labs(x = "Number of Accommodates", y = "Log Price",
    title = "Log Price VS. Accommodates") +
    theme(plot.title = element_text(size = 20,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15))

scatter_combined <- grid.arrange(bathrooms_log_price, bedrooms_log_price, beds_log_price, acco_log_price, ncol = 2, nrow = 2)
ggsave("plots/scatter.png", plot = scatter_combined, width = 10, height = 10, units = "in", dpi = 600)


## Correlation Heatmap
cor_matrix2 <- data %>%
    select_if(is.numeric) %>%
   cor() %>%
   as_tibble(rownames = 'var1') %>%
   pivot_longer(-var1, names_to = "var2", values_to = "corr")

cor_heatmap2 <- cor_matrix2 %>%
    ggplot(aes(var1, var2)) + 
    geom_tile(aes(fill = corr), color = "white") + 
    scale_fill_distiller("Correlation Coefficient \n",
      palette =  "YlOrRd",
      direction = 1, 
      limits = c(-1,1)
    ) +
    labs(x = "", y = "", title = "Correlation Heatmap of Numerical Variables") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 28,face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1, face = "bold"),
        axis.text.y = element_text(vjust = 1, size = 16, hjust = 1, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.key.size = unit(1.5, "cm",)
    ) +
    coord_fixed() +
    geom_text(aes(var1, var2, label = round(corr, 2)), color = "black", size = 6)

ggsave("correlation_heatmap_log.png", plot = cor_heatmap2, width = 20, height = 20, units = "in", dpi = 600)


## full Model
full <- lm(listings$price ~ ., data = listings)
summary(full)

## Full log model 
full_log <- lm(log(listings$price) ~ ., data = listings)
summary(full_log)
vif(full_log)

## Trying backward selection
backward_log = regsubsets(log(price) ~., data=listings, nvmax=18, method="backward")
backwards_log = summary(backward_log)
backwards_log$which

backwards_log$rsq
backwards_log$adjr2
backwards_log$cp

## Trying Forward Selection
forward_log = regsubsets(log(price) ~., data=listings, nvmax=18, method="forward")
forwards_log = summary(forward_log)
forwards_log$which

forwards_log$rsq
forwards_log$adjr2
forwards_log$cp

## Exhaustive subsets selection
subset_sel <- regsubsets(log(price) ~., data=listings, nvmax=18, method="exhaustive")
ss <- summary(subset_sel)
ss$which
ss$rsq
ss$adjr2
ss$cp
p=rowSums(ss$which)-1
data_cp <- data.frame(
  p = p,
  cp = ss$cp
)

# Plot Cp
cp_plot <- ggplot(data_cp, aes(x = p, y = cp)) +
  geom_point() +  # Points
  geom_text(aes(label = paste0("(", p, ", ", round(cp, 2), ")")), 
            hjust = -0.1, vjust = 0.5,angle = 45,  color = "blue", size = 5) +  
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "red") +  
  labs(
    title = "Mallow's Cp plot",
    x = "Number of Covariates",
    y = "Cp"
  ) +
  theme_minimal() +
  xlim(c(5, 18)) +
  ylim(c(0, 1200)) +
  theme(plot.title = element_text(size = 20,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15))


ggsave("plots/Cp.png", plot = cp_plot, width = 10, height = 10, units = "in", dpi = 150)
cp_plot

## Best model covariates
ss$adjr2[10]
ss$which[10,]
best_model_covariates=as_tibble(
        as.matrix(ss$which[10,]),
        rownames='covariate') %>%
        filter(covariate != '(Intercept)' & V1 !=FALSE) %>% 
        pull(covariate)
best_model_covariates

levels(listings$neighbourhood_group_cleansed)

listings_joined=listings %>% mutate(neighbourhood_group_cleansed=fct_collapse(neighbourhood_group_cleansed,OthersAndLosAngeles=c("City of Los Angeles","Other Cities")))
head(listings_joined)

best_model_covariates

## best model 
best_model=lm(log(price)~neighbourhood_group_cleansed+room_type+accommodates+bathrooms+bedrooms+beds+minimum_nights+review_scores_rating,data=listings_joined)
summary(best_model) %>% tidy(conf.int = TRUE) %>% mutate_if(is.numeric, round, 2)
plot(best_model$fitted.values,best_model$residual)
res_stand=best_model$residual/(summary(best_model)$sigma*sqrt(1-hatvalues(best_model)))
print(max(abs(res_stand)))
print(which.max(abs(res_stand)))
listings[which.max(abs(res_stand)),]
listings_joined=listings_joined[-which.max(abs(res_stand)),]

best_model=lm(log(price)~neighbourhood_group_cleansed+room_type+accommodates+bathrooms+bedrooms+beds+minimum_nights+review_scores_rating,data=listings_joined)
summary(best_model) %>% tidy(conf.int = TRUE) %>% mutate_if(is.numeric, round, 2)

residuals_df <- data.frame(
    Fitted = fitted(best_model),
    Residuals = residuals(best_model)
)

resid_plot <- ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "black", alpha = 0.5) +  # Scatter plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at y = 0
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 26,face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15))

ggsave("plots/resid.png", plot = resid_plot, width = 10, height = 10, units = "in", dpi = 600)
resid_plot

vif(best_model)