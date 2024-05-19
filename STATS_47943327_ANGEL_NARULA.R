
# QUESTON 1
# PART A
sales_data <- read.csv("sales.csv")
plot(sales_data$Index,sales_data$Sales, main="Scatter Plot of Sales vs Index",
     xlab="Consumer Confidence Index", ylab="Change in Retail Sales (%)")

# PART B
M1<- lm(Sales-Index, data=sales_data)
summary(M1)

par(mfrow=c(2,2))
plot(M1)

# PART C
M2<- lm(Sales~ poly(Index,2), data=sales_data)
summary(M2)

M3<- lm(Sales~ poly(Index,3), data=sales_data)
summary(M3)

# PART D

plot (sales_data$Index, sales_data$Sales, main="Sales vs Index",
      xlab= "Consumer Confidence Index", ylab="Change in Retail Sales")

lines(sales_data$Index, predict(M1), col="red")
lines(sales_data$Index, predict(M2), col="blue")
lines(sales_data$Index, predict(M3), col="green")

# PART E

anova(M1, M2, M3)

# PART F

par(mfrow= c(2,2))
plot(M3)


# QUESTION 2
# PART A

campaign_data <- read.csv("campaign.csv")

boxplot(Score~ Type, data=campaign_data, main= "Engagement Score by Campaign Type",
        xlab="Campaign Type", ylab="Engagement Score")
interaction.plot(campaign_data$Type, campaign_data$Region, campaign_data$Score,
                 main="Interaction Plot", xlab="Campaign Type", ylab="Engagement Score", col=c("yellow","pink"))

# PART B

# Full interaction model
# Y_ijk = μ + α_i + β_j + (αβ)_ij + ε_ijk
# where:
# Y_ijk: Engagement score
# μ: Overall mean
# α_i: Effect of the i-th level of campaign type
# β_j: Effect of the j-th level of region
# (αβ)_ij: Interaction effect between campaign type and region
# ε_ijk: Random error term


#PART C

# Fit the interaction model
interaction_model <- lm(Score ~ Type * Region, data=campaign_data)
summary(interaction_model)

# Hypotheses
# H0: There is no effect of campaign type, region, and their interaction on engagement score.
# H1: There is an effect of campaign type, region, and their interaction on engagement score.


par(mfrow=c(2,2))
plot(interaction_model)

# PART D

main_effects_model <- lm(Score ~ Type + Region, data=campaign_data)
summary(main_effects_model)

par(mfrow=c(2,2))
plot(main_effects_model)

# PART E
table(campaign_data$Type, campaign_data$Region)

tukey_test <- aov(Score ~ Type + Region, data=campaign_data)
TukeyHSD(tukey_test)




