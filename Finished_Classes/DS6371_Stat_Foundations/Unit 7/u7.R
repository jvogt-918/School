L = c(45,53.1,40.8,75.5,46.7,85.4,85.6,18.2,43.2)
U = c(23.6,13.3,34.2,18.1,56.1,-8.1,-20.1)
wilcox.test(U,L, correct = TRUE)

#Unit 7
install.packages("agricolae")
install.packages("multcomp")
#Unit 7
library(agricolae)
library(multcomp)

# Load and prep data
handicap_data <- Unit_7_Handicap_Data
handicap_data$Handicap <- as.factor(handicap_data$Handicap)

# ANOVA
model <- aov(Score ~ Handicap, data = handicap_data)

# Get MSE and SE for differences
mse <- summary(model)[[1]][2,3]
df_error <- summary(model)[[1]][2,1]
n_per_group <- mean(table(handicap_data$Handicap))
se_diff <- sqrt(2 * mse / n_per_group)

# Critical values
alpha <- 0.05
k <- 5  # number of groups
comparisons <- k*(k-1)/2

# Calculate half-widths
t_lsd <- qt(1 - alpha/2, df_error)
half_lsd <- t_lsd * se_diff

t_bon <- qt(1 - alpha/(2*comparisons), df_error)
half_bon <- t_bon * se_diff

q_tukey <- qtukey(1 - alpha, k, df_error) / sqrt(2)
half_tukey <- q_tukey * se_diff

f_scheffe <- sqrt((k-1) * qf(1 - alpha, k-1, df_error))
half_scheffe <- f_scheffe * se_diff

t_dunnett <- 2.37  # Approximate for 4 vs control, df=65
half_dunnett <- t_dunnett * se_diff

# Output
cat("LSD half-width:", round(half_lsd, 3), "\n",
    "Dunnett half-width:", round(half_dunnett, 3), "\n",
    "Tukey half-width:", round(half_tukey, 3), "\n",
    "Bonferroni half-width:", round(half_bon, 3), "\n",
    "Scheffe half-width:", round(half_scheffe, 3), "\n")
'