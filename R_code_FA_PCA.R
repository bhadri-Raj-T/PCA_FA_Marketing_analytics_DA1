#Install dependencies
#install.packages("corrplot") 
#install.packages("pheatmap")
#install.packages("psych")

#Read CSV files
df <- read.csv("diabetes.csv")

#Explore the data
head(df)
str(df)
dim(df)
summary(df)
colnames(df)

#Import nessasry libraries
library(corrplot)
library(pheatmap)
library(psych)

# Correlation Matrix (basic check)
# Select only numeric predictor columns (remove Outcome)
data_fa <- df[, c("Pregnancies","Glucose","BP","SkinThickness","Insulin","BMI","DBF","Age")]

# Correlation matrix
cor_matrix <- cor(data_fa, method = "pearson")
cor_rounded <- round(cor_matrix, 2)
print(cor_matrix)

corrplot(cor_rounded,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 0.8,           # Text label size
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("navy", "white", "red"))(200),
         na.label = "NA",
         cl.ratio = 0.2,         # Color legend size
         mar = c(0,0,2,0))       # Margins

# KMO (Kaiser-Meyer-Olkin) Test
KMO(cor_matrix)

# Bartlett’s Test of Sphericity
cortest.bartlett(cor_matrix, n = nrow(data_fa))

#eigen value
data_fa <- df[, c("Pregnancies","Glucose","BP","SkinThick","Insulin","BMI","DPF","Age")]
data_scaled <- scale(data_fa)

cor_matrix <- cor(data_scaled)

eig <- eigen(cor_matrix)

eig$values

# Enhanced scree plot with better colors and styling
eigenvalues <- eig$values

plot(eigenvalues, 
     type = "b",
     pch = 19,                    # Filled circles
     cex = 1.5,                  # Larger points
     lwd = 2,                    # Thicker line
     col = "steelblue",          # Line and point color
     xlab = "Factor Number", 
     ylab = "Eigenvalue",
     main = "Scree Plot",
     cex.main = 1.5,            # Larger title
     cex.lab = 1.2,            # Larger axis labels
     cex.axis = 1.1,           # Larger axis numbers
     ylim = c(0, max(eigenvalues) + 0.5),
     bty = "n")                # Remove box

# Add horizontal line at eigenvalue = 1
abline(h = 1, col = "coral3", lty = 2, lwd = 2)

# Add grid for better readability
grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

# Add text annotation for Kaiser criterion
text(x = length(eigenvalues) - 2, y = 1.2, 
     labels = "Kaiser Criterion (λ = 1)", 
     col = "coral3", cex = 0.9)


#logistic regression
# Select predictors
data_fa <- df[, c("Pregnancies","Glucose","BP","SkinThick",
                  "Insulin","BMI","DPF","Age")]

# Add outcome
data_log <- cbind(data_fa, Outcome = df$Outcome)

# List of predictor names
vars <- colnames(data_fa)

# Run logistic regression for each variable
for (v in vars) {
  formula <- as.formula(paste("Outcome ~", v))
  model <- glm(formula, data = data_log, family = binomial)
  
  cat("\n=============================\n")
  cat("Variable:", v, "\n")
  print(summary(model))
}

full_model <- glm(Outcome ~ Pregnancies + Glucose + BP +
                    SkinThick + Insulin + BMI + DPF + Age,
                  data = data_log,
                  family = binomial)

summary(full_model)
vif(full_model)


#Now FA and PCA iter 1
data_fa <- df[, c("Pregnancies","Glucose","BP","SkinThick","Insulin","BMI","DPF","Age")]
data_scaled <- scale(data_fa)
fa.parallel(data_scaled, fa="fa")
fa_model <- fa(data_scaled,
               nfactors = 3,
               rotate = "varimax",
               fm = "pa")      

print(fa_model)
fa_model$communality
fa_model$uniquenesses
fa.diagram(fa_model)

pca <- principal(data_scaled,
                 nfactors = 3,
                 rotate = "varimax"
                 ) 
print(pca)
summary(pca)
pca$rotation
fa.diagram(pca)

#Regression test after FA/PCA
# Extract component scores
component_scores <- as.data.frame(pca$scores)

# Rename components
colnames(component_scores) <- c("Obesity_factor", "Age_factor", "Metabolic_factor")

reg_data <- cbind(component_scores, Outcome = df$Outcome)

# Logistic Regression
log_model <- glm(Outcome ~ Obesity_factor + Age_factor + Metabolic_factor,
                 data = reg_data,
                 family = binomial)

summary(log_model)

library(car)
vif(log_model)
