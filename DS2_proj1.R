# Read the given dataset
data = read.csv("dataset-p1.csv")
#attach(data)

#Fit an Linear Regression model using the dataset
cat("=============================  (a).Fit an Linear Regression model using the dataset  =================\n\n")
model = lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15, data=data)
cat("RSS:- ",sum(resid(model)^2))
#deviance(model)
model_summary1 = summary(model)
print(model_summary1)

#Add a transformed X4^2 to the model features and report R^2
cat("===================== (b) Add a transformed X4 2 to the model features and report R 2 ===================\n\n")
#data_transformed = within(data,X4 <- X4^2)
data_transformed = data
data_transformed["X4^2"] <-NA
data_transformed$X4^2 < data["X4"]^2

model2 = lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X4^2, data=data_transformed)
model_summary2 = summary(model2)
cat("R-squared:- ",model_summary2$r.squared)


"We can consider a linear model to be statistically significant only when both these p-Values are 
less that the pre-determined statistical significance level, which is ideally 0.05. 
This is visually interpreted by the significance stars at the end of the row. The more the stars beside 
the variable’s p-Value, the more significant the variable."

# Removing X3,X6,X11,X12,X13 at first stage :
model3 = lm(Y~X1+X2+X4+X5+X7+X8+X9+X10+X14+X15, data=data) 
model_summary3 = summary(model3)
cat("===================== After removing insignificant cols  ===============\n\n")
print(model_summary3)

# Removing X1,X5,X9,X14 at second stage

model4 = lm(Y~X1+X2+X4+X7+X8+X10+X15, data=data) 
model_summary4 = summary(model4)
cat("R2:- ",model_summary4$r.squared)

cat("\n\n===================== (d) Calculating vif from car package ===================\n\n")
#Use Variance Inflation Factor. If vif > 10 then multicollinearity is strongly suggested.
#intsall.packages(car)

library(car)
inflation_fac = vif(model)
print(inflation_fac)
