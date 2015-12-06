# Alternate code excluding Fannie and Freddie

combined4b <- combined4[which(combined4$GVKEY != 4601),]
combined4b <- combined4b[which(combined4b$GVKEY != 15208),]

############## Split data into training and test ###################
# Set seed so the results are replicable 
set.seed(9)

# Select a random sample of rows
samples <- sort(sample.int(nrow(combined4b), 0.80*nrow(combined4b)))

# Subset the data into training and test datasets.
train <- combined4b[samples,] 
test <- combined4b[-samples,]


# Plot TDC1 against all financial variables
# Use Log for market value and cube root for other variables
par(mfrow=c(1,3))
plot(log(train$mv),log(train$TDC1),pch=20,xlab = "Log of Market Value", ylab = "Log of TDC1", main = "Market Value")
plot(train$dlc_cr,log(train$TDC1),pch=20,xlab = "Cube Root of Debt in Current Liabilities", ylab = "Log of TDC1", main = "Short-term Debt")
plot(train$dltt_cr,log(train$TDC1),pch=20,xlab = "Cube Root of Long Term Debt", ylab = "Log of TDC1", main = "Long Term Debt")
plot(train$pstk_cr,log(train$TDC1),pch=20,xlab = "Cube Root of Preferred Stock", ylab = "Log of TDC1", main = "Preferred Stock")
plot(train$che_cr,log(train$TDC1),pch=20,xlab = "Cube Root of Cash", ylab = "Log of TDC1", main = "Cash")

# Market value has the best relationship, followed by Long term debt.


# Regress log total compensation on log market value
ev.reg1 <- lm(log(TDC1) ~ log(mv) , data = train)
summary(ev.reg1)
par(mfrow=c(1,1))
plot(log(train$mv),log(train$TDC1),pch=20,xlab = "Log of Market Value", ylab = "Log of TDC1", main = "Market Value")
abline(ev.reg1)

# Can argue market value is more important than makret value because shareholders
# serve as the check on executive compensation, and shareholders care about MV.

# Show diagnosic plots
par(mfrow=c(1,3))
plot(ev.reg1$fitted.values,rstudent(ev.reg1), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(ev.reg1))
qqnorm(rstudent(ev.reg1))
abline(a=0,b=1)

# Examine companies with very low studentized residuals

# Create dataset of relevent variables
ev.reg1.diagnositcs <- train[which(!is.na(train$mv)),]
ev.reg1.diagnositcs$fitted.values <- ev.reg1$fitted.values
ev.reg1.diagnositcs$residuals <- ev.reg1$residuals
ev.reg1.diagnositcs$stresiduals <- rstudent(ev.reg1)
write.csv(ev.reg1.diagnositcs[which(ev.reg1.diagnositcs$stresiduals < -4),c("EXEC_FULLNAME", "CONAME", "TDC1", "mv","stresiduals")], file = "Regression Diagnostics - Underpaid CEOs.csv")

# All of the large residuals are negative - i.e. CEOs making way less than we predict.
# Two of the largest (using this trianing sample) are Steve Balmer of Microsoft and
# Warren Buffett of Berkshire Hathaway.  It is hard to predict for CEOs that simply 
# choose to accept a lower salary.

# Regress log total compensation on log market value and cube root of all other EV variables
ev.reg2 <- lm(log(TDC1) ~ log(mv) + dlc_cr + dltt_cr + pstk_cr + che_cr, data = train)
summary(ev.reg2)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(ev.reg2$fitted.values,rstudent(ev.reg2), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(ev.reg2))
qqnorm(rstudent(ev.reg2))
abline(a=0,b=1)

# Create dataset of relevent variables
ev.reg2.AIC <- step(ev.reg1, scope=formula(ev.reg2), direction="forward", k=2)
ev.reg2.BIC <- step(ev.reg1, scope=formula(ev.reg2), direction="forward", k=log(nrow(train)))
summary(ev.reg2.BIC)


#### Kitchen Sink Regression #########

train.select <- train[,c("TDC1", "logmv", "dltt_cr", "dlc_cr", "pstk_cr", "che_cr", 
                         "Industry_Code4", "bkvlps", "croa", "dpr", 
                         "epsfx", "gmargin","roa", "roe", "aturn", "dr",
                         "der", "wcap", "fincf_cr", "ivncf_cr", "oancf_cr")]


# Create null and full models
comb.reg.null <- lm(log(TDC1) ~ 1, data = train.select)
comb.reg.full <- lm(log(TDC1) ~ . + .^2, data = train.select)

# Run BIC on combined regression
comb.reg.BIC2 <- step(comb.reg.null, scope=formula(comb.reg.full), direction="forward", k=log(nrow(train)))
summary(comb.reg.BIC1)
