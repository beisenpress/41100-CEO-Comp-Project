# Alternate code excluding Fannie and Freddie

############## Remove FHFA from training data ###################

train.b <- train[which(train$GVKEY != 4601),]
train.b <- train.b[which(train.b$GVKEY != 15208),]


# Plot TDC1 against all financial variables
# Use Log for market value and cube root for other variables
par(mfrow=c(1,3))
plot(log(train.b$mv),log(train.b$TDC1),pch=20,xlab = "Log of Market Value", ylab = "Log of TDC1", main = "Market Value")
plot(train.b$dlc_cr,log(train.b$TDC1),pch=20,xlab = "Cube Root of Debt in Current Liabilities", ylab = "Log of TDC1", main = "Short-term Debt")
plot(train.b$dltt_cr,log(train.b$TDC1),pch=20,xlab = "Cube Root of Long Term Debt", ylab = "Log of TDC1", main = "Long Term Debt")
plot(train.b$pstk_cr,log(train.b$TDC1),pch=20,xlab = "Cube Root of Preferred Stock", ylab = "Log of TDC1", main = "Preferred Stock")
plot(train.b$che_cr,log(train.b$TDC1),pch=20,xlab = "Cube Root of Cash", ylab = "Log of TDC1", main = "Cash")

# Market value has the best relationship, followed by Long term debt.


# Regress log total compensation on log market value
ev.reg1 <- lm(log(TDC1) ~ log(mv) , data = train.b)
summary(ev.reg1)
par(mfrow=c(1,1))
plot(log(train.b$mv),log(train.b$TDC1),pch=20,xlab = "Log of Market Value", ylab = "Log of TDC1", main = "Market Value")
abline(ev.reg1)

# Can argue market value is more important than makret value because shareholders
# serve as the check on executive compensation, and shareholders care about MV.

# Show diagnosic plots
par(mfrow=c(1,3))
plot(ev.reg1$fitted.values,rstudent(ev.reg1), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(ev.reg1))
qqnorm(rstudent(ev.reg1))
abline(a=0,b=1)


# Regress log total compensation on log market value and cube root of all other EV variables
ev.reg2 <- lm(log(TDC1) ~ log(mv) + dlc_cr + dltt_cr + pstk_cr + che_cr, data = train.b)
summary(ev.reg2)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(ev.reg2$fitted.values,rstudent(ev.reg2), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(ev.reg2))
qqnorm(rstudent(ev.reg2))
abline(a=0,b=1)

# Create dataset of relevent variables
ev.reg2.AIC <- step(ev.reg1, scope=formula(ev.reg2), direction="forward", k=2)
ev.reg2.BIC <- step(ev.reg1, scope=formula(ev.reg2), direction="forward", k=log(nrow(train.b)))
summary(ev.reg2.BIC)


#### Kitchen Sink Regression #########

train.b.select <- train.b[,c("TDC1", "logmv", "dltt_cr", "dlc_cr", "pstk_cr", "che_cr", 
                         "Industry_Code4", "bkvlps", "croa", "dpr", 
                         "epsfx", "gmargin","roa", "roe", "aturn", "dr",
                         "der", "wcap", "fincf_cr", "ivncf_cr", "oancf_cr")]


# Create null and full models
comb.reg.null <- lm(log(TDC1) ~ 1, data = train.b.select)
comb.reg.full <- lm(log(TDC1) ~ . + .^2, data = train.b.select)

# Run BIC on combined regression
comb.reg.BIC2 <- step(comb.reg.null, scope=formula(comb.reg.full), direction="forward", k=log(nrow(train.b)))
summary(comb.reg.BIC2)
