


#setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Project/41100-CEO-Comp-Project")
#setwd("/Users/Leixin Zhao/Desktop/2015_16 Fall/41100 Applied Regression/Final Paper/41100-CEO-Comp-Project")
#setwd("~/Documents/Booth/Class/41100 - Regressions/Project")

# Import compensation data from Compustat ExecuComp dataset
ExecuComp <- read.csv("Execucomp GVKey All.csv")

# Import financials data from Compustat North America dataset
Financials <- read.csv("X Financial Variables Data.csv")

# Change "gvkey" to "GVKEY" so the data will merge
names(Financials)[1] <- "GVKEY"

###### Add variables to financial data
# Calculate Market Value
Financials$mv = Financials$csho * Financials$prcc_f

# Calculate enterprise value
Financials$ev = rowSums(cbind(Financials$mv, Financials$dlc, Financials$dltt, 
                              Financials$pstk, -Financials$che), na.rm=TRUE)


################ Select CEOs from Data ####################
# Barnes & Noble's CEO is not flagged.  Fix that manually:
ExecuComp$CEOANN[which(ExecuComp$EXECID == 45006)] = "CEO"

ceo.comp <- ExecuComp[which(ExecuComp$CEOANN == "CEO"),]

# Check for any duplicates.  This should be false.
any(duplicated(ceo.comp$GVKEY))

############ Select type of financial data used #########
# COMPUSTAT has a variable Industry Format (INDFMT.) 
# From compustat, it "describes the general industry presentation for the associated 
# data record. This allows you to view a company (such as Aetna) as an industrial company or 
# as a financial services company."
# To avoid duplicates we are going to select the industiral companies.
financials2 <- Financials[which(Financials$indfmt == "INDL"),]

# Select only 2014 data
financials3 <- financials2[which(financials2$fyear == 2014),]

# Select only companies that report in USD data
financials4 <- financials3[which(financials3$curcd == "USD"),]

# Check for any duplicates.
merge.dupes <- financials4[(duplicated(financials4$GVKEY) | duplicated(financials4$GVKEY, fromLast = TRUE)),]

# Duplicate check. Should be false
any(duplicated(financials4$GVKEY))

######### Merge Data #######################

# Merge two datasets
combined1 <- merge(ceo.comp,financials4, by = "GVKEY")

# Check for duplicates
any(duplicated(combined1$GVKEY))

############## Narrow down data ################

# Get only publically traded companies
combined2 <- combined1[which(combined1$EXCHANGE %in% c("NYS","ASE","NAS")),]

# Remove CEOs that are paid $1 or less.  Observations are not part of what we want to predict.
combined3 <- combined2[which(combined2$TDC1 > 0.001),]

# Put removed CEOs into a dataset
removedCEOs <- combined2[which(combined2$TDC1 <= 0.001),]

############# Additinal Variables ############
# Create a new dataset, combined4, to add new variables
combined4 <- combined3

# Create acalculated TDC1 variable.  This will help us break up the components later.
combined4$TDC1_Calc <- rowSums(cbind(combined4$SALARY, combined4$BONUS, combined4$NONEQ_INCENT, 
                                     combined4$STOCK_AWARDS_FV, combined4$OPTION_AWARDS_FV, 
                                     combined4$DEFER_RPT_AS_COMP_TOT, combined4$OTHCOMP), 
                               na.rm=TRUE)

# Calculate financial ratios.
combined4$croa <- combined4$oancf / combined4$at
combined4$dpr <- combined4$dvc / combined4$ni
combined4$gmargin <- (combined4$revt - combined4$cogs) / combined4$revt
combined4$pe <- combined4$mv / combined4$revt
combined4$margin <- combined4$ni / combined4$revt
combined4$roa <- combined4$ni / combined4$at
combined4$roe <- combined4$ni / combined4$seq
combined4$aturn <- combined4$revt / combined4$at
combined4$invturn <- combined4$sale / combined4$invt
combined4$dr <- combined4$dt / combined4$at
combined4$der <- combined4$dt / combined4$seq
combined4$atr <- (combined4$che + combined4$artfs) / combined4$lct
combined4$ic <- combined4$ebit / combined4$xint

# Cube root transformations select financial variables
combined4$der <- sign(combined4$der)*(abs(combined4$der)^(1/3))
combined4$roe <- sign(combined4$roe)*(abs(combined4$roe)^(1/3))
combined4$wcap <- sign(combined4$wcap)*(abs(combined4$wcap)^(1/3))

# Filter our outlier of financial variables. Clean N/As
combined4[is.na(combined4)] <- 0
combined4 <-combined4[which(combined4$bkvlps < 200),]
combined4 <-combined4[which(combined4$dpr >= 0),]
combined4 <-combined4[which(combined4$dpr < 100),]
combined4 <-combined4[which(combined4$der < 100),]
combined4 <-combined4[which(combined4$gmargin > -100),]

# Fill in missing enterprise value variables with zeros
combined4$dlc[is.na(combined4$dlc)] <- 0
combined4$dltt[is.na(combined4$dltt)] <- 0
combined4$pstk[is.na(combined4$pstk)] <- 0
combined4$che[is.na(combined4$che)] <- 0

# Create new variables for the cube root of enterprise value variables
combined4$dlc_cr <- sign(combined4$dlc)*(abs(combined4$dlc)^(1/3))
combined4$dltt_cr <- sign(combined4$dltt)*(abs(combined4$dltt)^(1/3))
combined4$pstk_cr <- sign(combined4$pstk)*(abs(combined4$pstk)^(1/3))
combined4$che_cr <- sign(combined4$che)*(abs(combined4$che)^(1/3))

# Create new variables for the cube root of enterprise value variables
combined4$fincf_cr <- sign(combined4$fincf)*(abs(combined4$fincf)^(1/3))
combined4$ivncf_cr <- sign(combined4$ivncf)*(abs(combined4$ivncf)^(1/3))
combined4$oancf_cr <- sign(combined4$oancf)*(abs(combined4$oancf)^(1/3))

################# Select Industry ##################################

# First pass, look at the first 2 digits of NAICS codes as separate industries
combined4$Industry_Code1 <- factor(substr(combined4$NAICS,0,2))
par(mfrow=c(1,1))
plot(combined4$Industry_Code1,log(combined4$TDC1),main="Log of TDC by Industry Code",xlab="Industry Code",ylab="Log of TDC1")
# Seems to be additional consolidation that can be performed
Industry_Dummy_Reg1 <- lm(log(TDC1) ~ Industry_Code1, data = combined4)
summary(Industry_Dummy_Reg1)

# Second Pass, use the first digit of NAICS codes as separate industries with goal to maintain spread
combined4$Industry_Code2 <- factor(substr(combined4$NAICS,0,1))
plot(combined4$Industry_Code2,log(combined4$TDC1))
# This technique works for some -- 1, 2, 4, 7, 8, 9 -- does not work well for 3, 5, or 6
Industry_Dummy_Reg2 <- lm(log(TDC1) ~ Industry_Code2, data = combined4)

# Third Pass, Check results 1 - 3 for common sense industry groupings
# Group 1: 11, 21, 22, 56 -- Agriculture, Hunting, Mining, Oil Extraction, Utilities, Waste Management
# Group 2: 23, 31, 32, 33, 42, 44, 45, 48, 49 -- Construction, Manufacturing, Trade, and Logistics
# Group 3: 51, 54, 55 -- Information and Professional Services
# Group 4: 52, 53 -- Finance, Insurance, Real Estate
# Group 4: 61, 62, 71, 81, 92 -- Public Services, Art and Entertainment
# Group 5: 99 -- Unclassified
combined4$Industry_Code3 <- factor(substr(combined4$NAICS,0,2))
levels(combined4$Industry_Code3) <- list("Outdoor Services" = c("11","21","22","56"), 
                                         "Supply Chain" = c("23","31","32","33","42","44","45","48","49"),
                                         "Information Services" = c("51","54","55"),
                                         "Financial Services" = c("52","53"),
                                         "Public Services" = c("61","62","71","72","81","92"),
                                         "Other" = c("99"))
plot(combined4$Industry_Code3,log(combined4$TDC1))
Industry_Dummy_Reg3 <- lm(log(TDC1) ~ Industry_Code3, data = combined4)
summary(Industry_Dummy_Reg3)
# Other than unclassified, these groupings appear to be the same
by(log(combined4$TDC1),combined4$Industry_Code3,function(x)mean(x))
by(log(combined4$TDC1),combined4$Industry_Code3,function(x)sd(x))

# Final Pass, run regression on NAICS codes, and create dummy variables for anything statistically different than an average industry
combined4$Industry_Code4 <- factor(substr(combined4$NAICS,0,2))
# Need to change the reference value to a standard value -- use 52
combined4 <- within(combined4, Industry_Code4 <- relevel(Industry_Code4, ref="52"))
Industry_Dummy_Reg4 <- lm(log(TDC1) ~ Industry_Code4, data = combined4)
summary(Industry_Dummy_Reg4)
# Industry Code 21, 31, 32, and 61 are statistically significant when compared to Industry Code 52 -- Average, tight spread, no outliers
levels(combined4$Industry_Code4) <- list("21" = "21",
                                         "31" = "31",
                                         "32" = "32",
                                         "61" = "61",
                                         "Standard" = c("11","22","23","33","42","44","48","45","49","51","52","53","54","55","56","62","71","72","81","92","99"))
combined4 <- within(combined4,Industry_Code4 <- relevel(Industry_Code4, ref="Standard"))
plot(combined4$Industry_Code4,log(combined4$TDC1))
Industry_Dummy_Reg4 <- lm(log(TDC1) ~ Industry_Code4, data = combined4)
summary(Industry_Dummy_Reg4)


BIC <- c(All_Dummy=extractAIC(Industry_Dummy_Reg1,k=log(nrow(combined4)))[2],
         First_Digit=extractAIC(Industry_Dummy_Reg2,k=log(nrow(combined4)))[2],
         Manual_Groups=extractAIC(Industry_Dummy_Reg3,k=log(nrow(combined4)))[2],
         Selected_Dummy=extractAIC(Industry_Dummy_Reg4,k=log(nrow(combined4)))[2]
)

BIC

# When comparing BIC, we see that having a standard industry and using a dummy for industries that are statistically different has the lowest BIC of the four ways we treated industry
# The two lowest models were ones with consolidated groupings of industries. This seems to suggest that individual industries do not have substantial predictive value on pay
# The regressions were penalized greatly for increasing the complexity -- especially with the first regression using a dummy for all industries
# Going forward our project will account for industry by using the Selected Industry factor (Industry_Code5) and allowing for interaction with other variables

############## Split data into training and test ###################
# Set seed so the results are replicable 
set.seed(9)

# Select a random sample of rows
samples <- sort(sample.int(nrow(combined4), 0.80*nrow(combined4)))

# Subset the data into training and test datasets.
train <- combined4[samples,] 
test <- combined4[-samples,]


##################### Enterprise Value Regressions  #################################

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

# Both AIC and BIC choose to add Long Term Debt (dltt) and Preferred Stock (pstk)
# AIC also adds Short Term Debt (dlc)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(ev.reg2.BIC$fitted.values,rstudent(ev.reg2.BIC), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(ev.reg2.BIC))
qqnorm(rstudent(ev.reg2.BIC))
abline(a=0,b=1)

# Show diagnostic plots by X variable
par(mfrow=c(1,2))
plot(ev.reg1.diagnositcs$pstk_cr,rstudent(ev.reg2.BIC), pch=20, main = "Cube root of Preferred Stock")
plot(ev.reg1.diagnositcs$dltt_cr,rstudent(ev.reg2.BIC), pch=20, main = "Cube root of Long-Term Debt")


# Add the industry classification to the regression
ev.reg3 <- lm(log(TDC1) ~ log(mv) + dlc_cr + dltt_cr + pstk_cr + che_cr + Industry_Code4, data = train)
summary(ev.reg3)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(ev.reg3$fitted.values,rstudent(ev.reg3), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(ev.reg3))
qqnorm(rstudent(ev.reg3))
abline(a=0,b=1)

# Re-do AIC and BIC with industry classification in the full model
ev.reg3.AIC <- step(ev.reg1, scope=formula(ev.reg3), direction="forward", k=2)
ev.reg3.BIC <- step(ev.reg1, scope=formula(ev.reg3), direction="forward", k=log(nrow(train)))
summary(ev.reg3.BIC)

# Neither AIC nor BIC adds industry classification

# Compare the two regressions on EV using BIC.
BIC <- c(ev.reg1=extractAIC(ev.reg1, k=log(nrow(train)))[2],
         ev.reg2=extractAIC(ev.reg2, k=log(nrow(train)))[2],
         ev.reg2.BIC=extractAIC(ev.reg2.BIC, k=log(nrow(train)))[2])
BIC

# Apply the formula e^((-1/2)*BIC) to each element of the array. 
eBIC <- exp(-0.5*(BIC-min(BIC)))

# Calculate the probabliliy by dividing each eBIC by the sum of all eBIC values
probs <- eBIC/sum(eBIC)
round(probs, 5)

############################# Financial Variable regressions #############################

#Select columns for analysis
trainclean <- train[c("TDC1","bkvlps","croa","dpr","epsfx","gmargin","roa","roe","aturn","dr","der", "wcap")]
#"atr", "invturn","ic","pe","margin","naics" need to solve for inf issue
#"mv", "ev", "fincf", "ivncf", "oancf", "at", "lt", "seq", "revt", "xopr", "ebitda", "Industry_Code4"

# Plot TDC1 against all financial variables Logged
# Plots will be missing points where there are missing values, but thats OK for now
par(mfrow=c(1,3))

plot(trainclean$bkvlps,log(trainclean$TDC1),pch=20,xlab = "Book Value per Share", ylab = "Log of TDC1")
plot(trainclean$croa,log(trainclean$TDC1),pch=20,xlab = "Cash Return on Total Assets", ylab = "Log of TDC1")
plot(trainclean$dpr,log(trainclean$TDC1),pch=20,xlab = "Dividend Payout Ratio", ylab = "Log of TDC1")

plot(trainclean$epsfx,log(trainclean$TDC1),pch=20,xlab = "EPS", ylab = "Log of TDC1")
plot(trainclean$gmargin,log(trainclean$TDC1),pch=20,xlab = "Gross Profit Margin", ylab = "Log of TDC1")
plot(trainclean$roa,log(trainclean$TDC1),pch=20,xlab = "Return on Asset", ylab = "Log of TDC1")

plot(trainclean$roe,log(trainclean$TDC1),pch=20,xlab = "Cube Root of Return on Equity", ylab = "Log of TDC1")
plot(trainclean$aturn,log(trainclean$TDC1),pch=20,xlab = "Asset Turnover", ylab = "Log of TDC1")
plot(trainclean$dr,log(trainclean$TDC1),pch=20,xlab = "Debt Ratio", ylab = "Log of TDC1")

plot(trainclean$der,log(trainclean$TDC1),pch=20,xlab = "Cube Root of Debt to Equity Ratio", ylab = "Log of TDC1")
plot(trainclean$wcap,log(trainclean$TDC1),pch=20,xlab = "Cube Root of Working Capital", ylab = "Log of TDC1")

#Forward stepwise BIC

fv.base <- lm(log(TDC1) ~ 1, data=trainclean)
fv.full <- lm(log(TDC1) ~ ., data=trainclean)

fv.reg.AIC <- step(fv.base, scope=formula(fv.full), direction="forward", k=2)
fv.reg.BIC <- step(fv.base, scope=formula(fv.full), direction="forward", k=log(nrow(trainclean)))

summary(fv.reg.BIC)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(fv.reg.BIC$fitted.values,rstudent(fv.reg.BIC), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(fv.reg.BIC))
qqnorm(rstudent(fv.reg.BIC))

################# Cash Flow Regression ##################################

#Select columns for analysis
cf.train <- train[c("TDC1","fincf", "ivncf", "oancf")]
cf.train$fincf_cr <- sign(cf.train$fincf)*(abs(cf.train$fincf)^(1/3))
cf.train$ivncf_cr <- sign(cf.train$ivncf)*(abs(cf.train$ivncf)^(1/3))
cf.train$oancf_cr <- sign(cf.train$oancf)*(abs(cf.train$oancf)^(1/3))


#Identify how many CEOs have all positive cash flows.  Only 6 out of the 1722 observations have all positive cash flows.
cf.train.f <- cf.train[which(cf.train$fincf > 0),] #482 out of 1722 variables remain
cf.train.i <- cf.train[which(cf.train$ivncf > 0),]  #160 out of 1722 remain
cf.train.o <- cf.train[which(cf.train$oancf > 0),]  #1282 out of 1722 remain
#Many of the CEOs are in companies with at least one of the cash flows as a negative value.  
#Operating Cash Flow is the best indication of a firm's performance and profitability.  Rather Investing and Financing
#mean that the firm is just taking out or selling assets.

#Plot the CFs against log(TDC1).  
frame()
par(mfrow=c(2,3))

#Plot CFs
plot(cf.train$fincf,log(cf.train$TDC1),pch=20,xlab = "Financing Cash Flow", ylab = "Log of TDC1")
plot(cf.train$ivncf,log(cf.train$TDC1),pch=20,xlab = "Investing Cash Flow", ylab = "Log of TDC1")
plot(cf.train$oancf,log(cf.train$TDC1),pch=20,xlab = "Operating Cash Flow", ylab = "Log of TDC1")

#Plot Cube Roots
plot(cf.train$fincf_cr,log(cf.train$TDC1),pch=20,xlab = "Financing Cash Flow Cube Root", ylab = "Log of TDC1")
plot(cf.train$ivncf_cr,log(cf.train$TDC1),pch=20,xlab = "Investing Cash Flow Cube Root", ylab = "Log of TDC1")
plot(cf.train$oancf_cr,log(cf.train$TDC1),pch=20,xlab = "Operating Cash Flow Cube Root", ylab = "Log of TDC1")


#OANCF demonstrates a high s value.  It is known that there may be collinearity considering that salary is an operating expense.  
#However, OANCF contains a number of other inputs and CEO compensation is just one of those items.  The ratio between CEO compensation and 
#OANCF would be neglible.  As such, OANCF should remain in the regression.

#Variable selection using AIC and BIC without logs
cf.null <- lm(log(cf.train$TDC1) ~ 1, data=cf.train)
cf.full <- lm(log(cf.train$TDC1) ~ ., data=cf.train)

cf.reg.AIC <- step(cf.null, scope=formula(cf.full), direction="forward", k=2)
cf.reg.BIC <- step(cf.null, scope=formula(cf.full), direction="forward", k=log(nrow(cf.train)))
cf.reg.AICi <- step(cf.reg.AIC, scope= ~. + .^2, direction="forward", k=2)
cf.reg.BICi <- step(cf.reg.BIC, scope= ~. + .^2, direction="forward", k=log(nrow(cf.train)))


#Determine the best model
BIC <- c(cf.reg.AIC=extractAIC(cf.reg.AIC, k=log(nrow(cf.train)))[2],
         cf.reg.BIC=extractAIC(cf.reg.BIC, k=log(nrow(cf.train)))[2],
         cf.reg.AICi=extractAIC(cf.reg.AICi, k=log(nrow(cf.train)))[2],
         cf.reg.BICi=extractAIC(cf.reg.BICi, k=log(nrow(cf.train)))[2])
eBIC <- exp(-0.5*(BIC-min(BIC)))

round(probs <- eBIC/sum(eBIC,1),5)
# AICi is the best model from stepwise forward regression

summary(cf.reg.AICi)
#lm(formula = log(cf.train$TDC1) ~ oancf_cr + ivncf_cr + fincf_cr + 
#     oancf + oancf_cr:fincf_cr + oancf_cr:ivncf_cr + ivncf_cr:oancf + 
#     fincf_cr:oancf + ivncf_cr:fincf_cr, data = cf.train)

summary(cf.reg.BICi)
#lm(formula = log(cf.train$TDC1) ~ oancf_cr + ivncf_cr + fincf_cr + 
#     oancf + oancf_cr:fincf_cr + oancf_cr:ivncf_cr + ivncf_cr:oancf + 
#     fincf_cr:oancf + ivncf_cr:fincf_cr, data = cf.train)

# Show diagnostic plots
par(mfrow=c(1,3))
plot(cf.reg.AICi$fitted.values,rstudent(cf.reg.AICi), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)
hist(rstudent(blah))
qqnorm(rstudent(blah))
abline(a=0,b=1)
