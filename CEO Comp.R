

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
# From compustat, it “describes the general industry presentation for the associated 
# data record. This allows you to view a company (such as Aetna) as an industrial company or 
# as a financial services company.”
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

removedCEOs <- combined2[which(combined2$TDC1 <= 0.001),]

############# Additinal Variables ############
# Create a new dataset, combined4, to add new variables
combined4 <- combined3

# Create acalculated TDC1 variable.  This will help us break up the components later.
combined4$TDC1_Calc <- rowSums(cbind(combined4$SALARY, combined4$BONUS, combined4$NONEQ_INCENT, 
                                     combined4$STOCK_AWARDS_FV, combined4$OPTION_AWARDS_FV, 
                                     combined4$DEFER_RPT_AS_COMP_TOT, combined4$OTHCOMP), 
                               na.rm=TRUE)
# Fill in missing with zeros
combined4$dlc[is.na(combined4$dlc)] <- 0
combined4$dltt[is.na(combined4$dltt)] <- 0
combined4$pstk[is.na(combined4$pstk)] <- 0
combined4$che[is.na(combined4$che)] <- 0

# Calculate the components of ev as a percent of ev.
combined4$dlc_ev <- combined4$dlc / combined4$ev
combined4$dltt_ev <- combined4$dltt / combined4$ev
combined4$pstk_ev <- combined4$pstk / combined4$ev
combined4$che_ev <- combined4$che / combined4$ev

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
                                         "Standard" = c("11","22","23","33","42","44","48","45","49","51","52","53","54","55","62","71","81","92","99"))
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

# Plot TDC1 against all financial variables Logged
# Plots will be missing points where there are missing values, but thats OK for now
par(mfrow=c(1,3))
plot(log(train$mv),log(train$TDC1),pch=20,xlab = "Log of Market Value", ylab = "Log of TDC1", main = "Market Value")
plot(log(train$dlc),log(train$TDC1),pch=20,xlab = "Log of Debt in Current Liabilities", ylab = "Log of TDC1", main = "Short-term Debt")
plot(log(train$dltt),log(train$TDC1),pch=20,xlab = "Log of Long Term Debt", ylab = "Log of TDC1", main = "Long Term Debt")
plot(log(train$pstk),log(train$TDC1),pch=20,xlab = "Log of Preferred Stock", ylab = "Log of TDC1", main = "Preferred Stock")
plot(log(train$che),log(train$TDC1),pch=20,xlab = "Log of Cash", ylab = "Log of TDC1", main = "Cash")

# Market value has the best relationship, followed by Long term debt.


# Regress log total compensation on log market value
reg1 <- lm(log(TDC1) ~ log(mv) , data = train)
summary(reg1)
par(mfrow=c(1,1))
plot(log(train$mv),log(train$TDC1),pch=20,xlab = "Log of Market Value", ylab = "Log of TDC1", main = "Market Value")
abline(reg1)

# Can argue market value is more important than makret value because shareholders
# serve as the check on executive compensation, and shareholders care about MV.

# Show diagnosic plots
par(mfrow=c(1,3))
plot(reg1$fitted.values,rstudent(reg1), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(reg1))
qqnorm(rstudent(reg1))
abline(a=0,b=1)

# Examine companies with very low studentized residuals

# Create dataset of relevent variables
reg1.diagnositcs <- train[which(!is.na(train$mv)),]
reg1.diagnositcs$fitted.values <- reg1$fitted.values
reg1.diagnositcs$residuals <- reg1$residuals
reg1.diagnositcs$stresiduals <- rstudent(reg1)
write.csv(reg1.diagnositcs[which(reg1.diagnositcs$stresiduals < -4),c("EXEC_FULLNAME", "CONAME", "TDC1", "mv","stresiduals")], file = "reg1.diagnostics")

# All of the large residuals are negative - i.e. CEOs making way less than we predict.
# Two of the largest (using this trianing sample) are Steve Balmer of Microsoft and
# Warren Buffett of Berkshire Hathaway.  It is hard to predict for CEOs that simply 
# choose to accept a lower salary.

# Regress log total compensation on log market value. 
#Also control composition of EV
reg2 <- lm(log(TDC1) ~ log(mv) + dlc + dltt + pstk + che, data = train)
summary(reg2)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(reg2$fitted.values,rstudent(reg2), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(reg2))
qqnorm(rstudent(reg2))
abline(a=0,b=1)

# Create dataset of relevent variables
reg2.AIC <- step(reg1, scope=formula(reg2), direction="forward", k=2)
reg2.BIC <- step(reg1, scope=formula(reg2), direction="forward", k=log(nrow(train)))
summary(reg2.BIC)


# Regress log total compensation on log market value. 
#Also control composition of EV
reg3 <- lm(log(TDC1) ~ log(mv) + dlc + dltt + che, data = train)
summary(reg3)

# Show diagnosic plots
par(mfrow=c(1,3))
plot(reg3$fitted.values,rstudent(reg3), pch=20, main = "Fitted Values and Studentized Residuals")
hist(rstudent(reg3))
qqnorm(rstudent(reg3))
abline(a=0,b=1)

# Create dataset of relevent variables
reg3.AIC <- step(reg1, scope=formula(reg3), direction="forward", k=2)
reg3.BIC <- step(reg1, scope=formula(reg3), direction="forward", k=log(nrow(train)))
summary(reg3.AIC)


# Compare the two regressions on EV using BIC.
BIC <- c(reg1=extractAIC(reg1, k=log(nrow(train)))[2],
         reg2=extractAIC(reg2, k=log(nrow(train)))[2],
         reg.BIC=extractAIC(reg.BIC, k=log(nrow(train)))[2])
BIC

# Apply the formula e^((-1/2)*BIC) to each element of the array. 
eBIC <- exp(-0.5*(BIC-min(BIC)))

# Calculate the probabliliy by dividing each eBIC by the sum of all eBIC values
probs <- eBIC/sum(eBIC)
round(probs, 5)