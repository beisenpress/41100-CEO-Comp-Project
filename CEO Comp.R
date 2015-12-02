

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