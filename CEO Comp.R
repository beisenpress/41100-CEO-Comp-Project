

#setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Project")
setwd("/Users/Leixin Zhao/Desktop/2015_16 Fall/41100 Applied Regression/Final Paper/41100-CEO-Comp-Project")
#setwd("~/Documents/Booth/Class/41100 - Regressions/Project")

# Import compensation data from Compustat ExecuComp dataset
ExecuComp <- read.csv("Execucomp GVKey All.csv")

# Import financials data from Compustat North America dataset
Financials <- read.csv("Select Total X Variables.csv")

# Change "gvkey" to "GVKEY" so the data will merge
names(Financials)[1] <- "GVKEY"

###### Add variables to financial data
# Calculate Market Value
Financials$mv = Financials$csho * Financials$prcc_c

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

# Calculate the components of ev as a percent of ev.
combined4$dlc_ev <- combined4$dlc / combined4$ev
combined4$dltt_ev <- combined4$dltt / combined4$ev
combined4$pstk_ev <- combined4$pstk / combined4$ev
combined4$che_ev <- combined4$che / combined4$ev

################# Select Industry ##################################

# First pass, look at the first 2 digits of NAICS codes as separate industries
combined4$Industry_Code1 <- factor(substr(combined4$NAICS,0,2))
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


BIC <- c(All_Dummy=extractAIC(Industry_Dummy_Reg1,k=log(nrow(combined4))),
         First_Digit=extractAIC(Industry_Dummy_Reg2,k=log(nrow(combined4))),
         Manual_Groups=extractAIC(Industry_Dummy_Reg3,k=log(nrow(combined4))),
         Selected_Dummy=extractAIC(Industry_Dummy_Reg4,k=log(nrow(combined4)))
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

##################### Regressions  #################################

# Regress log total compensation on log market value
reg1 <- lm(log(TDC1) ~ log(mv) , data = train)
summary(reg1)
plot(log(train$mv),log(train$TDC1),pch=20)
abline(reg1)
plot(reg1$fitted.values,rstudent(reg1), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)

# Regress log total compensation on log market value. 
#Also control composition of EV
reg2 <- lm(log(TDC1) ~ log(mv) + dlc_ev + dltt_ev + pstk_ev + che_ev, data = train)
summary(reg2)
plot(reg2$fitted.values,rstudent(reg2), pch=20, main = "Fitted Values and Studentized Residuals")
abline(h=0)

# Compare the two regressions on EV using BIC.
BIC <- c(reg1=extractAIC(reg1, k=log(nrow(train)))[2],
         reg2=extractAIC(reg2, k=log(nrow(train)))[2])
BIC

# Apply the formula e^((-1/2)*BIC) to each element of the array. 
eBIC <- exp(-0.5*(BIC-min(BIC)))

# Calculate the probabliliy by dividing each eBIC by the sum of all eBIC values
probs <- eBIC/sum(eBIC)
round(probs, 5)

###### Diagnostics: Check of Exchanges ########
exchg <- unique(data.frame(combined$exchg, combined$EXCHANGE))
exchg1 <- c(paste(combined$exchg, combined$EXCHANGE))
table(exchg1)
exh.errors <- combined[which(combined$exchg == 19 & combined$EXCHANGE == "NYS"),]
# One companty is 1 NYS.  It is KCI, which is not traded
# One company is 0 NYS.  It is intergys, which was aquired in 2015
# Two compaanies are 19 NYS. One is quicksilver, which went bankrupt in 2015
# So lets use the exchange varialbe from Execucomp

########## Diagnostics CEO Flags ##############
# Check that PCEO and CEOANN are identical. 
# Because we are using the most recent year's data, this should return "integer(0)"
which(ExecuComp$PCEO != ExecuComp$CEOANN)

# Create an indicator variable for CEO using the CEOANN variable
ExecuComp$CEO_Flag1 <- ifelse(ExecuComp$CEOANN == "CEO", 1, 0)

# Create an indicator variable for CEO using the TITLE variable
ExecuComp$CEO_Flag2 <- ifelse((regexpr("Chief Executive Officer", ExecuComp$TITLE) + 1)>0,1,0)

# Create a flag for CEO using the CEOANN variable and the TITLE Variable
ExecuComp$CEO_Flag0 <- ExecuComp$CEO_Flag1|ExecuComp$CEO_Flag2

# Narrow down data to only CEO Data
ceo.comp.test <- ExecuComp[which(ExecuComp$CEO_Flag0 == 1),]

# Check for duplicate CEOs
ceo.dups <- ceo.comp.test[(duplicated(ceo.comp.test$GVKEY) | duplicated(ceo.comp.test$GVKEY, fromLast = TRUE)),]

# There a a lot of duplicate CEOs.  Therefore, we should just use CEOANN

