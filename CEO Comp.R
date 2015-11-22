setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Project")

# Import compensation data from Compustat ExecuComp dataset
ExecuComp <- read.csv("Execucomp GVKey All.csv")

# Import financials data from Compustat North America dataset
Financials <- read.csv("Select Total X Variables.csv")

# Change "gvkey" to "GVKEY" so the data will merge
names(Financials)[1] <- "GVKEY"

###### Add variables to financial data
Financials$mv = Financials$csho * Financials$prcc_c
# Financials$ev = (mv + dlc + dltt + pstk) - che
Financials$ev = rowSums(cbind(Financials$mv, Financials$dcpstk, Financials$dltt, -Financials$che), na.rm=TRUE)

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

########### Scale Variables ##################
combined3$log.TDC1 <- log(combined3$TDC1)

# Create acalculated TDC1 variable.  This will help us break up the components later.
combined3$TDC1_Calc <- rowSums(cbind(combined3$SALARY, combined3$BONUS, combined3$NONEQ_INCENT, 
                                     combined3$STOCK_AWARDS_FV, combined3$OPTION_AWARDS_FV, 
                                     combined3$DEFER_RPT_AS_COMP_TOT, combined3$OTHCOMP), 
                               na.rm=TRUE)

######### Simple Regression  #######

# Regress total compensation on total assets, total Cash, Dividents, and total liabilities
reg1 <- lm(log(TDC1) ~ ev + ebitda, data = combined3)
summary(reg1)
plot(log(combined.public1$emp), log(combined.public1$TDC1))

plot(reg1$fitted.values,rstudent(reg1), pch=20, main = "Fitted Values and Studentized Residuals")


###### Diagnostics: Check of Exchanges ########
exchg <- unique(data.frame(combined$exchg, combined$EXCHANGE))
exchg1 <- c(paste(combined$exchg, combined$EXCHANGE))
table(exchg1)
exh.errors <- combined[which(combined$exchg == 19 & combined$EXCHANGE == "NYS"),]
# One companty is 1 NYS.  It is KCI, which is not traded
# One company is 0 NYS.  It is intergys, which was aquired in 2015
# Two compaanies are 19 NYS. One is quicksilver, which went bankrupt in 2015
# So lets use the exchange varialbe from Execucomp
