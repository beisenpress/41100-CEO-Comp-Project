setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Project")

# Import compensation data from Compustat ExecuComp dataset
ExecuComp <- read.csv("Execucomp GVKey All.csv")

# Import financials data from Compustat North America dataset
Financials <- read.csv("Initial Proposal Data.csv")

###### Add Measures of financial data
attach(Financials)
Financials$mv = csho * prcc_c
# Financials$ev = (mv + dlc + dltt + pstk) - che
Financials$ev = (mv + dcpstk + dltt) - che

########## CEO Flags ##############
# Create an indicator variable for CEO using the PCEO variable
ExecuComp$CEO_Flag1 <- ifelse(ExecuComp$PCEO == "CEO", 1, 0)

# Create an indicator variable for CEO using the TITLE variable
ExecuComp$CEO_Flag2 <- ifelse((regexpr("Chief Executive Officer", ExecuComp$TITLE) + 1)>0,1,0)

# Create an indicator variable for CEO using the PCEO variable
ExecuComp$CEO_Flag3 <- ifelse(ExecuComp$CEOANN == "CEO", 1, 0)

# Create a flag for CEO using either the PCEO variable or the CEOANN variable or the TITLE Variable
ExecuComp$CEO_Flag0 <- ExecuComp$CEO_Flag1|ExecuComp$CEO_Flag2|ExecuComp$CEO_Flag3

# Narrow down data to only CEO Data
ceo.comp <- combined[which(ExecuComp$CEO_Flag0 == 1),]

######### Merge Data #######################

# Merge two dataset
combined.all <- merge(ceo.comp,Financials, by = "GVKEY")

############## Narrow down data ################

# Get only publically traded companies
combined.public <- combined.all[which(combined.all$EXCHANGE %in% c("NYS","ASE","NAS")),]

######### Simple Regression  #######

# Drop observations where CEO has positive pay
combined.public1 <- combined.public[which(combined.public$TDC1 >0),]
combined.public2 <- combined.public1[which(combined.public1$ev >0),]

# Regress total compensation on total assets, total Cash, Dividents, and total liabilities
reg1 <- lm(log(TDC1) ~ log(ev), data = combined.public2)
summary(reg1)
plot(combined.public1$ev, log(combined.public1$TDC1))

plot(reg1$fitted.values,rstudent(reg1), pch=20, main = "Fitted Values and Studentized Residuals")







###### Check of Exchanges ########
exchg <- unique(data.frame(combined$exchg, combined$EXCHANGE))
exchg1 <- c(paste(combined$exchg, combined$EXCHANGE))
table(exchg1)
exh.errors <- combined[which(combined$exchg == 19 & combined$EXCHANGE == "NYS"),]
# One companty is 1 NYS.  It is KCI, which is not traded
# One company is 0 NYS.  It is intergys, which was aquired in 2015
# Two compaanies are 19 NYS. One is quicksilver, which went bankrupt in 2015
# So lets use the exchange varialbe from Execucomp
