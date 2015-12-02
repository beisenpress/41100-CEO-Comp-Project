##### Diagnostic Code ##########

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

