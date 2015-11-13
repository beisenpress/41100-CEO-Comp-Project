setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Project")

# Import compensation data from Compustat ExecuComp dataset
ExecuComp <- read.csv("Execucomp GVKey All.csv")

# Import financials data from Compustat North America dataset
Financials <- read.csv("NA GVKey 1.csv")

# Merge two dataset
combined <- merge(ExecuComp,Financials, by = "GVKEY")


########## CEO Flags ##############
# Create an indicator variable for CEO using the PCEO variable
combined$CEO_Flag1 <- ifelse(combined$PCEO == "CEO", 1, 0)

# Create an indicator variable for CEO using the TITLE variable
combined$CEO_Flag2 <- ifelse((regexpr("Chief Executive Officer", combined$TITLE) + 1)>0,1,0)

# Create an indicator variable for CEO using the PCEO variable
combined$CEO_Flag3 <- ifelse(combined$CEOANN == "CEO", 1, 0)

# Get a subset of data where the two CEO flags do not macth
CEO_Errors <- combined[which(combined$CEO_Flag0 != combined$CEO_Flag3),]

# Create a flag for CEO using either the PCEO variable or the CEOANN variable or the TITLE Variable
combined$CEO_Flag0 <- combined$CEO_Flag1|combined$CEO_Flag2|combined$CEO_Flag3
