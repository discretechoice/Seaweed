###############################################################################
#
#    An empirical comparison of (un)correlated random parameter logit and 
#    hybrid choice models for environmental valuation: which model to use?
#
###############################################################################

# Preliminaries
rm(list = ls(all = TRUE))      
Sys.setenv(LANG = "en")
set.seed(17)

library(psych)
library(Hmisc)
library(PerformanceAnalytics)
library(FactoMineR)
library(cluster)
library(mlogit) 
library(gmnl) 

data <- read.table("ThreeModelComparisonENERGY.txt", header=TRUE) 


#################################
# Getting rid of invalid entries
#################################

# 4 respondents had the same ID, we're not sure how that happened, leaving them out just to be safe
data  <- subset(data,Double_id > 2)

# Leaving out respondents who answered too quickly to the survey (anything less than 6 minutes - not enough time to read the questions)
data  <- subset(data,too_short > 1)

# 47 cases were not within our age range of 18-65
data  <- subset(data,age > 17)
data  <- subset(data,age < 66)

# Getting rid of the protesters
data <- subset(data,ChoiceSum < 30)

# Recoding the missing values (these are coded as 99999 in Pythonbiogeme)
data[data ==99999] <- NA


###########################
# Getting rid of outliers
###########################

# Leaving out those who claimed their household paid 6000 pounds (or more) per year on electricity bills
data  <- subset(data,pay_elecbill < 6000)

# Leaving out those who claimed more than 6 adults lived in the household
data  <- subset(data,num_adults < 7)

# Leaving out those who claimed more than 9 children lived in the household
data  <- subset(data,num_children < 10)

# Leaving out those who claimed they lived 900 miles or more from the coast
data  <- subset(data,distance_coast < 900)


# Leaving out those who claimed "don't know" in the attitudinal questions:
data  <- subset(data,env1 < 5)
data  <- subset(data,env2 < 5)
data  <- subset(data,env3 < 5)
data  <- subset(data,env4 < 5)
data  <- subset(data,env5 < 5)
data  <- subset(data,env6 < 5)
data  <- subset(data,env7 < 5)

# Only keeping those who answered all the soc-dem questions we need:
data  <- subset(data,income < 10)



# Data for England
data.eng <- subset(data, country == 1 )

# Data for Scotland
data.scot <- subset(data, country == 3 )

# Data for Northern Ireland
data.ni <- subset(data, country == 2 )




# Preparing the variables for Table 3
data.eng$cohabit   <- (data.eng$marital_status == 2 | data.eng$marital_status == 5 )
data.scot$cohabit  <- (data.scot$marital_status == 2 | data.scot$marital_status == 5 )
data.ni$cohabit    <- (data.ni$marital_status == 2 | data.ni$marital_status == 5 )

data.eng$employed  <- data.eng$economic_status < 4
data.scot$employed <- data.scot$economic_status < 4
data.ni$employed   <- data.ni$economic_status < 4

data.eng$higheduc  <- data.eng$education > 4
data.scot$higheduc <- data.scot$education > 4
data.ni$higheduc   <- data.ni$education > 4

data.eng$highincome  <- data.eng$income > 4
data.scot$highincome <- data.scot$income > 4
data.ni$highincome   <- data.ni$income > 4

data.eng$green    <- data.eng$buy_green_energy == 1
data.scot$green   <- data.scot$buy_green_energy == 1
data.ni$green     <- data.ni$buy_green_energy == 1


####################################################
#
#     Table 3: Descriptive statistics
#
###################################################

# Continuous variables

# England 
psych::describe(data.eng$age)
psych::describe(data.eng$num_child)
psych::describe(data.eng$ideo)

# Scotland
psych::describe(data.scot$age)
psych::describe(data.scot$num_child)
psych::describe(data.scot$ideo)

# Northern Ireland
psych::describe(data.ni$age)
psych::describe(data.ni$num_child)
psych::describe(data.ni$ideo)

# Dummy variables
# England
round(100*prop.table(table(data.eng$female)),digits=1)
round(100*prop.table(table(data.eng$cohabit)),digits=1)
round(100*prop.table(table(data.eng$employed)),digits=1)
round(100*prop.table(table(data.eng$higheduc)),digits=1)
round(100*prop.table(table(data.eng$highincome)),digits=1)
round(100*prop.table(table(data.eng$green)),digits=1)

# Scotland
round(100*prop.table(table(data.scot$female)),digits=1)
round(100*prop.table(table(data.scot$cohabit)),digits=1)
round(100*prop.table(table(data.scot$employed)),digits=1)
round(100*prop.table(table(data.scot$higheduc)),digits=1)
round(100*prop.table(table(data.scot$highincome)),digits=1)
round(100*prop.table(table(data.scot$green)),digits=1)

# Northern Ireland
round(100*prop.table(table(data.ni$female)),digits=1)
round(100*prop.table(table(data.ni$cohabit)),digits=1)
round(100*prop.table(table(data.ni$employed)),digits=1)
round(100*prop.table(table(data.ni$higheduc)),digits=1)
round(100*prop.table(table(data.ni$highincome)),digits=1)
round(100*prop.table(table(data.ni$green)),digits=1)

####################################################
#
#   Table 4: Responses to the attitudinal questions
#
###################################################

# England

# Env1: "State of the environment"
round(100*prop.table(table(data.eng$env1)),digits=1)

# Env2: "Economic factors"
round(100*prop.table(table(data.eng$env2)),digits=1)

# Env3: "Social factors"
round(100*prop.table(table(data.eng$env3)),digits=1)

# Env4: "How important is protecting the environment to you personally?"
round(100*prop.table(table(data.eng$env4)),digits=1)

# Env5: "You are willing to buy environmentally friendly products even if they cost a little bit more."
round(100*prop.table(table(data.eng$env5)),digits=1)

# Env6: "As an individual you can play a role in protecting the environment in your country."
round(100*prop.table(table(data.eng$env6)),digits=1)

# Env7: "The big polluters should be mainly responsible for making good the environmental damage they cause."
round(100*prop.table(table(data.eng$env7)),digits=1)


# Scotland

# Env1: "State of the environment"
round(100*prop.table(table(data.scot$env1)),digits=1)

# Env2: "Economic factors"
round(100*prop.table(table(data.scot$env2)),digits=1)

# Env3: "Social factors"
round(100*prop.table(table(data.scot$env3)),digits=1)

# Env4: "How important is protecting the environment to you personally?"
round(100*prop.table(table(data.scot$env4)),digits=1)

# Env5: "You are willing to buy environmentally friendly products even if they cost a little bit more."
round(100*prop.table(table(data.scot$env5)),digits=1)

# Env6: "As an individual you can play a role in protecting the environment in your country."
round(100*prop.table(table(data.scot$env6)),digits=1)

# Env7: "The big polluters should be mainly responsible for making good the environmental damage they cause."
round(100*prop.table(table(data.scot$env7)),digits=1)


# Northern Ireland

# Env1: "State of the environment"
round(100*prop.table(table(data.ni$env1)),digits=1)

# Env2: "Economic factors"
round(100*prop.table(table(data.ni$env2)),digits=1)

# Env3: "Social factors"
round(100*prop.table(table(data.ni$env3)),digits=1)

# Env4: "How important is protecting the environment to you personally?"
round(100*prop.table(table(data.ni$env4)),digits=1)

# Env5: "You are willing to buy environmentally friendly products even if they cost a little bit more."
round(100*prop.table(table(data.ni$env5)),digits=1)

# Env6: "As an individual you can play a role in protecting the environment in your country."
round(100*prop.table(table(data.ni$env6)),digits=1)

# Env7: "The big polluters should be mainly responsible for making good the environmental damage they cause."
round(100*prop.table(table(data.ni$env7)),digits=1)


#############################################################

# Preparing parameters for Table 5

# England
n.rows.eng     <- length(data.eng[,1])

ind.columns.eng <- cbind(         data.eng$env1      ,
                                  data.eng$env2      ,
                                  data.eng$env3      ,
                                  data.eng$env4      ,
                                  data.eng$env5      ,
                                  data.eng$env6      ,
                                  data.eng$env7     )

dimnames(ind.columns.eng) <- list(seq(1,n.rows.eng),c( "env1 "  ,     
                                                       "env2 "  ,     
                                                       "env3 "  ,     
                                                       "env4 "  ,     
                                                       "env5 "  ,     
                                                       "env6 "  ,     
                                                       "env7 "  )   ) 
fit.eng <- princomp(ind.columns.eng, cor=TRUE)
eigenvalues.eng <- eigen(cor(ind.columns.eng))


# Scotland
n.rows.scot     <- length(data.scot[,1])

ind.columns.scot <- cbind(        data.scot$env1      ,
                                  data.scot$env2      ,
                                  data.scot$env3      ,
                                  data.scot$env4      ,
                                  data.scot$env5      ,
                                  data.scot$env6      ,
                                  data.scot$env7     )

dimnames(ind.columns.scot) <- list(seq(1,n.rows.scot),c( "env1 "  ,     
                                                         "env2 "  ,     
                                                         "env3 "  ,     
                                                         "env4 "  ,     
                                                         "env5 "  ,     
                                                         "env6 "  ,     
                                                         "env7 "  )   ) 
fit.scot <- princomp(ind.columns.scot, cor=TRUE)
eigenvalues.scot <- eigen(cor(ind.columns.scot))


# Northern Ireland
n.rows.ni     <- length(data.ni[,1])

ind.columns.ni <- cbind(          data.ni$env1      ,
                                  data.ni$env2      ,
                                  data.ni$env3      ,
                                  data.ni$env4      ,
                                  data.ni$env5      ,
                                  data.ni$env6      ,
                                  data.ni$env7     )

dimnames(ind.columns.ni) <- list(seq(1,n.rows.ni),c( "env1 "  ,     
                                                     "env2 "  ,     
                                                     "env3 "  ,     
                                                     "env4 "  ,     
                                                     "env5 "  ,     
                                                     "env6 "  ,     
                                                     "env7 "  )   ) 
fit.ni <- princomp(ind.columns.ni, cor=TRUE)
eigenvalues.ni <- eigen(cor(ind.columns.ni))

help('princomp')

################################################
#
#    Table 5: Exploratory factor analysis
#
################################################

# England

# Eigenvalues (under "values", this corresponds to Column 1)
eigenvalues.eng

# Percentage + Cumulative (Columns 2 + 3)
summary(fit.eng) 

# Factor loadings 
loadings.eng <- loadings(fit.eng) 
loadings.eng[] 

# Scotland

# Eigenvalues (under "values", this corresponds to Column 1)
eigenvalues.scot

# Percentage + Cumulative (Columns 2 + 3)
summary(fit.scot) 

# Factor loadins
loadings.scot <- loadings(fit.scot) 
loadings.scot[] 



# Northern Ireland

# Eigenvalues (under "values", this corresponds to Column 1)
eigenvalues.ni

# Percentage + Cumulative (Columns 2 + 3)
summary(fit.ni) 

# Factor loadins
loadings.ni <- loadings(fit.ni) 
loadings.ni[] 



# Cronbach's alpha
keys <- c(1,1,1,1,1,1,1)

# England
psych::alpha(ind.columns.eng, keys) 

# Scotland
psych::alpha(ind.columns.scot, keys) 

# Northern Ireland
psych::alpha(ind.columns.ni, keys) 

#
#  KMO and Bartlet's test
#

#
# England
#
dataset.eng <- data.frame(ind.columns.eng[,1],
                          ind.columns.eng[,2],
                          ind.columns.eng[,3],
                          ind.columns.eng[,4],
                          ind.columns.eng[,5],
                          ind.columns.eng[,6],
                          ind.columns.eng[,7]
)
names(dataset.eng) = c("env1",
                       "env2",
                       "env3",
                       "env4",
                       "env5",
                       "env6",
                       "env7" )
KMO(dataset.eng)
bartlett.test(dataset.eng[,1:7])

#
# Scotland
#
dataset.scot <- data.frame(ind.columns.scot[,1],
                           ind.columns.scot[,2],
                           ind.columns.scot[,3],
                           ind.columns.scot[,4],
                           ind.columns.scot[,5],
                           ind.columns.scot[,6],
                           ind.columns.scot[,7]
)
names(dataset.scot) = c("env1",
                       "env2",
                       "env3",
                       "env4",
                       "env5",
                       "env6",
                       "env7" )
KMO(dataset.scot)
bartlett.test(dataset.scot[,1:7])

#
# NI
#
dataset.ni   <- data.frame(ind.columns.ni[,1],
                           ind.columns.ni[,2],
                           ind.columns.ni[,3],
                           ind.columns.ni[,4],
                           ind.columns.ni[,5],
                           ind.columns.ni[,6],
                           ind.columns.ni[,7]
)
names(dataset.ni)   = c("env1",
                        "env2",
                        "env3",
                        "env4",
                        "env5",
                        "env6",
                        "env7" )
KMO(dataset.ni)
bartlett.test(dataset.ni[,1:7])




#####################################################

# Preparing data for RPL correlated estimations

# England
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr1hh2")]   <- "attr1hh2.1"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr1hh3")]   <- "attr1hh3.1"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr2coast2")]<- "attr2coast2.1"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr2coast3")]<- "attr2coast3.1"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr3cost")]  <- "attr3cost.1"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr4perk1")] <- "attr4perk1.1"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt1attr4perk2")] <- "attr4perk2.1"

colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr1hh2")]   <- "attr1hh2.2"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr1hh3")]   <- "attr1hh3.2"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr2coast2")]<- "attr2coast2.2"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr2coast3")]<- "attr2coast3.2"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr3cost")]  <- "attr3cost.2"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr4perk1")] <- "attr4perk1.2"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt2attr4perk2")] <- "attr4perk2.2"

colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr1hh2")]   <- "attr1hh2.3"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr1hh3")]   <- "attr1hh3.3"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr2coast2")]<- "attr2coast2.3"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr2coast3")]<- "attr2coast3.3"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr3cost")]  <- "attr3cost.3"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr4perk1")] <- "attr4perk1.3"
colnames(data.eng)[which(colnames(data.eng[1,]) == "alt3attr4perk2")] <- "attr4perk2.3"

# Scotland
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr1hh2")]   <- "attr1hh2.1"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr1hh3")]   <- "attr1hh3.1"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr2coast2")]<- "attr2coast2.1"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr2coast3")]<- "attr2coast3.1"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr3cost")]  <- "attr3cost.1"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr4perk1")] <- "attr4perk1.1"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt1attr4perk2")] <- "attr4perk2.1"

colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr1hh2")]   <- "attr1hh2.2"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr1hh3")]   <- "attr1hh3.2"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr2coast2")]<- "attr2coast2.2"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr2coast3")]<- "attr2coast3.2"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr3cost")]  <- "attr3cost.2"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr4perk1")] <- "attr4perk1.2"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt2attr4perk2")] <- "attr4perk2.2"

colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr1hh2")]   <- "attr1hh2.3"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr1hh3")]   <- "attr1hh3.3"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr2coast2")]<- "attr2coast2.3"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr2coast3")]<- "attr2coast3.3"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr3cost")]  <- "attr3cost.3"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr4perk1")] <- "attr4perk1.3"
colnames(data.scot)[which(colnames(data.scot[1,]) == "alt3attr4perk2")] <- "attr4perk2.3"

# Northern Ireland
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr1hh2")]   <- "attr1hh2.1"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr1hh3")]   <- "attr1hh3.1"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr2coast2")]<- "attr2coast2.1"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr2coast3")]<- "attr2coast3.1"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr3cost")]  <- "attr3cost.1"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr4perk1")] <- "attr4perk1.1"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt1attr4perk2")] <- "attr4perk2.1"

colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr1hh2")]   <- "attr1hh2.2"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr1hh3")]   <- "attr1hh3.2"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr2coast2")]<- "attr2coast2.2"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr2coast3")]<- "attr2coast3.2"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr3cost")]  <- "attr3cost.2"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr4perk1")] <- "attr4perk1.2"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt2attr4perk2")] <- "attr4perk2.2"

colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr1hh2")]   <- "attr1hh2.3"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr1hh3")]   <- "attr1hh3.3"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr2coast2")]<- "attr2coast2.3"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr2coast3")]<- "attr2coast3.3"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr3cost")]  <- "attr3cost.3"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr4perk1")] <- "attr4perk1.3"
colnames(data.ni)[which(colnames(data.ni[1,]) == "alt3attr4perk2")] <- "attr4perk2.3"

# Reshaping data from wide to long format using mlogit.data from mlogit package

# England
data.RPL.mlogit.eng <- mlogit.data(data.eng, 
                                   id.var = "ID",             
                                   choice = "Choice",         
                                   varying = 10:30,            
                                   shape = "wide",            
                                   opposite = "attr3cost",
                                   sep = ".")                 

# Scotland
data.RPL.mlogit.scot <- mlogit.data(data.scot, 
                                    id.var = "ID",             
                                    choice = "Choice",         
                                    varying = 10:30,            
                                    shape = "wide",            
                                    opposite = "attr3cost",
                                    sep = ".")  

# Northern Ireland
data.RPL.mlogit.ni <- mlogit.data(data.ni, 
                                   id.var = "ID",             
                                   choice = "Choice",         
                                   varying = 10:30,            
                                   shape = "wide",            
                                   opposite = "attr3cost",
                                   sep = ".")  



# creating variables for ASCs
data.RPL.mlogit.eng$asc1 <- as.numeric(data.RPL.mlogit.eng$alt == 1)
data.RPL.mlogit.eng$asc3 <- as.numeric(data.RPL.mlogit.eng$alt == 3)

data.RPL.mlogit.scot$asc1 <- as.numeric(data.RPL.mlogit.scot$alt == 1)
data.RPL.mlogit.scot$asc3 <- as.numeric(data.RPL.mlogit.scot$alt == 3)

data.RPL.mlogit.ni$asc1 <- as.numeric(data.RPL.mlogit.ni$alt == 1)
data.RPL.mlogit.ni$asc3 <- as.numeric(data.RPL.mlogit.ni$alt == 3)

data.RPL.mlogit.eng$cohabit  <- as.numeric(data.eng$cohabit==1)
data.RPL.mlogit.scot$cohabit <- as.numeric(data.scot$cohabit==1)
data.RPL.mlogit.ni$cohabit   <- as.numeric(data.ni$cohabit==1)   

data.RPL.mlogit.eng$employed  <- as.numeric(data.eng$employed==1)  
data.RPL.mlogit.scot$employed <- as.numeric(data.scot$employed==1)
data.RPL.mlogit.ni$employed   <- as.numeric(data.ni$employed==1)

data.RPL.mlogit.eng$higheduc  <- as.numeric(data.eng$higheduc==1)  
data.RPL.mlogit.scot$higheduc <- as.numeric(data.scot$higheduc==1) 
data.RPL.mlogit.ni$higheduc   <- as.numeric(data.ni$higheduc==1)   

data.RPL.mlogit.eng$highincome  <- as.numeric(data.eng$highincome==1)
data.RPL.mlogit.scot$highincome <- as.numeric(data.scot$highincome==1)
data.RPL.mlogit.ni$highincome   <- as.numeric(data.ni$highincome==1)

data.RPL.mlogit.eng$green  <- as.numeric(data.eng$green==1) 
data.RPL.mlogit.scot$green <- as.numeric(data.scot$green==1)   
data.RPL.mlogit.ni$green   <- as.numeric(data.ni$green==1)     


# These are based on previous estimations with fewer Halton draws
starting.values <- c(  -0.07255563	,
                       -3.50699363	,
                       1.30498919	,
                       1.61980359	,
                       0.12844135	,
                       0.40975196	,
                       -3.7637689	,
                       0.22639324	,
                       -0.3103079	,
                       0.0107419	  ,
                       -0.25200081	,
                       0.01068855	,
                       -0.04262433	,
                       -0.15845373	,
                       -0.1946511	,
                       0.11407792	,
                       -0.12755719	,
                       -0.12881859	,
                       0.01283428	,
                       -0.00569702	,
                       -0.00076641	,
                       -0.1620551	,
                       -0.14923833	,
                       -0.08145467	,
                         0.1216028	,
                       -0.19387811	,
                       0.14861022	,
                       -0.00488603	,
                       0.01622755	,
                       -0.22609591	,
                       0.05009858	,
                       -0.12769103	,
                          0.015208	,
                       -0.04289574	,
                       0.04573513	,
                       -0.15927272	,
                       -0.01425274	,
                       0.12734422	,
                       0.16522256	,
                       0.08766753	,
                       -0.3135928	,
                       0.29838176	,
                       -0.19882979	,
                       -0.09540918	,
                       -0.18496169	,
                       0.03499288	,
                       -0.47212571	,
                       -0.36630959	,
                       -0.0774227	,
                       -0.07403913	,
                       -0.32447988	,
                       -0.1744563	,
                       -0.01036466	,
                       -0.39060217	,
                       -0.01188783	,
                       0.16714939	,
                       0.28221264	,
                       -0.06494776	,
                       0.20517586	,
                       -0.08816781	,
                       -0.10164791	,
                       -0.02644356	,
                       -0.08317852	,
                       -0.0173593	,
                         0.5495189	,
                       0.29532174	,
                       0.05471286	,
                       0.09476594	,
                       0.24242824	,
                       -0.06742431	,
                       -0.05377912	,
                       -0.12973873	,
                       0.74320171	,
                       1.61189306	,
                       -0.31697469	,
                       -0.39003623	,
                       -0.37310063	,
                       -0.0044016	,
                       0.36019599	,
                       0.53835707	,
                       -0.26031315	,
                       -0.64781215	,
                       0.39718666	,
                       -0.51464981	,
                       -0.9460077	,
                       0.80543498	,
                       1.36587737	,
                       0.17740562	,
                         0.1995831	,
                         0.2291909	,
                       0.50707419	,
                       -0.32977535	,
                       -0.18898475	,
                       0.01591088	,
                       1.23788974	,
                       -0.0630139	,
                       -0.05322457	,
                       0.50951176	,
                         0.3248842	,
                       -0.06985429	)


# Recording the output in a text file
sink("ThreeModelComparisonENERGY-Output-RPL-C-England.txt", append=FALSE, split=TRUE)

# RPL model estimation
rpl.output.eng <- gmnl(Choice ~ ( asc1 +  asc3 
                                 + attr1hh2 
                                 + attr1hh3 
                                 + attr2coast2 
                                 + attr2coast3 
                                 + attr3cost 
                                 + attr4perk1 
                                 + attr4perk2
)| 0 |0 | (    age
             + female    
             + cohabit   
             + num_children  
             + higheduc  
             + employed  
             + green     
             + ideo 
             + highincome -1
),
data    = data.RPL.mlogit.eng,
model = 'mixl', 
R = 2000,  
haltons = list("primes" = c(2, 5, 7, 11, 13, 17, 19), "drop" = rep(19, 7)),
panel = TRUE,
print.level=3,
method = "BFGS",
start = starting.values,
ranp = c(attr1hh2    = "n",
         attr1hh3    = "n", 
         attr2coast2 = "n", 
         attr2coast3 = "n", 
         attr3cost   = "ln", 
         attr4perk1  = "n", 
         attr4perk2  = "n"),
mvar = list(attr1hh2   = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr1hh3   = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr2coast2= c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr2coast3= c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr3cost  = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr4perk1 = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr4perk2 = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome")),
correlation = TRUE,
iterlim = 600)      


summary(rpl.output.eng)

# Saving the output
beta_hats_eng <- rpl.output.eng$coefficients[1:100]

# Standard deviations
vcov(rpl.output.eng, what = 'ranp', type = 'sd', se = 'true')

sink()


###########
# Scotland
###########


# Recording the output in a text file
sink("ThreeModelComparisonENERGY-Output-RPL-C-Scotland.txt", append=FALSE, split=TRUE)

# RPL model estimation
rpl.output.scot <- gmnl(Choice ~ ( asc1 +  asc3 
                                 + attr1hh2 
                                 + attr1hh3 
                                 + attr2coast2 
                                 + attr2coast3 
                                 + attr3cost 
                                 + attr4perk1 
                                 + attr4perk2
)| 0 |0 | (  age
             + female    
             + cohabit   
             + num_children  
             + higheduc  
             + employed  
             + green     
             + ideo 
             + highincome -1
),
data    = data.RPL.mlogit.scot,
model = 'mixl', 
R = 2000,          
haltons = list("primes" = c(2, 5, 7, 11, 13, 17, 19), "drop" = rep(19, 7)),
panel = TRUE,
print.level=3,
method = "BFGS",
start = starting.values,
ranp = c(attr1hh2    = "n",
         attr1hh3    = "n", 
         attr2coast2 = "n", 
         attr2coast3 = "n", 
         attr3cost   = "ln", 
         attr4perk1  = "n", 
         attr4perk2  = "n"),
mvar = list(attr1hh2   = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr1hh3   = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr2coast2= c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr2coast3= c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr3cost  = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr4perk1 = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr4perk2 = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome")),
correlation = TRUE)       


summary(rpl.output.scot)

# Saving the output
beta_hats_scot <- rpl.output.scot$coefficients[1:100]

# Standard deviations
vcov(rpl.output.scot, what = 'ranp', type = 'sd', se = 'true')

sink()




###################
# Northern Ireland
###################


# Recording the output in a text file
sink("ThreeModelComparisonENERGY-Output-RPL-C-NI.txt", append=FALSE, split=TRUE)

# RPL model estimation
rpl.output.ni <- gmnl(Choice ~ ( asc1 +  asc3 
                              + attr1hh2 
                              + attr1hh3 
                              + attr2coast2 
                              + attr2coast3 
                              + attr3cost 
                              + attr4perk1 
                              + attr4perk2
)| 0 |0 | (  age
             + female    
             + cohabit   
             + num_children  
             + higheduc  
             + employed  
             + green     
             + ideo 
             + highincome -1
),
data    = data.RPL.mlogit.ni,
model = 'mixl', 
R = 2000,                           
haltons = list("primes" = c(2, 5, 7, 11, 13, 17, 19), "drop" = rep(19, 7)),
panel = TRUE,
print.level=3,
method = "BFGS",
start = starting.values,
ranp = c(attr1hh2    = "n",
         attr1hh3    = "n", 
         attr2coast2 = "n", 
         attr2coast3 = "n", 
         attr3cost   = "ln", 
         attr4perk1  = "n", 
         attr4perk2  = "n"),
mvar = list(attr1hh2   = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr1hh3   = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr2coast2= c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr2coast3= c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr3cost  = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr4perk1 = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome"),
            attr4perk2 = c("age","female","cohabit","num_children","higheduc","employed","green","ideo","highincome")),
correlation = TRUE)        


summary(rpl.output.ni)

# Saving the output
beta_hats_ni <- rpl.output.ni$coefficients[1:100]

# Standard deviations
vcov(rpl.output.ni, what = 'ranp', type = 'sd', se = 'true')

sink()



###########################################################

# Preparing the data for WTP simulations:

# Only want one observation per person
data.eng <- subset(data.eng , ChoiceQuestion < 2)
data.scot<- subset(data.scot, ChoiceQuestion < 2)
data.ni  <- subset(data.ni  , ChoiceQuestion < 2)

# Setting the parameters
n.rows.eng  <- length(data.eng[,1])
n.rows.scot <- length(data.scot[,1])
n.rows.ni   <- length(data.ni[,1])



# RPL model with correlations (RPL-C)

# England
GammaV_eng <- c(
  beta_hats_eng[73]   ,         0           ,       0             ,       0             ,         0            ,           0          ,          0        ,      
  beta_hats_eng[74]   , beta_hats_eng[80]   ,       0             ,       0             ,         0            ,           0          ,          0        ,     
  beta_hats_eng[75]   , beta_hats_eng[81]   , beta_hats_eng[86]   ,       0             ,         0            ,           0          ,          0        ,      
  beta_hats_eng[76]   , beta_hats_eng[82]   , beta_hats_eng[87]   , beta_hats_eng[91]   ,         0            ,           0          ,          0        ,     
  beta_hats_eng[77]   , beta_hats_eng[83]   , beta_hats_eng[88]   , beta_hats_eng[92]   , beta_hats_eng[95]    ,           0          ,          0        ,    
  beta_hats_eng[78]   , beta_hats_eng[84]   , beta_hats_eng[89]   , beta_hats_eng[93]   , beta_hats_eng[96]    , beta_hats_eng[98]    ,          0        ,  
  beta_hats_eng[79]   , beta_hats_eng[85]   , beta_hats_eng[90]   , beta_hats_eng[94]   , beta_hats_eng[97]    , beta_hats_eng[99]    , beta_hats_eng[100] ) 

Gamma_eng <- matrix(GammaV_eng,7,7,byrow=TRUE)                                                   
VarCov_eng <- Gamma_eng %*% t(Gamma_eng)


# Scotland
GammaV_scot <- c(
  beta_hats_scot[73]   ,          0           ,        0             ,         0             ,          0            ,            0          ,          0        ,      
  beta_hats_scot[74]   , beta_hats_scot[80]   ,        0             ,         0             ,          0            ,            0          ,          0        ,     
  beta_hats_scot[75]   , beta_hats_scot[81]   , beta_hats_scot[86]   ,         0             ,          0            ,            0          ,          0        ,      
  beta_hats_scot[76]   , beta_hats_scot[82]   , beta_hats_scot[87]   , beta_hats_scot [91]   ,          0            ,            0          ,          0        ,     
  beta_hats_scot[77]   , beta_hats_scot[83]   , beta_hats_scot[88]   , beta_hats_scot [92]   , beta_hats_scot[95]    ,            0          ,          0        ,    
  beta_hats_scot[78]   , beta_hats_scot[84]   , beta_hats_scot[89]   , beta_hats_scot [93]   , beta_hats_scot[96]    , beta_hats_scot[98]    ,          0        ,  
  beta_hats_scot[79]   , beta_hats_scot[85]   , beta_hats_scot[90]   , beta_hats_scot [94]   , beta_hats_scot[97]    , beta_hats_scot[99]    , beta_hats_scot[100] ) 

Gamma_scot <- matrix(GammaV_scot,7,7,byrow=TRUE)                                                   
VarCov_scot <- Gamma_scot %*% t(Gamma_scot)

# Northern Ireland
GammaV_ni <- c(
  beta_hats_ni[73]   ,         0          ,       0            ,         0          ,         0           ,           0          ,          0        ,      
  beta_hats_ni[74]   , beta_hats_ni[80]   ,       0            ,         0          ,         0           ,           0          ,          0        ,     
  beta_hats_ni[75]   , beta_hats_ni[81]   , beta_hats_ni[86]   ,         0          ,         0           ,           0          ,          0        ,      
  beta_hats_ni[76]   , beta_hats_ni[82]   , beta_hats_ni[87]   , beta_hats_ni[91]   ,         0           ,           0          ,          0        ,     
  beta_hats_ni[77]   , beta_hats_ni[83]   , beta_hats_ni[88]   , beta_hats_ni[92]   , beta_hats_ni[95]    ,           0          ,          0        ,    
  beta_hats_ni[78]   , beta_hats_ni[84]   , beta_hats_ni[89]   , beta_hats_ni[93]   , beta_hats_ni[96]    , beta_hats_ni [98]    ,          0        ,  
  beta_hats_ni[79]   , beta_hats_ni[85]   , beta_hats_ni[90]   , beta_hats_ni[94]   , beta_hats_ni[97]    , beta_hats_ni [99]    , beta_hats_ni[100] ) 

Gamma_ni <- matrix(GammaV_ni,7,7,byrow=TRUE)                                                   
VarCov_ni <- Gamma_ni %*% t(Gamma_ni)





# RPL model with SocDem interactions (RPL-UC) - Values taken from the Pythonbiogeme output files

# RPL-UC-England
RPL_UC_ENG_ASC1	<-	0.0719
RPL_UC_ENG_ASC3	<-	-3.1
RPL_UC_ENG_bbATTR1ageEnvhh2	<-	0.0124
RPL_UC_ENG_bbATTR1ageEnvhh3	<-	0.0219
RPL_UC_ENG_bbATTR1cohabitEnvhh2	<-	0.0321
RPL_UC_ENG_bbATTR1cohabitEnvhh3	<-	-0.275
RPL_UC_ENG_bbATTR1employedEnvhh2	<-	-0.218
RPL_UC_ENG_bbATTR1employedEnvhh3	<-	-0.293
RPL_UC_ENG_bbATTR1femaleEnvhh2	<-	0.0872
RPL_UC_ENG_bbATTR1femaleEnvhh3	<-	0.262
RPL_UC_ENG_bbATTR1greenEnvhh2	<-	-0.000881
RPL_UC_ENG_bbATTR1greenEnvhh3	<-	0.101
RPL_UC_ENG_bbATTR1hh2	<-	0.808
RPL_UC_ENG_bbATTR1hh3	<-	0.852
RPL_UC_ENG_bbATTR1higheducEnvhh2	<-	0.214
RPL_UC_ENG_bbATTR1higheducEnvhh3	<-	0.273
RPL_UC_ENG_bbATTR1highincomeEnvhh2	<-	0.543
RPL_UC_ENG_bbATTR1highincomeEnvhh3	<-	0.751
RPL_UC_ENG_bbATTR1numchildEnvhh2	<-	-0.111
RPL_UC_ENG_bbATTR1numchildEnvhh3	<-	0.0918
RPL_UC_ENG_bbATTR1polorientEnvhh2	<-	-0.114
RPL_UC_ENG_bbATTR1polorientEnvhh3	<-	-0.143
RPL_UC_ENG_bbATTR2ageEnvcoast2	<-	-0.00935
RPL_UC_ENG_bbATTR2ageEnvcoast3	<-	-0.00607
RPL_UC_ENG_bbATTR2coast2	<-	0.17
RPL_UC_ENG_bbATTR2coast3	<-	-0.127
RPL_UC_ENG_bbATTR2cohabitEnvcoast2	<-	0.125
RPL_UC_ENG_bbATTR2cohabitEnvcoast3	<-	-0.00202
RPL_UC_ENG_bbATTR2employedEnvcoast2	<-	-0.082
RPL_UC_ENG_bbATTR2employedEnvcoast3	<-	-0.0612
RPL_UC_ENG_bbATTR2femaleEnvcoast2	<-	-0.108
RPL_UC_ENG_bbATTR2femaleEnvcoast3	<-	-0.132
RPL_UC_ENG_bbATTR2greenEnvcoast2	<-	-0.00392
RPL_UC_ENG_bbATTR2greenEnvcoast3	<-	-0.142
RPL_UC_ENG_bbATTR2higheducEnvcoast2	<-	0.0301
RPL_UC_ENG_bbATTR2higheducEnvcoast3	<-	-0.032
RPL_UC_ENG_bbATTR2highincomeEnvcoast2	<-	-0.141
RPL_UC_ENG_bbATTR2highincomeEnvcoast3	<-	-0.174
RPL_UC_ENG_bbATTR2numchildEnvcoast2	<-	-0.0164
RPL_UC_ENG_bbATTR2numchildEnvcoast3	<-	0.0715
RPL_UC_ENG_bbATTR2polorientEnvcoast2	<-	0.0137
RPL_UC_ENG_bbATTR2polorientEnvcoast3	<-	0.00834
RPL_UC_ENG_bbATTR3ageEnvcost	<-	0.0296
RPL_UC_ENG_bbATTR3cohabitEnvcost	<-	-0.0358
RPL_UC_ENG_bbATTR3cost	<-	-4.39
RPL_UC_ENG_bbATTR3employedEnvcost	<-	0.0688
RPL_UC_ENG_bbATTR3femaleEnvcost	<-	0.225
RPL_UC_ENG_bbATTR3greenEnvcost	<-	-0.779
RPL_UC_ENG_bbATTR3higheducEnvcost	<-	0.0346
RPL_UC_ENG_bbATTR3highincomeEnvcost	<-	-0.102
RPL_UC_ENG_bbATTR3numchildEnvcost	<-	-0.0558
RPL_UC_ENG_bbATTR3polorientEnvcost	<-	-0.0635
RPL_UC_ENG_bbATTR4ageEnvperk1	<-	-0.0083
RPL_UC_ENG_bbATTR4ageEnvperk2	<-	-0.00443
RPL_UC_ENG_bbATTR4cohabitEnvperk1	<-	0.0502
RPL_UC_ENG_bbATTR4cohabitEnvperk2	<-	-0.115
RPL_UC_ENG_bbATTR4employedEnvperk1	<-	0.232
RPL_UC_ENG_bbATTR4employedEnvperk2	<-	0.263
RPL_UC_ENG_bbATTR4femaleEnvperk1	<-	-0.00338
RPL_UC_ENG_bbATTR4femaleEnvperk2	<-	-0.00606
RPL_UC_ENG_bbATTR4greenEnvperk1	<-	-0.136
RPL_UC_ENG_bbATTR4greenEnvperk2	<-	-0.0222
RPL_UC_ENG_bbATTR4higheducEnvperk1	<-	-0.0591
RPL_UC_ENG_bbATTR4higheducEnvperk2	<-	-0.0123
RPL_UC_ENG_bbATTR4highincomeEnvperk1	<-	0.0325
RPL_UC_ENG_bbATTR4highincomeEnvperk2	<-	0.223
RPL_UC_ENG_bbATTR4numchildEnvperk1	<-	-0.0129
RPL_UC_ENG_bbATTR4numchildEnvperk2	<-	0.0418
RPL_UC_ENG_bbATTR4perk1	<-	0.221
RPL_UC_ENG_bbATTR4perk2	<-	-0.447
RPL_UC_ENG_bbATTR4polorientEnvperk1	<-	-0.00293
RPL_UC_ENG_bbATTR4polorientEnvperk2	<-	0.0396
RPL_UC_ENG_sdbATTR1hh2	<-	0.397
RPL_UC_ENG_sdbATTR1hh3	<-	1.27
RPL_UC_ENG_sdbATTR2coast2	<-	0.813
RPL_UC_ENG_sdbATTR2coast3	<-	1.18
RPL_UC_ENG_sdbATTR3cost	<-	1.12
RPL_UC_ENG_sdbATTR4perk1	<-	0.358
RPL_UC_ENG_sdbATTR4perk2	<-	0.603



# RPL-UC-Scotland
RPL_UC_SCOT_ASC1	<-	-0.031
RPL_UC_SCOT_ASC3	<-	-3.25
RPL_UC_SCOT_bbATTR1ageEnvhh2	<-	0.0117
RPL_UC_SCOT_bbATTR1ageEnvhh3	<-	0.0143
RPL_UC_SCOT_bbATTR1cohabitEnvhh2	<-	-0.0088
RPL_UC_SCOT_bbATTR1cohabitEnvhh3	<-	-0.0247
RPL_UC_SCOT_bbATTR1employedEnvhh2	<-	-0.28
RPL_UC_SCOT_bbATTR1employedEnvhh3	<-	-0.221
RPL_UC_SCOT_bbATTR1femaleEnvhh2	<-	-0.327
RPL_UC_SCOT_bbATTR1femaleEnvhh3	<-	-0.0774
RPL_UC_SCOT_bbATTR1greenEnvhh2	<-	0.085
RPL_UC_SCOT_bbATTR1greenEnvhh3	<-	0.0495
RPL_UC_SCOT_bbATTR1hh2	<-	1.4
RPL_UC_SCOT_bbATTR1hh3	<-	1.75
RPL_UC_SCOT_bbATTR1higheducEnvhh2	<-	-0.139
RPL_UC_SCOT_bbATTR1higheducEnvhh3	<-	-0.154
RPL_UC_SCOT_bbATTR1highincomeEnvhh2	<-	-0.115
RPL_UC_SCOT_bbATTR1highincomeEnvhh3	<-	0.163
RPL_UC_SCOT_bbATTR1numchildEnvhh2	<-	-0.0211
RPL_UC_SCOT_bbATTR1numchildEnvhh3	<-	-0.141
RPL_UC_SCOT_bbATTR1polorientEnvhh2	<-	-0.122
RPL_UC_SCOT_bbATTR1polorientEnvhh3	<-	-0.175
RPL_UC_SCOT_bbATTR2ageEnvcoast2	<-	-0.00615
RPL_UC_SCOT_bbATTR2ageEnvcoast3	<-	-0.00947
RPL_UC_SCOT_bbATTR2coast2	<-	-0.00168
RPL_UC_SCOT_bbATTR2coast3	<-	0.226
RPL_UC_SCOT_bbATTR2cohabitEnvcoast2	<-	-0.221
RPL_UC_SCOT_bbATTR2cohabitEnvcoast3	<-	0.0875
RPL_UC_SCOT_bbATTR2employedEnvcoast2	<-	0.0241
RPL_UC_SCOT_bbATTR2employedEnvcoast3	<-	0.238
RPL_UC_SCOT_bbATTR2femaleEnvcoast2	<-	0.0216
RPL_UC_SCOT_bbATTR2femaleEnvcoast3	<-	0.118
RPL_UC_SCOT_bbATTR2greenEnvcoast2	<-	-0.0682
RPL_UC_SCOT_bbATTR2greenEnvcoast3	<-	-0.0921
RPL_UC_SCOT_bbATTR2higheducEnvcoast2	<-	-0.103
RPL_UC_SCOT_bbATTR2higheducEnvcoast3	<-	-0.303
RPL_UC_SCOT_bbATTR2highincomeEnvcoast2	<-	-0.187
RPL_UC_SCOT_bbATTR2highincomeEnvcoast3	<-	-0.176
RPL_UC_SCOT_bbATTR2numchildEnvcoast2	<-	0.0519
RPL_UC_SCOT_bbATTR2numchildEnvcoast3	<-	0.0652
RPL_UC_SCOT_bbATTR2polorientEnvcoast2	<-	0.0546
RPL_UC_SCOT_bbATTR2polorientEnvcoast3	<-	-0.0801
RPL_UC_SCOT_bbATTR3ageEnvcost	<-	0.0241
RPL_UC_SCOT_bbATTR3cohabitEnvcost	<-	-0.31
RPL_UC_SCOT_bbATTR3cost	<-	-3.68
RPL_UC_SCOT_bbATTR3employedEnvcost	<-	-0.249
RPL_UC_SCOT_bbATTR3femaleEnvcost	<-	-0.315
RPL_UC_SCOT_bbATTR3greenEnvcost	<-	-0.2
RPL_UC_SCOT_bbATTR3higheducEnvcost	<-	-0.156
RPL_UC_SCOT_bbATTR3highincomeEnvcost	<-	-0.254
RPL_UC_SCOT_bbATTR3numchildEnvcost	<-	-0.0191
RPL_UC_SCOT_bbATTR3polorientEnvcost	<-	-0.0188
RPL_UC_SCOT_bbATTR4ageEnvperk1	<-	-0.00773
RPL_UC_SCOT_bbATTR4ageEnvperk2	<-	-0.00947
RPL_UC_SCOT_bbATTR4cohabitEnvperk1	<-	0.167
RPL_UC_SCOT_bbATTR4cohabitEnvperk2	<-	0.163
RPL_UC_SCOT_bbATTR4employedEnvperk1	<-	-0.14
RPL_UC_SCOT_bbATTR4employedEnvperk2	<-	0.0254
RPL_UC_SCOT_bbATTR4femaleEnvperk1	<-	0.128
RPL_UC_SCOT_bbATTR4femaleEnvperk2	<-	0.394
RPL_UC_SCOT_bbATTR4greenEnvperk1	<-	0.00986
RPL_UC_SCOT_bbATTR4greenEnvperk2	<-	0.069
RPL_UC_SCOT_bbATTR4higheducEnvperk1	<-	0.222
RPL_UC_SCOT_bbATTR4higheducEnvperk2	<-	0.0992
RPL_UC_SCOT_bbATTR4highincomeEnvperk1	<-	-0.0731
RPL_UC_SCOT_bbATTR4highincomeEnvperk2	<-	-0.0762
RPL_UC_SCOT_bbATTR4numchildEnvperk1	<-	-0.1
RPL_UC_SCOT_bbATTR4numchildEnvperk2	<-	-0.0122
RPL_UC_SCOT_bbATTR4perk1	<-	0.171
RPL_UC_SCOT_bbATTR4perk2	<-	-0.354
RPL_UC_SCOT_bbATTR4polorientEnvperk1	<-	-0.0162
RPL_UC_SCOT_bbATTR4polorientEnvperk2	<-	-0.00822
RPL_UC_SCOT_sdbATTR1hh2	<-	0.0556
RPL_UC_SCOT_sdbATTR1hh3	<-	1.15
RPL_UC_SCOT_sdbATTR2coast2	<-	0.647
RPL_UC_SCOT_sdbATTR2coast3	<-	1.3
RPL_UC_SCOT_sdbATTR3cost	<-	1.16
RPL_UC_SCOT_sdbATTR4perk1	<-	0.322
RPL_UC_SCOT_sdbATTR4perk2	<-	0.772


# RPL-UC-Northern Ireland
RPL_UC_NI_ASC1	<-	-0.0474
RPL_UC_NI_ASC3	<-	-2.98
RPL_UC_NI_bbATTR1ageEnvhh2	<-	-0.00128
RPL_UC_NI_bbATTR1ageEnvhh3	<-	0.0128
RPL_UC_NI_bbATTR1cohabitEnvhh2	<-	-0.2
RPL_UC_NI_bbATTR1cohabitEnvhh3	<-	-0.0152
RPL_UC_NI_bbATTR1employedEnvhh2	<-	0.348
RPL_UC_NI_bbATTR1employedEnvhh3	<-	0.534
RPL_UC_NI_bbATTR1femaleEnvhh2	<-	0.139
RPL_UC_NI_bbATTR1femaleEnvhh3	<-	0.163
RPL_UC_NI_bbATTR1greenEnvhh2	<-	0.189
RPL_UC_NI_bbATTR1greenEnvhh3	<-	-0.0703
RPL_UC_NI_bbATTR1hh2	<-	0.431
RPL_UC_NI_bbATTR1hh3	<-	0.32
RPL_UC_NI_bbATTR1higheducEnvhh2	<-	0.0867
RPL_UC_NI_bbATTR1higheducEnvhh3	<-	0.199
RPL_UC_NI_bbATTR1highincomeEnvhh2	<-	0.082
RPL_UC_NI_bbATTR1highincomeEnvhh3	<-	-0.219
RPL_UC_NI_bbATTR1numchildEnvhh2	<-	0.0729
RPL_UC_NI_bbATTR1numchildEnvhh3	<-	0.0543
RPL_UC_NI_bbATTR1polorientEnvhh2	<-	-0.00368
RPL_UC_NI_bbATTR1polorientEnvhh3	<-	-0.0521
RPL_UC_NI_bbATTR2ageEnvcoast2	<-	-0.00308
RPL_UC_NI_bbATTR2ageEnvcoast3	<-	-0.0107
RPL_UC_NI_bbATTR2coast2	<-	0.0336
RPL_UC_NI_bbATTR2coast3	<-	0.212
RPL_UC_NI_bbATTR2cohabitEnvcoast2	<-	-0.293
RPL_UC_NI_bbATTR2cohabitEnvcoast3	<-	-0.277
RPL_UC_NI_bbATTR2employedEnvcoast2	<-	0.182
RPL_UC_NI_bbATTR2employedEnvcoast3	<-	-0.0549
RPL_UC_NI_bbATTR2femaleEnvcoast2	<-	0.0896
RPL_UC_NI_bbATTR2femaleEnvcoast3	<-	0.0777
RPL_UC_NI_bbATTR2greenEnvcoast2	<-	0.277
RPL_UC_NI_bbATTR2greenEnvcoast3	<-	0.395
RPL_UC_NI_bbATTR2higheducEnvcoast2	<-	0.184
RPL_UC_NI_bbATTR2higheducEnvcoast3	<-	-0.0814
RPL_UC_NI_bbATTR2highincomeEnvcoast2	<-	-0.238
RPL_UC_NI_bbATTR2highincomeEnvcoast3	<-	-0.407
RPL_UC_NI_bbATTR2numchildEnvcoast2	<-	-0.00587
RPL_UC_NI_bbATTR2numchildEnvcoast3	<-	0.0979
RPL_UC_NI_bbATTR2polorientEnvcoast2	<-	-0.0389
RPL_UC_NI_bbATTR2polorientEnvcoast3	<-	-0.0152
RPL_UC_NI_bbATTR3ageEnvcost	<-	0.0202
RPL_UC_NI_bbATTR3cohabitEnvcost	<-	0.0432
RPL_UC_NI_bbATTR3cost	<-	-4.56
RPL_UC_NI_bbATTR3employedEnvcost	<-	0.0705
RPL_UC_NI_bbATTR3femaleEnvcost	<-	0.318
RPL_UC_NI_bbATTR3greenEnvcost	<-	-0.349
RPL_UC_NI_bbATTR3higheducEnvcost	<-	-0.225
RPL_UC_NI_bbATTR3highincomeEnvcost	<-	-0.322
RPL_UC_NI_bbATTR3numchildEnvcost	<-	-0.00788
RPL_UC_NI_bbATTR3polorientEnvcost	<-	0.0196
RPL_UC_NI_bbATTR4ageEnvperk1	<-	-0.00838
RPL_UC_NI_bbATTR4ageEnvperk2	<-	-0.00548
RPL_UC_NI_bbATTR4cohabitEnvperk1	<-	0.242
RPL_UC_NI_bbATTR4cohabitEnvperk2	<-	-0.0129
RPL_UC_NI_bbATTR4employedEnvperk1	<-	0.136
RPL_UC_NI_bbATTR4employedEnvperk2	<-	0.0554
RPL_UC_NI_bbATTR4femaleEnvperk1	<-	-0.0268
RPL_UC_NI_bbATTR4femaleEnvperk2	<-	-0.0335
RPL_UC_NI_bbATTR4greenEnvperk1	<-	-0.0657
RPL_UC_NI_bbATTR4greenEnvperk2	<-	-0.282
RPL_UC_NI_bbATTR4higheducEnvperk1	<-	-0.408
RPL_UC_NI_bbATTR4higheducEnvperk2	<-	-0.667
RPL_UC_NI_bbATTR4highincomeEnvperk1	<-	-0.268
RPL_UC_NI_bbATTR4highincomeEnvperk2	<-	-0.0706
RPL_UC_NI_bbATTR4numchildEnvperk1	<-	-0.15
RPL_UC_NI_bbATTR4numchildEnvperk2	<-	-0.00929
RPL_UC_NI_bbATTR4perk1	<-	0.405
RPL_UC_NI_bbATTR4perk2	<-	-0.402
RPL_UC_NI_bbATTR4polorientEnvperk1	<-	0.0175
RPL_UC_NI_bbATTR4polorientEnvperk2	<-	0.109
RPL_UC_NI_sdbATTR1hh2	<-	0.08
RPL_UC_NI_sdbATTR1hh3	<-	1.09
RPL_UC_NI_sdbATTR2coast2	<-	0.521
RPL_UC_NI_sdbATTR2coast3	<-	0.681
RPL_UC_NI_sdbATTR3cost	<-	1.04
RPL_UC_NI_sdbATTR4perk1	<-	0.493
RPL_UC_NI_sdbATTR4perk2	<-	0.723








# HCM Model - Values are taken from the Pythonbiogeme output files

# HCM-England
HCM_ENG_ASC1	<-	0.0722
HCM_ENG_ASC3	<-	-3.08
HCM_ENG_alpha1LVEnv	<-	2.03
HCM_ENG_alpha2LVEnv	<-	1.03
HCM_ENG_alpha3LVEnv	<-	1.12
HCM_ENG_alpha4LVEnv	<-	1.94
HCM_ENG_alpha5LVEnv	<-	1.72
HCM_ENG_alpha6LVEnv	<-	1.18
HCM_ENG_alpha7LVEnv	<-	0.51
HCM_ENG_bbATTR1LVEnvhh2	<-	0.0067
HCM_ENG_bbATTR1LVEnvhh3	<-	-0.117
HCM_ENG_bbATTR1hh2	<-	0.745
HCM_ENG_bbATTR1hh3	<-	1.09
HCM_ENG_bbATTR2LVEnvcoast2	<-	0.0669
HCM_ENG_bbATTR2LVEnvcoast3	<-	-0.075
HCM_ENG_bbATTR2coast2	<-	-0.176
HCM_ENG_bbATTR2coast3	<-	-0.476
HCM_ENG_bbATTR3LVEnvcost	<-	0.591
HCM_ENG_bbATTR3cost	<-	-3.48
HCM_ENG_bbATTR4LVEnvperk1	<-	-0.00422
HCM_ENG_bbATTR4LVEnvperk2	<-	-0.00319
HCM_ENG_bbATTR4perk1	<-	0.0204
HCM_ENG_bbATTR4perk2	<-	-0.228
HCM_ENG_bsc_age	<-	0.0048
HCM_ENG_bsc_cohabit	<-	0.0139
HCM_ENG_bsc_employed	<-	-0.0115
HCM_ENG_bsc_female	<-	0.122
HCM_ENG_bsc_green	<-	-0.692
HCM_ENG_bsc_higheduc	<-	-0.199
HCM_ENG_bsc_highincome	<-	0.0334
HCM_ENG_bsc_numchild	<-	-0.109
HCM_ENG_bsc_polorient	<-	-0.025
HCM_ENG_delta1LVEnv2	<-	3.58
HCM_ENG_delta1LVEnv3	<-	3.94
HCM_ENG_delta2LVEnv2	<-	2.73
HCM_ENG_delta2LVEnv3	<-	3.38
HCM_ENG_delta3LVEnv2	<-	2.68
HCM_ENG_delta3LVEnv3	<-	2.98
HCM_ENG_delta4LVEnv2	<-	4.39
HCM_ENG_delta4LVEnv3	<-	2.94
HCM_ENG_delta5LVEnv2	<-	3.85
HCM_ENG_delta5LVEnv3	<-	2.94
HCM_ENG_delta6LVEnv2	<-	3.25
HCM_ENG_delta6LVEnv3	<-	2.76
HCM_ENG_delta7LVEnv2	<-	2.65
HCM_ENG_delta7LVEnv3	<-	2.05
HCM_ENG_sdbATTR1hh2	<-	0.499
HCM_ENG_sdbATTR1hh3	<-	1.32
HCM_ENG_sdbATTR2coast2	<-	0.82
HCM_ENG_sdbATTR2coast3	<-	1.19
HCM_ENG_sdbATTR3cost	<-	1.1
HCM_ENG_sdbATTR4perk1	<-	0.442
HCM_ENG_sdbATTR4perk2	<-	0.624
HCM_ENG_tau1LVEnv1	<-	-2.22
HCM_ENG_tau2LVEnv1	<-	-1.08
HCM_ENG_tau3LVEnv1	<-	-1.56
HCM_ENG_tau4LVEnv1	<-	-0.367
HCM_ENG_tau5LVEnv1	<-	-2.24
HCM_ENG_tau6LVEnv1	<-	-0.535
HCM_ENG_tau7LVEnv1	<-	0.318



# HCM-Scotland
HCM_SCOT_ASC1	<-	-0.0402
HCM_SCOT_ASC3	<-	-3.22
HCM_SCOT_alpha1LVEnv	<-	1.91
HCM_SCOT_alpha2LVEnv	<-	1.14
HCM_SCOT_alpha3LVEnv	<-	1.08
HCM_SCOT_alpha4LVEnv	<-	2.3
HCM_SCOT_alpha5LVEnv	<-	1.73
HCM_SCOT_alpha6LVEnv	<-	1.57
HCM_SCOT_alpha7LVEnv	<-	0.719
HCM_SCOT_bbATTR1LVEnvhh2	<-	-0.13
HCM_SCOT_bbATTR1LVEnvhh3	<-	-0.291
HCM_SCOT_bbATTR1hh2	<-	0.795
HCM_SCOT_bbATTR1hh3	<-	1.15
HCM_SCOT_bbATTR2LVEnvcoast2	<-	0.0456
HCM_SCOT_bbATTR2LVEnvcoast3	<-	0.0154
HCM_SCOT_bbATTR2coast2	<-	-0.182
HCM_SCOT_bbATTR2coast3	<-	-0.437
HCM_SCOT_bbATTR3LVEnvcost	<-	0.322
HCM_SCOT_bbATTR3cost	<-	-3.56
HCM_SCOT_bbATTR4LVEnvperk1	<-	-0.0493
HCM_SCOT_bbATTR4LVEnvperk2	<-	-0.146
HCM_SCOT_bbATTR4perk1	<-	-0.105
HCM_SCOT_bbATTR4perk2	<-	-0.385
HCM_SCOT_bsc_age	<-	-0.00139
HCM_SCOT_bsc_cohabit	<-	-0.0239
HCM_SCOT_bsc_employed	<-	0.0838
HCM_SCOT_bsc_female	<-	-0.176
HCM_SCOT_bsc_green	<-	-0.747
HCM_SCOT_bsc_higheduc	<-	0.00493
HCM_SCOT_bsc_highincome	<-	-0.102
HCM_SCOT_bsc_numchild	<-	-0.0268
HCM_SCOT_bsc_polorient	<-	0.105
HCM_SCOT_delta1LVEnv2	<-	3.55
HCM_SCOT_delta1LVEnv3	<-	3.82
HCM_SCOT_delta2LVEnv2	<-	2.82
HCM_SCOT_delta2LVEnv3	<-	2.76
HCM_SCOT_delta3LVEnv2	<-	2.91
HCM_SCOT_delta3LVEnv3	<-	3.12
HCM_SCOT_delta4LVEnv2	<-	4.69
HCM_SCOT_delta4LVEnv3	<-	3.16
HCM_SCOT_delta5LVEnv2	<-	3.9
HCM_SCOT_delta5LVEnv3	<-	2.71
HCM_SCOT_delta6LVEnv2	<-	3.77
HCM_SCOT_delta6LVEnv3	<-	3.86
HCM_SCOT_delta7LVEnv2	<-	2.95
HCM_SCOT_delta7LVEnv3	<-	3.05
HCM_SCOT_sdbATTR1hh2	<-	0.19
HCM_SCOT_sdbATTR1hh3	<-	1.11
HCM_SCOT_sdbATTR2coast2	<-	0.658
HCM_SCOT_sdbATTR2coast3	<-	1.32
HCM_SCOT_sdbATTR3cost	<-	1.19
HCM_SCOT_sdbATTR4perk1	<-	0.295
HCM_SCOT_sdbATTR4perk2	<-	0.77
HCM_SCOT_tau1LVEnv1	<-	-1.29
HCM_SCOT_tau2LVEnv1	<-	-0.426
HCM_SCOT_tau3LVEnv1	<-	-1.11
HCM_SCOT_tau4LVEnv1	<-	0.474
HCM_SCOT_tau5LVEnv1	<-	-1.51
HCM_SCOT_tau6LVEnv1	<-	-0.00614
HCM_SCOT_tau7LVEnv1	<-	0.49




# HCM-Northern Ireland
HCM_NI_ASC1	<-	-0.0399
HCM_NI_ASC3	<-	-2.98
HCM_NI_alpha1LVEnv	<-	2.96
HCM_NI_alpha2LVEnv	<-	1.27
HCM_NI_alpha3LVEnv	<-	0.983
HCM_NI_alpha4LVEnv	<-	1.82
HCM_NI_alpha5LVEnv	<-	1.63
HCM_NI_alpha6LVEnv	<-	1.28
HCM_NI_alpha7LVEnv	<-	0.384
HCM_NI_bbATTR1LVEnvhh2	<-	-0.175
HCM_NI_bbATTR1LVEnvhh3	<-	-0.325
HCM_NI_bbATTR1hh2	<-	0.67
HCM_NI_bbATTR1hh3	<-	0.972
HCM_NI_bbATTR2LVEnvcoast2	<-	-0.111
HCM_NI_bbATTR2LVEnvcoast3	<-	-0.054
HCM_NI_bbATTR2coast2	<-	-0.257
HCM_NI_bbATTR2coast3	<-	-0.475
HCM_NI_bbATTR3LVEnvcost	<-	0.264
HCM_NI_bbATTR3cost	<-	-3.58
HCM_NI_bbATTR4LVEnvperk1	<-	-0.217
HCM_NI_bbATTR4LVEnvperk2	<-	-0.25
HCM_NI_bbATTR4perk1	<-	-0.143
HCM_NI_bbATTR4perk2	<-	-0.536
HCM_NI_bsc_age	<-	0.00385
HCM_NI_bsc_cohabit	<-	-0.0409
HCM_NI_bsc_employed	<-	-0.116
HCM_NI_bsc_female	<-	-0.131
HCM_NI_bsc_green	<-	-0.754
HCM_NI_bsc_higheduc	<-	-0.145
HCM_NI_bsc_highincome	<-	0.215
HCM_NI_bsc_numchild	<-	-0.0916
HCM_NI_bsc_polorient	<-	-0.00767
HCM_NI_delta1LVEnv2	<-	4.86
HCM_NI_delta1LVEnv3	<-	3.88
HCM_NI_delta2LVEnv2	<-	2.99
HCM_NI_delta2LVEnv3	<-	2.4
HCM_NI_delta3LVEnv2	<-	2.7
HCM_NI_delta3LVEnv3	<-	2.23
HCM_NI_delta4LVEnv2	<-	4.3
HCM_NI_delta4LVEnv3	<-	2.45
HCM_NI_delta5LVEnv2	<-	3.99
HCM_NI_delta5LVEnv3	<-	3.16
HCM_NI_delta6LVEnv2	<-	3.71
HCM_NI_delta6LVEnv3	<-	1.33
HCM_NI_delta7LVEnv2	<-	2.63
HCM_NI_delta7LVEnv3	<-	1.72
HCM_NI_sdbATTR1hh2	<-	0.215
HCM_NI_sdbATTR1hh3	<-	0.995
HCM_NI_sdbATTR2coast2	<-	0.435
HCM_NI_sdbATTR2coast3	<-	0.775
HCM_NI_sdbATTR3cost	<-	1.02
HCM_NI_sdbATTR4perk1	<-	0.476
HCM_NI_sdbATTR4perk2	<-	0.693
HCM_NI_tau1LVEnv1	<-	-3.52
HCM_NI_tau2LVEnv1	<-	-1.22
HCM_NI_tau3LVEnv1	<-	-1.73
HCM_NI_tau4LVEnv1	<-	-0.967
HCM_NI_tau5LVEnv1	<-	-2.65
HCM_NI_tau6LVEnv1	<-	-0.644
HCM_NI_tau7LVEnv1	<-	0.235





##########################################################
#                                                        #
#                   Simulating WTP                       #
#                                                        #
##########################################################

################################################
###  RPL model with no correlations (RPL-UC) ###
################################################

N.eng <- n.rows.eng
N.scot<- n.rows.scot
N.ni  <- n.rows.ni
NumIter <-1000

# England
WTP_RPL_UC_ENG_hh2 <- rep(0,N.eng)
for ( i in 1:NumIter){
  WTP_RPL_UC_ENG_hh2[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <- (-(RPL_UC_ENG_bbATTR1hh2  + RPL_UC_ENG_bbATTR1ageEnvhh2*data.eng$age  + RPL_UC_ENG_bbATTR1cohabitEnvhh2*data.eng$cohabit  + RPL_UC_ENG_bbATTR1employedEnvhh2*data.eng$employed  + RPL_UC_ENG_bbATTR1femaleEnvhh2*data.eng$female  + RPL_UC_ENG_bbATTR1greenEnvhh2*data.eng$green  + RPL_UC_ENG_bbATTR1higheducEnvhh2*data.eng$higheduc  + RPL_UC_ENG_bbATTR1highincomeEnvhh2*data.eng$highincome  + RPL_UC_ENG_bbATTR1numchildEnvhh2*data.eng$num_children  + RPL_UC_ENG_bbATTR1polorientEnvhh2*data.eng$ideo  + abs(RPL_UC_ENG_sdbATTR1hh2) * rep(rnorm(N.eng)))/
                                                              (-exp(RPL_UC_ENG_bbATTR3cost + RPL_UC_ENG_bbATTR3ageEnvcost*data.eng$age + RPL_UC_ENG_bbATTR3cohabitEnvcost*data.eng$cohabit + RPL_UC_ENG_bbATTR3employedEnvcost*data.eng$employed + RPL_UC_ENG_bbATTR3femaleEnvcost*data.eng$female + RPL_UC_ENG_bbATTR3greenEnvcost*data.eng$green + RPL_UC_ENG_bbATTR3higheducEnvcost*data.eng$higheduc + RPL_UC_ENG_bbATTR3highincomeEnvcost*data.eng$highincome + RPL_UC_ENG_bbATTR3numchildEnvcost*data.eng$num_children + RPL_UC_ENG_bbATTR3polorientEnvcost*data.eng$ideo + abs(RPL_UC_ENG_sdbATTR3cost)* rep(rnorm(N.eng)))))
}

WTP_RPL_UC_ENG_hh3 <- rep(0,N.eng)
for ( i in 1:NumIter){
  WTP_RPL_UC_ENG_hh3[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <- (-(RPL_UC_ENG_bbATTR1hh3  + RPL_UC_ENG_bbATTR1ageEnvhh3*data.eng$age  + RPL_UC_ENG_bbATTR1cohabitEnvhh3*data.eng$cohabit  + RPL_UC_ENG_bbATTR1employedEnvhh3*data.eng$employed  + RPL_UC_ENG_bbATTR1femaleEnvhh3*data.eng$female  + RPL_UC_ENG_bbATTR1greenEnvhh3*data.eng$green  + RPL_UC_ENG_bbATTR1higheducEnvhh3*data.eng$higheduc  + RPL_UC_ENG_bbATTR1highincomeEnvhh3*data.eng$highincome  + RPL_UC_ENG_bbATTR1numchildEnvhh3*data.eng$num_children  + RPL_UC_ENG_bbATTR1polorientEnvhh3*data.eng$ideo  + abs(RPL_UC_ENG_sdbATTR1hh3) * rep(rnorm(N.eng)))/
                                                              (-exp(RPL_UC_ENG_bbATTR3cost + RPL_UC_ENG_bbATTR3ageEnvcost*data.eng$age + RPL_UC_ENG_bbATTR3cohabitEnvcost*data.eng$cohabit + RPL_UC_ENG_bbATTR3employedEnvcost*data.eng$employed + RPL_UC_ENG_bbATTR3femaleEnvcost*data.eng$female + RPL_UC_ENG_bbATTR3greenEnvcost*data.eng$green + RPL_UC_ENG_bbATTR3higheducEnvcost*data.eng$higheduc + RPL_UC_ENG_bbATTR3highincomeEnvcost*data.eng$highincome + RPL_UC_ENG_bbATTR3numchildEnvcost*data.eng$num_children + RPL_UC_ENG_bbATTR3polorientEnvcost*data.eng$ideo + abs(RPL_UC_ENG_sdbATTR3cost)* rep(rnorm(N.eng)))))
}

WTP_RPL_UC_ENG_coast2 <- rep(0,N.eng)
for ( i in 1:NumIter){
  WTP_RPL_UC_ENG_coast2[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <- (-(RPL_UC_ENG_bbATTR2coast2 + RPL_UC_ENG_bbATTR2ageEnvcoast2*data.eng$age + RPL_UC_ENG_bbATTR2cohabitEnvcoast2*data.eng$cohabit + RPL_UC_ENG_bbATTR2employedEnvcoast2*data.eng$employed + RPL_UC_ENG_bbATTR2femaleEnvcoast2*data.eng$female + RPL_UC_ENG_bbATTR2greenEnvcoast2*data.eng$green + RPL_UC_ENG_bbATTR2higheducEnvcoast2*data.eng$higheduc + RPL_UC_ENG_bbATTR2highincomeEnvcoast2*data.eng$highincome + RPL_UC_ENG_bbATTR2numchildEnvcoast2*data.eng$num_children + RPL_UC_ENG_bbATTR2polorientEnvcoast2*data.eng$ideo + abs(RPL_UC_ENG_sdbATTR2coast2) * rep(rnorm(N.eng)))/
                                                                 (-exp(RPL_UC_ENG_bbATTR3cost   + RPL_UC_ENG_bbATTR3ageEnvcost*data.eng$age   + RPL_UC_ENG_bbATTR3cohabitEnvcost*data.eng$cohabit   + RPL_UC_ENG_bbATTR3employedEnvcost*data.eng$employed   + RPL_UC_ENG_bbATTR3femaleEnvcost*data.eng$female   + RPL_UC_ENG_bbATTR3greenEnvcost*data.eng$green   + RPL_UC_ENG_bbATTR3higheducEnvcost*data.eng$higheduc   + RPL_UC_ENG_bbATTR3highincomeEnvcost*data.eng$highincome   + RPL_UC_ENG_bbATTR3numchildEnvcost*data.eng$num_children   + RPL_UC_ENG_bbATTR3polorientEnvcost*data.eng$ideo   + abs(RPL_UC_ENG_sdbATTR3cost)   * rep(rnorm(N.eng)))))
}

WTP_RPL_UC_ENG_coast3 <- rep(0,N.eng)
for ( i in 1:NumIter){
  WTP_RPL_UC_ENG_coast3[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <- (-(RPL_UC_ENG_bbATTR2coast3 + RPL_UC_ENG_bbATTR2ageEnvcoast3*data.eng$age + RPL_UC_ENG_bbATTR2cohabitEnvcoast3*data.eng$cohabit + RPL_UC_ENG_bbATTR2employedEnvcoast3*data.eng$employed + RPL_UC_ENG_bbATTR2femaleEnvcoast3*data.eng$female + RPL_UC_ENG_bbATTR2greenEnvcoast3*data.eng$green + RPL_UC_ENG_bbATTR2higheducEnvcoast3*data.eng$higheduc + RPL_UC_ENG_bbATTR2highincomeEnvcoast3*data.eng$highincome + RPL_UC_ENG_bbATTR2numchildEnvcoast3*data.eng$num_children + RPL_UC_ENG_bbATTR2polorientEnvcoast3*data.eng$ideo + abs(RPL_UC_ENG_sdbATTR2coast3) * rep(rnorm(N.eng)))/
                                                                 (-exp(RPL_UC_ENG_bbATTR3cost   + RPL_UC_ENG_bbATTR3ageEnvcost*data.eng$age   + RPL_UC_ENG_bbATTR3cohabitEnvcost*data.eng$cohabit   + RPL_UC_ENG_bbATTR3employedEnvcost*data.eng$employed   + RPL_UC_ENG_bbATTR3femaleEnvcost*data.eng$female   + RPL_UC_ENG_bbATTR3greenEnvcost*data.eng$green   + RPL_UC_ENG_bbATTR3higheducEnvcost*data.eng$higheduc   + RPL_UC_ENG_bbATTR3highincomeEnvcost*data.eng$highincome   + RPL_UC_ENG_bbATTR3numchildEnvcost*data.eng$num_children   + RPL_UC_ENG_bbATTR3polorientEnvcost*data.eng$ideo   + abs(RPL_UC_ENG_sdbATTR3cost)   * rep(rnorm(N.eng)))))
}

WTP_RPL_UC_ENG_perk1 <- rep(0,N.eng)
for ( i in 1:NumIter){
  WTP_RPL_UC_ENG_perk1[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <- (-(RPL_UC_ENG_bbATTR4perk1 + RPL_UC_ENG_bbATTR4ageEnvperk1*data.eng$age + RPL_UC_ENG_bbATTR4cohabitEnvperk1*data.eng$cohabit + RPL_UC_ENG_bbATTR4employedEnvperk1*data.eng$employed + RPL_UC_ENG_bbATTR4femaleEnvperk1*data.eng$female + RPL_UC_ENG_bbATTR4greenEnvperk1*data.eng$green + RPL_UC_ENG_bbATTR4higheducEnvperk1*data.eng$higheduc + RPL_UC_ENG_bbATTR4highincomeEnvperk1*data.eng$highincome + RPL_UC_ENG_bbATTR4numchildEnvperk1*data.eng$num_children + RPL_UC_ENG_bbATTR4polorientEnvperk1*data.eng$ideo + abs(RPL_UC_ENG_sdbATTR4perk1) * rep(rnorm(N.eng)))/
                                                                (-exp(RPL_UC_ENG_bbATTR3cost  + RPL_UC_ENG_bbATTR3ageEnvcost*data.eng$age  + RPL_UC_ENG_bbATTR3cohabitEnvcost*data.eng$cohabit  + RPL_UC_ENG_bbATTR3employedEnvcost*data.eng$employed  + RPL_UC_ENG_bbATTR3femaleEnvcost*data.eng$female  + RPL_UC_ENG_bbATTR3greenEnvcost*data.eng$green  + RPL_UC_ENG_bbATTR3higheducEnvcost*data.eng$higheduc  + RPL_UC_ENG_bbATTR3highincomeEnvcost*data.eng$highincome  + RPL_UC_ENG_bbATTR3numchildEnvcost*data.eng$num_children  + RPL_UC_ENG_bbATTR3polorientEnvcost*data.eng$ideo  + abs(RPL_UC_ENG_sdbATTR3cost)  * rep(rnorm(N.eng)))))
}

WTP_RPL_UC_ENG_perk2 <- rep(0,N.eng)
for ( i in 1:NumIter){
  WTP_RPL_UC_ENG_perk2[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <- (-(RPL_UC_ENG_bbATTR4perk2 + RPL_UC_ENG_bbATTR4ageEnvperk2*data.eng$age + RPL_UC_ENG_bbATTR4cohabitEnvperk2*data.eng$cohabit + RPL_UC_ENG_bbATTR4employedEnvperk2*data.eng$employed + RPL_UC_ENG_bbATTR4femaleEnvperk2*data.eng$female + RPL_UC_ENG_bbATTR4greenEnvperk2*data.eng$green + RPL_UC_ENG_bbATTR4higheducEnvperk2*data.eng$higheduc + RPL_UC_ENG_bbATTR4highincomeEnvperk2*data.eng$highincome + RPL_UC_ENG_bbATTR4numchildEnvperk2*data.eng$num_children + RPL_UC_ENG_bbATTR4polorientEnvperk2*data.eng$ideo + abs(RPL_UC_ENG_sdbATTR4perk2) * rep(rnorm(N.eng)))/
                                                                (-exp(RPL_UC_ENG_bbATTR3cost  + RPL_UC_ENG_bbATTR3ageEnvcost*data.eng$age  + RPL_UC_ENG_bbATTR3cohabitEnvcost*data.eng$cohabit  + RPL_UC_ENG_bbATTR3employedEnvcost*data.eng$employed  + RPL_UC_ENG_bbATTR3femaleEnvcost*data.eng$female  + RPL_UC_ENG_bbATTR3greenEnvcost*data.eng$green  + RPL_UC_ENG_bbATTR3higheducEnvcost*data.eng$higheduc  + RPL_UC_ENG_bbATTR3highincomeEnvcost*data.eng$highincome  + RPL_UC_ENG_bbATTR3numchildEnvcost*data.eng$num_children  + RPL_UC_ENG_bbATTR3polorientEnvcost*data.eng$ideo  + abs(RPL_UC_ENG_sdbATTR3cost)  * rep(rnorm(N.eng)))))
}


###################

# Scotland
WTP_RPL_UC_SCOT_hh2 <- rep(0,N.scot)
for ( i in 1:NumIter){
  WTP_RPL_UC_SCOT_hh2[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- (-(RPL_UC_SCOT_bbATTR1hh2  + RPL_UC_SCOT_bbATTR1ageEnvhh2*data.scot$age  + RPL_UC_SCOT_bbATTR1cohabitEnvhh2*data.scot$cohabit  + RPL_UC_SCOT_bbATTR1employedEnvhh2*data.scot$employed  + RPL_UC_SCOT_bbATTR1femaleEnvhh2*data.scot$female  + RPL_UC_SCOT_bbATTR1greenEnvhh2*data.scot$green  + RPL_UC_SCOT_bbATTR1higheducEnvhh2*data.scot$higheduc  + RPL_UC_SCOT_bbATTR1highincomeEnvhh2*data.scot$highincome  + RPL_UC_SCOT_bbATTR1numchildEnvhh2*data.scot$num_children  + RPL_UC_SCOT_bbATTR1polorientEnvhh2*data.scot$ideo  + abs(RPL_UC_SCOT_sdbATTR1hh2) * rep(rnorm(N.scot)))/
                                                                  (-exp(RPL_UC_SCOT_bbATTR3cost + RPL_UC_SCOT_bbATTR3ageEnvcost*data.scot$age + RPL_UC_SCOT_bbATTR3cohabitEnvcost*data.scot$cohabit + RPL_UC_SCOT_bbATTR3employedEnvcost*data.scot$employed + RPL_UC_SCOT_bbATTR3femaleEnvcost*data.scot$female + RPL_UC_SCOT_bbATTR3greenEnvcost*data.scot$green + RPL_UC_SCOT_bbATTR3higheducEnvcost*data.scot$higheduc + RPL_UC_SCOT_bbATTR3highincomeEnvcost*data.scot$highincome + RPL_UC_SCOT_bbATTR3numchildEnvcost*data.scot$num_children + RPL_UC_SCOT_bbATTR3polorientEnvcost*data.scot$ideo + abs(RPL_UC_SCOT_sdbATTR3cost)* rep(rnorm(N.scot)))))
}

WTP_RPL_UC_SCOT_hh3 <- rep(0,N.scot)
for ( i in 1:NumIter){
  WTP_RPL_UC_SCOT_hh3[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- (-(RPL_UC_SCOT_bbATTR1hh3  + RPL_UC_SCOT_bbATTR1ageEnvhh3*data.scot$age  + RPL_UC_SCOT_bbATTR1cohabitEnvhh3*data.scot$cohabit  + RPL_UC_SCOT_bbATTR1employedEnvhh3*data.scot$employed  + RPL_UC_SCOT_bbATTR1femaleEnvhh3*data.scot$female  + RPL_UC_SCOT_bbATTR1greenEnvhh3*data.scot$green  + RPL_UC_SCOT_bbATTR1higheducEnvhh3*data.scot$higheduc  + RPL_UC_SCOT_bbATTR1highincomeEnvhh3*data.scot$highincome  + RPL_UC_SCOT_bbATTR1numchildEnvhh3*data.scot$num_children  + RPL_UC_SCOT_bbATTR1polorientEnvhh3*data.scot$ideo  + abs(RPL_UC_SCOT_sdbATTR1hh3) * rep(rnorm(N.scot)))/
                                                                  (-exp(RPL_UC_SCOT_bbATTR3cost + RPL_UC_SCOT_bbATTR3ageEnvcost*data.scot$age + RPL_UC_SCOT_bbATTR3cohabitEnvcost*data.scot$cohabit + RPL_UC_SCOT_bbATTR3employedEnvcost*data.scot$employed + RPL_UC_SCOT_bbATTR3femaleEnvcost*data.scot$female + RPL_UC_SCOT_bbATTR3greenEnvcost*data.scot$green + RPL_UC_SCOT_bbATTR3higheducEnvcost*data.scot$higheduc + RPL_UC_SCOT_bbATTR3highincomeEnvcost*data.scot$highincome + RPL_UC_SCOT_bbATTR3numchildEnvcost*data.scot$num_children + RPL_UC_SCOT_bbATTR3polorientEnvcost*data.scot$ideo + abs(RPL_UC_SCOT_sdbATTR3cost)* rep(rnorm(N.scot)))))
}

WTP_RPL_UC_SCOT_coast2 <- rep(0,N.scot)
for ( i in 1:NumIter){
  WTP_RPL_UC_SCOT_coast2[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- (-(RPL_UC_SCOT_bbATTR2coast2 + RPL_UC_SCOT_bbATTR2ageEnvcoast2*data.scot$age + RPL_UC_SCOT_bbATTR2cohabitEnvcoast2*data.scot$cohabit + RPL_UC_SCOT_bbATTR2employedEnvcoast2*data.scot$employed + RPL_UC_SCOT_bbATTR2femaleEnvcoast2*data.scot$female + RPL_UC_SCOT_bbATTR2greenEnvcoast2*data.scot$green + RPL_UC_SCOT_bbATTR2higheducEnvcoast2*data.scot$higheduc + RPL_UC_SCOT_bbATTR2highincomeEnvcoast2*data.scot$highincome + RPL_UC_SCOT_bbATTR2numchildEnvcoast2*data.scot$num_children + RPL_UC_SCOT_bbATTR2polorientEnvcoast2*data.scot$ideo + abs(RPL_UC_SCOT_sdbATTR2coast2) * rep(rnorm(N.scot)))/
                                                                     (-exp(RPL_UC_SCOT_bbATTR3cost   + RPL_UC_SCOT_bbATTR3ageEnvcost*data.scot$age   + RPL_UC_SCOT_bbATTR3cohabitEnvcost*data.scot$cohabit   + RPL_UC_SCOT_bbATTR3employedEnvcost*data.scot$employed   + RPL_UC_SCOT_bbATTR3femaleEnvcost*data.scot$female   + RPL_UC_SCOT_bbATTR3greenEnvcost*data.scot$green   + RPL_UC_SCOT_bbATTR3higheducEnvcost*data.scot$higheduc   + RPL_UC_SCOT_bbATTR3highincomeEnvcost*data.scot$highincome   + RPL_UC_SCOT_bbATTR3numchildEnvcost*data.scot$num_children   + RPL_UC_SCOT_bbATTR3polorientEnvcost*data.scot$ideo   + abs(RPL_UC_SCOT_sdbATTR3cost)   * rep(rnorm(N.scot)))))
}

WTP_RPL_UC_SCOT_coast3 <- rep(0,N.scot)
for ( i in 1:NumIter){
  WTP_RPL_UC_SCOT_coast3[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- (-(RPL_UC_SCOT_bbATTR2coast3 + RPL_UC_SCOT_bbATTR2ageEnvcoast3*data.scot$age + RPL_UC_SCOT_bbATTR2cohabitEnvcoast3*data.scot$cohabit + RPL_UC_SCOT_bbATTR2employedEnvcoast3*data.scot$employed + RPL_UC_SCOT_bbATTR2femaleEnvcoast3*data.scot$female + RPL_UC_SCOT_bbATTR2greenEnvcoast3*data.scot$green + RPL_UC_SCOT_bbATTR2higheducEnvcoast3*data.scot$higheduc + RPL_UC_SCOT_bbATTR2highincomeEnvcoast3*data.scot$highincome + RPL_UC_SCOT_bbATTR2numchildEnvcoast3*data.scot$num_children + RPL_UC_SCOT_bbATTR2polorientEnvcoast3*data.scot$ideo + abs(RPL_UC_SCOT_sdbATTR2coast3) * rep(rnorm(N.scot)))/
                                                                     (-exp(RPL_UC_SCOT_bbATTR3cost   + RPL_UC_SCOT_bbATTR3ageEnvcost*data.scot$age   + RPL_UC_SCOT_bbATTR3cohabitEnvcost*data.scot$cohabit   + RPL_UC_SCOT_bbATTR3employedEnvcost*data.scot$employed   + RPL_UC_SCOT_bbATTR3femaleEnvcost*data.scot$female   + RPL_UC_SCOT_bbATTR3greenEnvcost*data.scot$green   + RPL_UC_SCOT_bbATTR3higheducEnvcost*data.scot$higheduc   + RPL_UC_SCOT_bbATTR3highincomeEnvcost*data.scot$highincome   + RPL_UC_SCOT_bbATTR3numchildEnvcost*data.scot$num_children   + RPL_UC_SCOT_bbATTR3polorientEnvcost*data.scot$ideo   + abs(RPL_UC_SCOT_sdbATTR3cost)   * rep(rnorm(N.scot)))))
}

WTP_RPL_UC_SCOT_perk1 <- rep(0,N.scot)
for ( i in 1:NumIter){
  WTP_RPL_UC_SCOT_perk1[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- (-(RPL_UC_SCOT_bbATTR4perk1 + RPL_UC_SCOT_bbATTR4ageEnvperk1*data.scot$age + RPL_UC_SCOT_bbATTR4cohabitEnvperk1*data.scot$cohabit + RPL_UC_SCOT_bbATTR4employedEnvperk1*data.scot$employed + RPL_UC_SCOT_bbATTR4femaleEnvperk1*data.scot$female + RPL_UC_SCOT_bbATTR4greenEnvperk1*data.scot$green + RPL_UC_SCOT_bbATTR4higheducEnvperk1*data.scot$higheduc + RPL_UC_SCOT_bbATTR4highincomeEnvperk1*data.scot$highincome + RPL_UC_SCOT_bbATTR4numchildEnvperk1*data.scot$num_children + RPL_UC_SCOT_bbATTR4polorientEnvperk1*data.scot$ideo + abs(RPL_UC_SCOT_sdbATTR4perk1) * rep(rnorm(N.scot)))/
                                                                    (-exp(RPL_UC_SCOT_bbATTR3cost  + RPL_UC_SCOT_bbATTR3ageEnvcost*data.scot$age  + RPL_UC_SCOT_bbATTR3cohabitEnvcost*data.scot$cohabit  + RPL_UC_SCOT_bbATTR3employedEnvcost*data.scot$employed  + RPL_UC_SCOT_bbATTR3femaleEnvcost*data.scot$female  + RPL_UC_SCOT_bbATTR3greenEnvcost*data.scot$green  + RPL_UC_SCOT_bbATTR3higheducEnvcost*data.scot$higheduc  + RPL_UC_SCOT_bbATTR3highincomeEnvcost*data.scot$highincome  + RPL_UC_SCOT_bbATTR3numchildEnvcost*data.scot$num_children  + RPL_UC_SCOT_bbATTR3polorientEnvcost*data.scot$ideo  + abs(RPL_UC_SCOT_sdbATTR3cost)  * rep(rnorm(N.scot)))))
}

WTP_RPL_UC_SCOT_perk2 <- rep(0,N.scot)
for ( i in 1:NumIter){
  WTP_RPL_UC_SCOT_perk2[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- (-(RPL_UC_SCOT_bbATTR4perk2 + RPL_UC_SCOT_bbATTR4ageEnvperk2*data.scot$age + RPL_UC_SCOT_bbATTR4cohabitEnvperk2*data.scot$cohabit + RPL_UC_SCOT_bbATTR4employedEnvperk2*data.scot$employed + RPL_UC_SCOT_bbATTR4femaleEnvperk2*data.scot$female + RPL_UC_SCOT_bbATTR4greenEnvperk2*data.scot$green + RPL_UC_SCOT_bbATTR4higheducEnvperk2*data.scot$higheduc + RPL_UC_SCOT_bbATTR4highincomeEnvperk2*data.scot$highincome + RPL_UC_SCOT_bbATTR4numchildEnvperk2*data.scot$num_children + RPL_UC_SCOT_bbATTR4polorientEnvperk2*data.scot$ideo + abs(RPL_UC_SCOT_sdbATTR4perk2) * rep(rnorm(N.scot)))/
                                                                    (-exp(RPL_UC_SCOT_bbATTR3cost  + RPL_UC_SCOT_bbATTR3ageEnvcost*data.scot$age  + RPL_UC_SCOT_bbATTR3cohabitEnvcost*data.scot$cohabit  + RPL_UC_SCOT_bbATTR3employedEnvcost*data.scot$employed  + RPL_UC_SCOT_bbATTR3femaleEnvcost*data.scot$female  + RPL_UC_SCOT_bbATTR3greenEnvcost*data.scot$green  + RPL_UC_SCOT_bbATTR3higheducEnvcost*data.scot$higheduc  + RPL_UC_SCOT_bbATTR3highincomeEnvcost*data.scot$highincome  + RPL_UC_SCOT_bbATTR3numchildEnvcost*data.scot$num_children  + RPL_UC_SCOT_bbATTR3polorientEnvcost*data.scot$ideo  + abs(RPL_UC_SCOT_sdbATTR3cost)  * rep(rnorm(N.scot)))))
}

#######################

# Northern Ireland
WTP_RPL_UC_NI_hh2 <- rep(0,N.ni)
for ( i in 1:NumIter){
  WTP_RPL_UC_NI_hh2[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <- (-(RPL_UC_NI_bbATTR1hh2  + RPL_UC_NI_bbATTR1ageEnvhh2*data.ni$age  + RPL_UC_NI_bbATTR1cohabitEnvhh2*data.ni$cohabit  + RPL_UC_NI_bbATTR1employedEnvhh2*data.ni$employed  + RPL_UC_NI_bbATTR1femaleEnvhh2*data.ni$female  + RPL_UC_NI_bbATTR1greenEnvhh2*data.ni$green  + RPL_UC_NI_bbATTR1higheducEnvhh2*data.ni$higheduc  + RPL_UC_NI_bbATTR1highincomeEnvhh2*data.ni$highincome  + RPL_UC_NI_bbATTR1numchildEnvhh2*data.ni$num_children  + RPL_UC_NI_bbATTR1polorientEnvhh2*data.ni$ideo  + abs(RPL_UC_NI_sdbATTR1hh2) * rep(rnorm(N.ni)))/
                                                          (-exp(RPL_UC_NI_bbATTR3cost + RPL_UC_NI_bbATTR3ageEnvcost*data.ni$age + RPL_UC_NI_bbATTR3cohabitEnvcost*data.ni$cohabit + RPL_UC_NI_bbATTR3employedEnvcost*data.ni$employed + RPL_UC_NI_bbATTR3femaleEnvcost*data.ni$female + RPL_UC_NI_bbATTR3greenEnvcost*data.ni$green + RPL_UC_NI_bbATTR3higheducEnvcost*data.ni$higheduc + RPL_UC_NI_bbATTR3highincomeEnvcost*data.ni$highincome + RPL_UC_NI_bbATTR3numchildEnvcost*data.ni$num_children + RPL_UC_NI_bbATTR3polorientEnvcost*data.ni$ideo + abs(RPL_UC_NI_sdbATTR3cost)* rep(rnorm(N.ni)))))
}

WTP_RPL_UC_NI_hh3 <- rep(0,N.ni)
for ( i in 1:NumIter){
  WTP_RPL_UC_NI_hh3[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <- (-(RPL_UC_NI_bbATTR1hh3  + RPL_UC_NI_bbATTR1ageEnvhh3*data.ni$age  + RPL_UC_NI_bbATTR1cohabitEnvhh3*data.ni$cohabit  + RPL_UC_NI_bbATTR1employedEnvhh3*data.ni$employed  + RPL_UC_NI_bbATTR1femaleEnvhh3*data.ni$female  + RPL_UC_NI_bbATTR1greenEnvhh3*data.ni$green  + RPL_UC_NI_bbATTR1higheducEnvhh3*data.ni$higheduc  + RPL_UC_NI_bbATTR1highincomeEnvhh3*data.ni$highincome  + RPL_UC_NI_bbATTR1numchildEnvhh3*data.ni$num_children  + RPL_UC_NI_bbATTR1polorientEnvhh3*data.ni$ideo  + abs(RPL_UC_NI_sdbATTR1hh3) * rep(rnorm(N.ni)))/
                                                          (-exp(RPL_UC_NI_bbATTR3cost + RPL_UC_NI_bbATTR3ageEnvcost*data.ni$age + RPL_UC_NI_bbATTR3cohabitEnvcost*data.ni$cohabit + RPL_UC_NI_bbATTR3employedEnvcost*data.ni$employed + RPL_UC_NI_bbATTR3femaleEnvcost*data.ni$female + RPL_UC_NI_bbATTR3greenEnvcost*data.ni$green + RPL_UC_NI_bbATTR3higheducEnvcost*data.ni$higheduc + RPL_UC_NI_bbATTR3highincomeEnvcost*data.ni$highincome + RPL_UC_NI_bbATTR3numchildEnvcost*data.ni$num_children + RPL_UC_NI_bbATTR3polorientEnvcost*data.ni$ideo + abs(RPL_UC_NI_sdbATTR3cost)* rep(rnorm(N.ni)))))
}

WTP_RPL_UC_NI_coast2 <- rep(0,N.ni)
for ( i in 1:NumIter){
  WTP_RPL_UC_NI_coast2[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <- (-(RPL_UC_NI_bbATTR2coast2 + RPL_UC_NI_bbATTR2ageEnvcoast2*data.ni$age + RPL_UC_NI_bbATTR2cohabitEnvcoast2*data.ni$cohabit + RPL_UC_NI_bbATTR2employedEnvcoast2*data.ni$employed + RPL_UC_NI_bbATTR2femaleEnvcoast2*data.ni$female + RPL_UC_NI_bbATTR2greenEnvcoast2*data.ni$green + RPL_UC_NI_bbATTR2higheducEnvcoast2*data.ni$higheduc + RPL_UC_NI_bbATTR2highincomeEnvcoast2*data.ni$highincome + RPL_UC_NI_bbATTR2numchildEnvcoast2*data.ni$num_children + RPL_UC_NI_bbATTR2polorientEnvcoast2*data.ni$ideo + abs(RPL_UC_NI_sdbATTR2coast2) * rep(rnorm(N.ni)))/
                                                             (-exp(RPL_UC_NI_bbATTR3cost   + RPL_UC_NI_bbATTR3ageEnvcost*data.ni$age   + RPL_UC_NI_bbATTR3cohabitEnvcost*data.ni$cohabit   + RPL_UC_NI_bbATTR3employedEnvcost*data.ni$employed   + RPL_UC_NI_bbATTR3femaleEnvcost*data.ni$female   + RPL_UC_NI_bbATTR3greenEnvcost*data.ni$green   + RPL_UC_NI_bbATTR3higheducEnvcost*data.ni$higheduc   + RPL_UC_NI_bbATTR3highincomeEnvcost*data.ni$highincome   + RPL_UC_NI_bbATTR3numchildEnvcost*data.ni$num_children   + RPL_UC_NI_bbATTR3polorientEnvcost*data.ni$ideo   + abs(RPL_UC_NI_sdbATTR3cost)   * rep(rnorm(N.ni)))))
}

WTP_RPL_UC_NI_coast3 <- rep(0,N.ni)
for ( i in 1:NumIter){
  WTP_RPL_UC_NI_coast3[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <- (-(RPL_UC_NI_bbATTR2coast3 + RPL_UC_NI_bbATTR2ageEnvcoast3*data.ni$age + RPL_UC_NI_bbATTR2cohabitEnvcoast3*data.ni$cohabit + RPL_UC_NI_bbATTR2employedEnvcoast3*data.ni$employed + RPL_UC_NI_bbATTR2femaleEnvcoast3*data.ni$female + RPL_UC_NI_bbATTR2greenEnvcoast3*data.ni$green + RPL_UC_NI_bbATTR2higheducEnvcoast3*data.ni$higheduc + RPL_UC_NI_bbATTR2highincomeEnvcoast3*data.ni$highincome + RPL_UC_NI_bbATTR2numchildEnvcoast3*data.ni$num_children + RPL_UC_NI_bbATTR2polorientEnvcoast3*data.ni$ideo + abs(RPL_UC_NI_sdbATTR2coast3) * rep(rnorm(N.ni)))/
                                                             (-exp(RPL_UC_NI_bbATTR3cost   + RPL_UC_NI_bbATTR3ageEnvcost*data.ni$age   + RPL_UC_NI_bbATTR3cohabitEnvcost*data.ni$cohabit   + RPL_UC_NI_bbATTR3employedEnvcost*data.ni$employed   + RPL_UC_NI_bbATTR3femaleEnvcost*data.ni$female   + RPL_UC_NI_bbATTR3greenEnvcost*data.ni$green   + RPL_UC_NI_bbATTR3higheducEnvcost*data.ni$higheduc   + RPL_UC_NI_bbATTR3highincomeEnvcost*data.ni$highincome   + RPL_UC_NI_bbATTR3numchildEnvcost*data.ni$num_children   + RPL_UC_NI_bbATTR3polorientEnvcost*data.ni$ideo   + abs(RPL_UC_NI_sdbATTR3cost)   * rep(rnorm(N.ni)))))
}

WTP_RPL_UC_NI_perk1 <- rep(0,N.ni)
for ( i in 1:NumIter){
  WTP_RPL_UC_NI_perk1[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <- (-(RPL_UC_NI_bbATTR4perk1 + RPL_UC_NI_bbATTR4ageEnvperk1*data.ni$age + RPL_UC_NI_bbATTR4cohabitEnvperk1*data.ni$cohabit + RPL_UC_NI_bbATTR4employedEnvperk1*data.ni$employed + RPL_UC_NI_bbATTR4femaleEnvperk1*data.ni$female + RPL_UC_NI_bbATTR4greenEnvperk1*data.ni$green + RPL_UC_NI_bbATTR4higheducEnvperk1*data.ni$higheduc + RPL_UC_NI_bbATTR4highincomeEnvperk1*data.ni$highincome + RPL_UC_NI_bbATTR4numchildEnvperk1*data.ni$num_children + RPL_UC_NI_bbATTR4polorientEnvperk1*data.ni$ideo + abs(RPL_UC_NI_sdbATTR4perk1) * rep(rnorm(N.ni)))/
                                                            (-exp(RPL_UC_NI_bbATTR3cost  + RPL_UC_NI_bbATTR3ageEnvcost*data.ni$age  + RPL_UC_NI_bbATTR3cohabitEnvcost*data.ni$cohabit  + RPL_UC_NI_bbATTR3employedEnvcost*data.ni$employed  + RPL_UC_NI_bbATTR3femaleEnvcost*data.ni$female  + RPL_UC_NI_bbATTR3greenEnvcost*data.ni$green  + RPL_UC_NI_bbATTR3higheducEnvcost*data.ni$higheduc  + RPL_UC_NI_bbATTR3highincomeEnvcost*data.ni$highincome  + RPL_UC_NI_bbATTR3numchildEnvcost*data.ni$num_children  + RPL_UC_NI_bbATTR3polorientEnvcost*data.ni$ideo  + abs(RPL_UC_NI_sdbATTR3cost)  * rep(rnorm(N.ni)))))
}

WTP_RPL_UC_NI_perk2 <- rep(0,N.ni)
for ( i in 1:NumIter){
  WTP_RPL_UC_NI_perk2[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <- (-(RPL_UC_NI_bbATTR4perk2 + RPL_UC_NI_bbATTR4ageEnvperk2*data.ni$age + RPL_UC_NI_bbATTR4cohabitEnvperk2*data.ni$cohabit + RPL_UC_NI_bbATTR4employedEnvperk2*data.ni$employed + RPL_UC_NI_bbATTR4femaleEnvperk2*data.ni$female + RPL_UC_NI_bbATTR4greenEnvperk2*data.ni$green + RPL_UC_NI_bbATTR4higheducEnvperk2*data.ni$higheduc + RPL_UC_NI_bbATTR4highincomeEnvperk2*data.ni$highincome + RPL_UC_NI_bbATTR4numchildEnvperk2*data.ni$num_children + RPL_UC_NI_bbATTR4polorientEnvperk2*data.ni$ideo + abs(RPL_UC_NI_sdbATTR4perk2) * rep(rnorm(N.ni)))/
                                                            (-exp(RPL_UC_NI_bbATTR3cost  + RPL_UC_NI_bbATTR3ageEnvcost*data.ni$age  + RPL_UC_NI_bbATTR3cohabitEnvcost*data.ni$cohabit  + RPL_UC_NI_bbATTR3employedEnvcost*data.ni$employed  + RPL_UC_NI_bbATTR3femaleEnvcost*data.ni$female  + RPL_UC_NI_bbATTR3greenEnvcost*data.ni$green  + RPL_UC_NI_bbATTR3higheducEnvcost*data.ni$higheduc  + RPL_UC_NI_bbATTR3highincomeEnvcost*data.ni$highincome  + RPL_UC_NI_bbATTR3numchildEnvcost*data.ni$num_children  + RPL_UC_NI_bbATTR3polorientEnvcost*data.ni$ideo  + abs(RPL_UC_NI_sdbATTR3cost)  * rep(rnorm(N.ni)))))
}



#######################
###   HYBRID MODEL  ###
#######################

# England

# Need to create the latent variable first:
LV.eng <- rep(0,N.eng)
length(LV.eng)


for ( i in 1:NumIter){                                 
  LV.eng[(((i-1)*N.eng)+1):(((i-1)*N.eng)+N.eng)] <-  HCM_ENG_bsc_age	   	 	     * data.eng$age	   	    	+
                                                      HCM_ENG_bsc_cohabit		     * data.eng$cohabit		    +
                                                      HCM_ENG_bsc_employed	     * data.eng$employed      +
                                                      HCM_ENG_bsc_female		     * data.eng$female	 	    +
                                                      HCM_ENG_bsc_green		       * data.eng$green		      +
                                                      HCM_ENG_bsc_higheduc	     * data.eng$higheduc      +
                                                      HCM_ENG_bsc_highincome     * data.eng$highincome    +
                                                      HCM_ENG_bsc_numchild    	 * data.eng$num_children	+
                                                      HCM_ENG_bsc_polorient    	 * data.eng$ideo        	+
                                                      rep(rnorm(N.eng))
}



#  WTP from Hybrid Model 
WTP_HCM_ENG_hh2    <- (-(HCM_ENG_bbATTR1hh2  + HCM_ENG_bbATTR1LVEnvhh2 * LV.eng + HCM_ENG_sdbATTR1hh2 * rep(rnorm(N.eng*NumIter)))/
                   (-exp(HCM_ENG_bbATTR3cost + HCM_ENG_bbATTR3LVEnvcost* LV.eng + HCM_ENG_sdbATTR3cost* rep(rnorm(N.eng*NumIter)))))

WTP_HCM_ENG_hh3    <- (-(HCM_ENG_bbATTR1hh3  + HCM_ENG_bbATTR1LVEnvhh3 * LV.eng + HCM_ENG_sdbATTR1hh3 * rep(rnorm(N.eng*NumIter)))/
                   (-exp(HCM_ENG_bbATTR3cost + HCM_ENG_bbATTR3LVEnvcost* LV.eng + HCM_ENG_sdbATTR3cost* rep(rnorm(N.eng*NumIter)))))

WTP_HCM_ENG_coast2 <- (-(HCM_ENG_bbATTR2coast2 + HCM_ENG_bbATTR2LVEnvcoast2 * LV.eng + HCM_ENG_sdbATTR2coast2 * rep(rnorm(N.eng*NumIter)))/
                   (-exp(HCM_ENG_bbATTR3cost   + HCM_ENG_bbATTR3LVEnvcost   * LV.eng + HCM_ENG_sdbATTR3cost   * rep(rnorm(N.eng*NumIter)))))

WTP_HCM_ENG_coast3 <- (-(HCM_ENG_bbATTR2coast3 + HCM_ENG_bbATTR2LVEnvcoast3 * LV.eng + HCM_ENG_sdbATTR2coast3 * rep(rnorm(N.eng*NumIter)))/
                   (-exp(HCM_ENG_bbATTR3cost   + HCM_ENG_bbATTR3LVEnvcost   * LV.eng + HCM_ENG_sdbATTR3cost   * rep(rnorm(N.eng*NumIter)))))

WTP_HCM_ENG_perk1  <- (-(HCM_ENG_bbATTR4perk1 + HCM_ENG_bbATTR4LVEnvperk1 * LV.eng + HCM_ENG_sdbATTR4perk1 * rep(rnorm(N.eng*NumIter)))/
                   (-exp(HCM_ENG_bbATTR3cost  + HCM_ENG_bbATTR3LVEnvcost  * LV.eng + HCM_ENG_sdbATTR3cost  * rep(rnorm(N.eng*NumIter)))))

WTP_HCM_ENG_perk2  <- (-(HCM_ENG_bbATTR4perk2 + HCM_ENG_bbATTR4LVEnvperk2 * LV.eng + HCM_ENG_sdbATTR4perk2 * rep(rnorm(N.eng*NumIter)))/
                   (-exp(HCM_ENG_bbATTR3cost  + HCM_ENG_bbATTR3LVEnvcost  * LV.eng + HCM_ENG_sdbATTR3cost  * rep(rnorm(N.eng*NumIter)))))


#############################################

# Scotland

# Need to create the latent variable first:
LV.scot <- rep(0,N.scot)
length(LV.scot)


for ( i in 1:NumIter){                                
  LV.scot[(((i-1)*N.scot)+1):(((i-1)*N.scot)+N.scot)] <- HCM_SCOT_bsc_age	   	       * data.scot$age	   	    	+
                                                         HCM_SCOT_bsc_cohabit		     * data.scot$cohabit		    +
                                                         HCM_SCOT_bsc_employed	     * data.scot$employed       +
                                                         HCM_SCOT_bsc_female		     * data.scot$female	 	      +
                                                         HCM_SCOT_bsc_green		       * data.scot$green		      +
                                                         HCM_SCOT_bsc_higheduc	     * data.scot$higheduc       +
                                                         HCM_SCOT_bsc_highincome     * data.scot$highincome     +
                                                         HCM_SCOT_bsc_numchild    	 * data.scot$num_children	  +
                                                         HCM_SCOT_bsc_polorient    	 * data.scot$ideo        	  +
                                                         rep(rnorm(N.scot))
}



# WTP from Hybrid Model
WTP_HCM_SCOT_hh2    <- (-(HCM_SCOT_bbATTR1hh2  + HCM_SCOT_bbATTR1LVEnvhh2 * LV.scot + HCM_SCOT_sdbATTR1hh2 * rep(rnorm(N.scot*NumIter)))/
                    (-exp(HCM_SCOT_bbATTR3cost + HCM_SCOT_bbATTR3LVEnvcost* LV.scot + HCM_SCOT_sdbATTR3cost* rep(rnorm(N.scot*NumIter)))))

WTP_HCM_SCOT_hh3    <- (-(HCM_SCOT_bbATTR1hh3  + HCM_SCOT_bbATTR1LVEnvhh3 * LV.scot + HCM_SCOT_sdbATTR1hh3 * rep(rnorm(N.scot*NumIter)))/
                    (-exp(HCM_SCOT_bbATTR3cost + HCM_SCOT_bbATTR3LVEnvcost* LV.scot + HCM_SCOT_sdbATTR3cost* rep(rnorm(N.scot*NumIter)))))

WTP_HCM_SCOT_coast2 <- (-(HCM_SCOT_bbATTR2coast2 + HCM_SCOT_bbATTR2LVEnvcoast2 * LV.scot + HCM_SCOT_sdbATTR2coast2 * rep(rnorm(N.scot*NumIter)))/
                    (-exp(HCM_SCOT_bbATTR3cost   + HCM_SCOT_bbATTR3LVEnvcost   * LV.scot + HCM_SCOT_sdbATTR3cost   * rep(rnorm(N.scot*NumIter)))))

WTP_HCM_SCOT_coast3 <- (-(HCM_SCOT_bbATTR2coast3 + HCM_SCOT_bbATTR2LVEnvcoast3 * LV.scot + HCM_SCOT_sdbATTR2coast3 * rep(rnorm(N.scot*NumIter)))/
                    (-exp(HCM_SCOT_bbATTR3cost   + HCM_SCOT_bbATTR3LVEnvcost   * LV.scot + HCM_SCOT_sdbATTR3cost   * rep(rnorm(N.scot*NumIter)))))

WTP_HCM_SCOT_perk1  <- (-(HCM_SCOT_bbATTR4perk1 + HCM_SCOT_bbATTR4LVEnvperk1 * LV.scot + HCM_SCOT_sdbATTR4perk1 * rep(rnorm(N.scot*NumIter)))/
                    (-exp(HCM_SCOT_bbATTR3cost  + HCM_SCOT_bbATTR3LVEnvcost  * LV.scot + HCM_SCOT_sdbATTR3cost  * rep(rnorm(N.scot*NumIter)))))

WTP_HCM_SCOT_perk2  <- (-(HCM_SCOT_bbATTR4perk2 + HCM_SCOT_bbATTR4LVEnvperk2 * LV.scot + HCM_SCOT_sdbATTR4perk2 * rep(rnorm(N.scot*NumIter)))/
                    (-exp(HCM_SCOT_bbATTR3cost  + HCM_SCOT_bbATTR3LVEnvcost  * LV.scot + HCM_SCOT_sdbATTR3cost  * rep(rnorm(N.scot*NumIter)))))

###########################################

# Northern Ireland

# Need to create the latent variable first:
LV.ni <- rep(0,N.ni)
length(LV.ni)


for ( i in 1:NumIter){                                
  LV.ni[(((i-1)*N.ni)+1):(((i-1)*N.ni)+N.ni)] <-  HCM_NI_bsc_age	   	 	 * data.ni$age	   	    	+
                                                  HCM_NI_bsc_cohabit		 * data.ni$cohabit		    +
                                                  HCM_NI_bsc_employed	   * data.ni$employed       +
                                                  HCM_NI_bsc_female		   * data.ni$female	 	      +
                                                  HCM_NI_bsc_green		   * data.ni$green		      +
                                                  HCM_NI_bsc_higheduc	   * data.ni$higheduc       +
                                                  HCM_NI_bsc_highincome  * data.ni$highincome     +
                                                  HCM_NI_bsc_numchild    * data.ni$num_children	  +
                                                  HCM_NI_bsc_polorient   * data.ni$ideo        	  +
                                                  rep(rnorm(N.ni))
}



# WTP from Hybrid Model - NEED TO FINISH THIS
WTP_HCM_NI_hh2    <- (-(HCM_NI_bbATTR1hh2  + HCM_NI_bbATTR1LVEnvhh2 * LV.ni + HCM_NI_sdbATTR1hh2 * rep(rnorm(N.ni*NumIter)))/
                  (-exp(HCM_NI_bbATTR3cost + HCM_NI_bbATTR3LVEnvcost* LV.ni + HCM_NI_sdbATTR3cost* rep(rnorm(N.ni*NumIter)))))

WTP_HCM_NI_hh3    <- (-(HCM_NI_bbATTR1hh3  + HCM_NI_bbATTR1LVEnvhh3 * LV.ni + HCM_NI_sdbATTR1hh3 * rep(rnorm(N.ni*NumIter)))/
                  (-exp(HCM_NI_bbATTR3cost + HCM_NI_bbATTR3LVEnvcost* LV.ni + HCM_NI_sdbATTR3cost* rep(rnorm(N.ni*NumIter)))))

WTP_HCM_NI_coast2 <- (-(HCM_NI_bbATTR2coast2 + HCM_NI_bbATTR2LVEnvcoast2 * LV.ni + HCM_NI_sdbATTR2coast2 * rep(rnorm(N.ni*NumIter)))/
                  (-exp(HCM_NI_bbATTR3cost   + HCM_NI_bbATTR3LVEnvcost   * LV.ni + HCM_NI_sdbATTR3cost   * rep(rnorm(N.ni*NumIter)))))

WTP_HCM_NI_coast3 <- (-(HCM_NI_bbATTR2coast3 + HCM_NI_bbATTR2LVEnvcoast3 * LV.ni + HCM_NI_sdbATTR2coast3 * rep(rnorm(N.ni*NumIter)))/
                  (-exp(HCM_NI_bbATTR3cost   + HCM_NI_bbATTR3LVEnvcost   * LV.ni + HCM_NI_sdbATTR3cost   * rep(rnorm(N.ni*NumIter)))))

WTP_HCM_NI_perk1  <- (-(HCM_NI_bbATTR4perk1 + HCM_NI_bbATTR4LVEnvperk1 * LV.ni + HCM_NI_sdbATTR4perk1 * rep(rnorm(N.ni*NumIter)))/
                  (-exp(HCM_NI_bbATTR3cost  + HCM_NI_bbATTR3LVEnvcost  * LV.ni + HCM_NI_sdbATTR3cost  * rep(rnorm(N.ni*NumIter)))))

WTP_HCM_NI_perk2  <- (-(HCM_NI_bbATTR4perk2 + HCM_NI_bbATTR4LVEnvperk2 * LV.ni + HCM_NI_sdbATTR4perk2 * rep(rnorm(N.ni*NumIter)))/
                  (-exp(HCM_NI_bbATTR3cost  + HCM_NI_bbATTR3LVEnvcost  * LV.ni + HCM_NI_sdbATTR3cost  * rep(rnorm(N.ni*NumIter)))))





############################################
###  RPL model with correlations (RPL-C) ###
############################################


# England

# First creating the error component (or eta_i) for NumIter (10,000 subjects)
hh2error.eng.uncor   <- rnorm(NumIter*N.eng,0,1)
hh3error.eng.uncor   <- rnorm(NumIter*N.eng,0,1)
coast2error.eng.uncor<- rnorm(NumIter*N.eng,0,1)
coast3error.eng.uncor<- rnorm(NumIter*N.eng,0,1)
costerror.eng.uncor  <- rnorm(NumIter*N.eng,0,1)
perk1error.eng.uncor <- rnorm(NumIter*N.eng,0,1)
perk2error.eng.uncor <- rnorm(NumIter*N.eng,0,1)

# Binding them together and making a matrix
betas.error.eng.uncor <-cbind(hh2error.eng.uncor  ,
                             hh3error.eng.uncor   ,
                             coast2error.eng.uncor,
                             coast3error.eng.uncor,
                             costerror.eng.uncor  ,
                             perk1error.eng.uncor ,
                             perk2error.eng.uncor )

betas.error.eng.uncor_mat <- matrix(betas.error.eng.uncor,NumIter*N.eng,7,byrow=FALSE)                                                   

# Creating L * eta_i, where L is the lower triangular matrix from the Cholesky decomposition
L_times_eta_eng <- betas.error.eng.uncor_mat %*% t(Gamma_eng)

# Making Beta_mean into a matrix (these are the actual estimates from the RPL-C output)
Beta_mean_eng <- cbind( beta_hats_eng[3]   , 
                        beta_hats_eng[4]   ,
                        beta_hats_eng[5]   ,
                        beta_hats_eng[6]   ,
                        beta_hats_eng[7]   ,
                        beta_hats_eng[8]   ,
                        beta_hats_eng[9]   )

Beta_mean_mat_eng <- matrix(Beta_mean_eng,NumIter*N.eng,7,byrow=TRUE)

Sd_mean_eng <- cbind( sqrt(VarCov_eng[1,1]), 
                      sqrt(VarCov_eng[2,2]),
                      sqrt(VarCov_eng[3,3]),
                      sqrt(VarCov_eng[4,4]),
                      sqrt(VarCov_eng[5,5]),
                      sqrt(VarCov_eng[6,6]),
                      sqrt(VarCov_eng[7,7]))

Sd_mean_mat_eng <- matrix(Sd_mean_eng,NumIter*N.eng,7,byrow=TRUE)



Matrix_age_eng          <- matrix(data.eng$age,          NumIter*N.eng,1,byrow=TRUE)
Matrix_female_eng       <- matrix(data.eng$female,       NumIter*N.eng,1,byrow=TRUE)
Matrix_cohabit_eng      <- matrix(data.eng$cohabit,      NumIter*N.eng,1,byrow=TRUE)
Matrix_employed_eng     <- matrix(data.eng$employed,     NumIter*N.eng,1,byrow=TRUE)
Matrix_green_eng        <- matrix(data.eng$green,        NumIter*N.eng,1,byrow=TRUE)
Matrix_higheduc_eng     <- matrix(data.eng$higheduc,     NumIter*N.eng,1,byrow=TRUE)
Matrix_highinc_eng      <- matrix(data.eng$highincome,   NumIter*N.eng,1,byrow=TRUE)
Matrix_ideo_eng         <- matrix(data.eng$ideo,         NumIter*N.eng,1,byrow=TRUE)
Matrix_num_children_eng <- matrix(data.eng$num_children, NumIter*N.eng,1,byrow=TRUE)


WTP_RPL_C_ENG_hh2 <- (-(Beta_mean_mat_eng[,1]  + L_times_eta_eng[,1] + beta_hats_eng[10]*Matrix_age_eng + beta_hats_eng[11]*Matrix_female_eng + beta_hats_eng[12]*Matrix_cohabit_eng + beta_hats_eng[13]*Matrix_num_children_eng + beta_hats_eng[14]*Matrix_higheduc_eng + beta_hats_eng[15]*Matrix_employed_eng +  beta_hats_eng[16]*Matrix_green_eng + beta_hats_eng[17]*Matrix_ideo_eng + beta_hats_eng[18]*Matrix_highinc_eng + Sd_mean_mat_eng[,1]*rep(rnorm(N.eng*NumIter)))/
                  (-exp(Beta_mean_mat_eng[,5]  + L_times_eta_eng[,5] + beta_hats_eng[46]*Matrix_age_eng + beta_hats_eng[47]*Matrix_female_eng + beta_hats_eng[48]*Matrix_cohabit_eng + beta_hats_eng[49]*Matrix_num_children_eng + beta_hats_eng[50]*Matrix_higheduc_eng + beta_hats_eng[51]*Matrix_employed_eng +  beta_hats_eng[52]*Matrix_green_eng + beta_hats_eng[53]*Matrix_ideo_eng + beta_hats_eng[54]*Matrix_highinc_eng + Sd_mean_mat_eng[,5]*rep(rnorm(N.eng*NumIter)))))

WTP_RPL_C_ENG_hh3 <- (-(Beta_mean_mat_eng[,2]  + L_times_eta_eng[,2] + beta_hats_eng[19]*Matrix_age_eng + beta_hats_eng[20]*Matrix_female_eng + beta_hats_eng[21]*Matrix_cohabit_eng + beta_hats_eng[22]*Matrix_num_children_eng + beta_hats_eng[23]*Matrix_higheduc_eng + beta_hats_eng[24]*Matrix_employed_eng +  beta_hats_eng[25]*Matrix_green_eng + beta_hats_eng[26]*Matrix_ideo_eng + beta_hats_eng[27]*Matrix_highinc_eng + Sd_mean_mat_eng[,2]*rep(rnorm(N.eng*NumIter)))/
                  (-exp(Beta_mean_mat_eng[,5]  + L_times_eta_eng[,5] + beta_hats_eng[46]*Matrix_age_eng + beta_hats_eng[47]*Matrix_female_eng + beta_hats_eng[48]*Matrix_cohabit_eng + beta_hats_eng[49]*Matrix_num_children_eng + beta_hats_eng[50]*Matrix_higheduc_eng + beta_hats_eng[51]*Matrix_employed_eng +  beta_hats_eng[52]*Matrix_green_eng + beta_hats_eng[53]*Matrix_ideo_eng + beta_hats_eng[54]*Matrix_highinc_eng + Sd_mean_mat_eng[,5]*rep(rnorm(N.eng*NumIter)))))

WTP_RPL_C_ENG_coast2 <- (-(Beta_mean_mat_eng[,3]  + L_times_eta_eng[,3] + beta_hats_eng[28]*Matrix_age_eng + beta_hats_eng[29]*Matrix_female_eng + beta_hats_eng[30]*Matrix_cohabit_eng + beta_hats_eng[31]*Matrix_num_children_eng + beta_hats_eng[32]*Matrix_higheduc_eng + beta_hats_eng[33]*Matrix_employed_eng +  beta_hats_eng[34]*Matrix_green_eng + beta_hats_eng[35]*Matrix_ideo_eng + beta_hats_eng[36]*Matrix_highinc_eng + Sd_mean_mat_eng[,3]*rep(rnorm(N.eng*NumIter)))/
                     (-exp(Beta_mean_mat_eng[,5]  + L_times_eta_eng[,5] + beta_hats_eng[46]*Matrix_age_eng + beta_hats_eng[47]*Matrix_female_eng + beta_hats_eng[48]*Matrix_cohabit_eng + beta_hats_eng[49]*Matrix_num_children_eng + beta_hats_eng[50]*Matrix_higheduc_eng + beta_hats_eng[51]*Matrix_employed_eng +  beta_hats_eng[52]*Matrix_green_eng + beta_hats_eng[53]*Matrix_ideo_eng + beta_hats_eng[54]*Matrix_highinc_eng + Sd_mean_mat_eng[,5]*rep(rnorm(N.eng*NumIter)))))

WTP_RPL_C_ENG_coast3 <- (-(Beta_mean_mat_eng[,4]  + L_times_eta_eng[,4] + beta_hats_eng[37]*Matrix_age_eng + beta_hats_eng[38]*Matrix_female_eng + beta_hats_eng[39]*Matrix_cohabit_eng + beta_hats_eng[40]*Matrix_num_children_eng + beta_hats_eng[41]*Matrix_higheduc_eng + beta_hats_eng[42]*Matrix_employed_eng +  beta_hats_eng[43]*Matrix_green_eng + beta_hats_eng[44]*Matrix_ideo_eng + beta_hats_eng[45]*Matrix_highinc_eng + Sd_mean_mat_eng[,4]*rep(rnorm(N.eng*NumIter)))/
                     (-exp(Beta_mean_mat_eng[,5]  + L_times_eta_eng[,5] + beta_hats_eng[46]*Matrix_age_eng + beta_hats_eng[47]*Matrix_female_eng + beta_hats_eng[48]*Matrix_cohabit_eng + beta_hats_eng[49]*Matrix_num_children_eng + beta_hats_eng[50]*Matrix_higheduc_eng + beta_hats_eng[51]*Matrix_employed_eng +  beta_hats_eng[52]*Matrix_green_eng + beta_hats_eng[53]*Matrix_ideo_eng + beta_hats_eng[54]*Matrix_highinc_eng + Sd_mean_mat_eng[,5]*rep(rnorm(N.eng*NumIter)))))

WTP_RPL_C_ENG_perk1 <- (-(Beta_mean_mat_eng[,6]  + L_times_eta_eng[,6] + beta_hats_eng[55]*Matrix_age_eng + beta_hats_eng[56]*Matrix_female_eng + beta_hats_eng[57]*Matrix_cohabit_eng + beta_hats_eng[58]*Matrix_num_children_eng + beta_hats_eng[59]*Matrix_higheduc_eng + beta_hats_eng[60]*Matrix_employed_eng +  beta_hats_eng[61]*Matrix_green_eng + beta_hats_eng[62]*Matrix_ideo_eng + beta_hats_eng[63]*Matrix_highinc_eng + Sd_mean_mat_eng[,6]*rep(rnorm(N.eng*NumIter)))/
                    (-exp(Beta_mean_mat_eng[,5]  + L_times_eta_eng[,5] + beta_hats_eng[46]*Matrix_age_eng + beta_hats_eng[47]*Matrix_female_eng + beta_hats_eng[48]*Matrix_cohabit_eng + beta_hats_eng[49]*Matrix_num_children_eng + beta_hats_eng[50]*Matrix_higheduc_eng + beta_hats_eng[51]*Matrix_employed_eng +  beta_hats_eng[52]*Matrix_green_eng + beta_hats_eng[53]*Matrix_ideo_eng + beta_hats_eng[54]*Matrix_highinc_eng + Sd_mean_mat_eng[,5]*rep(rnorm(N.eng*NumIter)))))

WTP_RPL_C_ENG_perk2 <- (-(Beta_mean_mat_eng[,7]  + L_times_eta_eng[,7] + beta_hats_eng[64]*Matrix_age_eng + beta_hats_eng[65]*Matrix_female_eng + beta_hats_eng[66]*Matrix_cohabit_eng + beta_hats_eng[67]*Matrix_num_children_eng + beta_hats_eng[68]*Matrix_higheduc_eng + beta_hats_eng[69]*Matrix_employed_eng +  beta_hats_eng[70]*Matrix_green_eng + beta_hats_eng[71]*Matrix_ideo_eng + beta_hats_eng[72]*Matrix_highinc_eng + Sd_mean_mat_eng[,7]*rep(rnorm(N.eng*NumIter)))/
                    (-exp(Beta_mean_mat_eng[,5]  + L_times_eta_eng[,5] + beta_hats_eng[46]*Matrix_age_eng + beta_hats_eng[47]*Matrix_female_eng + beta_hats_eng[48]*Matrix_cohabit_eng + beta_hats_eng[49]*Matrix_num_children_eng + beta_hats_eng[50]*Matrix_higheduc_eng + beta_hats_eng[51]*Matrix_employed_eng +  beta_hats_eng[52]*Matrix_green_eng + beta_hats_eng[53]*Matrix_ideo_eng + beta_hats_eng[54]*Matrix_highinc_eng + Sd_mean_mat_eng[,5]*rep(rnorm(N.eng*NumIter)))))



#########################

# Scotland

# First creating the error component (or eta_i) for NumIter (10,000 subjects)
hh2error.scot.uncor   <- rnorm(NumIter*N.scot,0,1)
hh3error.scot.uncor   <- rnorm(NumIter*N.scot,0,1)
coast2error.scot.uncor<- rnorm(NumIter*N.scot,0,1)
coast3error.scot.uncor<- rnorm(NumIter*N.scot,0,1)
costerror.scot.uncor  <- rnorm(NumIter*N.scot,0,1)
perk1error.scot.uncor <- rnorm(NumIter*N.scot,0,1)
perk2error.scot.uncor <- rnorm(NumIter*N.scot,0,1)

# Binding them together and making a matrix
betas.error.scot.uncor <-cbind(hh2error.scot.uncor   ,
                               hh3error.scot.uncor   ,
                               coast2error.scot.uncor,
                               coast3error.scot.uncor,
                               costerror.scot.uncor  ,
                               perk1error.scot.uncor ,
                               perk2error.scot.uncor )

betas.error.scot.uncor_mat <- matrix(betas.error.scot.uncor,NumIter*N.scot,7,byrow=FALSE)                                                   

# Creating L * eta_i, where L is the lower triangular matrix from the Cholesky decomposition
L_times_eta_scot <- betas.error.scot.uncor_mat %*% t(Gamma_scot)

# Making Beta_mean into a matrix (these are the actual estimates from the RPL-C output)
Beta_mean_scot <- cbind( beta_hats_scot[3]   , 
                         beta_hats_scot[4]   ,
                         beta_hats_scot[5]   ,
                         beta_hats_scot[6]   ,
                         beta_hats_scot[7]   ,
                         beta_hats_scot[8]   ,
                         beta_hats_scot[9]   )

Beta_mean_mat_scot <- matrix(Beta_mean_scot,NumIter*N.scot,7,byrow=TRUE)

Sd_mean_scot <- cbind( sqrt(VarCov_scot[1,1]), 
                       sqrt(VarCov_scot[2,2]),
                       sqrt(VarCov_scot[3,3]),
                       sqrt(VarCov_scot[4,4]),
                       sqrt(VarCov_scot[5,5]),
                       sqrt(VarCov_scot[6,6]),
                       sqrt(VarCov_scot[7,7]))

Sd_mean_mat_scot <- matrix(Sd_mean_scot,NumIter*N.scot,7,byrow=TRUE)


Matrix_age_scot          <- matrix(data.scot$age,          NumIter*N.scot,1,byrow=TRUE)
Matrix_female_scot       <- matrix(data.scot$female,       NumIter*N.scot,1,byrow=TRUE)
Matrix_cohabit_scot      <- matrix(data.scot$cohabit,      NumIter*N.scot,1,byrow=TRUE)
Matrix_employed_scot     <- matrix(data.scot$employed,     NumIter*N.scot,1,byrow=TRUE)
Matrix_green_scot        <- matrix(data.scot$green,        NumIter*N.scot,1,byrow=TRUE)
Matrix_higheduc_scot     <- matrix(data.scot$higheduc,     NumIter*N.scot,1,byrow=TRUE)
Matrix_highinc_scot      <- matrix(data.scot$highincome,   NumIter*N.scot,1,byrow=TRUE)
Matrix_ideo_scot         <- matrix(data.scot$ideo,         NumIter*N.scot,1,byrow=TRUE)
Matrix_num_children_scot <- matrix(data.scot$num_children, NumIter*N.scot,1,byrow=TRUE)


WTP_RPL_C_SCOT_hh2 <- (-(Beta_mean_mat_scot[,1]  + L_times_eta_scot[,1] + beta_hats_scot[10]*Matrix_age_scot + beta_hats_scot[11]*Matrix_female_scot + beta_hats_scot[12]*Matrix_cohabit_scot + beta_hats_scot[13]*Matrix_num_children_scot + beta_hats_scot[14]*Matrix_higheduc_scot + beta_hats_scot[15]*Matrix_employed_scot +  beta_hats_scot[16]*Matrix_green_scot + beta_hats_scot[17]*Matrix_ideo_scot + beta_hats_scot[18]*Matrix_highinc_scot + Sd_mean_mat_scot[,1]*rep(rnorm(N.scot*NumIter)))/
                   (-exp(Beta_mean_mat_scot[,5]  + L_times_eta_scot[,5] + beta_hats_scot[46]*Matrix_age_scot + beta_hats_scot[47]*Matrix_female_scot + beta_hats_scot[48]*Matrix_cohabit_scot + beta_hats_scot[49]*Matrix_num_children_scot + beta_hats_scot[50]*Matrix_higheduc_scot + beta_hats_scot[51]*Matrix_employed_scot +  beta_hats_scot[52]*Matrix_green_scot + beta_hats_scot[53]*Matrix_ideo_scot + beta_hats_scot[54]*Matrix_highinc_scot + Sd_mean_mat_scot[,5]*rep(rnorm(N.scot*NumIter)))))

WTP_RPL_C_SCOT_hh3 <- (-(Beta_mean_mat_scot[,2]  + L_times_eta_scot[,2] + beta_hats_scot[19]*Matrix_age_scot + beta_hats_scot[20]*Matrix_female_scot + beta_hats_scot[21]*Matrix_cohabit_scot + beta_hats_scot[22]*Matrix_num_children_scot + beta_hats_scot[23]*Matrix_higheduc_scot + beta_hats_scot[24]*Matrix_employed_scot +  beta_hats_scot[25]*Matrix_green_scot + beta_hats_scot[26]*Matrix_ideo_scot + beta_hats_scot[27]*Matrix_highinc_scot + Sd_mean_mat_scot[,2]*rep(rnorm(N.scot*NumIter)))/
                   (-exp(Beta_mean_mat_scot[,5]  + L_times_eta_scot[,5] + beta_hats_scot[46]*Matrix_age_scot + beta_hats_scot[47]*Matrix_female_scot + beta_hats_scot[48]*Matrix_cohabit_scot + beta_hats_scot[49]*Matrix_num_children_scot + beta_hats_scot[50]*Matrix_higheduc_scot + beta_hats_scot[51]*Matrix_employed_scot +  beta_hats_scot[52]*Matrix_green_scot + beta_hats_scot[53]*Matrix_ideo_scot + beta_hats_scot[54]*Matrix_highinc_scot + Sd_mean_mat_scot[,5]*rep(rnorm(N.scot*NumIter)))))

WTP_RPL_C_SCOT_coast2 <- (-(Beta_mean_mat_scot[,3]  + L_times_eta_scot[,3] + beta_hats_scot[28]*Matrix_age_scot + beta_hats_scot[29]*Matrix_female_scot + beta_hats_scot[30]*Matrix_cohabit_scot + beta_hats_scot[31]*Matrix_num_children_scot + beta_hats_scot[32]*Matrix_higheduc_scot + beta_hats_scot[33]*Matrix_employed_scot +  beta_hats_scot[34]*Matrix_green_scot + beta_hats_scot[35]*Matrix_ideo_scot + beta_hats_scot[36]*Matrix_highinc_scot + Sd_mean_mat_scot[,3]*rep(rnorm(N.scot*NumIter)))/
                      (-exp(Beta_mean_mat_scot[,5]  + L_times_eta_scot[,5] + beta_hats_scot[46]*Matrix_age_scot + beta_hats_scot[47]*Matrix_female_scot + beta_hats_scot[48]*Matrix_cohabit_scot + beta_hats_scot[49]*Matrix_num_children_scot + beta_hats_scot[50]*Matrix_higheduc_scot + beta_hats_scot[51]*Matrix_employed_scot +  beta_hats_scot[52]*Matrix_green_scot + beta_hats_scot[53]*Matrix_ideo_scot + beta_hats_scot[54]*Matrix_highinc_scot + Sd_mean_mat_scot[,5]*rep(rnorm(N.scot*NumIter)))))

WTP_RPL_C_SCOT_coast3 <- (-(Beta_mean_mat_scot[,4]  + L_times_eta_scot[,4] + beta_hats_scot[37]*Matrix_age_scot + beta_hats_scot[38]*Matrix_female_scot + beta_hats_scot[39]*Matrix_cohabit_scot + beta_hats_scot[40]*Matrix_num_children_scot + beta_hats_scot[41]*Matrix_higheduc_scot + beta_hats_scot[42]*Matrix_employed_scot +  beta_hats_scot[43]*Matrix_green_scot + beta_hats_scot[44]*Matrix_ideo_scot + beta_hats_scot[45]*Matrix_highinc_scot + Sd_mean_mat_scot[,4]*rep(rnorm(N.scot*NumIter)))/
                      (-exp(Beta_mean_mat_scot[,5]  + L_times_eta_scot[,5] + beta_hats_scot[46]*Matrix_age_scot + beta_hats_scot[47]*Matrix_female_scot + beta_hats_scot[48]*Matrix_cohabit_scot + beta_hats_scot[49]*Matrix_num_children_scot + beta_hats_scot[50]*Matrix_higheduc_scot + beta_hats_scot[51]*Matrix_employed_scot +  beta_hats_scot[52]*Matrix_green_scot + beta_hats_scot[53]*Matrix_ideo_scot + beta_hats_scot[54]*Matrix_highinc_scot + Sd_mean_mat_scot[,5]*rep(rnorm(N.scot*NumIter)))))

WTP_RPL_C_SCOT_perk1 <- (-(Beta_mean_mat_scot[,6]  + L_times_eta_scot[,6] + beta_hats_scot[55]*Matrix_age_scot + beta_hats_scot[56]*Matrix_female_scot + beta_hats_scot[57]*Matrix_cohabit_scot + beta_hats_scot[58]*Matrix_num_children_scot + beta_hats_scot[59]*Matrix_higheduc_scot + beta_hats_scot[60]*Matrix_employed_scot +  beta_hats_scot[61]*Matrix_green_scot + beta_hats_scot[62]*Matrix_ideo_scot + beta_hats_scot[63]*Matrix_highinc_scot + Sd_mean_mat_scot[,6]*rep(rnorm(N.scot*NumIter)))/
                     (-exp(Beta_mean_mat_scot[,5]  + L_times_eta_scot[,5] + beta_hats_scot[46]*Matrix_age_scot + beta_hats_scot[47]*Matrix_female_scot + beta_hats_scot[48]*Matrix_cohabit_scot + beta_hats_scot[49]*Matrix_num_children_scot + beta_hats_scot[50]*Matrix_higheduc_scot + beta_hats_scot[51]*Matrix_employed_scot +  beta_hats_scot[52]*Matrix_green_scot + beta_hats_scot[53]*Matrix_ideo_scot + beta_hats_scot[54]*Matrix_highinc_scot + Sd_mean_mat_scot[,5]*rep(rnorm(N.scot*NumIter)))))

WTP_RPL_C_SCOT_perk2 <- (-(Beta_mean_mat_scot[,7]  + L_times_eta_scot[,7] + beta_hats_scot[64]*Matrix_age_scot + beta_hats_scot[65]*Matrix_female_scot + beta_hats_scot[66]*Matrix_cohabit_scot + beta_hats_scot[67]*Matrix_num_children_scot + beta_hats_scot[68]*Matrix_higheduc_scot + beta_hats_scot[69]*Matrix_employed_scot +  beta_hats_scot[70]*Matrix_green_scot + beta_hats_scot[71]*Matrix_ideo_scot + beta_hats_scot[72]*Matrix_highinc_scot + Sd_mean_mat_scot[,7]*rep(rnorm(N.scot*NumIter)))/
                     (-exp(Beta_mean_mat_scot[,5]  + L_times_eta_scot[,5] + beta_hats_scot[46]*Matrix_age_scot + beta_hats_scot[47]*Matrix_female_scot + beta_hats_scot[48]*Matrix_cohabit_scot + beta_hats_scot[49]*Matrix_num_children_scot + beta_hats_scot[50]*Matrix_higheduc_scot + beta_hats_scot[51]*Matrix_employed_scot +  beta_hats_scot[52]*Matrix_green_scot + beta_hats_scot[53]*Matrix_ideo_scot + beta_hats_scot[54]*Matrix_highinc_scot + Sd_mean_mat_scot[,5]*rep(rnorm(N.scot*NumIter)))))





#########################

# Northern Ireland

# First creating the error component (or eta_i) for NumIter (10,000 subjects)
hh2error.ni.uncor   <- rnorm(NumIter*N.ni,0,1)
hh3error.ni.uncor   <- rnorm(NumIter*N.ni,0,1)
coast2error.ni.uncor<- rnorm(NumIter*N.ni,0,1)
coast3error.ni.uncor<- rnorm(NumIter*N.ni,0,1)
costerror.ni.uncor  <- rnorm(NumIter*N.ni,0,1)
perk1error.ni.uncor <- rnorm(NumIter*N.ni,0,1)
perk2error.ni.uncor <- rnorm(NumIter*N.ni,0,1)

# Binding them together and making a matrix
betas.error.ni.uncor <-  cbind(hh2error.ni.uncor   ,
                               hh3error.ni.uncor   ,
                               coast2error.ni.uncor,
                               coast3error.ni.uncor,
                               costerror.ni.uncor  ,
                               perk1error.ni.uncor ,
                               perk2error.ni.uncor )

betas.error.ni.uncor_mat <- matrix(betas.error.ni.uncor,NumIter*N.ni,7,byrow=FALSE)                                                   

# Creating L * eta_i, where L is the lower triangular matrix from the Cholesky decomposition
L_times_eta_ni <- betas.error.ni.uncor_mat %*% t(Gamma_ni)

# Making Beta_mean into a matrix (these are the actual estimates from the RPL-C output)
Beta_mean_ni   <- cbind( beta_hats_ni[3]   , 
                         beta_hats_ni[4]   ,
                         beta_hats_ni[5]   ,
                         beta_hats_ni[6]   ,
                         beta_hats_ni[7]   ,
                         beta_hats_ni[8]   ,
                         beta_hats_ni[9]   )

Beta_mean_mat_ni <- matrix(Beta_mean_ni,NumIter*N.ni,7,byrow=TRUE)

Sd_mean_ni <-   cbind( sqrt(VarCov_ni[1,1]), 
                       sqrt(VarCov_ni[2,2]),
                       sqrt(VarCov_ni[3,3]),
                       sqrt(VarCov_ni[4,4]),
                       sqrt(VarCov_ni[5,5]),
                       sqrt(VarCov_ni[6,6]),
                       sqrt(VarCov_ni[7,7]))

Sd_mean_mat_ni <- matrix(Sd_mean_ni,NumIter*N.ni,7,byrow=TRUE)


Matrix_age_ni          <- matrix(data.ni$age,          NumIter*N.ni,1,byrow=TRUE)
Matrix_female_ni       <- matrix(data.ni$female,       NumIter*N.ni,1,byrow=TRUE)
Matrix_cohabit_ni      <- matrix(data.ni$cohabit,      NumIter*N.ni,1,byrow=TRUE)
Matrix_employed_ni     <- matrix(data.ni$employed,     NumIter*N.ni,1,byrow=TRUE)
Matrix_green_ni        <- matrix(data.ni$green,        NumIter*N.ni,1,byrow=TRUE)
Matrix_higheduc_ni     <- matrix(data.ni$higheduc,     NumIter*N.ni,1,byrow=TRUE)
Matrix_highinc_ni      <- matrix(data.ni$highincome,   NumIter*N.ni,1,byrow=TRUE)
Matrix_ideo_ni         <- matrix(data.ni$ideo,         NumIter*N.ni,1,byrow=TRUE)
Matrix_num_children_ni <- matrix(data.ni$num_children, NumIter*N.ni,1,byrow=TRUE)


WTP_RPL_C_NI_hh2 <- (-(Beta_mean_mat_ni[,1]  + L_times_eta_ni[,1] + beta_hats_ni[10]*Matrix_age_ni + beta_hats_ni[11]*Matrix_female_ni + beta_hats_ni[12]*Matrix_cohabit_ni + beta_hats_ni[13]*Matrix_num_children_ni + beta_hats_ni[14]*Matrix_higheduc_ni + beta_hats_ni[15]*Matrix_employed_ni +  beta_hats_ni[16]*Matrix_green_ni + beta_hats_ni[17]*Matrix_ideo_ni + beta_hats_ni[18]*Matrix_highinc_ni + Sd_mean_mat_ni[,1]*rep(rnorm(N.ni*NumIter)))/
                 (-exp(Beta_mean_mat_ni[,5]  + L_times_eta_ni[,5] + beta_hats_ni[46]*Matrix_age_ni + beta_hats_ni[47]*Matrix_female_ni + beta_hats_ni[48]*Matrix_cohabit_ni + beta_hats_ni[49]*Matrix_num_children_ni + beta_hats_ni[50]*Matrix_higheduc_ni + beta_hats_ni[51]*Matrix_employed_ni +  beta_hats_ni[52]*Matrix_green_ni + beta_hats_ni[53]*Matrix_ideo_ni + beta_hats_ni[54]*Matrix_highinc_ni + Sd_mean_mat_ni[,5]*rep(rnorm(N.ni*NumIter)))))

WTP_RPL_C_NI_hh3 <- (-(Beta_mean_mat_ni[,2]  + L_times_eta_ni[,2] + beta_hats_ni[19]*Matrix_age_ni + beta_hats_ni[20]*Matrix_female_ni + beta_hats_ni[21]*Matrix_cohabit_ni + beta_hats_ni[22]*Matrix_num_children_ni + beta_hats_ni[23]*Matrix_higheduc_ni + beta_hats_ni[24]*Matrix_employed_ni +  beta_hats_ni[25]*Matrix_green_ni + beta_hats_ni[26]*Matrix_ideo_ni + beta_hats_ni[27]*Matrix_highinc_ni + Sd_mean_mat_ni[,2]*rep(rnorm(N.ni*NumIter)))/
                 (-exp(Beta_mean_mat_ni[,5]  + L_times_eta_ni[,5] + beta_hats_ni[46]*Matrix_age_ni + beta_hats_ni[47]*Matrix_female_ni + beta_hats_ni[48]*Matrix_cohabit_ni + beta_hats_ni[49]*Matrix_num_children_ni + beta_hats_ni[50]*Matrix_higheduc_ni + beta_hats_ni[51]*Matrix_employed_ni +  beta_hats_ni[52]*Matrix_green_ni + beta_hats_ni[53]*Matrix_ideo_ni + beta_hats_ni[54]*Matrix_highinc_ni + Sd_mean_mat_ni[,5]*rep(rnorm(N.ni*NumIter)))))

WTP_RPL_C_NI_coast2 <- (-(Beta_mean_mat_ni[,3]  + L_times_eta_ni[,3] + beta_hats_ni[28]*Matrix_age_ni + beta_hats_ni[29]*Matrix_female_ni + beta_hats_ni[30]*Matrix_cohabit_ni + beta_hats_ni[31]*Matrix_num_children_ni + beta_hats_ni[32]*Matrix_higheduc_ni + beta_hats_ni[33]*Matrix_employed_ni +  beta_hats_ni[34]*Matrix_green_ni + beta_hats_ni[35]*Matrix_ideo_ni + beta_hats_ni[36]*Matrix_highinc_ni + Sd_mean_mat_ni[,3]*rep(rnorm(N.ni*NumIter)))/
                    (-exp(Beta_mean_mat_ni[,5]  + L_times_eta_ni[,5] + beta_hats_ni[46]*Matrix_age_ni + beta_hats_ni[47]*Matrix_female_ni + beta_hats_ni[48]*Matrix_cohabit_ni + beta_hats_ni[49]*Matrix_num_children_ni + beta_hats_ni[50]*Matrix_higheduc_ni + beta_hats_ni[51]*Matrix_employed_ni +  beta_hats_ni[52]*Matrix_green_ni + beta_hats_ni[53]*Matrix_ideo_ni + beta_hats_ni[54]*Matrix_highinc_ni + Sd_mean_mat_ni[,5]*rep(rnorm(N.ni*NumIter)))))

WTP_RPL_C_NI_coast3 <- (-(Beta_mean_mat_ni[,4]  + L_times_eta_ni[,4] + beta_hats_ni[37]*Matrix_age_ni + beta_hats_ni[38]*Matrix_female_ni + beta_hats_ni[39]*Matrix_cohabit_ni + beta_hats_ni[40]*Matrix_num_children_ni + beta_hats_ni[41]*Matrix_higheduc_ni + beta_hats_ni[42]*Matrix_employed_ni +  beta_hats_ni[43]*Matrix_green_ni + beta_hats_ni[44]*Matrix_ideo_ni + beta_hats_ni[45]*Matrix_highinc_ni + Sd_mean_mat_ni[,4]*rep(rnorm(N.ni*NumIter)))/
                    (-exp(Beta_mean_mat_ni[,5]  + L_times_eta_ni[,5] + beta_hats_ni[46]*Matrix_age_ni + beta_hats_ni[47]*Matrix_female_ni + beta_hats_ni[48]*Matrix_cohabit_ni + beta_hats_ni[49]*Matrix_num_children_ni + beta_hats_ni[50]*Matrix_higheduc_ni + beta_hats_ni[51]*Matrix_employed_ni +  beta_hats_ni[52]*Matrix_green_ni + beta_hats_ni[53]*Matrix_ideo_ni + beta_hats_ni[54]*Matrix_highinc_ni + Sd_mean_mat_ni[,5]*rep(rnorm(N.ni*NumIter)))))

WTP_RPL_C_NI_perk1 <- (-(Beta_mean_mat_ni[,6]  + L_times_eta_ni[,6] + beta_hats_ni[55]*Matrix_age_ni + beta_hats_ni[56]*Matrix_female_ni + beta_hats_ni[57]*Matrix_cohabit_ni + beta_hats_ni[58]*Matrix_num_children_ni + beta_hats_ni[59]*Matrix_higheduc_ni + beta_hats_ni[60]*Matrix_employed_ni +  beta_hats_ni[61]*Matrix_green_ni + beta_hats_ni[62]*Matrix_ideo_ni + beta_hats_ni[63]*Matrix_highinc_ni + Sd_mean_mat_ni[,6]*rep(rnorm(N.ni*NumIter)))/
                   (-exp(Beta_mean_mat_ni[,5]  + L_times_eta_ni[,5] + beta_hats_ni[46]*Matrix_age_ni + beta_hats_ni[47]*Matrix_female_ni + beta_hats_ni[48]*Matrix_cohabit_ni + beta_hats_ni[49]*Matrix_num_children_ni + beta_hats_ni[50]*Matrix_higheduc_ni + beta_hats_ni[51]*Matrix_employed_ni +  beta_hats_ni[52]*Matrix_green_ni + beta_hats_ni[53]*Matrix_ideo_ni + beta_hats_ni[54]*Matrix_highinc_ni + Sd_mean_mat_ni[,5]*rep(rnorm(N.ni*NumIter)))))

WTP_RPL_C_NI_perk2 <- (-(Beta_mean_mat_ni[,7]  + L_times_eta_ni[,7] + beta_hats_ni[64]*Matrix_age_ni + beta_hats_ni[65]*Matrix_female_ni + beta_hats_ni[66]*Matrix_cohabit_ni + beta_hats_ni[67]*Matrix_num_children_ni + beta_hats_ni[68]*Matrix_higheduc_ni + beta_hats_ni[69]*Matrix_employed_ni +  beta_hats_ni[70]*Matrix_green_ni + beta_hats_ni[71]*Matrix_ideo_ni + beta_hats_ni[72]*Matrix_highinc_ni + Sd_mean_mat_ni[,7]*rep(rnorm(N.ni*NumIter)))/
                   (-exp(Beta_mean_mat_ni[,5]  + L_times_eta_ni[,5] + beta_hats_ni[46]*Matrix_age_ni + beta_hats_ni[47]*Matrix_female_ni + beta_hats_ni[48]*Matrix_cohabit_ni + beta_hats_ni[49]*Matrix_num_children_ni + beta_hats_ni[50]*Matrix_higheduc_ni + beta_hats_ni[51]*Matrix_employed_ni +  beta_hats_ni[52]*Matrix_green_ni + beta_hats_ni[53]*Matrix_ideo_ni + beta_hats_ni[54]*Matrix_highinc_ni + Sd_mean_mat_ni[,5]*rep(rnorm(N.ni*NumIter)))))




###################################################################################################

#####################################
###  Graphing the different WTPs  ###
#####################################

# England (Figure 2)
boxplot(cbind(WTP_HCM_ENG_hh2      ,
              WTP_RPL_UC_ENG_hh2   ,
              WTP_RPL_C_ENG_hh2    ,
              WTP_HCM_ENG_hh3      ,
              WTP_RPL_UC_ENG_hh3   ,
              WTP_RPL_C_ENG_hh3    ,
              WTP_HCM_ENG_coast2   ,
              WTP_RPL_UC_ENG_coast2,
              WTP_RPL_C_ENG_coast2 ,
              WTP_HCM_ENG_coast3   ,
              WTP_RPL_UC_ENG_coast3,
              WTP_RPL_C_ENG_coast3 ,
              WTP_HCM_ENG_perk1    ,
              WTP_RPL_UC_ENG_perk1 ,
              WTP_RPL_C_ENG_perk1  ,
              WTP_HCM_ENG_perk2    ,
              WTP_RPL_UC_ENG_perk2 ,
              WTP_RPL_C_ENG_perk2
),
horizontal=TRUE,
col=c("grey100","grey96","grey92","grey88","grey84","grey80","grey76","grey72","grey68","grey64","grey60","grey56","grey52","grey48","grey44","grey40","grey36","grey32"),
outline=FALSE,
whisklty = 0,
staplelty = 0,
xlab=" ",
yaxt="n",
range=0.1,
ylim=c(-60,250),
main = "WTP: ENGLAND")  


legend (127,19,c("FB-RPL corr","FB-RPL","FB-HCM","Letter-RPL corr","Letter-RPL","Letter-HCM","30% coast-RPL corr","30% coast-RPL","30% coast-HCM","20% coast-RPL corr","20% coast-RPL","20% coast-HCM","130,000 hh-RPL corr","130,000 hh-RPL","130,000 hh-HCM","85,000 hh-RPL corr","85,000 hh-RPL","85,000 hh-HCM"),
        fill=c("grey32","grey36","grey40","grey44","grey48","grey52","grey56","grey60","grey64","grey68","grey72","grey76","grey80","grey84","grey88","grey92","grey96","grey100"), cex=1)

# dimensions of graph: 867 x 545


######################################

# Scotland (Figure 3) 
boxplot(cbind(WTP_HCM_SCOT_hh2      ,
              WTP_RPL_UC_SCOT_hh2   ,
              WTP_RPL_C_SCOT_hh2    ,
              WTP_HCM_SCOT_hh3      ,
              WTP_RPL_UC_SCOT_hh3   ,
              WTP_RPL_C_SCOT_hh3    ,
              WTP_HCM_SCOT_coast2   ,
              WTP_RPL_UC_SCOT_coast2,
              WTP_RPL_C_SCOT_coast2 ,
              WTP_HCM_SCOT_coast3   ,
              WTP_RPL_UC_SCOT_coast3,
              WTP_RPL_C_SCOT_coast3 ,
              WTP_HCM_SCOT_perk1    ,
              WTP_RPL_UC_SCOT_perk1 ,
              WTP_RPL_C_SCOT_perk1  ,
              WTP_HCM_SCOT_perk2    ,
              WTP_RPL_UC_SCOT_perk2 ,
              WTP_RPL_C_SCOT_perk2
),
horizontal=TRUE,
col=c("grey100","grey96","grey92","grey88","grey84","grey80","grey76","grey72","grey68","grey64","grey60","grey56","grey52","grey48","grey44","grey40","grey36","grey32"),
outline=FALSE,
whisklty = 0,
staplelty = 0,
xlab=" ",
yaxt="n",
range=0.1,
ylim=c(-60,250),
main = "WTP: SCOTLAND")  


legend (127,19,c("FB-RPL corr","FB-RPL","FB-HCM","Letter-RPL corr","Letter-RPL","Letter-HCM","30% coast-RPL corr","30% coast-RPL","30% coast-HCM","20% coast-RPL corr","20% coast-RPL","20% coast-HCM","130,000 hh-RPL corr","130,000 hh-RPL","130,000 hh-HCM","85,000 hh-RPL corr","85,000 hh-RPL","85,000 hh-HCM"),
        fill=c("grey32","grey36","grey40","grey44","grey48","grey52","grey56","grey60","grey64","grey68","grey72","grey76","grey80","grey84","grey88","grey92","grey96","grey100"), cex=1)

# dimensions of graph: 867 x 545


####################################


# Northern Ireland (Figure 4)
boxplot(cbind(WTP_HCM_NI_hh2      ,
              WTP_RPL_UC_NI_hh2   ,
              WTP_RPL_C_NI_hh2    ,
              WTP_HCM_NI_hh3      ,
              WTP_RPL_UC_NI_hh3   ,
              WTP_RPL_C_NI_hh3    ,
              WTP_HCM_NI_coast2   ,
              WTP_RPL_UC_NI_coast2,
              WTP_RPL_C_NI_coast2 ,
              WTP_HCM_NI_coast3   ,
              WTP_RPL_UC_NI_coast3,
              WTP_RPL_C_NI_coast3 ,
              WTP_HCM_NI_perk1    ,
              WTP_RPL_UC_NI_perk1 ,
              WTP_RPL_C_NI_perk1  ,
              WTP_HCM_NI_perk2    ,
              WTP_RPL_UC_NI_perk2 ,
              WTP_RPL_C_NI_perk2
),
horizontal=TRUE,
col=c("grey100","grey96","grey92","grey88","grey84","grey80","grey76","grey72","grey68","grey64","grey60","grey56","grey52","grey48","grey44","grey40","grey36","grey32"),
outline=FALSE,
whisklty = 0,
staplelty = 0,
xlab=" ",
yaxt="n",
range=0.1,
ylim=c(-60,250),
main = "WTP: NORTHERN IRELAND")  


legend (127,19,c("FB-RPL corr","FB-RPL","FB-HCM","Letter-RPL corr","Letter-RPL","Letter-HCM","30% coast-RPL corr","30% coast-RPL","30% coast-HCM","20% coast-RPL corr","20% coast-RPL","20% coast-HCM","130,000 hh-RPL corr","130,000 hh-RPL","130,000 hh-HCM","85,000 hh-RPL corr","85,000 hh-RPL","85,000 hh-HCM"),
        fill=c("grey32","grey36","grey40","grey44","grey48","grey52","grey56","grey60","grey64","grey68","grey72","grey76","grey80","grey84","grey88","grey92","grey96","grey100"), cex=1)

# dimensions of graph: 867 x 545

#####################################################

# End of R script

