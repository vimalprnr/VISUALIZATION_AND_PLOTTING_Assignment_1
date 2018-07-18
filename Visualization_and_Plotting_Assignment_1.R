################################
##Question 1 Solution
setwd("D://ACADGILD//VImal//VISUALIZATION AND PLOTTING//CODING//INPUT")
titan<-read.table(file = 'titanic.txt', sep = '\t', header = TRUE)
Family_tab <- table(titan$Family)
barplot(Family_tab)

####################################
## Question 2 Solution
setwd("D://ACADGILD//VImal//VISUALIZATION AND PLOTTING//CODING//INPUT")
titan<-read.table(file = 'titanic.txt', sep = '\t', header = TRUE)
titan_subdf <- as.data.frame(table(titan$survived,titan$Family))
colnames(titan_subdf) <- c("Survived","Family","Freq")
family<- as.vector(titan_subdf$Family)
family <- unique(family)
survivalData <-as.data.frame(0)
tot_mem<-0
k <- 0
for(famName in family)
{
  k<- k+1
  j<- which(famName==titan_subdf$Family)
  tot_mem<-0
  for (i in j) 
  {
    if(titan_subdf$Survived[i] == 0)
    {
      tot_mem <- tot_mem+titan_subdf$Freq[i]
    }
    else
    {
      tot_mem <- tot_mem+titan_subdf$Freq[i]
      survived <- titan_subdf$Freq[i]
    }
  }
  survivalData[k,1] <- famName
  survivalData[k,2] <- tot_mem
  survivalData[k,3] <- survived
  survivalData[k,4] <- ((survived*100)/tot_mem)
}

colnames(survivalData) <- c("Family","MemberCount","Survived","SurvivalRate")
temp_table <- as.table(survivalData$SurvivalRate, names = survivalData$Family)
names(temp_table) <- survivalData$Family
barplot(temp_table)
##################################################################
##Question 3 Solution
#Note: The Link provided in the assignment document is not an Online link. Using titanic_train dataset.
library(titanic)
library(mice)
sum(is.na(titanic_train$Age))
mice_imputes = mice(titanic_train, m=5, maxit = 40)
Imputed_data=complete(mice_imputes,5)
hist(titanic_train$Age,  main='Actual Data',col="blue")
hist(Imputed_data$Age, main='Imputed Data',col="yellow")

