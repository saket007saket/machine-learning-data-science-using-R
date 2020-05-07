##Here i m performing association rules to analyse the people who have survived or not survived based on theirclass and gender##

## train.csv of titanic data

install.packages('arules')#this package is used to apply association rules 
library(arules)

Titanic = read.csv(Titanic.csv")

str(Titanic)#it is used give entire structure of table,if see the s.No,class,survived are in integer where as sex column is 
#factor type of data 
Titanic$Class = as.factor(Titanic$Class)
Titanic$Survived = as.factor(Titanic$Survived)
#since a_rules work on factor (categorical data) therefore we r converting class and survived as factor type of data 
str(Titanic)

Titanic = Titanic[,-c(1,2)]#since in table S.no is not given then we are removing the this column 
rules = apriori(Titanic)#from arules library we r using the apriori method to set all the apriori rules in and stored in variable rules



arules::inspect(rules)#here it is used to see the inspection rules --->for 900 observation it is giving 7 rules
#if we seet the first line then it means if person is not servived and is male then support ,confidence,lift is high
#so we can conclude that male person is not servived 
#{class=2,survived=0}--->{sex=male} support=0.10 ,confi=0.93,lift=1.44,count=91 we can conclude that 91 times class=2 not survived
#person are male 
#similarly {class=2,sex=male}---->{survived=0} have same value , similarly {class=3,sex=male}--->{survived=0} also indicate same
#thing so overall we have conclude that in entire ship male person surival count was very less
#{class=1,sex=female}--->{survived=1} have lift ratio is highest so we can conclude that class 1 female has been survived 


rules.sorted = sort(rules,by = "lift") #now we will sort the rules based on the lift ratio and store rules.sorted varaible
arules::inspect(rules.sorted)#now if again inspect the sorted rules 

# rules with rhs containing Survived only
rules = apriori(Titanic,parameter = list(minlen = 1,supp = 0.2,conf = 0.5)
              ,appearance = list(rhs = "Survived=0"))
#if we want to set the apriori rules with consequence as "Survived=0" and support=0.2 ,conf-0.5 then we have to mention that
#means we r checking what are the class and gender combination who has not survived 

#minlen=1 will tell about atleast {1 item} -->{1 item} should be map based on that we will make apriori rules

arules::inspect(rules)
#it gives some assosiation like {class=3}--->{survived=0} i.e among all the class most of the people died from class 3 
#{sex=male}--->{survived=0}i.e among both f and m male people died more
#{class=3,sex=male}-->{survived=0} from here we can conclude that from class 3 male person has not survived 
#note:-but from here we cannot conclude that class=3,sex=f has been survived ,for that we have to again set the rules appearrence for survived
?apriori                
table(Titanic$Survived)
#custom function to remove the redundant
rules.sorted=sort(rules,by="lift")#here we r sorting the rules by lift value 
subset.matrix=is.subset(rules.sorted,rules.sorted,sparse = FALSE) #we r creating matrix using sorted rules 
subset.matrix[lower.tri(subset.matrix,diag = T)]=NA #here we r considering lower triangular matrix part with diagonal 
redundant=colSums(subset.matrix,na.rm = T)>=1


rules.pruned=rules.sorted[!redundant]#we r removing redundant from rules 

#we r inspecting pruned rules 

?is.redundant
inspect(rules.pruned)
