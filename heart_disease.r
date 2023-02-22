heart_disease <- read.csv("Heart_Disease_Prediction.csv",header=TRUE)

head(heart_disease,10)

colnames(heart_disease)

summary(heart_disease)

sample(heart_disease,10)

print(ncol(heart_disease))
print(nrow(heart_disease))

print(heart_disease$Age)
#print(count(heart_disease,Sex))


#1-male 0-female
print(unique(heart_disease$Sex))
male_count = length(which(heart_disease$Sex==1))
print(male_count)

female_count <- length(which(heart_disease$Sex==0))
print(female_count)

#heart_disease

#categories in  chest pain
types_of_chestpains = unique(heart_disease$Chest.pain.type)
print(types_of_chestpains)

#categories in exercise angina
print(unique(heart_disease$Exercise.angina))

#categories in heart disease
print(unique(heart_disease$Heart.Disease))
print(length(which(heart_disease$Heart.Disease=="Presence")))
print(length(which(heart_disease$Heart.Disease=="Absence")))



hage_p = length(which(heart_disease$Heart.Disease=="Presence" &  heart_disease$Age>50))
hage_a = length(which(heart_disease$Heart.Disease=="Absence" &  heart_disease$Age>50))
print(hage_a)
print("pie chart showing the presence of heart disease over the age of 50")
pie(c(hage_p,hage_a),labels=c("heart disease presence","heart disease absence"))

#Heart diseases in Male population 
mp = length(which(heart_disease$Heart.Disease=="Presence" &  heart_disease$Sex==1))
ma = length(which(heart_disease$Heart.Disease=="Absence" &  heart_disease$Sex==1))
print(ma)
print("pie chart showing the presence of heart disease of male")
pie(c(mp,ma),labels=c("heart disease presence in male","heart disease absence in male"))

#heart diseases when BP is greater than or less than 120
bp_h = length(which(heart_disease$Heart.Disease=="Presence" &  heart_disease$BP>=120))
#No of people suffering heart diseases with BP is greater than or equal to 120
bp_l = length(which(heart_disease$Heart.Disease=="Presence" &  heart_disease$BP<120))
#No of people suffering heart diseases with BP is less than 120
pie(c(bp_h,bp_l),labels=c("heart disease presence when BP is greater than or equal to 120","heart disease absence when BP is less than equal to 120"))


barplot(heart_disease$Cholesterol)
#col= heart_disease$Heart.Disease=='Presence' %?% "red" %:% "blue")
barplot(sort(heart_disease$Cholesterol), col = ifelse(heart_disease$Heart.Disease =="Presence",'red','green'))
#there is no particular relation between cholesterol and heart diseases,there may be relation in according with some more attributes
hist(sort(heart_disease$Cholesterol), col = ifelse(heart_disease$Heart.Disease =="Presence",'red','green'))
#histogram says that people with cholesterol in range 200-250 are highly prone to heart diseases.

#calculating no of people suffering with different chestpains and also heart diseses
cpain1 = length(which(heart_disease$Chest.pain.type=="1" &  heart_disease$Heart.Disease=="Presence"))
cpain2 = length(which(heart_disease$Chest.pain.type=="2" &  heart_disease$Heart.Disease=="Presence"))
cpain3 = length(which(heart_disease$Chest.pain.type=="3" &  heart_disease$Heart.Disease=="Presence"))
cpain4 = length(which(heart_disease$Chest.pain.type=="4" &  heart_disease$Heart.Disease=="Presence"))
chest_pain_count <- c(cpain1,cpain2,cpain3,cpain4)
pie(chest_pain_count,labels=c("type1","type2","type3","type4"),
    main="types of chestpains in people who are suffering with heart diseases")
#The above pie chart represents that type4 chest pain is highly prone to heart disease. 






