library(readxl)
#downloading data set
PatientsUpdated <- read_excel("D:/Health Scholars Project/Red Cap Project/Data/PatientsUpdated.xlsx")

#Importation Checks
#Checkingthat dimensions of the dataset "Patientsupdated" matches the 
#dimensions of the datsaset in Excel
dim(PatientsUpdated)

#Making sure the beginning and end of the dataset matches
tail(PatientsUpdated)

#Checking the categorization of each variable done by r
str(PatientsUpdated)

#downloading dataset 
Priordiagnosiscodes <- read_excel("D:/Health Scholars Project/Red Cap Project/Data/Priordiagnosiscodes.xlsx") 

#Dimensions for Diagnnosis Codes dataset
dim(PriorDiagnosisCodes)

#Beginning and end of Diagnosis Codes dataset
head(PriorDiagnosisCodes) 

#Checking the types of variables in the dataset done by r
str(PriorDiagnosisCodes) 

#Overall Summary Statistics of Numeric Variables
summary(PatientsUpdated)

#Average Lenght of Study is approximately 6 days
#Average Age of Patients 55
#Average APRDRG Weight is 1.4 

#Number of Male and Female Participants in PatientsUpdated DataSet
table(PatientsUpdated$Gender)

#Number of Participants in each Group (Should be equal since participants are matched)
table(PatientsUpdated$Language_Group)

#Number of Participants in each Payor Group
table(PatientsUpdated$Payor_Group)

#Participants status at discharge for PatientsUpdated Dataset
table(PatientsUpdated$Status_at_discharge) 

#Patient RACE 1
table(PatientsUpdated$RACE1)

#Patient RACE 2
table(PatientsUpdated$RACE2)

#Number of Hispanic Patients (N = No, Y = Yes, U = Unknown)
table(PatientsUpdated$HISPANIC)

#Histogram of lenght of stay (LOS)
ggplot(data = PatientsUpdated, mapping = aes(x = LOS)) +
  geom_histogram()

#The histogram shows that the distribution of lenghts of stay is skewed to the 
#right.Log transformed LOS becuase it was skewed to the right ggplot(data = PatientsUpdated,

ggplot(data = PatientsUpdated, mapping = aes(x = log(LOS))) + geom_histogram()
#Using the log of LOS helps with skewness.

#Histogram for AGe
ggplot(data = PatientsUpdated, mapping = aes(x = Age)) + geom_histogram()

#log transformed APR DRG Weight
#- The histogram for APRDRG weight is right skewed so I used the log to transform the
ggplot(data = PatientsUpdated, mapping = aes(x = log(APRDRG_Weight))) +
  geom_histogram()



#Number readmitted withing 30 days of discharge; 1 if yes, else missing
ggplot(data = PatientsUpdated, mapping = aes(x = as.factor(readmit_flag))) +
  geom_bar() +
  labs(x = "Number of people readmitted")

#ED visits after discharge
ggplot(data = PatientsUpdated, mapping = aes(x = as.factor(ED_Visit))) +
  geom_bar() +
  labs(x = "Number of ED_Visit")

# Checking for Correlations 

#subset datasets of AGE, LOS, and APRDRF_Weight
cor.1 <- PatientsUpdated %>%
  select(Age, LOS, APRDRG_Weight)
#Correlation between these variables
cor(cor.1) 

# Interactions

#There seems to be a slight interaction between Language_Group
ggplot(data = PatientsUpdated, mapping = aes(x = Age, y = log(LOS), color = as.factor(Language_Group), shape = as.factor(Language_Group))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#seems to be no interaction here
ggplot(data = PatientsUpdated, mapping = aes(x = Age, y = log(APRDRG_Weight), color = as.factor(Language_Group), shape = as.factor(Language_Group))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#slight interaction lower left
ggplot(data = PatientsUpdated, mapping = aes(x = LOS, y = log(APRDRG_Weight), color = as.factor(Language_Group), shape = as.factor(Language_Group))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

#Number of Different diagnosis Codes
length(unique(PriorDiagnosisCodes$ICD10)) 

#Number of Unique Participants
length(unique(PriorDiagnosisCodes$study_id))

#Slighlty skewewd right
ggplot(data = PriorDiagnosisCodes, mapping = aes(x = Days_PriorTo_Admit)) +
  geom_histogram() 

#log transformation version of Days_PriorTo_Admit does not seem to improve skewness
ggplot(data = PriorDiagnosisCodes, mapping = aes(x = log(Days_PriorTo_Admit))) +
  geom_histogram()

# Linear Regression Modelling 

#creating indicator variable called language, race_ind, status_ind, hispanic_ind in the PatientsUpdated Dataset to use in the linear regression model
PatientsUpdated <- PatientsUpdated %>%
  mutate(
    Language = ifelse(Language_Group == "English", 1, 0),
    race_ind = ifelse(RACE1 == "WH", 1, 0),
    status_ind = ifelse(Status_at_discharge == "Alive", 1, 0),
    hispanic_ind = ifelse(HISPANIC == "Y", 1, 0))

model1 <- lm(log(LOS) ~ as.factor(Language) + Age + log(APRDRG_Weight) + as.factor(race_ind) + as.factor(hispanic_ind), data = PatientsUpdated)
summary(model1) 

#Linear Regression Assumption checks
par(mfrow = c(2,2))
plot(model1)

# Logistic Regression Modelling for hospital readmission within 30 days of 
#discharge and emergency department visit within 30 days of discharge 

#creating more indicator variables to perform logistic regression
PatientsUpdated <- PatientsUpdated %>%
  mutate(
    readmit_ind = ifelse(is.na(readmit_flag), 0, 1),
    EDvisit_ind = ifelse(is.na(ED_Visit), 0, 1)
  )
#model for readmitted
model1a <- glm(readmit_ind ~ Language + Age + log(APRDRG_Weight) + race_ind + hispanic_ind + status_ind + EDvisit_ind, family = binomial(link = "logit"), data = PatientsUpdated)
summary(model1a) 

#model for ED_visit
model1b <- glm(EDvisit_ind ~ Language + Age + race_ind + log(APRDRG_Weight) + hispanic_ind + readmit_ind + status_ind, family = binomial(link = "logit"), data = PatientsUpdated)
summary(model1b)

#Modelling for the patients updated dataset with language group 
# as response 

model1 <- glm(as.factor(Language_Group) ~ log(LOS), data = PatientsUpdated, family = "binomial")
summary(model1)

model2 <- glm(as.factor(Language_Group) ~ log(APRDRG_Weight), data = PatientsUpdated, family = "binomial")
summary(model2) 

model3 <- glm(as.factor(Language_Group) ~ log(LOS) + log(APRDRG_Weight) + race_ind + readmit_ind + EDvisit_ind + status_ind + DX_flg + hispanic_ind, data = PatientsUpdated, family = "binomial")
summary(model3)

model1a <- glm(ED_Visit ~ Language, data = PatientsUpdated, family = "binomial")

model1b <- glm(readmit_flag ~ Language, data = PatientsUpdated, family = "binomial")
summary(model1b)

# Logistic regression model for mortality 

#Treating Status at discharge as a factor
PatientsUpdated <- PatientsUpdated %>%
  mutate(Status_at_discharge = as_factor(Status_at_discharge),
         Gender = as_factor(Gender),
         Payor_Group = as.factor(Payor_Group)
  )
#Checking Classes
levels(PatientsUpdated$Status_at_discharge) #Default order of classes for Status 

levels(PatientsUpdated$Gender) #Default order of classes for Gender 

levels(PatientsUpdated$Payor_Group) #Default order of classes for Payor_Group
 
#Reordering order of levels for Status at Discharge
PatientsUpdated$Status_at_discharge <- factor(PatientsUpdated$Status_at_discharge, levels(PatientsUpdated))

#M
Mortality.model <- glm(as.factor(Status_at_discharge) ~ Language + race_ind + Age + Payor_Group + APRDRG_Weight + hispanic_ind, data = PatientsUpdated, family = "binomial")
summary(Mortality.model)

Mortality.model2 <- glm(as.factor(Status_at_discharge) ~ Language + LOS + Age + EDvisit_ind, data = PatientsUpdated, family = "binomial")
summary(Mortality.model2)

Mortality.model3 <- glm(as.factor(Status_at_discharge) ~Language + LOS + Age + log(APRDRG_Weight) + Language, data = PatientsUpdated, family = "binomial")
summary(Mortality.model3)
