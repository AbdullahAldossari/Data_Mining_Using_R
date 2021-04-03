##Reading data set
##creating dataset


#I want to test the English proficiency of international students in the US 
# To see the see the fully proficient international student 
#and how they do in courses and college 

####Installing package

install.packages("neuralnet") 

library(neuralnet)

#There is an english test that test the language skills and technical knowledge 
#7 students were tested and if a student pass the test
#they will be placed and accepted at the college 
#Creating dataset 
#LS = Language Skills from 0 to 100
#TK= Technical Knowledge from 0 to 100 
##Placed= Student accepted at the college 

LS=c(20,10,30,20,80,30)
TK=c(90,20,40,50,50,80)
Placed=c(1,0,0,0,1,1)
Intstudents$Accepted<-Intstudents$Placed =="1"
Intstudents$Rejected<-Intstudents$Placed =="0"

nn=neuralnet(Accepted + Rejected~LS+TK,data=Intstudents, hidden=3,act.fct = "logistic",
             linear.output = FALSE)


Intstudents=data.frame(LS,TK,Placed)

nn=neuralnet(Placed~LS+TK,data=Intstudents, hidden=3,act.fct = "logistic",
             linear.output = FALSE)


#Display the prediction 

nn$weights

# display predictions
prediction(nn)

#Plot the model 
plot(nn)

##Creating a test dataset 

LS_Test=c(30,40,85)
TK_Test=c(85,50,40)
test_data=data.frame(LS_Test,TK_Test)

## Prediction using compute function , neural network
Predict=compute(nn,test_data)
Predict$net.result
##The results are .97, .008, .024
#Lets imprve the accuracy 
#Converting it to binary classes
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred
#Predictions are 1,0,0 which means there is an improve in accuracy 


#lets try it with a different hidden neurolant 

Intstudents$Accepted<-Intstudents$Placed =="1"
Intstudents$Rejected<-Intstudents$Placed =="0"

nn1=neuralnet(Accepted + Rejected~LS+TK,data=Intstudents, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

#Display the prediction 

nn1$weights

# display predictions
prediction(nn1)

#Plotting the model 
plot(nn1)






## Prediction using compute function , neural network
Predict=compute(nn1,test_data)
Predict$net.result
##The results here is different than when i used only 3 hidden n values
#The results are .99, .384, .957 
#Lets imprve the accuracy 
#Converting it to binary classes
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred
#Predictions are 1,0,0 which means there is an improve in accuracy 
