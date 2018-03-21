library(ggplot2)             #including graph package ggplot2
data<- read.csv(file.choose(), header = T)  # Read and store data into a variabe data
attach(data)
Demonitisation[Demonitisation == 'not Yes'] <- 'No'       # Convert Not Yes Into No
data1 <- data[,-8]
data2 <- cbind(data1, Demonitisation)
categoryinc <- cut(monthly.income, c(-1,30000,100000,500000), labels = c("Poor","Middle","Rich")) #Categorize income into three groups
data3 <- cbind(data2, categoryinc) # join the column categoryinc to data
categoryage <- cut(age, c(-1,21,60,150), labels = c("Teenager", "Adult", "Old"))  #Categorize age into three groups
data4 <- cbind(data3, categoryage)
data5=subset(data4,age<150)                   #Remove people with age more than 150
data6=subset(data5, Urban!="NA")                 # Remove people with urban data not available
# Manish
q1<- qplot(x=Demonitisation, data=data2, geom="bar")
q1+ggtitle("No. of ppl against/support of demonitisation")

# Manish
q11<- qplot(x=categoryinc, data=data3,fill=Demonitisation, geom="bar")
q11+ggtitle("No. of ppl against/support of demonitisation income category wise")

q12<-qplot(x=Residence, data=data2,fill=Demonitisation, geom="bar")+theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q12+ggtitle("No. of ppl against/support of demonitisation state wise")

q13<- qplot(x=categoryage, data=data4,fill=Demonitisation, geom="bar")
q13+ggtitle("No. of ppl against/support of demonitisation age category wise")

q14<- qplot(x=sex, data=data2,fill=Demonitisation, geom="bar")
q14+ggtitle("No. of ppl against/support of demonitisation gender wise")

q15<- qplot(x=Urban, data=data2,fill=Demonitisation, geom="bar")
q15+ggtitle("No. of ppl against/support of demonitisation urban wise")

q16<- qplot(x=Urban, data=data6,fill=categoryinc, geom="bar")
q16+ggtitle("income category distribution urban/rural wise")

q17<- qplot(x=categoryage, data=data6,fill=categoryinc, geom="bar")
q17+ggtitle("income category distribution age wise")

q18<- qplot(x=Residence, data=data6,fill=categoryinc, geom="bar")+ theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q18+ggtitle("income category distribution state wise")

q19<- qplot(x=Residence, data=data6,fill=Urban, geom="bar")+ theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q19+ggtitle("Urban distribution state wise")

q20<- qplot(x=Residence, data=data6,fill=sex, geom="bar")+ theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q20+ggtitle("sex ratio state wise")

q21 <- qplot(data=data6, x=categoryinc) +geom_bar(aes(fill=sex))+ scale_fill_manual(values=c("red","blue"))
q21+ggtitle("gender wise income category")


q2 <- qplot(data=data3[categoryinc=='Poor',], x=Residence) +geom_bar(aes(fill=Demonitisation, y = (..count..)/sum(..count..)))+ theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q2+ggtitle("state wise support/against Demonitisation for poor category")

q3 <- qplot(data=data3[categoryinc=='Middle',], x=Residence) +geom_bar(aes(fill=Demonitisation))+ theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q3+ggtitle("state wise support/against Demonitisation for Middle category")

q4 <- qplot(data=data3[categoryinc=='Rich',], x=Residence) +geom_bar(aes(fill=Demonitisation))+theme(legend.position="top",axis.text.x = element_text(angle=90, hjust=1))
q4+ggtitle("state wise support/against Demonitisation for Rich category")
