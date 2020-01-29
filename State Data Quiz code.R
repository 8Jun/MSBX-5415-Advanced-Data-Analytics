data(state)
statedata = cbind(data.frame(state.x77),
state.abb,state.area,state.center, state.division, state.name, state.region)

str(statedata)

#Q1
q_1 <- tapply(statedata$HS.Grad, list(statedata$state.region), mean)
q_1
#Answer: West

#Q2
q_2 <- tapply(statedata$Murder, statedata$state.region, median)
q_2
plot_q_2 <- boxplot(statedata$Murder ~ statedata$state.region)
plot_q_2
#Answer: South

#Q3
northeast <- subset(statedata, statedata$state.region=="Northeast")
q_3 <- boxplot(northeast$Murder ~ northeast$state.name)
q_3
#Answer: New York

#Q4
life_expectancy <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(life_expectancy)
#Answer: -2.180e-5

#Q5
model_q_5 <- lm(statedata$Life.Exp ~ statedata$Income, data = statedata)
plot(statedata$Income ~ statedata$Life.Exp)
abline(model_q_5, col="red")
#Answer: Positively associated

#Q6
#Answer: False

#Q7
model_1 <- lm(Life.Exp ~ Income + HS.Grad + Frost + Murder , data=statedata)
summary(model_1)
model_2 <- lm(Life.Exp ~ HS.Grad + Population + Income + Frost, data=statedata)
summary(model_2)
model_3 <- lm(Life.Exp ~ Frost + Murder + HS.Grad + Illiteracy, data=statedata)
summary(model_3)
model_4 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(model_4)
#Answer: model_4 variables

#Q8
sort(predict(model_4))
#Answer: Alabama

#Q9
sort(predict(model_4))
#Answer: Washington

#Q10
sort(model_4$residuals)
#Answer: Indiana

