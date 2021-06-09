#Statistics 231 Group 16
rawdata = read.csv(file.choose() , stringsAsFactors=FALSE)

#RAW DATA
summary(rawdata)

# Linear Regression Model price/mileage
plot(price~mileage, data=rawdata)
LinearReg1_raw = lm(price~mileage, data=rawdata)
LinearReg1_raw
abline(LinearReg1_raw, col="red")

# Linear Regression Model price/age
plot(price~age, data=rawdata)
LinearReg2_raw = lm(price~age, data=rawdata)
LinearReg2_raw
abline(LinearReg2_raw, col="red")

# Multiple Linear Regression Model
MultiLinearReg_raw = lm(price~mileage+age, data=rawdata)
MultiLinearReg_raw
summary(MultiLinearReg_raw)

#Colors
#__________________________________________________________________________________
# Black
black_data <- subset(rawdata, color=="black", select=price:color)
summary(black_data)

# Multiple Linear Regression Model
MultiLinearReg_black = lm(price~mileage+age, data=black_data)
MultiLinearReg_black
summary(MultiLinearReg_black)

#__________________________________________________________________________________
# White
white_data <- subset(rawdata, color=="white", select=price:color)
summary(white_data)

# Multiple Linear Regression Model
MultiLinearReg_white = lm(price~mileage+age, data=white_data)
MultiLinearReg_white
summary(MultiLinearReg_white)

#__________________________________________________________________________________
# Silver
silver_data <- subset(rawdata, color=="silver", select=price:color)
summary(silver_data)

# Multiple Linear Regression Model
MultiLinearReg_silver = lm(price~mileage+age, data=silver_data)
MultiLinearReg_silver
summary(MultiLinearReg_silver)

#States
#__________________________________________________________________________________
#VA data
va_data <- subset(rawdata, state=="VA", select=price:age)
summary(va_data)

# Linear Regression Model price/mileage
plot(price~mileage, data=va_data)
LinearReg1_va = lm(price~mileage, data=va_data)
LinearReg1_va
abline(LinearReg1_va, col="red")

# Linear Regression Model price/age
plot(price~age, data=va_data)
LinearReg2_va = lm(price~age, data=va_data)
LinearReg2_va
abline(LinearReg2_va, col="red")

# Multiple Linear Regression Model
MultiLinearReg_va = lm(price~mileage+age, data=va_data)
MultiLinearReg_va
summary(MultiLinearReg_va)

#__________________________________________________________________________________
#NY DATA
ny_data <- subset(rawdata, state=="NY", select=price:age)
summary(ny_data)

# Linear Regression Model price/mileage
plot(price~mileage, data=ny_data)
LinearReg1_ny = lm(price~mileage, data=ny_data)
LinearReg1_ny
abline(LinearReg1_ny, col="red")

# Linear Regression Model price/age
plot(price~age, data=ny_data)
LinearReg2_ny = lm(price~age, data=ny_data)
LinearReg2_ny
abline(LinearReg2_ny, col="red")

# Multiple Linear Regression Model
MultiLinearReg_ny = lm(price~mileage+age, data=ny_data)
MultiLinearReg_ny
summary(MultiLinearReg_ny)

#__________________________________________________________________________________
# CA DATA
ca_data <- subset(rawdata, state=="CA", select=price:age)
summary(ca_data)

# Linear Regression Model price/mileage
plot(price~mileage, data=ca_data)
LinearReg1_ca = lm(price~mileage, data=ca_data)	
LinearReg1_ca
abline(LinearReg1_ca, col="red")

# Linear Regression Model price/age
plot(price~age, data=ca_data)
LinearReg2_ca = lm(price~age, data=ca_data)
LinearReg2_ca
abline(LinearReg2_ca, col="red")

# Multiple Linear Regression Model
MultiLinearReg_ca = lm(price~mileage+age, data=ca_data)
MultiLinearReg_ca
summary(MultiLinearReg_ca)

#__________________________________________________________________________________
# TX DATA
tx_data <- subset(rawdata, state=="TX", select=price:age)
summary(tx_data)

# Linear Regression Model price/mileage
plot(price~mileage, data=tx_data)
LinearReg1_tx = lm(price~mileage, data=tx_data)	
LinearReg1_tx
abline(LinearReg1_tx, col="red")

# Linear Regression Model price/age
plot(price~age, data=tx_data)
LinearReg2_tx = lm(price~age, data=tx_data)
LinearReg2_tx
abline(LinearReg2_tx, col="red")

# Multiple Linear Regression Model
MultiLinearReg_tx = lm(price~mileage+age, data=tx_data)
MultiLinearReg_tx
summary(MultiLinearReg_tx)

#__________________________________________________________________________________
# NC DATA
nc_data <- subset(rawdata, state=="NC", select=price:age)
summary(nc_data)

# Linear Regression Model price/mileage
plot(price~mileage, data=nc_data)
LinearReg1_nc = lm(price~mileage, data=nc_data)	
LinearReg1_nc
abline(LinearReg1_nc, col="red")

# Linear Regression Model price/age
plot(price~age, data=nc_data)
LinearReg2_nc = lm(price~age, data=nc_data)
LinearReg2_nc
abline(LinearReg2_nc, col="red")

# Multiple Linear Regression Model
MultiLinearReg_nc = lm(price~mileage+age, data=nc_data)
MultiLinearReg_nc
summary(MultiLinearReg_nc)

#__________________________________________________________________________________
#Final comparison
#Mileage
plot(rawdata$mileage,rawdata$price , xlab="Mileage", ylab="Price", title("How of Car's mileage effect prices"))
abline(LinearReg1_raw, lwd=2, col="red")
abline(LinearReg1_va, lwd=2, col="blue")
abline(LinearReg1_ny, lwd=2, col="green")
abline(LinearReg1_ca, lwd=2, col="black")
abline(LinearReg1_tx, lwd=2, col="orange")
abline(LinearReg1_nc, lwd=2, col="purple")
legend("topright", legend=c("Total Data Set", "Virginia", "New York", "California", "Texas", "North Carolina"), 
       col=c("red", "blue", "green", "black", "orange", "purple"), lty=1:2, cex=0.7)

#Age
plot(rawdata$age,rawdata$price , xlab="Age", ylab="Price", title("How of Car's age effect prices"))
abline(LinearReg2_raw, lwd=2, col="red")
abline(LinearReg2_va, lwd=2, col="blue")
abline(LinearReg2_ny, lwd=2, col="green")
abline(LinearReg2_ca, lwd=2, col="black")
abline(LinearReg2_tx, lwd=2, col="orange")
abline(LinearReg2_nc, lwd=2, col="purple")
legend("topright", legend=c("Total Data Set", "Virginia", "New York", "California", "Texas", "North Carolina"), 
       col=c("red", "blue", "green", "black", "orange", "purple"), lwd=2, cex=0.7)

#Predictions
one_ten = c(1:10)
age = c(one_ten, one_ten, one_ten, one_ten, one_ten, one_ten, one_ten, one_ten, one_ten, one_ten)
mileage = c(one_ten*1000, one_ten*2000, one_ten*3000, one_ten*4000, one_ten*5000, one_ten*6000, one_ten*7000, one_ten*8000, one_ten*9000, one_ten*10000)
dummy_Values = data.frame(age, mileage)
prediction_raw = predict.lm(MultiLinearReg_raw, dummy_Values)
prediction_va = predict.lm(MultiLinearReg_va, dummy_Values)
prediction_ny = predict.lm(MultiLinearReg_ny, dummy_Values)
prediction_ca = predict.lm(MultiLinearReg_ca, dummy_Values)
prediction_tx = predict.lm(MultiLinearReg_tx, dummy_Values)
prediction_nc = predict.lm(MultiLinearReg_nc, dummy_Values)
compare_prediction = data.frame(prediction_raw, prediction_va, prediction_ny, prediction_ca, prediction_tx, prediction_nc)
boxplot(compare_prediction, main="Predicted Prices in Different States", ylab="Prices",names = c("Total", "Virginia", "New York", "California", "Texas", "North Carolina"))

#Hypothesis testing for mean of Difference in mean of Prices in different States
va_ny = t.test(prediction_va,prediction_ny, mu=0)
va_ca = t.test(prediction_va,prediction_ca, mu=0)
va_tx = t.test(prediction_va,prediction_tx, mu=0)
va_nc = t.test(prediction_va,prediction_nc, mu=0)
ny_ca = t.test(prediction_ny,prediction_ca, mu=0)
ny_tx = t.test(prediction_ny,prediction_tx, mu=0)
ny_nc = t.test(prediction_ny,prediction_nc, mu=0)
ca_tx = t.test(prediction_ca,prediction_tx, mu=0)
ca_nc = t.test(prediction_ca,prediction_nc, mu=0)
tx_nc = t.test(prediction_tx,prediction_nc, mu=0)
va_ny
va_ca
va_tx
va_nc
ny_ca
ny_tx
ny_nc
ca_tx
ca_nc
tx_nc
p_values = c(va_ny$p.value, va_ca$p.value, va_tx$p.value, va_nc$p.value, ny_ca$p.value, ny_tx$p.value, ny_nc$p.value, ca_tx$p.value, ca_nc$p.value, tx_nc$p.value)
p_values
