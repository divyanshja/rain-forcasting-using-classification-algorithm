data <- read.csv('weatherAUS.csv',stringsAsFactors = FALSE)

str(data)

summary(data)

sum(is.na(data$MinTemp))
sum(is.na(data$MaxTemp))
sum(is.na(data$Rainfall))
sum(is.na(data$Evaporation))
sum(is.na(data$Sunshine))
sum(is.na(data$WindGustDir))
sum(is.na(data$WindGustSpeed))
sum(is.na(data$WindDir9am))
sum(is.na(data$WindDir3pm))
sum(is.na(data$WindSpeed9am))
sum(is.na(data$WindSpeed3pm))
sum(is.na(data$Humidity9am))
sum(is.na(data$Humidity3pm))
sum(is.na(data$Pressure9am))
sum(is.na(data$Pressure3pm))
sum(is.na(data$Cloud9am))
sum(is.na(data$Cloud3pm))
sum(is.na(data$Temp9am))
sum(is.na(data$Temp3pm))
sum(is.na(data$RISK_MM))
sum(is.na(data$RainToday))
sum(is.na(data$RISK_MM))
sum(is.na(data$RainTomorrow))




for(i in 1:142193)
{
  if(is.na(data$MinTemp[i]) == "TRUE")
  {
    data$MinTemp[i] = round(mean(data$MinTemp,na.rm=TRUE),digits = 1)
  }
}



for(i in 1:142193)
{
  if(is.na(data$MaxTemp[i]) == "TRUE")
  {
    data$MaxTemp[i] = round(mean(data$MaxTemp,na.rm=TRUE),digits = 1)
  }
}



for(i in 1:142193)
{
  if(is.na(data$Rainfall[i]) == "TRUE")
  {
    data$Rainfall[i] = round(mean(data$Rainfall,na.rm=TRUE),digits = 1)
  }
}



for(i in 1:142193)
{
  if(is.na(data$Evaporation[i]) == "TRUE")
  {
    data$Evaporation[i] = round(mean(data$Evaporation,na.rm=TRUE),digits = 1)
  }
}


for(i in 1:142193)
{
  if(is.na(data$Sunshine[i]) == "TRUE")
  {
    data$Sunshine[i] = round(mean(data$Sunshine,na.rm=TRUE),digits = 1)
  }
}


for(i in 1:142193)
{
  if(is.na(data$WindGustSpeed[i]) == "TRUE")
  {
    data$WindGustSpeed[i] = round(mean(data$WindGustSpeed,na.rm=TRUE),digits = 1)
  }
}


for(i in 1:142193)
{
  if(is.na(data$WindSpeed9am[i]) == "TRUE")
  {
    data$WindSpeed9am[i] = round(mean(data$WindSpeed9am,na.rm=TRUE),digits = 1)
  }
}




for(i in 1:142193)
{
  if(is.na(data$WindSpeed3pm[i]) == "TRUE")
  {
    data$WindSpeed3pm[i] = round(mean(data$WindSpeed3pm,na.rm=TRUE),digits = 0)
  }
}



for(i in 1:142193)
{
  if(is.na(data$Humidity9am[i]) == "TRUE")
  {
    data$Humidity9am[i] = round(mean(data$Humidity9am,na.rm=TRUE),digits = 0)
  }
}


for(i in 1:142193)
{
  if(is.na(data$Humidity3pm[i]) == "TRUE")
  {
    data$Humidity3pm[i] = round(mean(data$Humidity3pm,na.rm=TRUE),digits = 0)
  }
}



for(i in 1:142193)
{
  if(is.na(data$Pressure9am[i]) == "TRUE")
  {
    data$Pressure9am[i] = round(mean(data$Pressure9am,na.rm=TRUE),digits = 1)
  }
}



for(i in 1:142193)
{
  if(is.na(data$Pressure3pm[i]) == "TRUE")
  {
    data$Pressure3pm[i] = round(mean(data$Pressure3pm,na.rm=TRUE),digits = 1)
  }
}





for(i in 1:142193)
{
  if(is.na(data$Cloud9am[i]) == "TRUE")
  {
    data$Cloud9am[i] = round(mean(data$Cloud9am,na.rm=TRUE),digits = 0)
  }
}


for(i in 1:142193)
{
  if(is.na(data$Cloud3pm[i]) == "TRUE")
  {
    data$Cloud3pm[i] = round(mean(data$Cloud3pm,na.rm=TRUE),digits = 0)
  }
}


for(i in 1:142193)
{
  if(is.na(data$Temp9am[i]) == "TRUE")
  {
    data$Temp9am[i] = round(mean(data$Temp9am,na.rm=TRUE),digits = 1)
  }
}


for(i in 1:142193)
{
  if(is.na(data$Temp3pm[i]) == "TRUE")
  {
    data$Temp3pm[i] = round(mean(data$Temp3pm,na.rm=TRUE),digits = 1)
  }
}


for(i in 1:142193)
{
  if(is.na(data$RISK_MM[i]) == "TRUE")
  {
    data$RISK_MM[i] = round(mean(data$RISK_MM,na.rm=TRUE),digits = 1)
  }
}



#for leval encoding of  attribute (WindGustDir)


n <- 0 
for(unique_value in unique(data$WindGustDir)){
  n = n+1
}
print(n)

values <- c(seq(1:n))
i <- 1
for(unique_value in unique(data$WindGustDir)){
  values[i] <- unique_value
  print(values[i])
  i <- i+1
}

for(i in 1:142193)
{
  for(j in 1:n)
  {
    if(data$WindGustDir[i] == values[j])
    {
      data$WindGustDir[i] <- j
      break
    }
  }
}
print("Done")



for(i in 1:142193)
{
  if(data$WindGustDir[i] == 15)
  {
    data$WindGustDir[i] = "NA"
  }
}



n <- 0 
for(unique_value in unique(data$WindDir9am)){
  n = n+1
}
print(n)

values <- c(seq(1:n))
i <- 1
for(unique_value in unique(data$WindDir9am)){
  values[i] <- unique_value
  print(values[i])
  i <- i+1
}

for(i in 1:142193)
{
  for(j in 1:n)
  {
    if(data$WindDir9am[i] == values[j])
    {
      data$WindDir9am[i] <- j
      break
    }
  }
}
print("Done")



for(i in 1:142193)
{
  if(data$WindDir9am[i] == 9)
  {
    data$WindDir9am[i] = "NA"
  }
}




n <- 0 
for(unique_value in unique(data$WindDir3pm)){
  n = n+1
}
print(n)

values <- c(seq(1:n))
i <- 1
for(unique_value in unique(data$WindDir3pm)){
  values[i] <- unique_value
  print(values[i])
  i <- i+1
}

for(i in 1:142193)
{
  for(j in 1:n)
  {
    if(data$WindDir3pm[i] == values[j])
    {
      data$WindDir3pm[i] <- j
      break
    }
  }
}
print("Done")


for(i in 1:142193)
{
  if(data$WindDir3pm[i] == 16)
  {
    data$WindDir3pm[i] = "NA"
  }
}



n <- 0 
for(unique_value in unique(data$RainToday)){
  n = n+1
}
print(n)

values <- c(seq(1:n))
i <- 1
for(unique_value in unique(data$RainToday)){
  values[i] <- unique_value
  print(values[i])
  i <- i+1
}

for(i in 1:142193)
{
  for(j in 1:n)
  {
    if(data$RainToday[i] == values[j])
    {
      data$RainToday[i]<- j
      break
    }
  }
}

for(i in 1:142193)
{
if(data$RainToday[i] == 3)
{
  data$RainToday[i] = "NA"
}
}



write.csv(data,'data1.csv')


dataset <- read.csv('data1.csv',stringsAsFactors = FALSE)


sum(is.na(dataset$RainToday))
sum(is.na(dataset$WindGustDir))

sum(is.na(dataset$WindDir9am))
sum(is.na(dataset$WindDir3pm))






for(i in 1:142193)
{
  if(is.na(dataset$WindGustDir[i]) == "TRUE")
  {
    dataset$WindGustDir[i] = round(mean(dataset$WindGustDir,na.rm=TRUE),digits = 0)
  }
}

for(i in 1:142193)
{
  if(is.na(dataset$WindDir9am[i]) == "TRUE")
  {
    dataset$WindDir9am[i] = round(mean(dataset$WindDir9am,na.rm=TRUE),digits = 0)
  }
}

for(i in 1:142193)
{
  if(is.na(dataset$WindDir3pm[i]) == "TRUE")
  {
    dataset$WindDir3pm[i] = round(mean(dataset$WindDir3pm,na.rm=TRUE),digits = 0)
  }
}

for(i in 1:142193)
{
  if(is.na(dataset$RainToday[i]) == "TRUE")
  {
    dataset$RainToday[i] = round(mean(dataset$RainToday,na.rm=TRUE),digits = 0)
  }
}



n <- 0 
for(unique_value in unique(dataset$RainTomorrow)){
  n = n+1
}
print(n)

values <- c(seq(1:n))
i <- 1
for(unique_value in unique(dataset$RainTomorrow)){
  values[i] <- unique_value
  print(values[i])
  i <- i+1
}

for(i in 1:142193)
{
  for(j in 1:n)
  {
    if(dataset$RainTomorrow[i] == values[j])
    {
      dataset$RainTomorrow[i]<- j
      break
    }
  }
}


write.csv(dataset,'cleandata.csv')










dataset <- read.csv('cleandata.csv')

str(dataset)




    