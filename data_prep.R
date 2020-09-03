options(stringsAsFactors = FALSE)
setwd("~/Uni/Lena Dante/BA")

survey_data <- read.csv2(file = "2020-08-27-Rohdaten von Umfrage umfrage_theatervermittlung_am_Gymnasium (alle Teilnehmer).csv")
names(survey_data)[1] <- "ID"

#Frage1 - Selection
names(survey_data)[3] <- "F1"
order_vector = c(1,2,3,9,4,5,7,8,6,10)
var_names = names(table(survey_data$F1))[order(order_vector)]
coding = c(-1,-2,0,6,5,4,3,2,1,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F1[j] == var_names[i]){
      survey_data$F1[j] = coding[i]
    }
  }
}


#Frage2 - Multiple Choice 
names(survey_data)[4:11] <- c("F2_1", "F2_2", "F2_3", "F2_4", "F2_5", "F2_6", "F2_7", "F2_8")
for(i in 1:7){
  var_name = names(table(survey_data[,i+3]))[3]
  
  for(j in 1:length(survey_data$ID)){
    if(survey_data[j,i+3] == var_name){
      survey_data[j,i+3] = 1
    }
    else if(survey_data[j,i+3] == " -2"){
      survey_data[j,i+3] = -2
    }
    else{
      survey_data[j,i+3] = 0
    }
      
  }
}

text_variable = character(length = length(survey_data$F2_8[survey_data$F2_8 != ""]))
fill_count = 1
for(j in 1:length(survey_data$ID)){
  if(survey_data$F2_8[j] == ""){
    survey_data$F2_8[j] = -1
  }
  else{
    text_variable[fill_count] = survey_data$F2_8[j]
    fill_count = fill_count +1
  }
}

#Frage 3 - Likert 
names(survey_data)[12] <- "F3"
order_vector = c(1,2,4,6,3,5,7)
var_names = names(table(survey_data$F3))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F3[j] == var_names[i]){
      survey_data$F3[j] = coding[i]
    }
  }
}

#Frage 4 - Likert 
names(survey_data)[13] <- "F4"
order_vector = c(1,2,4,6,3,5,7)
var_names = names(table(survey_data$F4))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F4[j] == var_names[i]){
      survey_data$F4[j] = coding[i]
    }
  }
}

#Frage 5 - Selection 
names(survey_data)[14] <- "F5"
scale = names(table(survey_data$F5))
coding = c(-1,-2,1,2,3,0,-3)
for(i in 1:length(scale)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F5[j] == scale[i]){
      survey_data$F5[j] = coding[i]
    }
  }
}

#Frage 6 - Selection 
names(survey_data)[15] <- "F6"
scale = names(table(survey_data$F6))
coding = c(-1,-2,1,2,3,0)
for(i in 1:length(scale)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F6[j] == scale[i]){
      survey_data$F6[j] = coding[i]
    }
  }
}
#aufteilen in Ja - Nein , Dauer

#Frage 7 - Selection
names(survey_data)[16] <- "F7"
scale = names(table(survey_data$F7))
coding = c(-1,-2,1,0,-3)
for(i in 1:length(scale)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F7[j] == scale[i]){
      survey_data$F7[j] = coding[i]
    }
  }
}

#Frage 8 - Selection
names(survey_data)[17] <- "F8"
scale = names(table(survey_data$F8))
coding = c(-1,-2,1,2,3,0,-3)
for(i in 1:length(scale)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F8[j] == scale[i]){
      survey_data$F8[j] = coding[i]
    }
  }
}


#Frage 9 - Selection
names(survey_data)[18] <- "F9"
scale = names(table(survey_data$F9))
coding = c(-1,-2,1,0)
for(i in 1:length(scale)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F9[j] == scale[i]){
      survey_data$F9[j] = coding[i]
    }
  }
}

#Frage 10 - Likert "oft"
names(survey_data)[19:32] <- c("F10_1", "F10_2", "F10_3", "F10_4", "F10_5", "F10_6", "F10_7", "F10_8", "F10_9", "F10_10", "F10_11", "F10_12", "F10_13", "F10_14")
#1
order_vector = c(1,2,6,5,3,7,4)
var_names = names(table(survey_data$F10_1))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_1[j] == var_names[i]){
      survey_data$F10_1[j] = coding[i]
    }
  }
}
#2
order_vector = c(1,2,6,5,8,3,7,4)
var_names = names(table(survey_data$F10_2))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_2[j] == var_names[i]){
      survey_data$F10_2[j] = coding[i]
    }
  }
}
#3
order_vector = c(1,2,6,5,8,3,7,4)
var_names = names(table(survey_data$F10_3))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_3[j] == var_names[i]){
      survey_data$F10_3[j] = coding[i]
    }
  }
}
#4
order_vector = c(1,2,6,5,8,3,7,4)
var_names = names(table(survey_data$F10_4))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_4[j] == var_names[i]){
      survey_data$F10_4[j] = coding[i]
    }
  }
}
#5
order_vector = c(1,2,6,5,3,4)
var_names = names(table(survey_data$F10_5))[order(order_vector)]
coding = c(-1,-2,1,2,3,4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_5[j] == var_names[i]){
      survey_data$F10_5[j] = coding[i]
    }
  }
}
#6
order_vector = c(1,2,5,3,4)
var_names = names(table(survey_data$F10_6))[order(order_vector)]
coding = c(-1,-2,1,2,3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_6[j] == var_names[i]){
      survey_data$F10_6[j] = coding[i]
    }
  }
}
#7
order_vector = c(1,2,6,5,8,3,7,4)
var_names = names(table(survey_data$F10_7))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_7[j] == var_names[i]){
      survey_data$F10_7[j] = coding[i]
    }
  }
}
#8
order_vector = c(1,2,5,6,3,4)
var_names = names(table(survey_data$F10_8))[order(order_vector)]
coding = c(-1,-2,1,2,3,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_8[j] == var_names[i]){
      survey_data$F10_8[j] = coding[i]
    }
  }
}
#9
order_vector = c(1,2,3,4)
var_names = names(table(survey_data$F10_9))[order(order_vector)]
coding = c(-1,-2,1,2)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_9[j] == var_names[i]){
      survey_data$F10_9[j] = coding[i]
    }
  }
}
#10
order_vector = c(1,2,6,5,7,3,4)
var_names = names(table(survey_data$F10_10))[order(order_vector)]
coding = c(-1,-2,1,2,3,4, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_10[j] == var_names[i]){
      survey_data$F10_10[j] = coding[i]
    }
  }
}
#11
order_vector = c(1,2,6,5,8,3,7,4)
var_names = names(table(survey_data$F10_11))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_11[j] == var_names[i]){
      survey_data$F10_11[j] = coding[i]
    }
  }
}
#12
order_vector = c(1,2,6,5,8,3,7,4)
var_names = names(table(survey_data$F10_12))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_12[j] == var_names[i]){
      survey_data$F10_12[j] = coding[i]
    }
  }
}
#13
order_vector = c(1,2,5,7,3,6,4)
var_names = names(table(survey_data$F10_13))[order(order_vector)]
coding = c(-1,-2,1,2,4,5, -3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F10_13[j] == var_names[i]){
      survey_data$F10_13[j] = coding[i]
    }
  }
}
#14
text_variable = character(length = length(survey_data$F10_14[survey_data$F10_14 != ""]))
fill_count = 1
for(j in 1:length(survey_data$ID)){
  if(survey_data$F10_14[j] == ""){
    survey_data$F10_14[j] = -1
  }
  else{
    text_variable[fill_count] = survey_data$F10_14[j]
    fill_count = fill_count +1
  }
}

#Frage 11
names(survey_data)[33:43] <- c("F11_1", "F11_2", "F11_3", "F11_4", "F11_5", "F11_6", "F11_7", "F11_8", "F11_9", "F11_10", "F11_11")

#1
order_vector = c(1,2,5,4,6,3)
var_names = names(table(survey_data$F11_1))[order(order_vector)]
coding = c(-1,-2,2,3,4,5)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_1[j] == var_names[i]){
      survey_data$F11_1[j] = coding[i]
    }
  }
}
#2
order_vector = c(1,2,6,5,3,7,4)
var_names = names(table(survey_data$F11_2))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_2[j] == var_names[i]){
      survey_data$F11_2[j] = coding[i]
    }
  }
}
#3
order_vector = c(1,2,6,5,8,7,3,4)
var_names = names(table(survey_data$F11_3))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_3[j] == var_names[i]){
      survey_data$F11_3[j] = coding[i]
    }
  }
}
#4
order_vector = c(1,2,6,5,9,8,3,7,4)
var_names = names(table(survey_data$F11_4))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_4[j] == var_names[i]){
      survey_data$F11_4[j] = coding[i]
    }
  }
}
#5
order_vector = c(1,2,6,5,9,8,3,7,4)
var_names = names(table(survey_data$F11_5))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_5[j] == var_names[i]){
      survey_data$F11_5[j] = coding[i]
    }
  }
}
#6
order_vector = c(1,2,5,7,6,3,4)
var_names = names(table(survey_data$F11_6))[order(order_vector)]
coding = c(-1,-2,1,2,3,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_6[j] == var_names[i]){
      survey_data$F11_6[j] = coding[i]
    }
  }
}
#7
order_vector = c(1,2,6,5,8,7,3,4)
var_names = names(table(survey_data$F11_7))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_7[j] == var_names[i]){
      survey_data$F11_7[j] = coding[i]
    }
  }
}
#8
order_vector = c(1,2,6,5,9,8,3,7,4)
var_names = names(table(survey_data$F11_8))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_8[j] == var_names[i]){
      survey_data$F11_8[j] = coding[i]
    }
  }
}
#9
order_vector = c(1,2,5,6,3,4)
var_names = names(table(survey_data$F11_9))[order(order_vector)]
coding = c(-1,-2,1,2,3,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_9[j] == var_names[i]){
      survey_data$F11_9[j] = coding[i]
    }
  }
}
#10
order_vector = c(1,2,4,3)
var_names = names(table(survey_data$F11_10))[order(order_vector)]
coding = c(-1,-2,1,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_10[j] == var_names[i]){
      survey_data$F11_10[j] = coding[i]
    }
  }
}
#11
order_vector = c(1,2,5,8,7,3,6,4)
var_names = names(table(survey_data$F11_11))[order(order_vector)]
coding = c(-1,-2,1,2,3,5,-3,-4)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F11_11[j] == var_names[i]){
      survey_data$F11_11[j] = coding[i]
    }
  }
}

#Frage 12
names(survey_data)[44] <- "F12"
scale = names(table(survey_data$F12))
coding = c(-1,-2,1,0,-3)
for(i in 1:length(scale)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F12[j] == scale[i]){
      survey_data$F12[j] = coding[i]
    }
  }
}

#Frage 13
names(survey_data)[45] <- "F13"
order_vector = c(1,2,8,9,3,4,5,6,7,10)
var_names = names(table(survey_data$F13))[order(order_vector)]
coding = c(-1,-2,5,6,7,8,9,10,11,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F13[j] == var_names[i]){
      survey_data$F13[j] = coding[i]
    }
  }
}

#Frage 14
names(survey_data)[46:51] <- c("F14_1", "F14_2", "F14_3", "F14_4", "F14_5", "F14_6")
#1
order_vector = c(1,2,5,4,3,8,7,6)
var_names = names(table(survey_data$F14_1))[order(order_vector)]
coding = c(-1,-2,0,1,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F14_1[j] == var_names[i]){
      survey_data$F14_1[j] = coding[i]
    }
  }
}
#2
order_vector = c(1,2,5,4,3,8,7,6)
var_names = names(table(survey_data$F14_2))[order(order_vector)]
coding = c(-1,-2,0,1,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F14_2[j] == var_names[i]){
      survey_data$F14_2[j] = coding[i]
    }
  }
}
#3
order_vector = c(1,2,5,4,3,8,7,6)
var_names = names(table(survey_data$F14_3))[order(order_vector)]
coding = c(-1,-2,0,1,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F14_3[j] == var_names[i]){
      survey_data$F14_3[j] = coding[i]
    }
  }
}
#4
order_vector = c(1,2,5,4,3,8,7,6)
var_names = names(table(survey_data$F14_4))[order(order_vector)]
coding = c(-1,-2,0,1,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F14_4[j] == var_names[i]){
      survey_data$F14_4[j] = coding[i]
    }
  }
}
#5
order_vector = c(1,2,5,4,3,6)
var_names = names(table(survey_data$F14_5))[order(order_vector)]
coding = c(-1,-2,0,1,3,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F14_5[j] == var_names[i]){
      survey_data$F14_5[j] = coding[i]
    }
  }
}
#6
order_vector = c(1,2,5,4,3,8,7,6)
var_names = names(table(survey_data$F14_6))[order(order_vector)]
coding = c(-1,-2,0,1,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F14_6[j] == var_names[i]){
      survey_data$F14_6[j] = coding[i]
    }
  }
}

#Frage 15

names(survey_data)[52:61] <- c("F15_1", "F15_2", "F15_3", "F15_4", "F15_5", "F15_6", "F15_7", "F15_8", "F15_9", "F15_10")
#1
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_1))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_1[j] == var_names[i]){
      survey_data$F15_1[j] = coding[i]
    }
  }
}
#2
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_2))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_2[j] == var_names[i]){
      survey_data$F15_2[j] = coding[i]
    }
  }
}
#3
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_3))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_3[j] == var_names[i]){
      survey_data$F15_3[j] = coding[i]
    }
  }
}
#4
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_4))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_4[j] == var_names[i]){
      survey_data$F15_4[j] = coding[i]
    }
  }
}
#5
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_5))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_5[j] == var_names[i]){
      survey_data$F15_5[j] = coding[i]
    }
  }
}
#6
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_6))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_6[j] == var_names[i]){
      survey_data$F15_6[j] = coding[i]
    }
  }
}
#7
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_7))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_7[j] == var_names[i]){
      survey_data$F15_7[j] = coding[i]
    }
  }
}
#8
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_8))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_8[j] == var_names[i]){
      survey_data$F15_8[j] = coding[i]
    }
  }
}
#9
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_9))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_9[j] == var_names[i]){
      survey_data$F15_9[j] = coding[i]
    }
  }
}
#10
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F15_10))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F15_10[j] == var_names[i]){
      survey_data$F15_10[j] = coding[i]
    }
  }
}

#Frage 16
names(survey_data)[62:64] <- c("F16_1", "F16_2", "F16_3")
#1
order_vector = c(1,2,6,4,3,8,5,7)
var_names = names(table(survey_data$F16_1))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F16_1[j] == var_names[i]){
      survey_data$F16_1[j] = coding[i]
    }
  }
}
#2
order_vector = c(1,2,6,4,3,8,5,7)
var_names = names(table(survey_data$F16_2))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F16_2[j] == var_names[i]){
      survey_data$F16_2[j] = coding[i]
    }
  }
}
#3
order_vector = c(1,2,6,4,3,8,5,7)
var_names = names(table(survey_data$F16_3))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F16_3[j] == var_names[i]){
      survey_data$F16_3[j] = coding[i]
    }
  }
}
#Frage 17
names(survey_data)[65:67] <- c("F17_1", "F17_2", "F17_3")
#1
order_vector = c(1,2,4,6,3,5,7)
var_names = names(table(survey_data$F17_1))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F17_1[j] == var_names[i]){
      survey_data$F17_1[j] = coding[i]
    }
  }
}
#2
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F17_2))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F17_2[j] == var_names[i]){
      survey_data$F17_2[j] = coding[i]
    }
  }
}
#3
order_vector = c(1,2,8,4,6,3,5,7)
var_names = names(table(survey_data$F17_3))[order(order_vector)]
coding = c(-1,-2,1,2,3,4,5,-3)
for(i in 1:length(var_names)){
  for(j in 1:length(survey_data$ID)){
    if(survey_data$F17_3[j] == var_names[i]){
      survey_data$F17_3[j] = coding[i]
    }
  }
}
#Frage 18
names(survey_data)[68:81] <- c("F18_1", "F18_2", "F18_3", "F18_4", "F18_5", "F18_6", "F18_7", "F18_8", "F18_9", "F18_10", "F18_11", "F18_12", "F18_13", "F18_14")
for(i in 1:14){
  var_name = names(table(survey_data[,i+67]))[3]
  
  for(j in 1:length(survey_data$ID)){
    if(survey_data[j,i+67] == var_name){
      survey_data[j,i+67] = 1
    }
    else if(survey_data[j,i+67] == "-2"){
      survey_data[j,i+67] = -2
    }
    else{
      survey_data[j,i+67] = 0
    }
    
  }
}
#Frage 19
names(survey_data)[82] <- "F19"
text_variable = character(length = length(survey_data$F19[survey_data$F19 != ""]))
fill_count = 1
for(j in 1:length(survey_data$ID)){
  if(survey_data$F19[j] == ""){
    survey_data$F19[j] = -1
  }
  else{
    text_variable[fill_count] = survey_data$F19[j]
    fill_count = fill_count +1
  }
}

#Frage 20 
names(survey_data)[83] <- "F20"
names(table(survey_data$F20))
for(j in 1:length(survey_data$ID)){
  if(is.na(survey_data$F20[j])){
    survey_data$F20[j] = -1
  }
}

#Frage 21
names(survey_data)[84] <- "F21"
var_name = names(table(survey_data[,84]))[3]
for(j in 1:length(survey_data$ID)){
  if(survey_data[j,84] == var_name){
    survey_data[j,84] = 1
  }
  else if(survey_data[j,84] == "-2"){
    survey_data[j,84] = -2
  }
  else if(survey_data[j,84] == "-1"){
    survey_data[j,84] = -1
  }
  else{
    survey_data[j,84] = 0
  }
  
}

names(survey_data)[85] <- "F22"
names(survey_data)[86] <- "F23"
text_variable = character(length = length(survey_data$F23[survey_data$F23 != ""]))
fill_count = 1
for(j in 1:length(survey_data$ID)){
  if(survey_data$F23[j] == ""){
    survey_data$F23[j] = -1
  }
  else{
    text_variable[fill_count] = survey_data$F23[j]
    fill_count = fill_count +1
  }
}

survey_data = survey_data[, 1:88]
survey_data = survey_data[survey_data$abgeschlossen == 1,]

survey_data_original <- read.csv2(file = "2020-08-27-Rohdaten von Umfrage umfrage_theatervermittlung_am_Gymnasium (alle Teilnehmer).csv")

