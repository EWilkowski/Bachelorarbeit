source("data_prep.R")
options(stringsAsFactors = FALSE)
#Working Directory Path anpassen
setwd("~/Uni/Lena Dante/BA")

test = survey_data[survey_data$F1 != -1,]
prop.table(table(test$F1))
#Verteilungen
prop.table(table(survey_data$F2_1))
prop.table(table(survey_data$F2_2))
prop.table(table(survey_data$F2_3))
prop.table(table(survey_data$F2_4))
prop.table(table(survey_data$F2_5))
prop.table(table(survey_data$F2_6))
prop.table(table(survey_data$F2_7))

#Multiple Regressionsanalyse - Regelmäßigkeit von Theaterbesuchen
test = survey_data[survey_data$F1 >= 0 & 
                    survey_data$F3 >= 0 & 
                    survey_data$F4 >= 0 & 
                    survey_data$F5 >= 0 & 
                    survey_data$F6 >= 0 & 
                    survey_data$F7 >= 0 & 
                    survey_data$F8 >= 0 #& 
                    # survey_data$F9 >= 0 &
                    # survey_data$F17_1 >= 0 &
                    # survey_data$F2_1 >= 0 &
                    # survey_data$F2_2 >= 0 &
                    # survey_data$F2_3 >= 0 &
                    # survey_data$F2_4 >= 0 &
                    # survey_data$F2_5 >= 0 &
                    # survey_data$F2_6 >= 0 &
                    # survey_data$F2_7 >= 0 
                    ,]

summary(lm(as.numeric(survey_data$F17_1)~ as.numeric(survey_data$F1) +
             as.numeric(survey_data$F3) +
             as.numeric(survey_data$F4) +
             as.numeric(survey_data$F5) +
             as.numeric(survey_data$F6) +
             as.numeric(survey_data$F7) +
             as.numeric(survey_data$F8) #+
             # as.numeric(survey_data$F9) +
             # as.numeric(survey_data$F2_1) +
             # as.numeric(survey_data$F2_2) +
             # as.numeric(survey_data$F2_3) +
             # as.numeric(survey_data$F2_4) +
             # as.numeric(survey_data$F2_5) +
             # as.numeric(survey_data$F2_6) +
             # as.numeric(survey_data$F2_7) 
             # 
             ))

test = survey_data[survey_data$F13 >= 0,]


#Mittelwerte
test = survey_data[survey_data$F18_1 == 0,]
test = test[test$F13 >= 0, ]
median(as.numeric(test$F13))


#test = survey_data[survey_data$F18_4 == 0,]
test = survey_data[survey_data$F10_1 >= 0 & 
                     survey_data$F10_2 >= 0 &
                     survey_data$F10_3 >= 0 &
                     survey_data$F10_4 >= 0 &
                     survey_data$F10_5 >= 0 &
                     survey_data$F10_6 >= 0 &
                     survey_data$F10_7 >= 0 &
                     survey_data$F10_8 >= 0 &
                     survey_data$F10_9 >= 0 &
                     survey_data$F10_10 >= 0 &
                     survey_data$F10_11 >= 0 &
                     survey_data$F10_12 >= 0 
            , ]

freq = c(mean(as.numeric(test$F10_1)), mean(as.numeric(test$F10_2)), 
         mean(as.numeric(test$F10_3)), mean(as.numeric(test$F10_4)),
         mean(as.numeric(test$F10_5)),mean(as.numeric(test$F10_6)),
         mean(as.numeric(test$F10_7)),mean(as.numeric(test$F10_8)),
         mean(as.numeric(test$F10_9)),mean(as.numeric(test$F10_10)),
         mean(as.numeric(test$F10_11)),mean(as.numeric(test$F10_12))
         )

freq2 = c(median(as.numeric(test$F10_1)), median(as.numeric(test$F10_2)), 
          median(as.numeric(test$F10_3)), median(as.numeric(test$F10_4)),
          median(as.numeric(test$F10_5)),median(as.numeric(test$F10_6)),
          median(as.numeric(test$F10_7)),median(as.numeric(test$F10_8)),
          median(as.numeric(test$F10_9)),median(as.numeric(test$F10_10)),
          median(as.numeric(test$F10_11)),median(as.numeric(test$F10_12))
)
max(freq)

#früh und Eltern/Verwandte
test = survey_data[(survey_data$F1 == 5 | survey_data$F1 == 6)  & (survey_data$F2_1 == 1 | survey_data$F2_2 == 1),]

test2 = survey_data[(survey_data$F1 != 5 & survey_data$F1 != 6)  | (survey_data$F2_1 != 1 & survey_data$F2_2 != 1),]

#Frage 3
mean(as.numeric(test$F3))
median(as.numeric(test$F3))
mean(as.numeric(test2$F3))
median(as.numeric(test2$F3))
t.test(as.numeric(test$F3), as.numeric(test2$F3), alternative = "two.sided")

#Frage 16
#Interesse vor Unterricht 
test = test[test$F16_1 >= 0,]
test2 = test2[test2$F16_1 >= 0,]

mean(as.numeric(test$F16_1))
median(as.numeric(test$F16_1))
mean(as.numeric(test2$F16_1))
median(as.numeric(test2$F16_1))
t.test(as.numeric(test$F16_1), as.numeric(test2$F16_1), alternative = "two.sided")

#Interesse während Unterricht
test = test[test$F16_2 >= 0,]
test2 = test2[test2$F16_2 >= 0,]

mean(as.numeric(test$F16_2))
median(as.numeric(test$F16_2))
mean(as.numeric(test2$F16_2))
median(as.numeric(test2$F16_2))
t.test(as.numeric(test$F16_2), as.numeric(test2$F16_2), alternative = "two.sided")

#Interesse nach dem Unterricht 
test = test[test$F16_3 >= 0,]
test2 = test2[test2$F16_3 >= 0,]

mean(as.numeric(test$F16_3))
median(as.numeric(test$F16_3))
mean(as.numeric(test2$F16_3))
median(as.numeric(test2$F16_3))
t.test(as.numeric(test$F16_3), as.numeric(test2$F16_3), alternative = "two.sided")

cgroupnew = logical(length(survey_data$ID))
for(i in 1:length(survey_data$ID)){
  if((survey_data$F1[i] == 5 | survey_data$F1[i] == 6)  & (survey_data$F2_1[i] == 1 | survey_data$F2_2[i] == 1)){
    cgroupnew[i] = TRUE
  }
  else if((survey_data$F1[i] != 5 & survey_data$F1[i] != 6)  | (survey_data$F2_1[i] != 1 & survey_data$F2_2[i] != 1)){
    cgroupnew[i] = FALSE
  } 
}
survey_data = cbind(survey_data, cgroupnew)
data_expanded = survey_data[survey_data$F16_1 >=0,]
interesse_theater <- ggplot(data = data_expanded, aes(x = F16_1,group = cgroupnew, fill = cgroupnew)) + geom_bar(aes(y =..count..), position = "dodge") + xlab("Interesse vor der Behandlung von Theater im Unterricht")+ ylab("Anzahl Teilnehmer*innen")+ scale_x_discrete(labels=c("1" = "Gar kein Interesse", "2" = "Eher wenig Interesse","3" = "Mittelmäßiges Interesse", "4" = "Durchaus Interesse", "5" = "Starkes Interesse")) #(..count..)/sum(..count..)
interesse_theater +scale_fill_manual("Gruppen", values = c("TRUE" = "darkblue", "FALSE" = "lightblue"), label = c("Gruppe spät gefördert","Gruppe früh gefördert"))




#Frage 17 
#regelmäßig Theater
test = test[test$F17_1 >= 0,]
test2 = test2[test2$F17_1 >= 0,]

mean(as.numeric(test$F17_1))
median(as.numeric(test$F17_1))
mean(as.numeric(test2$F17_1))
median(as.numeric(test2$F17_1))
t.test(as.numeric(test$F17_1), as.numeric(test2$F17_1), alternative = "two.sided")

#mehr ins Theater
test = test[test$F17_2 >= 0,]
test2 = test2[test2$F17_2 >= 0,]

mean(as.numeric(test$F17_2))
median(as.numeric(test$F17_2))
mean(as.numeric(test2$F17_2))
median(as.numeric(test2$F17_2))
t.test(as.numeric(test$F17_2), as.numeric(test2$F17_2), alternative = "two.sided")

#selbstständig ins Theater
test = test[test$F17_3 >= 0,]
test2 = test2[test2$F17_3 >= 0,]

mean(as.numeric(test$F17_3))
median(as.numeric(test$F17_3))
mean(as.numeric(test2$F17_3))
median(as.numeric(test2$F17_3))
t.test(as.numeric(test$F17_3), as.numeric(test2$F17_3), alternative = "two.sided")


#Theatertexte gelesen
test = survey_data[survey_data$F18_8 == 1, ]
test = test[test$F14_1 >= 0 & test$F14_2 >= 0 & test$F14_3 >= 0 & test$F14_4 >= 0 & test$F14_5 >= 0 & test$F14_6 >=0,]

freq = c(mean(as.numeric(test$F14_1)), mean(as.numeric(test$F14_2)), 
         mean(as.numeric(test$F14_3)), mean(as.numeric(test$F14_4)),
         mean(as.numeric(test$F14_5)),mean(as.numeric(test$F14_6)))

freq2 = c(median(as.numeric(test$F14_1)), median(as.numeric(test$F14_2)), 
          median(as.numeric(test$F14_3)), median(as.numeric(test$F14_4)),
          median(as.numeric(test$F14_5)),median(as.numeric(test$F14_6))
)

#Frage 11: Fächer

test = survey_data[survey_data$F11_11 >= 0 
                   , ]

mean(as.numeric(test$F11_11)) 
median(as.numeric(test$F11_11))

#Frage 15
test = survey_data[survey_data$F15_10 >= 0, ]

mean(as.numeric(test$F15_10)) 
median(as.numeric(test$F15_10))
prop.table(table(test$F15_8))


#Frage 4 (Theaterförderung) zu Interesse
test= survey_data[survey_data$F4 >= 0 & survey_data$F16_2 >= 0,]
cor(as.numeric(test$F4), as.numeric(test$F16_2))
cor(as.numeric(test$F4), as.numeric(test$F16_2), method ="spearman")

summary(lm(as.numeric(test$F16_2) ~ as.numeric(test$F4)))

test= survey_data[survey_data$F4 >= 0 & survey_data$F16_3 >= 0,]
cor(as.numeric(test$F4), as.numeric(test$F16_3))
cor(as.numeric(test$F4), as.numeric(test$F16_3), method = "spearman")

summary(lm(as.numeric(test$F16_3)~ as.numeric(test$F4)))



#Theater-AG zu Interesse
#F16_1
test = survey_data[survey_data$F6 >0 &survey_data$F16_1 >= 0, ]
test2 = survey_data[survey_data$F6 == 0 &survey_data$F16_1 >= 0,]

mean(as.numeric(test$F16_1))
median(as.numeric(test$F16_1))
mean(as.numeric(test2$F16_1))
median(as.numeric(test2$F16_1))
t.test(as.numeric(test$F16_1), as.numeric(test2$F16_1), alternative = "two.sided")

#16_2
test = survey_data[survey_data$F6 >0 &survey_data$F16_2 >= 0,]
test2 = survey_data[survey_data$F6 == 0&survey_data$F16_2 >= 0,]

mean(as.numeric(test$F16_2))
median(as.numeric(test$F16_2))
mean(as.numeric(test2$F16_2))
median(as.numeric(test2$F16_2))
t.test(as.numeric(test$F16_2), as.numeric(test2$F16_2), alternative = "two.sided")

#F16_3
test = survey_data[survey_data$F6 >0 &survey_data$F16_3 >= 0,]
test2 = survey_data[survey_data$F6 == 0&survey_data$F16_3 >= 0,]

mean(as.numeric(test$F16_3))
median(as.numeric(test$F16_3))
mean(as.numeric(test2$F16_3))
median(as.numeric(test2$F16_3))
t.test(as.numeric(test$F16_3), as.numeric(test2$F16_3), alternative = "two.sided")

#F17_1
test = survey_data[survey_data$F6 >0&survey_data$F17_1>= 0,]
test2 = survey_data[survey_data$F6 == 0&survey_data$F17_1 >= 0,]

mean(as.numeric(test$F17_1))
median(as.numeric(test$F17_1))
mean(as.numeric(test2$F17_1))
median(as.numeric(test2$F17_1))
t.test(as.numeric(test$F17_1), as.numeric(test2$F17_1), alternative = "two.sided")

#F17_2
test = survey_data[survey_data$F6 >0&survey_data$F17_2>= 0,]
test2 = survey_data[survey_data$F6 == 0&survey_data$F17_2>= 0,]

mean(as.numeric(test$F17_2))
median(as.numeric(test$F17_2))
mean(as.numeric(test2$F17_2))
median(as.numeric(test2$F17_2))
t.test(as.numeric(test$F17_2), as.numeric(test2$F17_2), alternative = "two.sided")

#F17_3
test = survey_data[survey_data$F6 >0&survey_data$F17_3>= 0,]
test2 = survey_data[survey_data$F6 == 0&survey_data$F17_3>= 0,]

mean(as.numeric(test$F17_3))
median(as.numeric(test$F17_3))
mean(as.numeric(test2$F17_3))
median(as.numeric(test2$F17_3))
t.test(as.numeric(test$F17_3), as.numeric(test2$F17_3), alternative = "two.sided")

#Frage 6_2 Initiative durch Gymnasium
test = survey_data[survey_data$F2_6 == 1 & survey_data$F17_3 >0 ,]
test2 = survey_data[survey_data$F2_6 == 0 & survey_data$F17_3 >0 ,]

mean(as.numeric(test$F17_1))
median(as.numeric(test$F17_1))
mean(as.numeric(test2$F17_1))
median(as.numeric(test2$F17_1))
t.test(as.numeric(test$F17_1), as.numeric(test2$F17_1), alternative = "two.sided")

mean(as.numeric(test$F17_2))
median(as.numeric(test$F17_2))
mean(as.numeric(test2$F17_2))
median(as.numeric(test2$F17_2))
t.test(as.numeric(test$F17_2), as.numeric(test2$F17_2), alternative = "two.sided")

mean(as.numeric(test$F17_3))
median(as.numeric(test$F17_3))
mean(as.numeric(test2$F17_3))
median(as.numeric(test2$F17_3))
t.test(as.numeric(test$F17_3), as.numeric(test2$F17_3), alternative = "two.sided")

#Grundschule 
test = survey_data[survey_data$F12 == 0 & survey_data$F17_3 > 0, ]
test2 = survey_data[survey_data$F12 == 1 & survey_data$F17_3 > 0, ]

mean(as.numeric(test$F17_1))
median(as.numeric(test$F17_1))
mean(as.numeric(test2$F17_1))
median(as.numeric(test2$F17_1))
t.test(as.numeric(test$F17_1), as.numeric(test2$F17_1), alternative = "two.sided")

mean(as.numeric(test$F17_2))
median(as.numeric(test$F17_2))
mean(as.numeric(test2$F17_2))
median(as.numeric(test2$F17_2))
t.test(as.numeric(test$F17_2), as.numeric(test2$F17_2), alternative = "two.sided")

mean(as.numeric(test$F17_3))
median(as.numeric(test$F17_3))
mean(as.numeric(test2$F17_3))
median(as.numeric(test2$F17_3))
t.test(as.numeric(test$F17_3), as.numeric(test2$F17_3), alternative = "two.sided")

#Frage 8: Theaterbesuche 

test = survey_data[survey_data$F8 >= 0 & survey_data$F17_1 > 0,]

box_theater <- ggplot(data = test, aes(x = F8, y = F17_1, group = F8, fill = F8)) + geom_boxplot(aes(fill = F8))+ labs(title="Regelmäßgkeit der Theaterbesuche", x="Häufigkeit der Theaterbesuche im Gymnasium", y="Regelmäßigkeit der Theaterbesuche") #,geom bar x = (..count..)/sum(..count..))
bars_theater <- ggplot(data = test, aes(y = F17_1)) + geom_bar(aes(fill = F8))+ xlab("Anzahl an Teilnehmer*innen") + ylab("'Ich gehe regelmäßig ins Theater'") + scale_y_discrete(labels=c("1" = "Stimme gar nicht zu", "2" = "Stimme eher nicht zu","3" = "Stimme teils-teils zu", "4" = "Stimme eher zu", "5" = "Stimme voll zu"))
bars_theater +coord_flip() + labs(fill = "Theaterbesuch mit Gymnasium") + scale_fill_discrete(labels = c("Nein", "Ja, aber nicht jedes Schuljahr", "Ja, etwa einmal im Schuljahr", "Ja, mehrmals im Schuljahr"))

#Frage 4 zu Frage 8
test = survey_data[(survey_data$F4 == 4 | survey_data$F4 == 5) & survey_data$F8 >= 0 ,]
test2 = survey_data[survey_data$F4 < 4 & survey_data$F4 > 0 & survey_data$F8 >= 0 ,]

mean(as.numeric(test$F8))
median(as.numeric(test$F8))
mean(as.numeric(test2$F8))
median(as.numeric(test2$F8))
t.test(as.numeric(test$F8), as.numeric(test2$F8), alternative = "two.sided")

prop.table(table(test$F8))
prop.table(table(test2$F8))
