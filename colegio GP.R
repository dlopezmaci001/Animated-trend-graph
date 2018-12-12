
library(scales)
library(caret)

df <- read.csv2("C:/Users/Daniel/Desktop/Spooon/Colegio GP.csv",sep=",")

df$school <- NULL

model<- train(G3 ~ ., data=df, method="lm")

summary(model)


# Reescalse results

df$G1 <- rescale(df$G1,to=c(0,10))

df$G2 <- rescale(df$G2,to=c(0,10))

df$G3 <- rescale(df$G3,to=c(0,10))

setwd("C:/Users/Daniel/Desktop/Spooon")
write.csv2(df,file="ColegioGP.csv",sep=";")

df <- df[1:300,]

# importance

varImp(model,scale = FALSE)

model_1 <- train(G3  ~ G2
                 + failures
                 + age
                 + HigherEdY
                 + goout             
                 + MjobHealth          
                 + MjobServices        
                 + G1                  
                 + MjobTeacher         
                 + FjobHome            
                 + traveltime          
                 + Gender.F            
                 + NurserySchoolY      
                 + Famsize.small       
                 + famrel              
                 + SchoolSupY          
                 + FjobServices        
                 + SchoolChooseHome    
                 + SchoolChooseCourse  
                 + ActivityYes, data=df,method='lm',tuneGrid=expand.grid(intercept=FALSE))


summary(model_1)

setwd("C:/Users/Daniel/Desktop/Spooon")
write.csv2(df,file="ColegioGP.csv",sep=";")
getwd()

######################### Plot ##############################################################


dfplot <- read.csv2("C:/Users/Daniel/Desktop/Spooon/notascsv.csv",sep=";")

dfplot <- dfplot[,1:3]

dfplot$Predicted.Grade <- as.factor(dfplot$Predicted.Grade)

ggplot(dfplot,aes(ChildId))+
  geom_line(aes(y=Pred.Grade,colour="Orange"))+
  geom_line(aes(y=Actual.Grade,colour="Blue"))

# Animation

# Set path of Rtools
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")
library(devtools)

#Manually "force" version to be accepted 
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
find_rtools() # is TRUE now

# Now you can install transformr then gganimate
devtools::install_github("thomasp85/transformr")

library(gganimate)
library(transformr)
library(RColorBrewer)

ggplot(dfplot,aes(x=ChildId,
                            y=Pred.Grade,
                            colour= Predicted.Grade))+
  geom_line(size=1.2)+
  transition_reveal(Predicted.Grade,ChildId)+
  labs(title = "Análisis Predictivo de las notas obtenidas por alumnos de secundaria",
       subtitle = "Comparativa de las notas obtenidas vs. predicciones del modelo",
       x= "ChildId",
       y= "Nota",
       caption= "© Spooon Insights 2018",
       colour="Predicted.Grade") +
  scale_color_discrete(labels= c("Nota calculada", 
                                 "Nota conseguida"))+

  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


anim_save("Evolucion_Notas.gif")
