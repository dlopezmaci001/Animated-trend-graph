

######################### Plot ##############################################################


dfplot <- read.csv2('YOUR PATH'/notascsv.csv",sep=";")

dfplot <- dfplot[,1:3]

dfplot$Predicted.Grade <- as.factor(dfplot$Predicted.Grade)

ggplot(dfplot,aes(ChildId))+
  geom_line(aes(y=Pred.Grade,colour="Orange"))+
  geom_line(aes(y=Actual.Grade,colour="Blue"))

# Animation
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
       colour="Predicted.Grade") +
  scale_color_discrete(labels= c("Nota calculada", 
                                 "Nota conseguida"))+

  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


anim_save("Evolucion_Notas.gif")
