
#___________ Guardar matrices en excel _______

#-------------------------
#       Paquetes
#------------------------

install.packages("xlsx")
library(xlsx)

# ------------------------
#  Descargar matriz en R
#---------------------------

BD<-datos::flores

#----------------------------
#    Crear archivo. csv
#----------------------------

# Matriz completa
write.csv(BD, file= "flores.csv")

# Fragmento de la matriz
write.csv(BD[1:50,], file="flores_set.csv")


