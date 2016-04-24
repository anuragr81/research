i=10
n=20
while [[ $i -lt $n ]];
do
   echo "s= rbind(s,data.frame(iesname=\"b20f0$i\" ,name=\"eggs\"))"
   i=`expr $i + 1`
done

