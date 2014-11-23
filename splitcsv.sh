if [ -z "$1" ]; then
  echo "Usage : $0 <file>"
  exit 1
fi
inputfile=$1;

count=1;

# Searches first line has "US Equity" 
python get_symbols.py $inputfile > /tmp/files.$$
sz=`wc -l $inputfile`
tailsz=`exprt $sz - 1`


cat /tmp/files.$$ | while read i;
do 
   echo "FILE:$i";
   echo "{print \$$count;}" > /tmp/tt.$$
   #cat /tmp/tt.$$
   awk -f /tmp/tt.$$ -F,, $inputfile | sed -e '1d'
   rm /tmp/tt.$$
   count=`expr $count + 1`
done
#rm /tmp/files.$$
