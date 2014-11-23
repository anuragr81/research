
echo "filename,companyname,exchange,ticker" > /tmp/tickers.$$

ls *output*csv | while read filename ;
do
 csventry=`echo $filename | sed -e 's/_Financials.*//' | sed -e 's/_NYSE_/,NYSE,/' | sed -e 's/_NasdaqGS_/,NasdaqGS,/'`
 echo $filename,$csventry >> /tmp/out.$$
done

awk -F, '{print "^",$4,"_";}' /tmp/tickers.$$ | sed -e 's/\ //g' | grep -v ticker | while read i ; do ls allequities/ | grep -E $i ; if [ $? -ne 0 ]; then echo "NOT FOUND: $i" ; fi; done

rm /tmp/tickers.$$
