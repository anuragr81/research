
#if [ -z "$1" ]  ; then
	#echo "Need directory to create as argument." 
	#exit 1
#fi

#TGTDIR=$1
TGTDIR=/tmp/test.$$
if [ -d $TGTDIR ]; then 
	echo "Already exists: $TGTDIR (rename or move)."
	exit 2
fi

mkdir $TGTDIR

#All copies
cp application.py $TGTDIR/application.py
cp .gitignore $TGTDIR/.gitignore
cp requirements.txt $TGTDIR/requirements.txt
cp index.html $TGTDIR/index.html

curdir=`pwd`
cd $TGTDIR
zip archive_flask_app `find . -type f`
if [ $? -eq 0 ] ; then
  mv archive_flask_app.zip $curdir/
  cd $curdir
  rm -fr $TGTDIR
else
  cd $curdir
fi

