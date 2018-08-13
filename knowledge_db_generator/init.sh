# Cleanup the working dir and initialize a new database

DB=$1
sudo rm -rf .tmp/*

python setup.py build
python setup.py install

cp ./querytool.py $HOME/ancr/src/libadalang_for_gps/build/python/querytool.py

rm $DB
querytool init $DB
