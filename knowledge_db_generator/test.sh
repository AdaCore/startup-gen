BDB=bdb
rm -rf .tmp/*

python setup.py build
python setup.py install

rm $BDB
modifybdb init $BDB

#for file in cmsis_packs/*.pack
#do
#        echo "Adding $file to database"
#        modifybdb add_package ./$file $BDB
#done

# We update all packages forcefully.
#querydb update --force all

#modifybdb add_package Keil.SAMD20_DFP.1.1.1.pack $BDB
#modifybdb add_package Keil.STM32F4xx_DFP.2.13.0.pack $BDB
modifybdb add_package LAPISSemiconductor.ML630Q46x_DFP.1.0.0.pack $BDB
#modifybdb add_package NXP.MKE15Z7_DFP.10.0.1.pack $BDB


querybdb packages $BDB
querybdb devices $BDB

du -h $BDB
