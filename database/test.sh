BDB=bdb
sudo rm -rf .tmp/*

python setup.py build
python setup.py install

rm $BDB
querytool init $BDB

for file in temporary/cmsis_packs/*.pack
do
        echo "Adding $file to database"
        querytool add_package ./$file $BDB
done

## We update all packages forcefully.
#querybdb add_package Keil.SAMD20_DFP.1.1.1.pack $BDB

#querytool add_package temporary/cmsis_packs/AmbiqMicro.Apollo_DFP.1.0.0.pack $BDB

#querytool add_package LAPISSemiconductor.ML630Q46x_DFP.0.9.0.pack $BDB
#querytool update_package LAPISSemiconductor.ML630Q46x_DFP $BDB

#querytool add_package Keil.SAMD20_DFP.1.1.1.pack $BDB
#querytool update_package Keil.SAMD20_DFP $BDB

#querytool add_package Keil.STM32F4xx_DFP.2.11.0.pack $BDB
#querytool update_package Keil.STM32F4xx_DFP $BDB

##querybdb packages $BDB
#querybdb add_package temporary/cmsis_packs/Zilog.ZNEO32_DFP.1.0.4.pack $BDB
#querybdb add_package LAPISSemiconductor.ML630Q46x_DFP.1.0.0.pack $BDB
##querybdb packages $BDB
#querybdb add_package NXP.MKE15Z7_DFP.10.0.1.pack $BDB


#querybdb packages $BDB
#querybdb devices $BDB
