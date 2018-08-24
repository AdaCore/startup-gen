# Test if the update process works
# We expect to update the package.

DATABASE=test_bdb
TEST=`basename $0`
TEST=${TEST%.*}
LOG=$TEST.log

./init.sh $DATABASE

echo "Adding..." > $LOG
querytool add_package Keil.STM32F4xx_DFP.2.12.0.pack $DATABASE >> $LOG
echo "Removing unzipped directory..." >> $LOG
rm -rf .tmp/Keil.STM32F4xx_DFP.2.12.0 >> $LOG
echo "Updating..." >> $LOG
querytool update_package Keil.STM32F4xx_DFP $DATABASE >> $LOG
RET=$(cat $LOG | tail -1)
case $RET in
    1) echo PASSED $TEST : tests results are in $LOG ;;
    *) echo FAILED $TEST : tests results are in $LOG ;;
esac

