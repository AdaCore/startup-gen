# Test if the update process works
# We expect to update the package.

DATABASE=test_bdb
TEST=`basename $0`
TEST=${TEST%.*}
LOG=$TEST.log

./init.sh $DATABASE
{ # Redirect all output to the log file.
	querytool setup $DATABASE
} > $LOG 2>&1

RET=$(cat $LOG | tail -1) # Get return value
case $RET in
    1) echo PASSED $TEST : tests results are in $LOG ;;
    *) echo FAILED $TEST : tests results are in $LOG ;;
esac
