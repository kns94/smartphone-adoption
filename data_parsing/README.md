# smartphone-adoption

Parsing call data records to fetch TAC records per day. 

The script assumes following four parameters:

  1. -s: The SQLITE3 dump file which is to be imported to SQLITE database (optional)
  2. -c: SQLITE3 database
  3. -o: Output directory to save records
  4. -f: Current directory where the code resides.

Usage:

* If you want to set up a cronjob:

  * python Crontab.py **SQLITE_DB** **OUTPUT_DIR** **HOUR** **MIN**
  
* If you want to run the script directly:
 
  * python fetchSubscriberTAC.py -c **SQLITE_DB** -o **OUTPUT_DIR** -f **FILE_DIR**
  
Next up:

  Ways to upload the records to a local server.
