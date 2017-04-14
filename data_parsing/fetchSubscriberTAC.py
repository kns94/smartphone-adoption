import os, sys
import sqlite3
import datetime
import argparse

class fetchSubscriberTAC:

    def __init__(self, hlr, output_directory):

        self.hlr_conn = sqlite3.connect(hlr, timeout = 100)
        self.hlr_cur = self.hlr_conn.cursor()
        self.output_directory = output_directory

        #Create a directory where the data would be saved, we'll think about uploading the data later on
        if not os.path.exists(output_directory):
            os.makedirs(output_directory)

    def fetchTAC(self):
        """Get TAC numbers from phones connected on the last day to present"""

        last_hour = datetime.datetime.now().date() - datetime.timedelta(hours = 1)
        last_hour = "{}{}{}".format("'", last_hour, "'")
        last_hour = datetime.date(2011, 4, 5)

        self.hlr_cur.execute("SELECT id FROM Subscriber WHERE updated >= {date};".format(date = last_hour))
        subscribers = self.hlr_cur.fetchall()

        parsed_data = {}
        unique_imei = {}
        #uid_count = 0

        for subscriber in subscribers:
            self.hlr_cur.execute("SELECT IMEI FROM Equipment WHERE id = (SELECT equipment_id FROM EquipmentWatch WHERE subscriber_id = {s_id});".format(s_id = subscriber[0]))
            parsed_imei = self.hlr_cur.fetchall()

            if len(parsed_imei) > 0:
                for imei in parsed_imei:
                    imei_number = imei[0] 

                    if imei_number not in unique_imei:
                        unique_imei[imei_number] = subscriber[0]

                    uid = unique_imei[imei_number]
                    parsed_data.setdefault((uid), str(imei_number)[:8])

        self.saveRecords(parsed_data)

    def saveRecords(self, parsed):
        """Save the dictionary"""
        
        date_today = int(datetime.datetime.now().timestamp())
        output_file_complete_path = "{}/{}{}".format(self.output_directory, date_today, '.log')
        out = open(output_file_complete_path, 'w+')

        for key, value in parsed.iteritems():
            line = "{},{},{}".format(key, value,'\n')
            out.write(line)
        out.close()

class preProcess:

    def importToSQLITE(self, db_file, sqlite_db_name):
        """Import a database to SQLITE3"""

        command = "{} {} {} {}".format('cat', db_file, '| sqlite3', sqlite_db_name)
        call(command, shell = True)

def main():

        parser = argparse.ArgumentParser()
        parser.add_argument("--sqlite_dump", "-s", help = "HLR SQLITE dump")
        parser.add_argument("--ccn_hlr", "-c", help = "HLR SQLITE3 Database")
        parser.add_argument("--output_dir", "-o", help = "Output directory to save records")
        parser.add_argument("--file_dir", "-f", help = "Current directory where the Python file exists")
        args = parser.parse_args()

        if args.file_dir:
            current_dir = args.file_dir
        else:
            current_dir = '.'

        if args.ccn_hlr:
            hlr = "{}/{}".format(current_dir, args.ccn_hlr)
        else:
            hlr = "{}/{}".format(current_dir, args.ccn_hlr)
            if args.sqlite_dump:
                hlr_dump = "{}/{}".format(current_dir, args.sqlite_dump)
                preProcess().importToSQLITE(hlr_dump, hlr)
            else:
                sys.exit("Please enter either the SQLITE3 dump or the database file")

        if args.output_dir:
            output_directory = "{}/{}".format(current_dir, args.output_dir)
        else:
            output_directory = "{}/{}".format(current_dir, "tac_records")

        crawler = fetchSubscriberTAC(hlr, output_directory)
        crawler.fetchTAC()

if __name__ == "__main__":
    main()