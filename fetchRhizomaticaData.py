""" Extract TAC Ids from Rhizomatica's database """

import os, sys
import psycopg2
import sqlite3
from subprocess import call
from datetime import datetime
import pandas as pd
import argparse

class fetchRhizomaticaData:

    def __init__(self, connection_arguments, rhizo_db, cdr_count_file, output_directory):
        """Initializing Databases. The Rhizomatica database which is in sqlite3 is refered to as rhizo_db and 
        the base station database in postgres is referred to as bst_db"""
        
        bst_connection_query = "{}{} {}{} {}{} {}{}".format("user=", connection_arguments["user_name"], "host=",\
            connection_arguments["host"], "password=", connection_arguments["password"], "port=", connection_arguments["port"])

        self.bst_conn = psycopg2.connect(bst_connection_query)
        self.bst_cur = self.bst_conn.cursor()

        #Enter database name of the sqlitedatabase
        self.rhizo_conn = sqlite3.connect(rhizo_db, timeout = 100)
        self.rhizo_cur = self.rhizo_conn.cursor()

        self.cdr_count_file = cdr_count_file
        self.output_directory = output_directory

        #Create a directory where the data would be saved, we'll think about uploading the data later on
        if not os.path.exists(output_directory):
            os.makedirs(output_directory)


    def getCDRCount(self):
        """A file containing last CDR count is saved, if the file is not found - get all data from scratch"""
    
        #files_directory = os.listdir(self.output_directory)
        files_directory = self.output_directory
        cdr_count_file_complete_path = "{}/{}".format(self.output_directory, self.cdr_count_file)
        
        if self.cdr_count_file in files_directory:
            with open(cdr_count_file_complete_path, 'r') as f:
                count = f.readline().strip().split('\n')[0]

                if count != '':
                    return int(count)
        
        return -1


    def getSubcriberId(self):
        """STEP 1. Get MSISDN from CDR file and join it with the subscriber table. [Get subscriber id, date] """

        since_Id = self.getCDRCount()

        #Well you have to hardcode something!
        query_parse_cdr = """SELECT cdr.id, subscribers.id, cdr.start_stamp from subscribers\
                INNER JOIN cdr ON cdr.caller_id_number = subscribers.msisdn;"""

        if since_Id != -1:
            query_parse_cdr = "{} {} {}".format(query_parse_cdr[ : -1], 'AND cdr.id >', since_Id)

        self.bst_cur.execute(query_parse_cdr)
        fetched_data = self.bst_cur.fetchall()

        if len(fetched_data) != 0:
            last_cdr_id = int(fetched_data[-1][0])
        else:
            last_cdr_id = since_Id
        
        subscriber_id = []

        for data in fetched_data:
            s_id = data[1]
            date = data[2].strftime('%m_%d_%Y')
            subscriber_id.append((s_id, date))

        return list(set(subscriber_id)), last_cdr_id
        

    def fetchTAC(self):
        """STEP 2. Parse the subscriber id to the equipment watch table and get equipment id
           STEP 3. Get IMEI from equipment table
           STEP 4. Save TAC to file with unique id and date
           STEP 5 save update cdr count to file"""

        subscriber_id, last_cdr_id = self.getSubcriberId()
        #parsed_data = pd.DataFrame(data=None, index=None, columns=['uid', 'tac', 'date'], dtype=None)
        parsed_data = {}
        unique_imei = {}
        uid_count = 0

        for data in subscriber_id:
            self.rhizo_cur.execute("SELECT IMEI FROM Equipment WHERE id = (SELECT equipment_id FROM EquipmentWatch WHERE subscriber_id = {s_id});".format(s_id = data[0]))
            parsed_imei = self.rhizo_cur.fetchall()

            if len(parsed_imei) > 0:
                for imei in parsed_imei:
                    imei_number = imei[0] 

                    if imei_number not in unique_imei:
                        uid_count += 1
                        unique_imei[imei_number] = uid_count

                    uid = unique_imei[imei_number]
                    parsed_data.setdefault((data[1], uid), str(imei_number)[:8])
                    
                    #parsed_data.loc[-1] = str(uid), str(imei_number)[:8], data[1]
                    #parsed_data.index = parsed_data.index + 1

        self.saveRecords(parsed_data, last_cdr_id)

    def saveRecords(self, parsed, last_cdr_id):
        """Save the dictionary and update the cdr count"""
        
        date_today = datetime.now().strftime('%d_%m_%Y')
        output_file_complete_path = "{}/{}{}".format(self.output_directory, date_today, '.csv')
        out = open(output_file_complete_path, 'w+')

        for key, value in parsed.iteritems():
            line = "{},{},{},{}".format(key[0], key[1], value,'\n')
            out.write(line)
        out.close()

        #Save the latest count
        cdr_count_file_complete_path = "{}/{}".format(self.output_directory, self.cdr_count_file)
        cdr_file = open(cdr_count_file_complete_path, 'w') 
        cdr_file.write(str(last_cdr_id))
        cdr_file.close()

class preProcess:

    def importToSQLITE(self, db_file, sqlite_db_name):
        """Import a database to SQLITE3"""

        command = "{} {} {} {}".format('cat', db_file, '| sqlite3', sqlite_db_name)
        call(command, shell = True)



def main():

        parser = argparse.ArgumentParser()
        parser.add_argument("--dump_file", "-d", help = "SQLITE dump file")
        parser.add_argument("--ccn_db", "-c", help = "SQLITE database of CCN")
        parser.add_argument("--output_dir", "-o", help = "Output directory to save records")
        parser.add_argument("--file_dir", "-f", help = "Current directory where the Python file exists")
        args = parser.parse_args()

        if args.file_dir:
            current_dir = args.file_dir
        else:
            current_dir = '.'

        user_name = 'postgres' #Username used to authenticate
        host = 'localhost' #database host address
        password = 'p' #password to authenticate
        port = 5432 #connection port number
        bst_connection_arguments = {"user_name": user_name, "host": host, "password": password, "port": port}

        if args.ccn_db:
            rhizo_db = "{}/{}".format(current_dir, args.ccn_db)
        else:
            rhizo_db = "{}/{}".format(current_dir, 'ccn_db')
            if args.dump_file:
                rhizo_db_file = "{}/{}".format(current_dir, args.dump_file)
                preProcess().importToSQLITE(rhizo_db_file, rhizo_db)
            else:
                sys.exit("Please enter either the SQLITE3 dump or the database file")

        cdr_count_file = 'cdrCount.txt'

        if args.output_dir:
            output_directory = "{}/{}".format(current_dir, args.output_dir)
        else:
            output_directory = "{}/{}".format(current_dir, "tac_records")

        crawler = fetchRhizomaticaData(bst_connection_arguments, rhizo_db, cdr_count_file, output_directory)
        crawler.fetchTAC()

if __name__ == "__main__":
    main()



