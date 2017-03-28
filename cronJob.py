"""Creating a cron job to parse through database everyday"""

import os, sys
from crontab import CronTab

class cronJob:

    def __init__(self):
        #Programmer should have root access
        self.cron = CronTab(user = True)

    def add_daily(self, command, hour, minute):
        """Scheduling the function to run the job every day"""

        cron_job = self.cron.new(command = command)
        cron_job.minute.on(int(minute))
        cron_job.hour.on(int(hour))
        cron_job.enable()
        self.cron.write_to_user(user = True)

        if self.cron.render():
            print self.cron.render()
            return True


def main(ccn_db, output_dir, hour, minute):
    """Commands to set up a cron job"""


    python_path = '/usr/bin/python2.7' #Set your python path 
    file_path = "{}/{}".format(os.getcwd(), 'fetchSubscriberTAC.py') #The fetchSubscriberTAC.py file should be in the same directory as this cron scheduler

    args = "{} {} {} {} {} {}".format("-f", os.getcwd(), "-c", ccn_db, "-o", output_dir) #Set you command line arguments accordingly
    command = "{} {} {}".format(python_path, file_path, args)

    cronJob().add_daily(command, int(hour), int(minute))

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
