import os, sys
import pandas as pd

mapping = pd.read_csv(sys.argv[2], sep = '|')

unique_tac = {}
tac_not_found = {}
mname_not_found = {}
manufacturer_not_found = {}
threeG = {}
fourG = {}

for files in os.listdir(sys.argv[1]):

    file = "{}\{}".format(sys.argv[1], files)

    if '.log' in file:

        header = True
        for line in open(file, 'r'):

            if not header:
                imei = line.strip().split(',')[1]

                try:
                    tac = int(str(imei)[:8])

                    if tac not in unique_tac:
                        unique_tac[tac] =  True

                        if tac not in tac_not_found:
                            if tac in list(mapping['TAC']):

                                sub = mapping[mapping['TAC'] == tac]

                                mname = sub['Marketing Name']
                                if not mname.get_value(mname.index[0]):
                                    mname_not_found[tac] = True

                                manufacturer = sub['Manufacturer (or) Applicant']
                                if not manufacturer.get_value(manufacturer.index[0]):
                                    manufacturer_not_found[tac] = True

                                band = sub['Band']
                                if 'LTE' in band.get_value(band.index[0]):
                                    fourG[tac] = True
                                    threeG[tac] = True
                                else:
                                    band_val = band.get_value(band.index[0])
                                    if 'UMTS' in band_val or 'HSDPA' in band_val or 'HSUPA' in band_val:
                                        threeG[tac] = True
                            else:
                                tac_not_found[tac] = True
                except ValueError:
                    pass
            else:
                header = False

print(len(unique_tac.keys()))
print(len(tac_not_found.keys()))
print(len(mname_not_found.keys()))
print(len(manufacturer_not_found.keys()))
print(len(fourG.keys()))
print(len(threeG.keys()))