import requests
import pandas as pd
import csv

def get_block(input):
    (lat, lon) = (input[1], input[0])
    paramters = {"lat": lat, "lon":lon,"format":'json'}
    response = requests.get("https://geo.fcc.gov/api/census/area", params = paramters).json()
    results = response.get("results")
    res_dict = results[0]
    block_fips = res_dict.get("block_fips")
    return block_fips

count = 0
headers = []
with open("block_tree.csv", 'w') as new:
    writer = csv.writer(new)
    with open("tree_data") as raw:
        tree_reader = csv.reader(raw)
        
        for row in tree_reader:
            if (count == 0):
                headers = row
                headers.append("block_fips")
                count+=1
                writer.writerow(headers)
                continue
            new_line = row
            new_line.append(get_block(row))
            writer.writerow(new_line)
        raw.close()
    new.close()


            


def parse_line(str):
    # take a line, and get the number under lat and long
    # Point x = lon, point y = lat
    # Lon is the first entry, lat is second

    strs = str.split(',')

    return (strs[1], strs[0])



