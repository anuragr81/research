
from itertools import product
from datetime import date as d
from datetime import timedelta as td
from functools import reduce



def populate_dates(start_date,offset,time_ranges):
    dates =[start_date+td(days=i) for i in range(0,offset+1) ]
    weekdates = [ dt.strftime("%Y-%m-%d") for dt in dates if dt.weekday() not in (5,6) ] 
    weekdates_times = [ x+", " + y for x,y in product(weekdates,time_ranges) ]
    return weekdates_times


if __name__ == "__main__":
    wkdts = populate_dates( time_ranges = [ "10:00-14:30" , "16:00-18:00" ], start_date = d(2020,11,16), offset=18) 
    for x in wkdts:
        print(x)
