
from itertools import product
from datetime import date as d
from datetime import timedelta as td
from functools import reduce



def populate_dates(start_date,offset,time_ranges, weekend_time_ranges=None):
    dates =[start_date+td(days=i) for i in range(0,offset+1) ]
    weekdates = [ dt.strftime("%Y-%m-%d") for dt in dates if dt.weekday() not in (5,6) ] 
    

    week_times = [ x+", " + y for x,y in product(weekdates,time_ranges) ]
    if weekend_time_ranges:

    	weekend_dates = [ dt.strftime("%Y-%m-%d") for dt in dates if dt.weekday() in (5,6) ]
    	week_times = week_times + [ x+", " + y for x,y in product(weekend_dates,weekend_time_ranges) ]
    return week_times


if __name__ == "__main__":
    wkdts = populate_dates( time_ranges = [ "10:00-11:30","11:30-13:00", "13:00-14:30", "16:00-17:00" ], start_date = d(2022,10,31), offset=16)
    for x in wkdts:
        print(x)
