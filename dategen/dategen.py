
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
    wkdts = populate_dates( time_ranges = [ "08:30-10:00" , "17:00-19:00","19:00-21:00" , "21:00-22:30" ], start_date = d(2020,12,25), offset=10, weekend_time_ranges = [ "08:30-10:00" , "10:00-13:00","13:00-17:00", "17:00-19:00","19:00-21:00" , "21:00-22:30" ])
    for x in wkdts:
        print(x)
