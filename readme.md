# sph-upcoming
define events in time and display upcoming ones

# configuration file
## example
line format

    time id options ...

supported daytime formats are hour:minute:seconds and kiloseconds of the day (default). the supported calendar date format is the iso date format yyyy-mm-dd. all times are currently utc+0 but there will be an option for local time soon

```
28.8 meeting weekday 1
24 eat interval 10 duration 0.6
"20:00" sleep
22 work end 55 weekday (1 2 3 4)
"00:00" day end "24:00"
42 each-second-day interval 2 interval-unit day
"2017-12-13" test title "an event"
"2017-12-13 28.8" test2 end "2017-12-14 8:00:12" title "another event"
(unquote uptime-start) variable-test end (unquote (+ 2000 uptime-start))
84 special-day depends (and (not work) (or test test2))
```

many kinds of event series can be defined. multiple definitions with the same id are treated as additional occurrences of the same event and merged

## syntax
```
line/event: time id option-name/option-value ...
time-date: string:"yyyy-mm-dd"
time-day: integer:day-ks/string:"hh[:mm[:ss]]"
time: time-day/time-date/"time-date time-day"/id
id: symbol
```

the syntax is scheme syntax, lines can be commented with ";"

```
;42 each-second-day interval 2 interval-unit day
```

## options
```
end: time
duration: integer
interval: integer
interval-unit: seconds/id/(id ...)
weekday: integer/(integer ...)
title: string
depends: id/(id ...)/(or/and/not depends ...)
start-base: id/(id ...)
```

# setup
## dependencies
* guile >= 2
* sph-lib

## install
```
./exe/install
```

see ./exe/install --help for more options

# license
gpl3+

# usage
create a configuration file with the path "$HOME/.config/sph/upcoming" and define your events in there, each line an event.
then start a server with \"upcoming --server\". once the server is running, use \"upcoming\" and other options to query events

```
$ upcoming --help
parameters
  options ... event-ids ...
description
  filter events in time with event definitions from a configuration file.
  first start a server with "upcoming --server". if the server is running, use "upcoming" and other options to query events.
  event definitions are loaded from the configuration file "/home/nonroot/.config/sph/upcoming"
options
  --about | -a
  --active[=integer] | -c [integer]  select up to n active events
  --config=string  use a different configuration file for the server
  --format=value  ks, hms, data-space, data-scm, data-csv
  --help | -h
  --interface
  --limit=integer  include at most n repetitions of distinct event ids
  --next[=integer] | -n [integer]  select up to n future events
  --previous[=integer] | -p [integer]  select up to n past events
  --server | -s  start a server that answers event queries
```

by default active events and the next event are shown

## display format
upcoming displays a table with event information. the table can be parsed easily by other programs to select relevant information

columns
```
active: > past, * active, < future event
start-diff: the time until or since the start of the event
end-diff: the time until or since the end of the event
duration
start: the date and time when the event starts. if no date is displayed it means the current day
end
id: event id
```

```
upcoming --next=4 --previous=2 --limit=1 --format=ks
```

### ks (default)
```
active diff-start diff-end duration start end id
> -9.03 -8.43 0.6 54 54 eat
> -21.03 -20.83 0.2 42 42 each-second-day
* -218.82 1781.17 2000 2017-09-20:17 2017-10-13:29 variable-test
* -63.03 23.36 86.39 0 86 day
< 0.96 1.56 0.6 64 64 eat
< 8.96 9.16 0.2 72 72 sleep
< 23.36 109.76 86.39 2017-09-23:0 2017-09-23:86 day
< 40.37 2040.37 2000 2017-09-23:17 2017-10-16:29 variable-test
```

### hms
```
active diff-start diff-end duration start end id
> -02:31:25 -02:21:25 00:10:00 15:00:00 15:10:00 eat
> -05:51:25 -05:48:05 00:03:20 11:40:00 11:43:20 each-second-day
* -60:47:59 494:45:21 555:33:20 2017-09-20:04:43:26 2017-10-13:08:16:46 variable-test
* -17:31:25 06:28:34 23:59:59 00:00:00 23:59:59 day
< 00:15:15 00:25:15 00:10:00 17:46:40 17:56:40 eat
< 02:28:35 02:31:55 00:03:20 20:00:00 20:03:20 sleep
< 06:28:35 30:28:34 23:59:59 2017-09-23:00:00:00 2017-09-23:23:59:59 day
< 11:12:01 566:45:21 555:33:20 2017-09-23:04:43:26 2017-10-16:08:16:46 variable-test
```

### scm
```
#(-1 -9100 -8500 600 1506092400 1506093000 eat ((id . eat) (start . 24) (interval . 10) (duration . 0.6)))
#(-1 -21100 -20900 200 1506080400 1506080600 each-second-day ((id . each-second-day) (start . 42) (interval . 2) (interval-unit . day)))
#(0 -218894 1781106 2000000 1505882606 1507882606 variable-test ((id . variable-test) (start . 17.006610000000002) (end . 2017.00661)))
#(0 -63100 23299 86399 1506038400 1506124799 day ((id . day) (start . "00:00") (end . "23:59:59")))
#(1 900 1500 600 1506102400 1506103000 eat ((id . eat) (start . 24) (interval . 10) (duration . 0.6)))
#(1 8900 9100 200 1506110400 1506110600 sleep ((id . sleep) (start . "20:00")))
#(1 23300 109699 86399 1506124800 1506211199 day ((id . day) (start . "00:00") (end . "23:59:59")))
#(1 40306 2040306 2000000 1506141806 1508141806 variable-test ((id . variable-test) (start . 17.006610000000002) (end . 2017.00661)))
```

### csv
```
-1,-9111,-8511,600,1506092400,1506093000,"eat","((id . eat) (start . 24) (interval . 10) (duration . 0.6))"
-1,-21111,-20911,200,1506080400,1506080600,"each-second-day","((id . each-second-day) (start . 42) (interval . 2) (interval-unit . day))"
0,-218905,1781095,2000000,1505882606,1507882606,"variable-test","((id . variable-test) (start . 17.006610000000002) (end . 2017.00661))"
0,-63111,23288,86399,1506038400,1506124799,"day","((id . day) (start . 00:00) (end . 23:59:59))"
1,889,1489,600,1506102400,1506103000,"eat","((id . eat) (start . 24) (interval . 10) (duration . 0.6))"
1,8889,9089,200,1506110400,1506110600,"sleep","((id . sleep) (start . 20:00))"
1,23289,109688,86399,1506124800,1506211199,"day","((id . day) (start . 00:00) (end . 23:59:59))"
1,40295,2040295,2000000,1506141806,1508141806,"variable-test","((id . variable-test) (start . 17.006610000000002) (end . 2017.00661))"
```

# possible enhancements
* localtime
* custom strptime time format
* "blocks" option. use case "vacation blocks work"

# implementation
event functions return a list of event objects for a time span relative to a given time

```
f(time, offset-start, offset-end) -> (event ...)
```

this turned out to work generically.

pseudocode example
```
days(tuesday, -1, 0) -> {name: monday, start: timestamp, end: timestamp}
```

configuration lines are converted to event functions, which are then applied with appropriate offsets and result event lists are subsequently filtered