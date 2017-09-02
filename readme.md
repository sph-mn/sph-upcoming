# sph-upcoming
define events in time and display upcoming ones

# status
it basically works.
the event series generation and filtering is functional, but there was not enough time to polish the rest. with user interest or donations this can be changed

# configuration file
## example

time id options ...

the time format is currently fixed to kiloseconds of the day and iso dates. this is what the author uses, other users might want different formats and it would be easy to implement given additional time

```
28.8 meeting weekday 1
24 eat interval 10 duration 0.6
72 sleep
22 work end 55 weekday (1 2 3 4)
42 each-second-day interval 2 interval-unit day
"2017-12-13" test title "test title"
"2017-12-13 28.8" test-2 end 30 title "test-2 title"
84 special-day depends (and work (or special-time holiday))
```

the event definition is quite flexible, allowing to define most kinds of event series in a single line

## syntax
```
line/event: time id option/value ...
time-date: string:"yyyy-mm-dd"
time-day: integer:day-ks
time: time-day/time-date/"time-date time-day"
id: symbol
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
start-depends: id/(id ...)
```

# usage
create a configuration file with the path "$HOME/.config/sph/upcoming" and define your events in there, each line an event.
then start a server with \"upcoming --server\". if the server is running, use \"upcoming\" and other options to query events

```
$ upcoming --help
parameters
  options ... event-ids ...
description
  filter events in time with event definitions from a configuration file.
  first start a server with "upcoming --server". if the server is running, use "upcoming" and other options to query events.
  event definitions are loaded from the configuration file "$HOME/.config/sph/upcoming"
options
  --about | -a
  --config=string | -c string  use a different configuration file
  --help | -h
  --interface
  --limit=integer  return only n repetitions of each unique event id
  --next[=integer] | -n [integer]  select up to n upcoming events
  --previous[=integer] | -p [integer]  select up to n previous events
  --server | -s  start a server that can answer event queries
```

## example output
```
$ upcoming --next=6 --limit=1
3.36 72.00 72.20 sleep
41.76 2017-09-03_24.00 2017-09-03_24.60 eat
129.16 2017-09-04_25.00 2017-09-04_52.00 work
132.96 2017-09-04_28.80 2017-09-04_29.00 resource-meeting
```

the first number is the number of kiloseconds from now until the event starts. the following to space separated paths are the start and end times and the last part is the id

# setup
## dependencies
* guile >= 2
* sph-lib

## install
./exe/install

see ./exe/install --help for more options

# license
gpl3+

# possible enhancements
* support for additional time formats
* allow event ids as start/end time, which uses the referenced events start or end time respectively
* code clean-up
* add a client library