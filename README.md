meetup
======
uncover the dynamics of a meetup group

you will require a meetup API key http://www.meetup.com/meetup_api/docs/

depth=1 default, only retrieves events and rsvps relative to the group  
depth=2 retrieves members profiles with the list of their topics and groups, it takes a LOT longer on large groups

the results will be saved as .Rdata and csv files

you can find a markdown report building from these files at examples/meetup.Rmd