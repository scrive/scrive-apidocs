#!/usr/bin/env python

import fileinput

import woothee

# USAGE:
# * pip install woothee (and probably update it as well)
# * save output of this query to stats.txt:
#
# select client_name, count(*) as cnt
# from evidence_log
# where time >= '2017-02-01 00:00:00.000000+00'
#   and time < '2017-03-01 00:00:00.000000+00'
#   and event_type = 10
# group by client_name
# order by cnt desc;
#
# * run this script with that file as input

DESKTOP_WINDOWSES = ['Windows 10',
                     'Windows 2000',
                     'Windows 7',
                     'Windows 8',
                     'Windows 8.1',
                     'Windows Vista',
                     'Windows XP']

if __name__ == '__main__':
    result = {}
    for line in list(fileinput.input())[2:]:
        ua, ua_count = map(lambda s: s.strip(), line.split('|'))
        wt = woothee.parse(ua)
        if wt['os'] in DESKTOP_WINDOWSES:
            os = 'Windows'
            if wt['name'] == 'Internet Explorer':
                name = 'IE' + wt['version']
            else:
                name = wt['name']
        else:
            os = wt['os']
            name = wt['name']
        key = os + '+' + name
        result.setdefault(key, 0)
        result[key] += int(ua_count)

    total = sum(result.values())

    print 'TOTAL'.ljust(36) + str(total).ljust(6) + ' (100.00%)'
    for key, val in sorted(result.items(),
                           key=lambda x: x[1],
                           reverse=True):
        avg = (100. * val) / total
        print key.ljust(36) + str(val).ljust(6) + ' (' + ('%.2f' % avg) + '%)'
