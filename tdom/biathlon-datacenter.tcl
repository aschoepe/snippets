#!/bin/sh
#\
exec /usr/local/bin/tclsh "$0" "$@"

# URL: http://biathlonresults.com

set file /Users/alex/Nextcloud/temp/DATACENTER.html 
package require tdom
set fd [open $file r]
set doc [dom parse -html [read $fd]]
close $fd
$doc documentElement root
$root setAttributeNS "" xmlns ""

set thisY 2021
set nextY 2022
set dateMap {}
lappend dateMap NOV ${thisY}-11-
lappend dateMap DEC ${thisY}-12-
lappend dateMap JAN ${nextY}-01-
lappend dateMap FEB ${nextY}-02-
lappend dateMap MAR ${nextY}-03-
puts $dateMap

package require uuid

proc ICS {name start ende ort {notiz {}}} {
  file mkdir biathlon
  set fd [open biathlon/$start.ics w]
  puts $fd "BEGIN:VCALENDAR
VERSION:2.0
CALSCALE:GREGORIAN
BEGIN:VTIMEZONE
TZID:Europe/Berlin
BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU
DTSTART:19810329T020000
TZNAME:MESZ
TZOFFSETTO:+0200
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU
DTSTART:19961027T030000
TZNAME:MEZ
TZOFFSETTO:+0100
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:[uuid::uuid generate]
SUMMARY:$name
DTSTART;TZID=Europe/Berlin:$start
DTEND;TZID=Europe/Berlin:$ende
LOCATION:$ort
DESCRIPTION:$notiz
SEQUENCE:0
BEGIN:VALARM
UID:03FD5080-F6E3-434D-93EB-7EA8C76C4D1B
TRIGGER;VALUE=DURATION:-PT1H
ACTION:AUDIO
ATTACH;VALUE=URI:Basso
END:VALARM
BEGIN:VALARM
UID:7573A0E0-C914-486B-9E86-26935D845806
TRIGGER;VALUE=DURATION:PT0S
ACTION:AUDIO
ATTACH;VALUE=URI:Basso
END:VALARM
END:VEVENT
END:VCALENDAR"
  close $fd
}


# <div class="au-target tab-pane fade in active" au-target-id="262" id="dbSchedule1">
#   <div class="panel panel-default au-target" click.delegate="EventSelected(sportEvent)" au-target-id="263">
#     <span>Oestersund (SWE)</span>
#     <a href="https://biathlonresults.com/#" class="list-group-item au-target" click.delegate="CompetitionSelected($event,competition)" role="button" au-target-id="274">
#       <td class="col100">02 DEC 13:45</td>
#       <td class="colFill au-target" show.bind="$this.IsSmallWidth" au-target-id="276">Women 7.5km Sprint</td>

foreach schedule [$root selectNodes {//div[@id="dbSchedule1"]}] {
  foreach panel [$schedule selectNodes {.//div[@au-target-id="263"]}] {
    puts "---"
    set node [lindex [$panel selectNodes {.//span/text()}] 0]
    set location [$node nodeValue]
    puts "$location"
    foreach competition [$panel selectNodes {.//a[@au-target-id="274"]}] {
      set node [lindex [$competition selectNodes {.//td[@class="col100"]/text()}] 0]
      set date [$node nodeValue]
      set date "[string map $dateMap [lindex $date 1]][lindex $date 0] [lindex $date 2]:00"
      set node [lindex [$competition selectNodes {.//td[@au-target-id="276"]/text()}] 0]
      set event [$node nodeValue]
      set length [lindex $event 1]
      switch -glob -- $event {
        {Mixed Relay*} {
          set event "🙎🏻‍♀️🙎🏻‍♂️ Mixed Staffel"
          set minutes 90
        }
        {Single Mixed Relay*} {
          set event "🙎🏻‍♀️🙎🏻‍♂️ Single Mixed Staffel"
          set minutes 45
        }
        {Men * Individual} {
          set event "🙎🏻‍♂️ Einzel $length"
          set minutes 120
        }
        {Men * Mass Start} {
          set event "🙎🏻‍♂️ Massenstart $length"
          set minutes 45
        }
        {Men * Relay} {
          set event "🙎🏻‍♂️ Staffel $length"
          set minutes 90
        }
        {Men * Pursuit} {
          set event "🙎🏻‍♂️ Verfolgung $length"
          set minutes 45
        }
        {Men * Sprint} {
          set event "🙎🏻‍♂️ Sprint $length"
          set minutes 120
        }
        {Women * Individual} {
          set event "🙎🏻‍♀️ Einzel $length"
          set minutes 120
        }
        {Women * Mass Start} {
          set event "🙎🏻‍♀️ Massenstart $length"
          set minutes 45
        }
        {Women * Pursuit} {
          set event "🙎🏻‍♀️ Verfolgung $length"
          set minutes 45
        }
        {Women * Relay} {
          set event "🙎🏻‍♀️ Staffel $length"
          set minutes 90
        }
        {Women * Sprint} {
          set event "🙎🏻‍♀️ Sprint $length"
          set minutes 120
        }
      }
      puts map=$dateMap
      puts "$date $event"

      set start [clock format [clock scan $date] -format {%Y%m%dT%H%M%S}]
      set ende [clock format [clock add [clock scan $date] $minutes minutes] -format {%Y%m%dT%H%M%S}]
      ICS $event $start $ende $location
    }
  }
}

$doc delete

# <?xml version="1.0" encoding="utf-8" standalone="yes" ?>
# <dok>
#     <!-- ein XML-Dokument -->
#     <kap title="Nettes Kapitel">
#         <pa>Ein Absatz</pa>
#         <pa>Noch ein Absatz</pa>
#         <pa>Und noch ein Absatz</pa>
#         <pa>Nett, oder?</pa>
#     </kap>
#     <kap title="Zweites Kapitel">
#         <pa>Ein Absatz</pa>
#         <pa format="bold">Erste Zeile</pa>
#         <pa format="bold">Zweite Zeile</pa>
#         <pa format="italic">Dritte Zeile</pa>
#     </kap>
# </dok>

# /dok	                             das erste Element dok
# /*	                               das äußerste Element unabhängig vom Namen (jedes wohlgeformte XML-Dokument hat genau ein äußerstes Element), hier dok
# //dok/kap	                         alle kap-Elemente innerhalb aller dok-Elemente
# //dok/kap[1]	                     alle jeweils ersten kap-Elemente innerhalb aller dok-Elemente
# //pa	                             alle pa-Elemente auf allen Ebenen
# //kap[@title='Nettes Kapitel']/pa	 alle Absätze der Kapitel mit Titel „Nettes Kapitel“.
# //kap/pa[2]	                       Jeweils das zweite pa-Element in den beiden Kapiteln.
# //kap[2]/pa[@format='bold'][2]	   Zweite Zeile mit dem Format 'bold' im 2. Kapitel.
# child::*	                         alle Kindelemente des gegenwärtigen Knotens
# child::pa	                         alle pa-Kinder des gegenwärtigen Knotens
# child::text()	                     alle Textknoten des gegenwärtigen Knotens
# .	                                 der gegenwärtige Knoten
# ./*	                               alle Kindelemente des gegenwärtigen Knotens
# ./pa	                             alle pa-Kinder des gegenwärtigen Knotens
# pa	                               alle pa-Kinder des gegenwärtigen Knotens
# attribute::*	                     alle Attribute des gegenwärtigen Knotens
# namespace::*	                     alle Namespaces des gegenwärtigen Knotens
# //kap[1]/pa[2]/text()	             Textinhalt des zweiten pa-Elements im ersten kap-Element (also "Noch ein Absatz")
