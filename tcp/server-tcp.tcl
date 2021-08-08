#!/bin/sh
#\
exec /usr/local/bin/tclsh8.6 "$0" -- "$@"

#
# 2021 Alexander Schoepe
#

set debug 1
set port 34034
set server localhost
set socket {}
set clients(counter) 0


proc ts { {ms {}} } {
  if {$ms eq {}} {
     set ms [clock milliseconds]
  }
  return [clock format [expr {$ms / 1000}] -format {%Y-%m-%dT%H:%M:%S}].[format %03d [expr {$ms % 1000}]]
}


proc clientClose { fd } {
  global debug clients

  if {$debug} {puts stderr "clientClose: $fd"}
  incr clients(counter) -1
  array unset clients $fd

  catch { close $fd }
}


proc clientRead { fd } {
  global debug forever argv

  if {[eof $fd]} {
    if {$debug} {puts stderr "clientRead: $fd EOF"}
    clientClose $fd
  } else {
    set n [gets $fd line]
    if {$n >= 0} {
      if {$debug} {puts stderr "clientRead: $fd $n bytes -> [string range $line 0 40] ..."}
      # mapping [string map {\\r \r \\n \n \\\\ \\} $data]

      switch -glob -- $line {
        ping* {
	  set pause [lindex $argv end]
	  if {[string is integer -strict $pause] && $pause > 0} {
	    if {$debug} {puts stderr "pause $pause ms"}
	    after $pause
	  }
	  clientWrite $fd "ping server [ts]"
        }
	quit {
	  set forever end
	  exit 0
	}
	default {
	}
      }

    }
  }
}


proc clientWrite { fd data } {
  global debug

  if {$debug} {puts stderr "clientWrite: $fd [string length $data] bytes -> [string range $data 0 40] ..."}
  catch {
    puts $fd $data
    flush $fd
  }
}


proc clientConnect { fd addr port } {
  global debug clients

  set clients($fd) [list $addr $port]
  incr options(counter)

  if {$debug} {puts stderr "clientConnect: $fd host $addr port $port"}
  fconfigure $fd -blocking 0 -buffering line -encoding iso8859-1
  fileevent $fd readable "clientRead $fd"
}


proc server {} {
  global debug server port forever

  if {[catch {socket -server clientConnect $port} fd] == 0} {
    if {$debug} {puts stderr "server: $fd host $server port $port"}
    vwait forever
    exit 0
  }
}


server
