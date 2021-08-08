#!/bin/sh
#\
exec /usr/local/bin/tclsh8.6 "$0" -- "$@"

#
# 2021 Alexander Schoepe
#

package require Tcl 8.6

set debug 1
set port 34034
set server localhost
set socket {}

proc ts { {ms {}} } {
  if {$ms eq {}} {
     set ms [clock milliseconds]
  }
  return [clock format [expr {$ms / 1000}] -format {%Y-%m-%dT%H:%M:%S}].[format %03d [expr {$ms % 1000}]]
}

if {[catch {socket $server $port} socket]} {
  puts stderr "exit: can not connect: $socket"
  exit 0
}
if {$debug} {puts stderr "connected: $socket to host $server port $port"}


if 0 {
fconfigure $socket -encoding iso8859-1

set data "ping client [ts]"
if {$debug} {puts stderr "sending: $socket [string length $data] bytes: [string range $data 0 40] ..."}
puts $socket $data
flush $socket
set data {}

if {$debug} {puts stderr "awaiting answer"}
gets $socket data
if {$debug} {puts stderr "received: $socket [string length $data] bytes: [string range $data 0 40] ..."}

if {$debug} {puts stderr "disconnect: $socket"}
close $socket
if {$debug} {puts stderr exit}
}


if 1 {
proc serverClose { fd } {
  global debug clients

  if {$debug} {puts stderr "serverClose: $fd"}
  catch { close $fd }
}


proc serverRead { fd } {
  global debug forever timeoutId

  if {[eof $fd]} {
    if {$debug} {puts stderr "serverRead: $fd EOF"}
    serverClose $fd
  } else {
    set n [gets $fd line]
    if {$n >= 0} {

      if {$debug} {puts "serverRead: $fd $n bytes -> [string range $line 0 40] ..."}
      switch -glob -- $line {
        ping* {
	  if {[info exists timeoutId]} {
	    if {$debug} {puts "cancel timeout"}
	    after cancel $timeoutId
	  }
          set forever $line
        }
        default {
        }
      }

    }
  }
}


fconfigure $socket -blocking 0 -buffering line -encoding iso8859-1
fileevent $socket readable "serverRead $socket"

set data "ping client [ts]"
if {$debug} {puts stderr "sending: $socket [string length $data] bytes: [string range $data 0 40] ..."}
puts $socket $data
flush $socket
set data {}

set timeout [lindex $argv end]
if {[string is integer -strict $timeout] && $timeout > 0} {
  if {$debug} {puts stderr "set timeout $timeout ms"}
  set timeoutId [after $timeout {set forever timeout}]
}


if {$debug} {puts stderr "awaiting answer"}
vwait forever
set data $forever
if {$debug} {puts stderr "received: $socket [string length $data] bytes: [string range $data 0 40] ..."}

if {$debug} {puts stderr "disconnect: $socket"}
close $socket
if {$debug} {puts stderr exit}
}
