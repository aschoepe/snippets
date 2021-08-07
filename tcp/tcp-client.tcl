#!/bin/sh
#\
exec /usr/local/bin/tclsh8.6 "$0" -- "$@"

#
# 2021 Alexander Schoepe
#

package require Tcl 8.6

set options(debug) 1
set options(log) 0
set options(port) 34034
set options(server) localhost
set options(socket) {}

proc ts { {ms {}} } {
  if {$ms eq {}} {
     set ms [clock milliseconds]
  }
  return [clock format [expr {$ms / 1000}] -format {%Y-%m-%dT%H:%M:%S}].[format %03d [expr {$ms % 1000}]]
}

if {[catch {socket $options(server) $options(port)} options(socket)]} {
  puts "exit: no socket"
  exit 0
}
puts connected

fconfigure $options(socket) -encoding iso8859-1

set data "ping client [ts]"
if {$data != {}} {
  puts "sending [string length $data] bytes: [string range $data 0 40] ..."
  puts $options(socket) $data
  flush $options(socket)
  set data {}

  puts receiving
  gets $options(socket) data
  puts stderr "received [string length $data] bytes: [string range $data 0 40] ..."
} else {
  puts "data empty"
}

puts exit
close $options(socket)
