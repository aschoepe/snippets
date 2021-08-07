#!/bin/sh
#\
exec /usr/local/bin/tclsh8.6 "$0" -- "$@"

#
# 2021 Alexander Schoepe
#

package require Tcl 8.6

set options(debug) 1
set options(log) 0
set options(log,tag) logTag
set options(log,prio) daemon.info
set options(port) 34034
set options(server) localhost
set options(socket) {}


proc ts { {ms {}} } {
  if {$ms eq {}} {
     set ms [clock milliseconds]
  }
  return [clock format [expr {$ms / 1000}] -format {%Y-%m-%dT%H:%M:%S}].[format %03d [expr {$ms % 1000}]]
}


proc handler { fd line } {
  global options

  set data "ping server [ts]"
  if {$options(debug)} {puts stderr "-> $data"}
  clientWrite $fd $data
}


proc clientClose { fd } {
  global options

  incr options(c,fd) -1

  if {$options(debug)} {puts stderr "disconnected $fd"}
  if {$options(log)} { log clientClose $fd }

  array unset options fd,$fd

  catch { close $fd }
}


proc clientRead { fd } {
  global options
  global forever

  if {[eof $fd]} {
    if {$options(debug)} {puts stderr "clientRead EOF"}
    clientClose $fd
  } else {
    set n [gets $fd line]
    if {$n >= 0} {

      puts "clientRead $n bytes -> [string range $line 0 40] ..."
      switch -glob -- $line {
        ping* {
          handler $fd $line
        }
	quit - Quit {
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
  global options

  puts "clientWrite [string length $data] bytes -> [string range $data 0 40] ..."
  catch {
    puts $fd $data
    flush $fd
  }
}


proc clientConnect { fd addr port } {
  global options

  set options(fd,$fd) [list $addr $port]
  incr options(c,fd)

  if {$options(debug)} {puts stderr "connected $fd $addr $port"}
  if {$options(log)} { log clientConnect $fd $addr $port }
  fconfigure $fd -blocking 0 -buffering line -encoding iso8859-1
  fileevent $fd readable "clientRead $fd"
}


#package require Syslog
#Syslog::Open localhost
proc log { args } {
  #Syslog::Log CelanoDho: $args
  puts stderr $args
} 

proc server { {port {}} } {
  global options
  global forever


  if {![string is integer -strict $port]} {
    set port $options(port)
  }

  if {$options(log)} { log Starting ... }

  if {$options(debug)} {puts stderr "port $port"}
  if {[catch {socket -server clientConnect $port} fd] == 0} {
    if {$options(log)} { log Server $fd $port }
    vwait forever
    exit 0
  }
  puts stderr $fd
}


proc clientPuts { data } {
  global options

  if {$options(socket) != "" && ![eof $options(socket)]} {
    catch {
      puts $options(socket) $data
      flush $options(socket)
    }
  }
}


proc clientGets {} {
  global options

  if {$options(socket) != "" && ![eof $options(socket)]} {
    if {![catch {gets $options(socket) data}]} {
      return [string map {\\r \r \\n \n \\\\ \\} $data]
    }
  }
  return {}
}


proc disconnect {} {
  global options

  if {$options(socket) != ""} {
    catch {close $options(socket)}
    set options(socket) {}
  }
}


server
