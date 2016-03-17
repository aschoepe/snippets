#
# Tcl Wrapper for "http::geturl" with automatic http authentication
#
# RFC 2617 HTTP Authentication: Basic and Digest Access Authentication
#
# MIT License (MIT)
# Copyright (c) 2016, Alexander Schoepe, Bochum, DE
# $Id: http-auth.tcl,v 1.1 2016/03/17 08:48:55 alex Exp alex $
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# 

#
# USAGE:
# 
# package require http::auth
# 
# # if using a realm-based authentication you need to write a callback.
# # see standard the ::http::auth::callback in this source.
# # if not, set username and password as follows:
#
# ::http::auth::config username USER
# ::http::auth::config password PASS
#
# # then call geturl
#
# ::http::geturl url ?options?
#

if {[package provide Tcl] < 8.6} {
  package require base64
}
package require http
package require md5


namespace eval ::http::auth {
  variable options
  variable noncecount

  dict set options username anonymous
  dict set options password {}
}


if {[info commands ::http::geturl_original] == {} && [lsearch -exact [interp aliases] {::http::auth::geturl}] == -1} {
  rename ::http::geturl ::http::geturl_original
  interp alias {} ::http::geturl {} ::http::auth::geturl
}


proc ::http::auth::config { name value } {
  variable options

  dict set options $name $value
}


proc ::http::auth::callback { realm *username *password } {
  variable options

  upvar ${*username} username
  upvar ${*password} password

  switch -- $realm {
    default {
      set username [dict get $options username]
      set password [dict get $options password]
    }
  }
}


proc ::http::auth::geturl { url args } {
  variable options
  variable noncecount

  set opts $args

  set method no-auth
  set token {}
  set executed {}
  set authorization {}
  set headerlist {}

  if {[set idx [lsearch -exact $opts {-headers}]] > -1} {
    foreach {name value} [lindex $opts ${idx}+1] {
      if {$name == {Authorization}} {
	set method [string tolower [lindex $value 0]]
      } else {
        lappend headerlist $name $value
      }
    }
    set opts [lreplace $opts $idx ${idx}+1]
  }
  dict set executed $method true
  if {[set idx [lsearch -exact $opts {-keepalive}]] > -1} {
    set keepalive [lindex $opts ${idx}+1]
    set opts [lreplace $opts $idx ${idx}+1]
    set args [lreplace $opts $idx ${idx}+1]
  }

  if {![info exists keepalive]} {
    if {[dict exists $options keepalive]} {
      set keepalive [dict get $options keepalive]
    } else {
      set keepalive 0
    }
  }


  set cnt 0
  while { 1 } {
    if {$token != {}} {
      http::cleanup $token
    }
    if {$cnt == 0} {
      set token [::http::geturl_original $url {*}$args -keepalive $keepalive]
    } else {
      set headers $headerlist
      lappend headers Authorization $authorization
      dict set executed [string tolower [lindex $authorization 0]] true
      set token [::http::geturl_original $url {*}$opts -keepalive $keepalive -headers $headers]
    }

    if {[http::ncode $token] == 401} {
      upvar $token state
      if {$state(querylength) < 1} {
        set opt(method) GET
      } else {
        set opt(method) POST
      }
      set opt(uri) $state(url)

      foreach {name value} $state(meta) {
	if {$name == {WWW-Authenticate}} {
	  set opt(realm) {}
	  set opt(username) {}
	  set opt(password) {}

	  if {[regexp {(\w+)\s(.*)} $value all method challange]} {
	    foreach item [split $challange {,}] {
	      if {[regexp {([^=]+)=(.*)} [string trim $item] all name value]} {
		set opt($name) [string trim $value {"}]
	      }
	    }
	  }
	  set method [string tolower $method]

	  callback $opt(realm) opt(username) opt(password)

	  switch -nocase -- $method {
	    Basic {
	      if {[package provide Tcl] < 8.6} {
		set authorization [list Basic [base64::encode $opt(username):$opt(password)]]
	      } else {
		set authorization [list Basic [binary encode base64 $opt(username):$opt(password)]]
	      }
	      if {[dict exists $executed $method]} {
		continue
	      } else {
	        break
	      }
	    }
	    Digest {
	      if {[info exists opt(nonce)] && [info exists opt(realm)] && [info exists opt(method)] && [info exists opt(uri)] &&
	          [info exists opt(algorithm)] && [string match "MD5*" $opt(algorithm)]} {

		set A1 [string tolower [md5::md5 -hex $opt(username):$opt(realm):$opt(password)]]
		set A2 [string tolower [md5::md5 -hex $opt(method):$opt(uri)]]

		if {![info exists noncecount($opt(nonce))]} {
		   set noncecount($opt(nonce)) 0
		}
		set opt(nc) [format %08d [incr noncecount($opt(nonce))]]
		set opt(cnonce) [format %08x%08x [clock clicks] [clock seconds]]

		switch -exact -- $opt(qop) {
		  {} { set opt(response) [string tolower [md5::md5 -hex $A1:$opt(nonce):$A2]] }
		  auth { set opt(response) [string tolower [md5::md5 -hex $A1:$opt(nonce):$opt(nc):$opt(cnonce):$opt(qop):$A2]] }
		  default { set opt(response) {} }
		}

		set list {}
		foreach item {username realm nonce uri response qop nc cnonce algorithm stale opaque} {
		  if {[info exists opt($item)] && [string trim $opt($item)] != {}} {
		    lappend list $item=\"$opt($item)\"
		  }
		}
		set authorization "Digest [join $list {, }]"
		if {[dict exists $executed $method]} {
		  continue
		} else {
		  break
		}
	      }
	    }
	  }
	}
      }
      if {[dict exists $executed $method]} {
	break
      }
    } else {
      break
    }
    incr cnt
  }
  return $token
}


package provide http::auth 1.0
