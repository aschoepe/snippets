#
# $Id: tgetopt.tcl,v 1.3 2014/01/17 19:38:38 alex Exp alex $
#
# Copyright (c) 2014, Alexander Schoepe, Bochum, DE
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# 3. Neither the names of its contributors may be used to endorse or promote
# products derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#


#
# tgetopt *result declaration to_parse
#
# returns with 1 when error occur otherwise 0
#
#
# result is a variable name of a dict
# -----------------------------------
# the result dict will be cleared first
#
# option is key without "-"
#
# special dict keys:
# "--" all arguments after "--" unparsed
# .error boolean error occurs
# .errmsg error messages as list if an error occurs
# .usage usage information as defined in declaration
#
#
# declaration is a list
# ---------------------
# declare options
#   name[.arg[.class][.var][.nec][.req]][.sec] ?default value?]
#
# name = option name without "-" and not starting or including "."
# option without .arguments is a boolean flag with default value 0
# .class = tcl character class (see: string is class -strict)
#   replace .class with class type, for example .double or .integer
#   integer classes could also be [u]int[eger][1-64]
#   xdigit class could also be xdigit[1-64]
# .nec argument is necessary an can not be empty
# .var get default value from variable name in default value
# .req option is required
# .sec option is secret and will not shown in usage
#
#
# to_parse is a list
# ------------------
# options need a leading "-"
# options could be abbreviated if not ambiguous
# "--" end of parsing
#

if 0 {
  package require tgetopt
  set myEnvVar defaultFromVariable
  set declaration {
    flag
    color.arg.nec white
    count.arg.wideinteger 0
    i.arg.int8 -1
    u.arg.uint8 1
    hex.arg.required.xdigit16 ff01
    txt.arg.var myEnvVar
    hidden.arg.sec confidential
  }
  if {[tgetopt result $declaration {-hex 1f40 -i -128 -u 255 -col red -count 1}]} {
    set message "wrong, unknown or ambiguous arguments: should be: [dict get $result .usage]"
    foreach item [dict get $result .errmsg] { append message "\n $item" }
    error $message
  } else {
    puts [dict get $result]
  }
}

proc ::tgetopt { *result declaration to_parse } {
  if {${*result} != {}} {
    upvar 1 ${*result} result
  }

  set error 0
  set errmsg {} 
  set result {}
  set require {}
  set usage {}

  set optopts {}
  set argcnt [llength $declaration]
  for {set argc 0} {$argc < $argcnt} {incr argc} {
    set opt [lindex $declaration $argc]
    if {[string index $opt 0] == {.}} {
      set error 1
      lappend errmsg "option name can not start with '.'"
      dict set result .error $error
      dict set result .errmsg $errmsg
      return $error
    }
    if {[regsub -- {\..*$} $opt {} name]} {
      regsub -- (${name}) $opt {} opt
    }
    if {[regsub -- {\.sec\M} $opt {} opt]} {
      dict set optopts $name secret 1
    } else {
      dict set optopts $name secret 0
    }
    if {![regsub -- {\.arg\M} $opt {} opt]} {
      dict set optopts $name arg 0
      dict set optopts $name required 0
      dict set result $name 0
    } else {
      dict set optopts $name arg 1
      if {[regsub -- {\.var\M} $opt {} opt]} {
	dict set optopts $name variable 1
      } else {
	dict set optopts $name variable 0
      }
      if {[regsub -- {\.nec\M} $opt {} opt]} {
	dict set optopts $name necessary 1
      } else {
	dict set optopts $name necessary 0
      }
      if {[regsub -- {\.req\M} $opt {} opt]} {
	dict set optopts $name required 1
	lappend require $name
      } else {
	dict set optopts $name required 0
      }
      dict set optopts $name class {}
      dict set optopts $name size {}
      if {[regexp {\.(u?int(?:eger)?)(\d*)\M} $opt a c s]} {
	if {![string is integer -strict $s]} {
	  set s 32
	}
	if {$s < 1} {
	  set s 1
	}
	if {$s > 64} {
	  set s 64
	}
	dict set optopts $name class integer
	dict set optopts $name type $c
	dict set optopts $name size $s
      } elseif {[regexp {\.(xdigit)(\d*)\M} $opt a c s]} {
	if {![string is integer -strict $s]} {
	  set s 0
	}
	if {$s < 0} {
	  set s 0
	}
	if {$s > 64} {
	  set s 64
	}
	dict set optopts $name class xdigit
	dict set optopts $name type $c
	dict set optopts $name size $s
      } else {
	if {[regexp {\.([a-z]*)\M} $opt a c]} {
	  if {[lsearch -exact {alnum alpha ascii boolean control digit double entier graph list lower print punct space upper wideinteger wordchar} $c] > -1} {
	    dict set optopts $name class $c
	    if {$c == {wideinteger}} {
	      dict set optopts $name type $c
	      dict set optopts $name size 64
	    }
	  }
	}
      }
      incr argc
      if {$argc < $argcnt} {
	dict set result $name [lindex $declaration $argc]
	if {[dict get $optopts $name variable] && [dict get $result $name] != {}} {
	  upvar [dict get $result $name] var
	  if {[info exists var]} {
	    dict set result $name $var
	  } else {
	    dict set result $name {}
	  }
	}
      } else {
	dict set result $name {}
	set error 1
	lappend errmsg "declaration of '$name' missing default value"
	dict set result .error $error
	dict set result .errmsg $errmsg
	return $error
      }
    }
    if {![dict get $optopts $name secret]} {
      if {[dict get $optopts $name arg]} {
	if {[dict get $optopts $name class] == {}} {
	  append usage " ?-${name} data?"
	} else {
	  if {[lsearch -exact {integer wideinteger xdigit} [dict get $optopts $name class]] > -1} {
	    append usage " ?-${name} [dict get $optopts $name type][dict get $optopts $name size]?"
	  } else {
	    append usage " ?-${name} [dict get $optopts $name class]?"
	  }
	}
      } else {
	append usage " ?-${name}?"
      }
    }
  }

  set argcnt [llength $to_parse]
  for {set argc 0} {$argc < $argcnt} {incr argc} {
    set opt [lindex $to_parse $argc]
    if {$opt == {--}} {
      dict set result -- [lrange $to_parse ${argc}+1 end]
      break
    } elseif {[string index $opt 0] == {-}} {
      set opt [string range $opt 1 end]
      if {[dict keys $optopts $opt] == $opt || [llength [set opt [dict keys $optopts ${opt}*]]] == 1} {
	if {[dict get $optopts $opt arg]} {
	  incr argc
	  if {$argc < $argcnt} {
	    dict set result $opt [lindex $to_parse $argc]
	    if {[dict get $optopts $opt class] != {}} {
	      if {![string is [dict get $optopts $opt class] -strict [dict get $result $opt]]} {
		set error 1
		lappend errmsg "${opt}: value not strict class [dict get $optopts $opt class]"
	      } else {
	        if {[lsearch -exact {integer wideinteger xdigit} [dict get $optopts $opt class]] > -1} {
		  if {[string index [dict get $optopts $opt type] 0] == {u}} {
		    set min 0
		    set max [expr {(1 << [dict get $optopts $opt size]) - 1}]
		  } else {
		    set min [expr {(1 << [dict get $optopts $opt size] - 1) * -1}]
		    set max [expr {$min * -1 - 1}]
		  }
		  if {[dict get $optopts $opt class] == {xdigit}} {
		    set value [scan [dict get $result $opt] %x]
		  } else {
		    set value [dict get $result $opt]
		  }
		  if {$value < $min || $value > $max} {
		    set error 1
		    lappend errmsg "${opt}: value not strict class [dict get $optopts $opt type][dict get $optopts $opt size]"
		  }
		}
	      }
	    } elseif {[dict exists $optopts $name necessary] && [dict get $optopts $name necessary] && [string trim [dict get $result $opt]] == {}} {
	      set error 1
	      lappend errmsg "${opt}: value necessary ca not be empty"
	    }
	    if {[set p [lsearch -exact $require $opt]] > -1} {
	      set require [lreplace $require $p $p {}]
	      if {$require == {{}}} {
	        set require {}
	      }
	    }
	  } else {
	    set error 1
	    lappend errmsg "${opt}: missing argument"
	  }
	} else {
	  dict set result $opt 1
	}
      } else {
	set error 1
	lappend errmsg "[lindex $to_parse $argc]: unknown or ambiguous option"
      }
    } else {
      set error 1
      lappend errmsg "${opt}: syntax error"
    }
  }

  if {![dict exists $result --]} {
    dict set result -- {}
  }
  if {[llength $require] > 0} {
    set error 1
    foreach item $require {
      lappend errmsg "${item}: required option"
    }
  }

  dict set result .error $error
  dict set result .errmsg $errmsg
  dict set result .usage [string trim $usage]

  return $error
}

package provide tgetopt 1.2
