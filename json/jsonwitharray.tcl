#!/bin/sh
#\
exec tclsh8.6 "$0" "$@"

package require json
package require json::write

proc tcl2json value {
  # Guess the type of the value; deep *UNSUPPORTED* magic!
  regexp {^value is a (.*?) with a refcount} [::tcl::unsupported::representation $value] -> type

  switch $type {
    string {
      return [json::write string $value]
    }
    dict {
      return [json::write object {*}[dict map {k v} $value {tcl2json $v}]]
    }
    list {
      return [json::write array {*}[lmap v $value {tcl2json $v}]]
    }
    int - double {
      return [expr {$value}]
    }
    booleanString {
      return [expr {$value ? "true" : "false"}]
    }
    default {
      # Some other type; do some guessing...
      if {$value eq "null"} {
        # Tcl has *no* null value at all; empty strings are semantically different and absent variables aren't values. So cheat!
        return $value
      } elseif {[string is integer -strict $value]} {
        return [expr {$value}]
      } elseif {[string is double -strict $value]} {
        return [expr {$value}]
      } elseif {[string is boolean -strict $value]} {
        return [expr {$value ? "true" : "false"}]
      }
      return [json::write string $value]
    }
  }
}

set example {
  {
    "menu": {
	"id": "file",
	"value": "File:",
	"popup": {
	    "menuitem": [
		{"value": "New", "onclick": "CreateNewDoc()"},
		{"value": "Open", "onclick": "OpenDoc()"},
		{"value": "Close", "onclick": "CloseDoc()"}
	    ]
	}
    }
  }
}

puts $example
# menu {id file value File: popup {menuitem {{value New onclick CreateNewDoc()} {value Open onclick OpenDoc()} {value Close onclick CloseDoc()}}}}

puts [set d [json::json2dict $example]]
puts [tcl2json $d]

set d [dict create]
dict set d menu id file
dict set d menu value File:

set entry [dict create]
dict set entry value New
dict set entry onclick CreateNewDoc()
lappend menuitems $entry

set entry [dict create]
dict set entry value Open
dict set entry onclick OpenDoc()
lappend menuitems $entry

set entry [dict create]
dict set entry value Close
dict set entry onclick CloseDoc()
lappend menuitems $entry

dict set d menu popup menuitem $menuitems

puts $d
puts [tcl2json $d]
