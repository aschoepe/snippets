#!/bin/sh
#\
exec tclsh8.6 "$0" "$@"

package require tdom

dom createNodeCmd elementNode a
dom createNodeCmd elementNode b
dom createNodeCmd elementNode c
dom createNodeCmd textNode t

set doc [dom createDocument root]
set root [$doc documentElement]

proc c_node {args} {
  c {*}$args {t "some text"}
}

$root appendFromScript {
  a v 1 {
    b {
      t text
    }
  }
  c_node a 1 b 2
}

puts [[$doc documentElement] asXML -indent 2]
