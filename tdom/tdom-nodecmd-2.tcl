#!/bin/sh
#\
exec tclsh8.6 "$0" "$@"

package require tdom

dom createNodeCmd elementNode a
dom createNodeCmd elementNode b
dom createNodeCmd textNode t

set doc [dom createDocument root]
set root [$doc documentElement]

proc c_node {args} {
  dom createNodeCmd elementNode c
  c {*}$args {t "some text"}
}

$root appendFromScript {
  a v 1 {
    b {
      t text
    }
  }
  #c
  c_node a 1 b 2
  c_node c 3
}

puts [[$doc documentElement] asXML -indent 2]
