#!/bin/sh
#\
exec tclsh8.6 "$0" "$@"

package require tdom

dom createNodeCmd elementNode a
dom createNodeCmd elementNode b
dom createNodeCmd elementNode c
dom createNodeCmd elementNode d
dom createNodeCmd elementNode e
dom createNodeCmd textNode t

set doc [dom createDocument root]
set root [$doc documentElement]

$root appendFromScript {
  a v 1 {
    b {
      t text
    }
    c {}
  }
}

set nodeC [$root selectNodes {/root/a/c}]

$nodeC appendFromScript {
  d v 1 {
    t appendFromScript
  }
}

set nodeD [$root selectNodes {/root/a/c/d}]

$nodeC insertBeforeFromScript {
  e {
    t insertBeforeFromScript
  }
} $nodeD

puts [[$doc documentElement] asXML -indent 2]
