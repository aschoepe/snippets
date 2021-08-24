# con = connection-id
# stm = statement to generate
# tab = table name
# ign = ignore fields on replace

proc sqlStm { con stm tab args } {
  array set opts {
    altnkey 0
    ignore {}
    tolower 0
  }

  set len [llength $args]
  set idx 0
  for {set idx 0} {$idx < $len} {incr idx} {
    switch -- [set opt [lindex $args $idx]] {
      -ignore {
        incr idx
        if {$idx < $len} {
          set opts([string range $opt 1 end]) [lindex $args $idx]
        } else {
          error "option '$opt': missing argument"
        }
      }
      -altnkey - -tolower {
        set opts([string range $opt 1 end]) 1
      }
      default {
        error "unknown option \"$opt\", should be: -altnkey, -ignore or -tolower"
      }
    }
  }

  catch {set dbs [$con configure -database]}
  set cols [dict keys [$con columns $tab]]
  #puts cols=$cols

  set pkey {}
  foreach items [$con primarykeys $tab] {
    if {![info exists dbs] || $dbs == [dict get $items tableSchema]} {
      lappend pkey [dict get $items columnName]
    }
  }
  #puts pkey=$pkey

  set keys {}
  foreach item $pkey {
    if {$opts(altnkey)} {
      lappend keys "$item = :KeYclAuSe_$item"
    } else {
      lappend keys "$item = :$item"
    }
  }
  #puts keys=$keys

  set vals {}
  foreach item $cols {
    lappend vals ":$item"
  }
  #puts vals=$vals

  switch -- $stm {
    insert {
      set sql "insert into $tab ([join $cols {, }]) values ([join $vals {, }])"
    }
    replace {
      if {$opts(ignore) eq {}} {
        set sql "replace into $tab ([join $cols {, }]) values ([join $vals {, }])"
      } else {
        set cols2 {}
        set vals2 {}
        foreach item $cols {
          if {$item ni $opts(ignore)} {
            lappend cols2 $item
            lappend vals2 ":$item"
          }
        }
        set sql "replace into $tab ([join $cols2 {, }]) values ([join $vals2 {, }])"
      }
    }
    select {
      if {$opts(tolower)} {
        set vals2 {}
        foreach item $cols {
          lappend vals2 "$item as \"[string tolower $item]\""
        }
        set keys2 {}
        foreach item $keys {
          lappend keys2 [string tolower $item]
        }
        set sql "select [join $vals2 {, }] from $tab where [join $keys2 { and }]"
      } else {
        set sql "select [join $cols {, }] from $tab where [join $keys { and }]"
      }
    }
    update {
      set vals2 {}
      foreach item $cols {
        if {$item ni $opts(ignore)} {
          lappend vals2 "$item = :$item"
        }
      }
      set sql "update $tab set [join $vals2 {, }] where [join $keys { and }]"
    }
    delete {
      set sql "delete from $tab where [join $keys { and }]"
    }
    default {
      error "missing statement type"
    }
  }

  return $sql
}

# Seqence / Last InsertID
# "insert into $tab ([join $vals {, }], $dval) values (:[join $keys {, :}], last_insert_id(1))"
# "insert into $tab ([join $vals {, }], $dval, $sval) values (:[join $keys {, :}], last_insert_id(1), now())"
# "update $tab set $dval = last_insert_id($dval + 1) where [join $keys { and }]"
# "update $tab set $dval = last_insert_id($dval + 1), $sval = now() where [join $keys { and }]"
# "select last_insert_id() $dval"

