# con = connection-id
# stm = statement to generate
# tab = table name
# ign = ignore fields on replace

proc sqlStm { con stm tab {ign {}} } {
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
    lappend keys "$item = :KeYclAuSe_$item"
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
      if {$ign eq {}} {
        set sql "replace into $tab ([join $cols {, }]) values ([join $vals {, }])"
      } else {
        set cols2 {}
        set vals2 {}
        foreach item $cols {
          if {[lsearch -exact $ign $item] == -1} {
            lappend cols2 $item
            lappend vals2 ":$item"
          }
        }
        set sql "replace into $tab ([join $cols2 {, }]) values ([join $vals2 {, }])"
      }
    }
    select {
      set sql "select [join $cols {, }] from $tab where [join $keys { and }]"
    }
    update {
      set vals2 {}
      foreach item $cols {
        lappend vals2 "$item = :$item"
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

