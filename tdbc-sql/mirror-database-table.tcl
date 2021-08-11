#!/bin/sh
#\
exec /usr/local/bin/tclsh8.6 "$0" "$@"

set use oratcl
# set use orajdbc

package require tdbc::mysql


set dst [tdbc::mysql::connection new -user USER -passwd PASS -host HOST -db DBNAME]

if {$use eq {orajdbc}} {
  set tclblend_init -Doracle.jdbc.timezoneAsRegion=false
  set env(LD_LIBRARY_PATH) /usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64/server
  set env(CLASSPATH) /usr/local/lib/tdbcjdbc0.1.1/drivers/oracle/ojdbc6.jar:/usr/local/lib/tdbcjdbc0.1.1/drivers/oracle/orai18n.jar
  package require tdbc::jdbc

  set src [tdbc::jdbc::connection new oracle.jdbc.driver.OracleDriver {jdbc:oracle:thin:@HOST:PORT:SID} USER PASS]
}

if {$use eq {oratcl}} {
  package require tdbc::oratcl

  set src [tdbc::oratcl::connection create db USER/PASS@SID]
}


proc sqlStm { con stm tab {ign {}} } {
  catch {set dbs [$con configure -database]}
  set cols [dict keys [$con columns $tab]]

  set pkey {}
  foreach items [$con primarykeys $tab] {
    if {![info exists dbs] || $dbs == [dict get $items tableSchema]} {
      lappend pkey [dict get $items columnName]
    }
  }

  set keys {}
  foreach item $pkey {
    lappend keys "$item = :KeYclAuSe_$item"
  }

  set vals {}
  foreach item $cols {
    lappend vals ":$item"
  }

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

set table TABLE {
set table2 [string tolower $table]

set sel [$src prepare "select * from $table"]
set rep [$dst prepare [sqlStm $dst replace $table2]]

$sel foreach row {
  set res [$rep execute $row]
  $res close
}
$sel close
$rep close

$src destroy
$dst destroy
