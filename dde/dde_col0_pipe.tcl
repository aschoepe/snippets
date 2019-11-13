#
# @(#)$Id: dde_col0_pipe.tcl,v 1.2 2007/05/15 08:38:12 alex Exp $
# (c) 2007 Alexander Schoepe
#

proc TkPresent {} {
  return [expr {![catch {package present Tk}]}]
}

if {[TkPresent]} {
  package require tile
  package require Tablelist_tile
}

package require msgcat
package require registry
package require dde

# -----------------------------------------------------------------------------

set setup(askquit) 1
set setup(command) {}
set setup(sep) |
set setup(len) 400

# -----------------------------------------------------------------------------

proc Slog { args } {
  #puts stderr $args
}

Slog starting

# -----------------------------------------------------------------------------

proc Exit { exitcode } {
  exit $exitcode
}

# -----------------------------------------------------------------------------

proc Quit { {choose _dialog} } {
  global setup

  switch -- $choose {
    _dialog {
      set w .quit
      if {[winfo exists $w]} {
        wm withdraw $w
        wm deiconify $w
        return
      }
      if {$setup(askquit)} {
        ttk::dialog $w -title [msgcat::mc Quit] -icon question \
          -message [msgcat::mc {Quit Program?}] \
          -detail [msgcat::mc {Do you wish to quit the Program?}] \
          -buttons [list no yes] \
          -labels [list no [msgcat::mc No] yes [msgcat::mc Yes]] \
          -command Quit
      } else {
        Quit yes
      }
    }
    yes {
      Exit 0
    }
    default {
    }
  }
}

# -----------------------------------------------------------------------------

proc TableColumnsSet { w list } {
  if {[winfo exists $w]} {
    foreach {tag data} $list {
      array set def $data
      if {[info exists def(def)]} {
        $w insertcolumns end [lindex $def(def) 0] [lrange $def(def) 1 end-1] [lindex $def(def) end]
        set col [expr {[$w columncount] - 1}]
        foreach {opt data} [array get def] {
          switch -- $opt {
            def {
              $w columnconfigure $col -name $tag
            }
            default {
              $w columnconfigure $col $opt $data
            }
          }
        }
      }
      unset def
    }
  }
}

# -----------------------------------------------------------------------------

proc SearchRegistry {} {
  global setup

  if {![catch {registry get HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\.xls {}} class]} {
    if {![catch {registry get HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\$class {}} type]} {
      if {![catch {registry get HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\$class\\shell\\open\\command {}} setup(command)]} {
        set setup(command) [string map {\\ \\\\} $setup(command)]
      }
    }
  }
}

# -----------------------------------------------------------------------------

proc CheckForExcel {} {
  global setup

  set setup(excel) 0
  foreach list [dde services {} System] {
    if {[lindex $list 0] == "Excel"} {
      set setup(excel) 1
      break
    }
  }
  if {$setup(excel)} {
    set setup(status) "Excel is running."
    LookupExcelSheets
  } else {
    set setup(status) "Excel is not running!"
    $setup(lb,excel) delete 0 end
  }
}

# -----------------------------------------------------------------------------

proc LookupExcelSheets {} {
  global setup

  set lb $setup(lb,excel)
  $lb delete 0 end

  foreach item [dde request Excel System Topics] {
    if {[regexp {^\[(.+)(\.[Xx][Ll][Ss])\](.+)$} $item all file ext tab]} {
      $lb insert end [list $file$ext $tab $all]
    }
  }
}

# -----------------------------------------------------------------------------

proc DataWindow { topic data } {
  global artikel 
  global lno
  global msg

  set alert .msg_[clock seconds]_[clock clicks]
  ttk::dialog $alert -icon info -message [msgcat::mc {Please wait!}]\n\n[msgcat::mc {Getting Data ...}]
  update

  set tl .tl_[clock seconds]_[clock clicks]
  toplevel $tl
  wm title $tl $topic
  wm withdraw $tl

  set f $tl.f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 0 -weight 1

  set f $tl.f.t
  grid [ttk::frame $f -padding 3 -relief groove] -row 0 -column 0 -sticky wnes
  grid columnconfigure $f 0 -weight 1
  grid columnconfigure $f 1 -weight 0
  grid rowconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 0
  set lb $f.lb
  grid [tablelist::tablelist $lb -height 16 -width 40 -showseparators 1 -titlecolumns 0 -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
  grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
  $lb configure -yscrollcommand "$f.sbv set"
  grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
  $lb configure -xscrollcommand "$f.sbh set"

  set f $tl.f.b
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky we
  grid columnconfigure $f 0 -weight 1
  grid [ttk::frame $f.f] -row 0 -column 0 -sticky wnes
  grid [ttk::button $f.nes -text [msgcat::mc {Table To Text}] -command "SaveData $lb"] -row 0 -column 1
  grid [ttk::button $f.c -text [msgcat::mc {Close}] -command "destroy $tl"] -row 0 -column 2

  TableColumnsSet $lb [subst {
      col0 {def {32 [msgcat::mc {Spalte 0}] right}}
  }]

  set lno 0
  foreach line [split $data \n] {
    if {$lno} {
      if {[string trim [set col0 [lindex $line 0]]] != ""} {
	$lb insert end [list $col0]
      }
    }
    incr lno
  }

  wm deiconify $tl
  focus -force $tl

  destroy $alert
}

# -----------------------------------------------------------------------------

proc SaveData { w } {
  global lno
  global msg
  global setup

  set data [$w get 0 end]
  $w delete 0 end

  set out {}
  set lno 1
  foreach line $data {
    lappend out $line
    incr lno
  }

  set file [tk_getSaveFile -parent . -title [msgcat::mc {Choose Text File}] -filetypes {{{Text Files} {.txt}}} -initialfile [msgcat::mc Untiteled].txt]
  if {![catch {open $file w} fd]} {
    set txt [join $out $setup(sep)]

    while {[string length $txt] > $setup(len)} {
      set pos [string last $setup(sep) $txt $setup(len)]
      if {$pos > -1} {
	incr pos -1
	set out [string range $txt 0 $pos]
	incr pos 2
	set txt [string range $txt $pos end]
	puts $fd $out
      } else {
	break
      }
    }
    if {[string length $txt] > 0} {
      puts $fd $txt
    }
    close $fd
  }
}

# -----------------------------------------------------------------------------

proc GetExcelSheet { W } {
  global setup

  if {[winfo class $W] == "Tablelist"} {
    set w $W
  } else {
    set w [tablelist::getTablelistPath $W]
  }

  if {[set sel [lindex [$w curselection] 0]] == ""} {
    return
  }

  foreach {dummy dummy topic} [$w get $sel] {}

  # max r 65536 c 256
  set rows 65536
  set cols 1

  if {[catch {string map {\r {} \" \\" ' \\' \{ ( \} )} [dde request Excel $topic Z1S1:Z${rows}S${cols}]} data] == 0} {
    DataWindow $topic $data
    unset data
  }
}

# -----------------------------------------------------------------------------

proc NewExcelSheet { w } {
  dde execute Excel System {[new(1)]}
  set topic [dde request Excel System "selection"] 
  set topic [string range $topic 0 [expr [string first "!" $topic]-1]]
  dde execute Excel $topic {[select("z1s1")]}
  clipboard clear 
  foreach line [$w get 0 end] {
    clipboard append [join $line \t]\n
  }
  dde execute Excel $topic {[paste()]}
}

# -----------------------------------------------------------------------------

proc StartExcel {} {
  global setup

  catch {eval exec $setup(command) &} rc
  CheckForExcel
}

# -----------------------------------------------------------------------------

proc Main {} {
  global setup

  wm title . {DDE Excel}
  wm protocol . WM_DELETE_WINDOW Quit

  set mb .menubar
  menu $mb
  . config -menu $mb

  set mn $mb.file
  $mb add cascade -label [msgcat::mc File] -menu $mn -underline 0
  menu $mn -tearoff 0

  $mn add command -label [msgcat::mc Quit] -underline 1 -state normal -command Quit -accelerator [msgcat::mc Ctrl]+q
  bind . <Control-Key-q> Quit

  set f .f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 1

  set f .f.t
  grid [ttk::frame $f -padding 3 -relief groove] -row 0 -column 0 -sticky we
  grid columnconfigure $f 1 -weight 1
  grid [ttk::label $f.l0 -text [msgcat::mc Status]] -row 0 -column 0
  grid [ttk::entry $f.e0 -textvariable setup(status) -width 16 -state readonly] -row 0 -column 1 -sticky we
  grid [ttk::label $f.l1 -text [msgcat::mc Command]] -row 1 -column 0
  grid [ttk::entry $f.e1 -textvariable setup(command) -width 64 -state readonly] -row 1 -column 1 -sticky we

  set f .f.e
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky we
  grid columnconfigure $f 4 -weight 1
  grid [ttk::label $f.l0 -text [msgcat::mc Separator]] -row 0 -column 0
  grid [ttk::entry $f.e0 -textvariable setup(sep) -width 1] -row 0 -column 1 -sticky we
  grid [ttk::label $f.l1 -text [msgcat::mc Length]] -row 0 -column 2
  grid [ttk::entry $f.e1 -textvariable setup(len) -width 6] -row 0 -column 3 -sticky we
  grid [ttk::frame $f.f] -row 0 -column 4 -sticky we


  set f .f.m
  grid [ttk::frame $f -padding 3 -relief groove] -row 2 -column 0 -sticky wnes
  grid columnconfigure $f 0 -weight 1
  grid columnconfigure $f 1 -weight 0
  grid rowconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 0
  set lb $f.lb
  grid [tablelist::tablelist $lb -height 24 -width 60 -showseparators 1 -titlecolumns 2 -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
  grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
  $lb configure -yscrollcommand "$f.sbv set"
  grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
  $lb configure -xscrollcommand "$f.sbh set"

  set f .f.b
  grid [ttk::frame $f -padding 3 -relief groove] -row 3 -column 0 -sticky we
  grid columnconfigure $f 0 -weight 1
  grid [ttk::frame $f.f] -row 0 -column 0 -sticky wnes
  grid [ttk::button $f.se -text [msgcat::mc {Start Excel}] -command StartExcel] -row 0 -column 1
  grid [ttk::button $f.les -text [msgcat::mc {Lookup Excel Sheets}] -command CheckForExcel] -row 0 -column 2

  TableColumnsSet $lb [subst {
    book {def {20 [msgcat::mc {Book}] left}}
    sheet {def {20 [msgcat::mc {Sheet}] left} -stretchable 1}
    topic {def {32 [msgcat::mc {Topic}] left} -hide 0}
  }]
  bind [$lb bodytag] <Double-1> "GetExcelSheet %W"

  set setup(lb,excel) $lb

  focus -force .
}

# -----------------------------------------------------------------------------

Main
SearchRegistry
CheckForExcel

