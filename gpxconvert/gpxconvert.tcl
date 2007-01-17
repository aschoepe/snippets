#
# GPXconvert written in pure tcl
#
# @(#)$Id: gpxconvert.tcl,v 1.56 2007/01/17 22:32:53 alex Exp alex $
# (c) 2005-2007 Alexander Schoepe
#
# The GPXconvert is free software; you can redistribute
# it and/or modify it under the terms of the BSD License.
#
# The GPXconvert is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the BSD License along with the
# GPXconvert; see the file LICENSE.
# If not, look at http://www.opensource.org/licenses/bsd-license.html.
#
# Always remember there is NO SUPPORT! eMail: schoepe@users.sourceforge.net
#
# ############################  NO WARRANTY  ##############################
#
# BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
# FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
# OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
# PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
# OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
# TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
# PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
# REPAIR OR CORRECTION.
#
# #######################   KEINE GEWÄHRLEISTUNG  #########################
#
# Da das Programm ohne jegliche Kosten lizenziert wird, besteht
# keinerlei Gewährleistung für das Programm, soweit dies gesetzlich zulässig
# ist. Sofern nicht anderweitig schriftlich bestätigt, stellen die
# Copyright-Inhaber und/oder Dritte das Programm so zur Verfügung, "wie es
# ist", ohne irgendeine Gewährleistung, weder ausdrücklich noch implizit,
# einschließlich - aber nicht begrenzt auf - Marktreife oder Verwendbarkeit
# für einen bestimmten Zweck. Das volle Risiko bezüglich Qualität und
# Leistungsfähigkeit des Programms liegt bei Ihnen. Sollte sich das Programm
# als fehlerhaft herausstellen, liegen die Kosten für notwendigen Service,
# Reparatur oder Korrektur bei Ihnen.
#
# #######################  LIMITATION DE GARANTIE  ########################
#
# Parce que l'utilisation de ce Programme est libre et gratuite, aucune
# garantie n'est fournie, comme le permet la loi. Sauf mention écrite,
# les détenteurs du copyright et/ou les tiers fournissent le Programme en
# l'état, sans aucune sorte de garantie explicite ou implicite, y compris
# les garanties de commercialisation ou d'adaptation dans un but
# particulier. Vous assumez tous les risques quant à la qualité et aux
# effets du Programme. Si le Programme est défectueux, Vous assumez le
# coût de tous les services, corrections ou réparations nécessaires.
#

proc TkPresent {} {
  return [expr {![catch {package present Tk}]}]
}

if {[TkPresent]} {
  package require tile
  package require Img
}

package require msgcat

namespace eval ::Gpx {
  variable options
  variable setup
  variable icon
  variable tags
  variable text
  variable type
  variable tv
  variable Pi
  variable zoom
  variable delta
  variable utm
  variable web
  variable images

  set options(debug) 0
  set options(sccsid,gpxconvert.tcl) {$Revision: 1.56 $ $Date: 2007/01/17 22:32:53 $}
  set options(script,main) gpxconvert
  set options(script,includes) {
  }
  set options(lang) [list {English en normal}]
  set options(info) [subst {GPXconvert\nCopyright 2005-2007 Alexander Schöpe\n$options(sccsid,gpxconvert.tcl)}]

  set setup(export) 1
  set setup(unzip) 1
  set setup(format) {csv tab pathaway}
  set setup(lang) de
  set setup(dconv) km
  set setup(cofmt) dm.m
  set setup(dfmt) %d.%m.%Y
  set setup(pofc) t
  set setup(path) {}
  set setup(gpx,ids) {}
  set setup(center,lat) 0.0
  set setup(center,lon) 0.0
  set setup(center,name) home
  set setup(home,lat) 0.0
  set setup(home,lon) 0.0
  set setup(askquit) 1
  set setup(autosave) 0
  set setup(showhint) 0
  set setup(showplaces) 0
  set setup(download,img) 0
  set setup(sf) 0.5
  set setup(sf,unit) km
  set options(sf) [expr {1 / 0.0254 / $setup(sf)}]
  set options(lb,cols) {gpxid addon group gc wp note}
  set options(selection) {}
  set options(scale) 1.0

  set Pi [expr {acos(-1.0)}]

  set delta 0

  set utm(a) 6378137.0
  set utm(b) 6356752.314
  set utm(sf) 0.9996

  set web(black) #000000
  set web(white) #FFFFFF
  set web(lightgray) #CCCCCC
  set web(gray) #999999
  set web(darkgray) #666666
  set web(blue) #0000FF
  set web(lightblue) #00FFFF
  set web(green) #00FF00
  set web(darkgreen) #33CC00
  set web(yellow) #FFFF33
  set web(red) #FF0000
  set web(orange) #FF9900
  set web(brown) #993300
  set web(background) #D4D0C8
}

# -----------------------------------------------------------------------------

proc Gpx::Cursor { {which {}} } {
  . configure -cursor $which
  foreach w [winfo children .] {
    if {[winfo class $w] == "Toplevel"} {
      $w configure -cursor $which
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Info {} {
  variable options
  global infotext

  set tl .info

  if {[winfo exists $tl]} {
    return
  }

  toplevel $tl
  wm withdraw $tl
  wm title $tl {GPXconvert}
  wm protocol $tl WM_DELETE_WINDOW [subst {destroy $tl}]

  set infotext $options(info)
  append infotext \n\n
  append infotext "THIS PACKAGE IS PROVIDED \"AS IS\" AND WITHOUT ANY EXPRESS OR IMPLIED\n"
  append infotext "WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES\n"
  append infotext "OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE."

  pack [ttk::frame $tl.f -padding 10]
  pack [ttk::label $tl.f.l -width 76 -anchor nw -textvariable infotext -image p_tcltk -compound left]
  pack [ttk::button $tl.f.b -text OK -command [subst {destroy $tl}]] -anchor e

  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::TableColumnsSet { w list } {
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

proc Gpx::MouseWheel { W X Y D } {
  variable delta

  set w [winfo containing -displayof $W $X $Y]
  if {$w != ""} {
    set x [expr {$X - [winfo rootx $w]}]
    set y [expr {$Y - [winfo rooty $w]}]
    set delta $D
    event generate $w <<Wheel>> -rootx $X -rooty $Y -x $x -y $y
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ScanDateTime { scan args } {
  set *v {}
  set f {%Y-%m-%d %H:%M:%S}

  set argc [llength $args]
  for {set argi 0} {$argi < $argc} {incr argi} {
    set opt [lindex $args $argi]
    switch -- $opt {
      -v -
      -var -
      -variable {
        set *v [lindex $args [incr argi]]
      }
      -f -
      -fmt -
      -format {
        set f [lindex $args [incr argi]]
      }
    }
  }

  set d  1
  set m  1
  set y  1970
  set H  0
  set M  0
  set S  0

  set scan [string trim $scan]
  if {[regexp {^(\d+)\.(\d+)\.(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?([+-])?(\d+)?:?(\d+)?$} $scan all d m y H M S F x a b] ||
      [regexp {^(\d+)-(\d+)-(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?([+-])?(\d+)?:?(\d+)?$} $scan all y m d H M S F x a b] ||
      [regexp {^(\d+)/(\d+)/(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?([+-])?(\d+)?:?(\d+)?$} $scan all m d y H M S F x a b]} {
    scan $y %u y

    if {[string is integer -strict $y] && $y >= 0 && $y <= 2038} {
      foreach name {y m d H M S F a b} {
	upvar 0 $name v
	set v [string trimleft $v 0]
        if {![string is integer -strict $v]} {
	  set v 0
	}
      }

      if {$y < 100} {
	if {$y < 50} {
	  incr y 2000
	} else {
	  incr y 1900
	}
      }

      set Y [format %04u $y]
      set y [format %02u $y]
      set m [format %02u $m]
      set d [format %02u $d]
      set H [format %02u $H]
      set M [format %02u $M]
      set S [format %02u $S]

      if {[catch {clock scan ${Y}${m}${d}T${H}${M}${S}} ct]} {
	if {${*v} != ""} {
	  return {}
	}
	return -1
      }

      if {${*v} == ""} {
	return [string map [list %Y $Y %y $y %m $m %d $d %H $H %M $M %S $S] $f]
      }
      upvar ${*v} v
      set v [string map [list %Y $Y %y $y %m $m %d $d %H $H %M $M %S $S] $f]
      return $ct
    }
  }
  if {${*v} != ""} {
    return {}
  }
  return -1
}

# -----------------------------------------------------------------------------

proc Gpx::CompareDateTime { a b } {
  if {[ScanDateTime $a -v a -f %Y%m%d%H%M%S] < 0} {
    set a 9900990099
  }
  if {[ScanDateTime $b -v b -f %Y%m%d%H%M%S] < 0} {
    set b 9900990099
  }

  if {$a < $b} {
    return -1
  } elseif {$a > $b} {
    return 1
  }
  return 0
}

# -----------------------------------------------------------------------------

proc Gpx::CompareDate { a b } {
  if {[ScanDateTime $a -v a -f %Y%m%d] < 0} {
    set a 9900
  }
  if {[ScanDateTime $b -v b -f %Y%m%d] < 0} {
    set b 9900
  }

  if {$a < $b} {
    return -1
  } elseif {$a > $b} {
    return 1
  }
  return 0
}

# -----------------------------------------------------------------------------

proc Gpx::ComparePofC { a b } {
  set a [lsearch -exact {N NNE NE ENE E ESE SE SSE S SSW SW WSW W WNW NW NNW N} $a]
  set b [lsearch -exact {N NNE NE ENE E ESE SE SSE S SSW SW WSW W WNW NW NNW N} $b]

  if {$a < $b} {
    return -1
  } elseif {$a > $b} {
    return 1
  }
  return 0
}

# -----------------------------------------------------------------------------

proc Gpx::ListboxReplace { gpxid } {
  variable options
  variable setup

  set lb $options(lb)

  set idx [ListboxIndex $gpxid]
  if {$idx > -1} {
    $lb delete $idx
    $lb insert $idx [list $gpxid $setup($gpxid,addon) $setup($gpxid,group) $setup($gpxid,gc) $setup($gpxid,wp) $setup($gpxid,note)]
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ListboxIndex { gpxid } {
  variable options
  variable setup

  set lb $options(lb)

  return [lsearch -exact [$lb getcolumns 0] $gpxid]
}

# -----------------------------------------------------------------------------

proc Gpx::ListboxToSetup {} {
  variable options
  variable setup

  set lb $options(lb)
  $lb finishediting

  set setup(gpx,ids) {}
  foreach item [$lb get 0 end] {
    foreach $options(lb,cols) $item {}
    lappend setup(gpx,ids) $gpxid
    set setup($gpxid,addon) $addon
    set setup($gpxid,group) $group
    set setup($gpxid,gc) $gc
    set setup($gpxid,wp) $wp
    set setup($gpxid,note) $note
  }
}

# -----------------------------------------------------------------------------

proc Gpx::SaveIni {} {
  variable options
  variable setup

  set setup(path) [file normalize $setup(path)]
  ListboxToSetup

  if {![catch {open [file join $options(pwd) gpxconvert.ini] w} fd]} {
    puts $fd "array set setup {"
    foreach {n v} [array get setup] {
      puts $fd "  [list $n $v]"
    }
    puts $fd "}"
    close $fd
  } else {
    ttk::dialog .saveini -icon error -buttons ok -title [msgcat::mc Error] -message "[msgcat::mc Error:] $fd"
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Quit { {choose _dialog} } {
  variable options
  variable setup

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
	  -command Gpx::Quit
      } else {
        Gpx::Quit yes
      }
    }
    yes {
      if {$setup(autosave)} {
        SaveIni
      }
      exit 0
    }
    default {
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::PointOfCompass { d } {
  if {![string is double -strict $d]} {
    return {}
  }
  return [lindex {N NNE NE ENE E ESE SE SSE S SSW SW WSW W WNW NW NNW N} [expr {round($d / 22.5)}]]
}

# -----------------------------------------------------------------------------

proc Gpx::DegToRad { deg } {
  variable Pi

  return [expr {$deg / 180.0 * $Pi}]
}

# -----------------------------------------------------------------------------

proc Gpx::RadToDeg { rad } {
  variable Pi

  return [expr {$rad / $Pi * 180.0}]
}

# -----------------------------------------------------------------------------

proc Gpx::ArcLengthOfMeridian { phi } {
  variable utm

  set n [expr {($utm(a) - $utm(b)) / ($utm(a) + $utm(b))}]
  set a [expr {(($utm(a) + $utm(b)) / 2.0) * (1.0 + (pow($n, 2.0) / 4.0) + (pow($n, 4.0) / 64.0))}]
  set b [expr {(-3.0 * $n / 2.0) + (9.0 * pow($n, 3.0) / 16.0) + (-3.0 * pow($n, 5.0) / 32.0)}]
  set g [expr {(15.0 * pow($n, 2.0) / 16.0) + (-15.0 * pow($n, 4.0) / 32.0)}]
  set d [expr {(-35.0 * pow($n, 3.0) / 48.0) + (105.0 * pow($n, 5.0) / 256.0)}]
  set e [expr {(315.0 * pow ($n, 4.0) / 512.0)}]
  set result [expr {$a * ($phi + ($b * sin(2.0 * $phi)) + ($g * sin(4.0 * $phi)) + ($d * sin(6.0 * $phi)) + ($e * sin(8.0 * $phi)))}]

  return $result
}

# -----------------------------------------------------------------------------

proc Gpx::UtmCentralMeridian { zone } {
  return [DegToRad [expr {(-183.0 + ($zone * 6.0))}]]
}

# -----------------------------------------------------------------------------

proc Gpx::FootpointLatitude { y } {
  variable utm

  set n [expr {($utm(a) - $utm(b)) / ($utm(a) + $utm(b))}]
  set a [expr {(($utm(a) + $utm(b)) / 2.0) * (1 + (pow($n, 2.0) / 4.0) + (pow($n, 4.0) / 64.0))}]
  set y_ [expr {$y / $a}]
  set b [expr {(3.0 * $n / 2.0) + (-27.0 * pow($n, 3.0) / 32.0) + (269.0 * pow($n, 5.0) / 512.0)}]
  set g [expr {(21.0 * pow($n, 2.0) / 16.0) + (-55.0 * pow($n, 4.0) / 32.0)}]
  set d [expr {(151.0 * pow($n, 3.0) / 96.0) + (-417.0 * pow($n, 5.0) / 128.0)}]
  set e [expr {(1097.0 * pow($n, 4.0) / 512.0)}]
  set result [expr {$y_ + ($b * sin(2.0 * $y_)) + ($g * sin(4.0 * $y_)) + ($d * sin(6.0 * $y_)) + ($e * sin(8.0 * $y_))}]
    
  return $result
}

# -----------------------------------------------------------------------------

proc Gpx::MapGeoToXy { phi lambda lambda0 } {
  variable utm

  set ep2 [expr {(pow($utm(a), 2.0) - pow($utm(b), 2.0)) / pow($utm(b), 2.0)}]
  set nu2 [expr {$ep2 * pow(cos($phi), 2.0)}]
  set N [expr {pow($utm(a), 2.0) / ($utm(b) * sqrt(1 + $nu2))}]

  set t [expr {tan($phi)}]
  set t2 [expr {$t * $t}]
  set tmp [expr {($t2 * $t2 * $t2) - pow($t, 6.0)}]

  set l [expr {$lambda - $lambda0}]

  set l3c [expr {1.0 - $t2 + $nu2}]
  set l4c [expr {5.0 - $t2 + 9 * $nu2 + 4.0 * ($nu2 * $nu2)}]
  set l5c [expr {5.0 - 18.0 * $t2 + ($t2 * $t2) + 14.0 * $nu2 - 58.0 * $t2 * $nu2}]
  set l6c [expr {61.0 - 58.0 * $t2 + ($t2 * $t2) + 270.0 * $nu2 - 330.0 * $t2 * $nu2}]
  set l7c [expr {61.0 - 479.0 * $t2 + 179.0 * ($t2 * $t2) - ($t2 * $t2 * $t2)}]
  set l8c [expr {1385.0 - 3111.0 * $t2 + 543.0 * ($t2 * $t2) - ($t2 * $t2 * $t2)}]

  set xy(x) [expr {$N * cos($phi) * $l
    + ($N / 6.0 * pow(cos($phi), 3.0) * $l3c * pow($l, 3.0))
    + ($N / 120.0 * pow(cos($phi), 5.0) * $l5c * pow($l, 5.0))
    + ($N / 5040.0 * pow(cos($phi), 7.0) * $l7c * pow($l, 7.0))}]

  set xy(y) [expr {[ArcLengthOfMeridian $phi]
    + ($t / 2.0 * $N * pow(cos($phi), 2.0) * pow($l, 2.0))
    + ($t / 24.0 * $N * pow(cos($phi), 4.0) * $l4c * pow($l, 4.0))
    + ($t / 720.0 * $N * pow(cos($phi), 6.0) * $l6c * pow($l, 6.0))
    + ($t / 40320.0 * $N * pow(cos($phi), 8.0) * $l8c * pow($l, 8.0))}]

  return [array get xy]
}

# -----------------------------------------------------------------------------

proc Gpx::MapXyToGeo { x y lambda0 } {
  variable utm

  set phif [FootpointLatitude $y]
  set ep2 [expr {(pow($utm(a), 2.0) - pow($utm(b), 2.0)) / pow($utm(b), 2.0)}]
  set cf [expr {cos($phif)}]
  set nuf2 [expr {$ep2 * pow($cf, 2.0)}]

  set Nf [expr {pow($utm(a), 2.0) / ($utm(b) * sqrt(1 + $nuf2))}]
  set Nfpow [expr {$Nf}]
	  
  set tf [expr {tan($phif)}]
  set tf2 [expr {$tf * $tf}]
  set tf4 [expr {$tf2 * $tf2}]
  
  set x1f [expr {1.0 / ($Nfpow * $cf)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x2f [expr {$tf / (2.0 * $Nfpow)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x3f [expr {1.0 / (6.0 * $Nfpow * $cf)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x4f [expr {$tf / (24.0 * $Nfpow)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x5f [expr {1.0 / (120.0 * $Nfpow * $cf)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x6f [expr {$tf / (720.0 * $Nfpow)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x7f [expr {1.0 / (5040.0 * $Nfpow * $cf)}]
  
  set Nfpow [expr {$Nfpow * $Nf}]
  set x8f [expr {$tf / (40320.0 * $Nfpow)}]
  
  set x2p [expr {-1.0 - $nuf2}]
  set x3p [expr {-1.0 - 2 * $tf2 - $nuf2}]
  set x4p [expr {5.0 + 3.0 * $tf2 + 6.0 * $nuf2 - 6.0 * $tf2 * $nuf2 - 3.0 * ($nuf2 *$nuf2) - 9.0 * $tf2 * ($nuf2 * $nuf2)}]
  set x5p [expr {5.0 + 28.0 * $tf2 + 24.0 * $tf4 + 6.0 * $nuf2 + 8.0 * $tf2 * $nuf2}]
  set x6p [expr {-61.0 - 90.0 * $tf2 - 45.0 * $tf4 - 107.0 * $nuf2 + 162.0 * $tf2 * $nuf2}]
  set x7p [expr {-61.0 - 662.0 * $tf2 - 1320.0 * $tf4 - 720.0 * ($tf4 * $tf2)}]
  set x8p [expr {1385.0 + 3633.0 * $tf2 + 4095.0 * $tf4 + 1575 * ($tf4 * $tf2)}]

  set philambda(lat) [expr {$phif + $x2f * $x2p * ($x * $x) + $x4f * $x4p * pow($x, 4.0)
    + $x6f * $x6p * pow($x, 6.0) + $x8f * $x8p * pow($x, 8.0)}]
  set philambda(lon) [expr {$lambda0 + $x1f * $x + $x3f * $x3p * pow($x, 3.0)
    + $x5f * $x5p * pow($x, 5.0) + $x7f * $x7p * pow($x, 7.0)}]

  return [array get philambda]
}

# -----------------------------------------------------------------------------

proc Gpx::GeoToUtmXy { lat lon zone } {
  variable utm

  array set xy [MapGeoToXy $lat $lon [UtmCentralMeridian $zone]]

  set xy(x) [expr {$xy(x) * $utm(sf) + 500000.0}]
  set xy(y) [expr {$xy(y) * $utm(sf)}]
  if {$xy(y) < 0.0} {
    set xy(y) [expr {$xy(y) + 10000000.0}]
  }

  return [array get xy]
}

# -----------------------------------------------------------------------------

proc Gpx::UtmXyToGeo { x y zone southhemi } {
  variable utm

  set x [expr {$x - 500000.0}]
  set x [expr {$x / $utm(sf)}]
	  
  if {$southhemi} {
    set y [expr {$y - 10000000.0}]
  }
  set y [expr {$y / $utm(sf)}]
  
  set cmeridian [UtmCentralMeridian $zone]
  return [MapXyToGeo $x $y $cmeridian]
}

# -----------------------------------------------------------------------------

proc Gpx::Utm { lat lon } {
  set zone [expr {floor(($lon + 180.0) / 6) + 1}]
  array set xy [GeoToUtmXy [DegToRad $lat] [DegToRad $lon] $zone]

  if {$xy(x) < 0.0} {
    set neg 1
  } else {
    set neg 0
  }
  set x [SetOrientation xy(x) $neg lon 1]

  if {$xy(x) < 0.0} {
    set neg 1
  } else {
    set neg 0
  }
  set y [SetOrientation xy(y) $neg lat 1]

  if {84.0 >= $lat && $lat >= 72.0} {
    set z X 
  } elseif {72.0 > $lat && $lat >= 64.0} {
    set z W 
  } elseif {64.0 > $lat && $lat >= 56.0} {
    set z V 
  } elseif {56.0 > $lat && $lat >= 48.0} {
    set z U 
  } elseif {48.0 > $lat && $lat >= 40.0} {
    set z T 
  } elseif {40.0 > $lat && $lat >= 32.0} {
    set z S 
  } elseif {32.0 > $lat && $lat >= 24.0} {
    set z R 
  } elseif {24.0 > $lat && $lat >= 16.0} {
    set z Q 
  } elseif {16.0 > $lat && $lat >= 8.0} {
    set z P 
  } elseif {8.0 > $lat && $lat >= 0.0} {
    set z N 
  } elseif {0.0 > $lat && $lat >= -8.0} {
    set z M 
  } elseif {-8.0 > $lat && $lat >= -16.0} {
    set z L 
  } elseif {-16.0 > $lat && $lat >= -24.0} {
    set z K 
  } elseif {-24.0 > $lat && $lat >= -32.0} {
    set z J 
  } elseif {-32.0 > $lat && $lat >= -40.0} {
    set z H 
  } elseif {-40.0 > $lat && $lat >= -48.0} {
    set z G 
  } elseif {-48.0 > $lat && $lat >= -56.0} {
    set z F 
  } elseif {-56.0 > $lat && $lat >= -64.0} {
    set z E 
  } elseif {-64.0 > $lat && $lat >= -72.0} {
    set z D 
  } elseif {-72.0 > $lat && $lat >= -80.0} {
    set z C 
  } else {
    set z Z 
  }

  return [format "%d%s %s %d %s %d" [expr {int($zone)}] $z $x [expr {int($xy(x))}] $y [expr {int($xy(y))}]]
}

# -----------------------------------------------------------------------------

proc Gpx::Acos { x } {
  # protect against rounding error on input argument
  if {[expr {abs($x)}] > 1} {
    set x [expr {$x / abs($x)}]
  }
  return [expr {acos($x)}]
}


proc Gpx::Mod { x y } {
  return [expr {$x - $y * floor($x / $y)}]
}


proc Gpx::ModCrs { x } {
  variable Pi

  return [Mod $x [expr {2 * $Pi}]]
}


proc Gpx::CourseAndDistance { lat1 lon1 lat2 lon2 }  {
  variable setup
  variable Pi

  set a [expr {6378.137 / 1.852}]
  set f [expr {1 / 298.257223563}]

  switch -- $setup(dconv) {
    km {
      set dc 1.852
    }
    sm {
      # 1.150779448 sm
      set dc [expr {185200.0 / 160934.40}]
    }
    ft {
      # 6076.11549 ft
      set dc [expr {185200.0 / 30.48}]
    }
    default {
      # nm
      set dc 1.0
    }
  }

  set eps 0.00000000005
  set iter 1
  set maxiter 100

  set lat1 [expr {($Pi / 180) * $lat1}]
  set lat2 [expr {($Pi / 180) * $lat2}]
  set lon1 [expr {($Pi / 180) * $lon1}]
  set lon2 [expr {($Pi / 180) * $lon2}]

  if {[expr {$lat1 + $lat2}] == 0.0 && [expr {abs($lon1 - $lon2)}] == $Pi} {
    # course and distance between antipodal points is undefined
    set lat1 [expr {$lat1 + 0.00001}] ; # allow algorithm to complete
  }
  if {$lat1 == $lat2 && ($lon1 == $lon2 || [expr {abs(abs($lon1 - $lon2) - 2 * $Pi)}] < $eps)} {
    # points 1 and 2 are identical- course undefined
    set out(d) 0
    set out(c12) {}
    set out(c21) {}
    return [array get out]
  }

  set r [expr {1 - $f}]
  set tu1 [expr {$r * tan($lat1)}]
  set tu2 [expr {$r * tan($lat2)}]
  set cu1 [expr {1.0 / sqrt(1.0 + $tu1 * $tu1)}]
  set su1 [expr {$cu1 * $tu1}]
  set cu2 [expr {1.0 / sqrt(1.0 + $tu2 * $tu2)}]
  set s1 [expr {$cu1 * $cu2}]
  set b1 [expr {$s1 * $tu2}]
  set f1 [expr {$b1 * $tu1}]
  set x [expr {$lon2 - $lon1}]
  set d [expr {$x + 1}] ; # force one pass
  while {[expr {abs($d - $x)}] > $eps && $iter < $maxiter} {
    incr iter
    set sx [expr {sin($x)}]
    set cx [expr {cos($x)}]
    set tu1 [expr {$cu2 * $sx}]
    set tu2 [expr {$b1 - $su1 * $cu2 * $cx}]
    set sy [expr {sqrt($tu1 * $tu1 + $tu2 * $tu2)}]
    set cy [expr {$s1 * $cx + $f1}]
    set y [expr {atan2($sy, $cy)}]
    set sa [expr {$s1 * $sx / $sy}]
    set c2a [expr {1 - $sa * $sa}]
    set cz [expr {$f1 + $f1}]
    if {$c2a > 0.0} {
      set cz [expr {$cy - $cz / $c2a}]
    }
    set e [expr {$cz * $cz * 2.0 - 1.0}]
    set c [expr {((-3.0 * $c2a + 4.0) * $f + 4.0) * $c2a * $f / 16.0}]
    set d $x
    set x [expr {(($e * $cy * $c + $cz) * $sy * $c + $y) * $sa}]
    set x [expr {(1.0 - $c) * $x * $f + $lon2 - $lon1}]
  }
  set faz [ModCrs [expr {atan2($tu1, $tu2)}]]
  set baz [ModCrs [expr {atan2([expr {$cu1 * $sx}], [expr {$b1 * $cx - $su1 * $cu2}]) + $Pi}]]

  set x [expr {sqrt((1 / ($r * $r) - 1) * $c2a + 1)}]
  set x [expr {$x + 1}]
  set x [expr {($x - 2.0) / $x}]
  set c [expr {1.0 - $x}]
  set c [expr {($x * $x / 4.0 + 1.0) / $c}]
  set d [expr {(0.375 * $x * $x - 1.0) * $x}]
  set x [expr {$e * $cy}]
  set s [expr {(((($sy * $sy * 4.0 - 3.0) * (1.0 - $e - $e) * $cz * $d / 6.0 - $x) * $d / 4.0 + $cz) * $sy * $d + $y) * $c * $a * $r}]

  if {[expr {abs($iter - $maxiter)}] < $eps} {
    # error "algorithm did not converge"
  }
  set out(d) [expr {$s * $dc}]
  set out(c12) [expr {$faz * (180 / $Pi)}]
  set out(c21) [expr {$baz * (180 / $Pi)}]

  return [array get out]
}

# -----------------------------------------------------------------------------

proc Gpx::SetOrientation { *d neg type orient } {
  upvar ${*d} d

  if {$orient} {
    switch -- $type {
      lat {
	if {$neg} {
	  return {S }
	} else {
	  return {N }
	}
      }
      lon -
      default {
	if {$neg} {
	  return {W }
	} else {
	  return {E }
	}
      }
    }
  }
  if {$neg} {
    set d [expr {$d * -1.0}]
  }
  return {}
}


proc Gpx::Wgs84 { val { fmt d.d } { type lon } { orient 0 } } {
  set rc [regexp -nocase {([NESW])?\s*([-+]?\d*[\.,]?\d*)[°]?\s*([NESW])?\s*(\d*[\.,]?\d*)[']?\s*(\d*[\.,]?\d*)["]?} $val x h d t m s]

  switch -- $type {
    lat {
      set len 2
    }
    lon -
    default {
      set len 3
    }
  }

  if {[string trim $h] != ""} {
    set o $h
  } elseif {[string trim $t] != ""} {
    set o $t
  } else {
    set o {}
  }

  set dV [string is double -strict $d]
  if {$dV && $d != [expr {floor($d)}]} {
    set dD 1
  } else {
    set dD 0
  }
  set mV [string is double -strict $m]
  if {$mV && $m != [expr {floor($m)}]} {
    set mD 1
  } else {
    set mD 0
  }
  set sV [string is double -strict $s]
  if {$sV && $s != [expr {floor($s)}]} {
    set sD 1
  } else {
    set sD 0
  }

  if { ($dD && $mD) || ($dD && $sD) || ($mD && $sD) || ($dD && $mV) || ($mD && $sV) || (!$dV && $mV) || (!$dV && $sV) || (!$mV && $sV) || !$dV } {
    return {}
  }

  if {$d < 0.0} {
    set neg 1
    set d [expr {abs($d)}]
  } else {
    set neg 0
  }

  switch -- $o {
    N - n { set neg 0 }
    E - e { set neg 0 }
    S - s { set neg 1 }
    W - w { set neg 1 }
  }

  switch -- $fmt {
    dm.m {
      if {!$mV} {
        set m [expr {60.0 * ($d - floor($d))}]
      } elseif {$sV} {
	set m [expr {$m + ($s * (1.0 / 60.0))}]
      }
      if {[format %f $m] == 60.0} {
        set d [expr {$d + 1.0}]
	set m 0
      }
      set o [SetOrientation d $neg $type $orient]
      set d [expr {int($d)}]
      return [format {%s%0*d %06.3f} $o $len $d $m]
    }
    dms -
    dms.s {
      if {!$mV} {
        set mT [expr {60.0 * ($d - floor($d))}]
      } else {
        set mT $m
      }
      if {!$sV} {
	set s [expr {60.0 * ($mT - floor($mT))}]
      }
      set m $mT
      if {[format %f $s] == 60.0} {
	set m [expr {$m + 1.0}]
	set s 0
      }
      if {[format %f $m] == 60.0} {
        set d [expr {$d + 1.0}]
	set m 0
      }
      set o [SetOrientation d $neg $type $orient]
      set m [expr {int($m)}]
      set d [expr {int($d)}]
      switch -- $fmt {
	dms.s {
	  return [format {%s%0*d %02d %06.3f} $o $len $d $m $s]
	}
        default {
	  return [format {%s%0*d %02d %02.f} $o $len $d $m $s]
	}
      }
    }
    d.d -
    default {
      if {$mV} {
	set d [expr {$d + ($m * (1.0 / 60.0))}]
	if {$sV} {
	  set d [expr {$d + ($s * (1.0 / 3660.0))}]
	}
      }
      set o [SetOrientation d $neg $type $orient]
      return [format {%s%f} $o $d]
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::CheckHomeCoords { *var name op } {
  variable setup
  upvar #0 ${*var} var

  set Gpx::tv(home,$name) [Gpx::Wgs84 $var($name) $setup(cofmt) $name 1]
}

# -----------------------------------------------------------------------------

proc Gpx::SettingsClose { w } {
  variable setup
  variable tv

  catch {destroy $w}

  trace remove variable Gpx::tv(lat) write Gpx::CheckHomeCoords
  trace remove variable Gpx::tv(lon) write Gpx::CheckHomeCoords

  foreach name {lat lon} {
    set d [Gpx::Wgs84 $tv(home,$name) d.d]
    if {[string is double -strict $d]} {
      set setup(home,$name) $d
    }
  }

  set changed 0
  foreach name {cofmt dconv pofc dfmt} {
    if {$tv($name) != $setup($name)} {
      set changed 1
    }
  }
  if {$changed} {
    RebuildViews
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Settings {} {
  variable setup
  variable tv

  set tl .settings
  if {[winfo exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl
  wm title $tl [msgcat::mc Settings]
  wm protocol $tl WM_DELETE_WINDOW [list Gpx::SettingsClose $tl]

  set nb $tl.nb

  set Gpx::tv(lat) {}
  set Gpx::tv(lon) {}
  set Gpx::tv(home,lat) [Gpx::Wgs84 $setup(home,lat) $setup(cofmt) lat 1]
  set Gpx::tv(home,lon) [Gpx::Wgs84 $setup(home,lon) $setup(cofmt) lon 1]

  foreach name {cofmt dconv pofc dfmt} {
    set tv($name) $setup($name)
  }

  pack [ttk::notebook $nb -padding 3] -expand 1 -fill both

  set tab $nb.coords
  $nb add [ttk::frame $tab -padding 3] -text [msgcat::mc {Coordinates}] -underline 0
  grid columnconfigure $tab 0 -weight 1
  grid rowconfigure $tab 2 -weight 1

    set f $tab.wgs84
    grid [ttk::labelframe $f -text "WGS84" -padding 3] -row 0 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1
      grid [ttk::radiobutton $f.dd -text {D.DDD°} -variable Gpx::setup(cofmt) -value d.d] -row 0 -column 0 -sticky w
      grid [ttk::radiobutton $f.dmm -text {D° M.MMM'} -variable Gpx::setup(cofmt) -value dm.m] -row 1 -column 0 -sticky w
      grid [ttk::radiobutton $f.dms -text {D° MM' SS"} -variable Gpx::setup(cofmt) -value dms] -row 2 -column 0 -sticky w
      grid [ttk::radiobutton $f.dmss -text {D° MM' SS.SSS"} -variable Gpx::setup(cofmt) -value dms.s] -row 3 -column 0 -sticky w

    set f $tab.home
    grid [ttk::labelframe $f -text [msgcat::mc {Home}] -padding 3] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 3 -weight 1
    grid rowconfigure $f 0 -weight 1
      grid [ttk::label $f.llat -text [msgcat::mc {Latitude} -anchor nw]] -row 0 -column 0 -sticky w
      grid [ttk::entry $f.elat -textvariable Gpx::tv(lat) -width 16] -row 0 -column 1
      grid [ttk::entry $f.hlat -textvariable Gpx::tv(home,lat) -width 16 -state readonly] -row 0 -column 2
      grid [ttk::label $f.llon -text [msgcat::mc {Longitude}]] -row 1 -column 0 -sticky w
      grid [ttk::entry $f.elon -textvariable Gpx::tv(lon) -width 16] -row 1 -column 1
      grid [ttk::entry $f.hlon -textvariable Gpx::tv(home,lon) -width 16 -state readonly] -row 1 -column 2
      grid [ttk::frame $f.f] -row 0 -column 3 -sticky wnes

    grid [ttk::frame $tab.f] -row 2 -column 0 -sticky wnes

  set tab $nb.dch
  $nb add [ttk::frame $tab -padding 3] -text "[msgcat::mc {Distance}]/[msgcat::mc {Direction}]" -underline 0
  grid columnconfigure $tab 0 -weight 1
  grid rowconfigure $tab 3 -weight 1

    set f $tab.dist
    grid [ttk::labelframe $f -text [msgcat::mc {Distance}] -padding 3] -row 0 -column 0 -sticky wnes
    grid columnconfigure $f 3 -weight 1
    grid rowconfigure $f 0 -weight 1
      grid [ttk::radiobutton $f.nm -text nm -variable Gpx::setup(dconv) -value nm] -row 0 -column 0 -sticky w
      grid [ttk::radiobutton $f.km -text km -variable Gpx::setup(dconv) -value km] -row 0 -column 1 -sticky w
      grid [ttk::radiobutton $f.sm -text sm -variable Gpx::setup(dconv) -value sm] -row 0 -column 2 -sticky w
      grid [ttk::radiobutton $f.ft -text ft -variable Gpx::setup(dconv) -value ft] -row 0 -column 3 -sticky w

    set f $tab.crs
    grid [ttk::labelframe $f -text [msgcat::mc {Direction}] -padding 3] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1
      grid [ttk::radiobutton $f.n -text [msgcat::mc {Angle}] -variable Gpx::setup(pofc) -value n] -row 0 -column 0 -sticky w
      grid [ttk::radiobutton $f.t -text [msgcat::mc {Point Of Compass}] -variable Gpx::setup(pofc) -value t] -row 1 -column 0 -sticky w

    set f $tab.calc
    grid [ttk::labelframe $f -text [msgcat::mc {Calculate}] -padding 3] -row 2 -column 0 -sticky wnes
    grid columnconfigure $f 1 -weight 1
    grid rowconfigure $f 0 -weight 1
      grid [ttk::label $f.l -text [msgcat::mc {To Waypoint}]] -row 0 -column 0 -sticky w
      grid [ttk::entry $f.cn -textvariable Gpx::setup(center,name) -width 16 -state readonly] -row 0 -column 1 -sticky w
      grid [ttk::button $f.b -text [msgcat::mc {Home}] -command Gpx::SetAsCenter] -row 0 -column 2

    grid [ttk::frame $tab.f] -row 3 -column 0 -sticky wnes

  set tab $nb.descr
  $nb add [ttk::frame $tab -padding 3] -text [msgcat::mc {Description}] -underline 0
  grid columnconfigure $tab 0 -weight 1
  grid rowconfigure $tab 2 -weight 1

    set f $tab.cache
    grid [ttk::labelframe $f -text [msgcat::mc {Cache}] -padding 3] -row 0 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1
      grid [ttk::checkbutton $f.hint -text "[msgcat::mc Show] [msgcat::mc Hint]" -variable Gpx::setup(showhint)] -row 0 -column 0 -sticky w
      grid [ttk::checkbutton $f.img -text "[msgcat::mc {Download missing images from the Internet}]" -variable Gpx::setup(download,img)] -row 1 -column 0 -sticky w

    set f $tab.display
    grid [ttk::labelframe $f -text [msgcat::mc {Display}] -padding 3] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1
      grid [ttk::checkbutton $f.places -text "[msgcat::mc Show] [msgcat::mc {place names}]" -variable Gpx::setup(showplaces)] -row 0 -column 0 -sticky w

    grid [ttk::frame $tab.f] -row 2 -column 0 -sticky wnes

  set tab $nb.misc
  $nb add [ttk::frame $tab -padding 3] -text [msgcat::mc {Miscellaneous}] -underline 0
  grid columnconfigure $tab 0 -weight 1
  grid rowconfigure $tab 2 -weight 1

    set f $tab.date
    grid [ttk::labelframe $f -text [msgcat::mc {Date}] -padding 3] -row 0 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 0 -weight 1
      grid [ttk::radiobutton $f.i -text [msgcat::mc {YYYY-MM-DD}] -variable Gpx::setup(dfmt) -value %Y-%m-%d] -row 0 -column 0 -sticky w
      grid [ttk::radiobutton $f.d -text [msgcat::mc {DD.MM.YYYY}] -variable Gpx::setup(dfmt) -value %d.%m.%Y] -row 1 -column 0 -sticky w
      grid [ttk::radiobutton $f.s -text [msgcat::mc {MM/DD/YYYY}] -variable Gpx::setup(dfmt) -value %m/%d/%Y] -row 2 -column 0 -sticky w

    set f $tab.wgs84
    grid [ttk::labelframe $f -text [msgcat::mc {Quit}] -padding 3] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 1
      grid [ttk::checkbutton $f.aq -text [msgcat::mc {Ask Before Quit}] -variable Gpx::setup(askquit)] -row 0 -column 0 -sticky w
      grid [ttk::checkbutton $f.as -text [msgcat::mc {Auto Save}] -variable Gpx::setup(autosave)] -row 1 -column 0 -sticky w

    grid [ttk::frame $tab.f] -row 2 -column 0 -sticky wnes

  trace add variable Gpx::tv(lat) write Gpx::CheckHomeCoords
  trace add variable Gpx::tv(lon) write Gpx::CheckHomeCoords

  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::ChooseDirectory {} {
  variable options
  variable setup

  set path [tk_chooseDirectory -initialdir $setup(path) -parent . -title [msgcat::mc {Choose Directory}] -mustexist 1]
  if {[string trim $path] != ""} {
    set setup(path) $path
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Unzip {} {
  variable options
  variable setup

  set lb $options(lb)
  set l $options(l)
  set pb $options(pb)

  foreach item [$lb get 0 end] {
    foreach $options(lb,cols) $item {}

    $pb step
    update
    if {[file exists ${gpxid}.zip]} {
      if {[catch {vfs::zip::Mount ${gpxid}.zip myzip} fd]} {
	set setup($gpxid,note) "[msgcat::mc Unzip]: $fd"
	ListboxReplace $gpxid
	$pb step
	$pb step
      } else {
	set setup($gpxid,note) {}

	set unzip 0
	if {[file exists myzip/${gpxid}.gpx]} {
	  if {![info exists setup($gpxid,fstat,ctime)] || ![string is integer -strict $setup($gpxid,fstat,ctime)]} {
	    set setup($gpxid,fstat,ctime) 0
	  }
	  file stat myzip/${gpxid}.gpx fstat
	  if {$fstat(ctime) != $setup($gpxid,fstat,ctime)} {
	    set unzip 1
	    set setup($gpxid,fstat,ctime) $fstat(ctime)
	  }
	  set setup($gpxid,note) [clock format $setup($gpxid,fstat,ctime) -format {%Y-%m-%d %H:%M:%S}]
	} else {
	  set setup($gpxid,note) {not found}
	}

	if {$unzip && [file exists myzip/${gpxid}.gpx]} {
	  $l configure -text "[msgcat::mc Unzip] ${gpxid}.zip [msgcat::mc to] ${addon}-${gpxid}.gpx"
	  update
	  file copy -force myzip/${gpxid}.gpx ${addon}-${gpxid}.gpx
	}
	$pb step
	update
	if {$unzip && [file exists myzip/${gpxid}-wpts.gpx]} {
	  $l configure -text "Unzip ${gpxid}-wpts.zip - ${addon}-${gpxid}-wpts.gpx"
	  update
	  file copy -force myzip/${gpxid}-wpts.gpx ${addon}-${gpxid}-wpts.gpx
	}

	ListboxReplace $gpxid

	$pb step
	update

	close $fd
      }
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Parse {} {
  variable options
  variable setup
  variable icon
  variable tags
  variable text
  variable type

  set lb $options(lb)
  set l $options(l)
  set pb $options(pb)

  array unset text
  array unset icon
  array unset type

  set idx 0
  foreach item [$lb get 0 end] {
    foreach $options(lb,cols) $item {}

    set tags($group) {}
    set file ${addon}-${gpxid}.gpx

    if {[file exists $file]} {
      $l configure -text "[msgcat::mc Reading] $file"
      $pb step
      update

      set fd [open $file r]
      fconfigure $fd -encoding utf-8

      set doc [dom parse -channel $fd]
      set root [$doc documentElement]
      $root setAttributeNS "" xmlns:my [$root getAttribute xmlns]
      set setup($gpxid,gc) [llength [$root selectNodes {/my:gpx/*[local-name()='wpt']}]]

      foreach wpt [$root getElementsByTagName wpt] {
	foreach item {name desc} {
	  foreach node [$wpt getElementsByTagName $item] {
	    switch -- $item {
	      desc {
		set desc [$node text]
	      }
	      name {
		set name [$node text]
	      }
	    }
	  }
	}
	foreach item [$wpt attributes] {
	  switch -- $item {
	    lon {
	      set lon [$wpt getAttribute $item]
	    }
	    lat {
	      set lat [$wpt getAttribute $item]
	    }
	  }
	}
	switch -glob -- $desc {
	  {*Traditional Cache*} {
	    set type($group,$lon,$lat) T
	    set icon($group,$lon,$lat) GC:traditional
	  }
	  {*Virtual Cache*} {
	    set type($group,$lon,$lat) V
	    set icon($group,$lon,$lat) GC:virtual
	  }
	  {*Unknown Cache*} {
	    set type($group,$lon,$lat) U
	    set icon($group,$lon,$lat) GC:mystery
	  }
	  {*Multi-cache*} {
	    set type($group,$lon,$lat) M
	    set icon($group,$lon,$lat) GC:multi
	  }
	  {*Event Cache*} {
	    set type($group,$lon,$lat) E
	    set icon($group,$lon,$lat) GC:event
	  }
	  {*Letterbox Hybrid*} {
	    set type($group,$lon,$lat) L
	    set icon($group,$lon,$lat) GC:letter
	  }
	  {*Earthcache*} {
	    set type($group,$lon,$lat) EC
	    set icon($group,$lon,$lat) GC:earth
	  }
	  {*Webcam Cache*} {
	    set type($group,$lon,$lat) C
	    set icon($group,$lon,$lat) GC:webcam
	  }
	  {*Locationless (Reverse) Cache*} {
	    set type($group,$lon,$lat) LR
	    set icon($group,$lon,$lat) GC:locationless
	  }
	  default {
	    set type($group,$lon,$lat) ?
	    set icon($group,$lon,$lat) GC:gcc
	  }
	}
	set text($group,$lon,$lat) "[string trim [string map {, . {Traditional Cache } T {Virtual Cache } V {Unknown Cache } ? {Multi-cache } M {Event Cache } E {Letterbox Hybrid } L {Earthcache } EC {Webcam Cache } C {Locationless (Reverse) Cache } LR { by } :} $desc]] [string trim $name]"
      }

      $doc delete
      close $fd
    }

    set file ${addon}-${gpxid}-wpts.gpx

    if {[file exists $file]} {
      $l configure -text "[msgcat::mc Reading] $file"
      $pb step
      update

      set fd [open $file r]
      fconfigure $fd -encoding utf-8

      set doc [dom parse -channel $fd]
      set root [$doc documentElement]
      $root setAttributeNS "" xmlns:my [$root getAttribute xmlns]
      set setup($gpxid,wp) [llength [$root selectNodes {/my:gpx/*[local-name()='wpt']}]]

      foreach wpt [$root getElementsByTagName wpt] {
	foreach item {name desc} {
	  foreach node [$wpt getElementsByTagName $item] {
	    switch -- $item {
	      desc {
		set desc [$node text]
	      }
	      name {
		set name [$node text]
	      }
	    }
	  }
	}
	foreach item [$wpt attributes] {
	  switch -- $item {
	    lon {
	      set lon [$wpt getAttribute $item]
	    }
	    lat {
	      set lat [$wpt getAttribute $item]
	    }
	  }
	}
	set type($group,$lon,$lat) WP
	set icon($group,$lon,$lat) GC:gcc
	set text($group,$lon,$lat) "$desc GC[string range $name 2 end] ([string range $name 0 1])"
      }

      $doc delete
      close $fd
    }

    ListboxReplace $gpxid
    incr idx
  }
}

# -----------------------------------------------------------------------------

proc Gpx::WriteGc16Bmp {} {
  image create photo gc16 -data {
    Qk04AgAAAAAAADYAAAAoAAAAEAAAABAAAAABABAAAAAAAAAAAAASCwAAEgsAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP9//3//f/9//3//f/9//3//f/9/
    /3//f/9//38AAAAA/3+WBpYClgKWAjIG1j7cf6h+KGZIbod+iHr/fwAAAAD/f5YClgK3AtAN
    hAxSPv1/qH4IXmd2h36Hev9/AAAAAP9/lgKWApYGUwYyBjs33H+ofmY9Rjlndod+/38AAAAA
    /3+3AvAFKRGWApYCOjfcf6h+JTFEBAYpR27/fwAAAAD/f7cGaxFsEbcGdgY6O9x/qX5Ibkhq
    KGZocv9/AAAAAP9/vmecb51rvme+Z95z/3+8f9t/23/bf7t//38AAAAA/3/sf09KTW/sf+t/
    9H/ec1M/UzszPxA+0j7/fwAAAAD/f8B/IynhXuB/QG+ue91vyhbJEkkSpBBpEv9/AAAAAP9/
    4H8ERmROw1pkMa973XPsHssWihopGssa/38AAAAA/3+ie6VapRTFOYB37n+9b6kZBhmrGusa
    yxr/fwAAAAD/fyRvCUanOUZOoHvuf31vSB3oFcoayhrKGv9/AAAAAP9/JG9Db8ReJGthc+5/
    3W/sHusWyhrKGsse/38AAAAA/3//f/9//3//f/9//3//f/9//3//f/9//3//fwAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==
  }

  image delete gc16
}
# -----------------------------------------------------------------------------

proc Gpx::Export {} {
  variable options
  variable setup
  variable icon
  variable tags
  variable text
  variable type

  foreach fmt $setup(format) {
    foreach tag [array names tags] {
      file delete -force $tag.asc"
      upvar 0 ${fmt}${tag} fd
      set fd [open ${fmt}-${tag}.asc w]
      fconfigure $fd -encoding iso8859-1
    }
  }

  foreach fmt $setup(format) {
    foreach {n v} [array get text] {
      foreach {tag lon lat} [split $n ,] {}
      upvar 0 ${fmt}${tag} fd
      switch -- $fmt {
	csv {
	  puts $fd [join [list [format %.5f $lon] [format %.5f $lat] \"$v\"] ,]
	}
	tab {
	  puts $fd [join [list [format %.5f $lon] [format %.5f $lat] $v] \t]
	}
	pathaway {
	  puts $fd [join [list $lon $lat $v $icon($n)] \t]
	}
      }
    }
  }

  foreach fmt $setup(format) {
    foreach tag [array names tags] {
      upvar 0 ${fmt}${tag} fd
      close $fd
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Convert {} {
  variable options
  variable setup

  set tl .convert
  if {[info exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl
  Cursor watch
  wm title $tl [msgcat::mc Convert]

  pack [ttk::frame $tl.f -padding 3 -relief groove]
  pack [ttk::frame $tl.f.t -padding 10]
  set l $tl.f.t.l
  pack [ttk::label $l -width 60 -anchor nw -text {} -image p_run32 -compound left]
  pack [ttk::frame $tl.f.b -padding 10] -fill x
  set pb $tl.f.b.pb
  pack [ttk::progressbar $tl.f.b.pb -length 200]

  set options(l) $l
  set options(pb) $pb

  wm deiconify $tl

  ListboxToSetup

  set lb $options(lb)
  $pb configure -maximum [expr {([$lb size] * 5) + [llength setup(format)]}]
  update
  
  cd $setup(path)
  if {$setup(unzip)} {
    Unzip
  }
  if {$setup(export)} {
    Parse
    Export
  }

  Cursor
  destroy $tl
}

# -----------------------------------------------------------------------------

proc Gpx::RowAdd { w row }  {
  variable options
  variable setup

  set tl .convert
  toplevel $tl
  wm withdraw $tl
  Cursor watch
  wm title $tl [msgcat::mc Analyse]

  pack [ttk::frame $tl.f -padding 3 -relief groove]
  pack [ttk::frame $tl.f.t -padding 10]
  set l $tl.f.t.l
  pack [ttk::label $l -width 60 -anchor nw -text {} -image p_run32 -compound left]
  pack [ttk::frame $tl.f.b -padding 10] -fill x
  set pb $tl.f.b.pb
  pack [ttk::progressbar $tl.f.b.pb -length 200]

  set types {{{ZIP Files} {.zip}}}
  set files [tk_getOpenFile -initialdir $setup(path) -parent . -title [msgcat::mc {Choose ZIP File}] -multiple 1 -filetypes $types]
  $pb configure -maximum [llength $files]
  update

  foreach file $files {
    set path [file normalize $setup(path)]
    set file [file normalize $file]

    wm deiconify $tl
    $l configure -text "[msgcat::mc Analysing] $file"
    update

    set gpxid [file rootname [file tail $file]]
    set idx [lsearch -exact $setup(gpx,ids) $gpxid]
    if {$idx > -1} {
      $pb step
      continue
    }

    if {[file exists $file] && [file extension $file] == ".zip" && [file dirname $file] == $path} {
      set name {}
      if {![catch {vfs::zip::Mount $file myzip} mnt]} {
	set file [file join myzip [file rootname [file tail $file]].gpx]
	if {[file exists $file]} {
	  set fd [open $file r]
	  fconfigure $fd -encoding utf-8
	  set doc [dom parse -channel $fd]
	  set root [$doc documentElement]
	  $root setAttributeNS "" xmlns:my [$root getAttribute xmlns]
	  set node [lindex [$root selectNodes {/my:gpx/my:name}] 0]
	  set name [$node text]
	  set gcC [llength [$root selectNodes {/my:gpx/*[local-name()='wpt']}]]
	  $doc delete
	  close $fd
	} else {
	  # gpx file does not exist
	  set gcC 0
	}
	set file [file join myzip [file rootname [file tail $file]]-wpts.gpx]
	if {[file exists $file]} {
	  set fd [open $file r]
	  fconfigure $fd -encoding utf-8
	  set doc [dom parse -channel $fd]
	  set root [$doc documentElement]
	  $root setAttributeNS "" xmlns:my [$root getAttribute xmlns]
	  set wpC [llength [$root selectNodes {/my:gpx/*[local-name()='wpt']}]]
	  $doc delete
	  close $fd
	} else {
	  # gpx file does not exist
	  set wpC 0
	}
	close $mnt
      } else {
	# can not unzip file
      }
      if {$name != ""} {
	lappend setup(gpx,ids) $gpxid
	set setup($gpxid,addon) $name
	set setup($gpxid,group) {}
	set setup($gpxid,gc) $gcC
	set setup($gpxid,wp) $wpC
	set setup($gpxid,note) {}
	if {$w == ""} {
	  set w $options(lb)
	}
	$w insert end [list $gpxid $setup($gpxid,addon) $setup($gpxid,group) $setup($gpxid,gc) $setup($gpxid,wp) $setup($gpxid,note)]
      }
    }
    $pb step
  }

  Cursor
  destroy $tl
}

# -----------------------------------------------------------------------------

proc Gpx::RowDelete { w row } {
  variable setup

  set gpxid [lindex [$w get $row] 0]
  array unset setup $gpxid,*
  set idx [lsearch -exact $setup(gpx,ids) $gpxid]
  if {$idx > -1} {
    set setup(gpx,ids) [lreplace $setup(gpx,ids) $idx $idx]
  }
  $w delete $row
}

# -----------------------------------------------------------------------------

proc Gpx::Popup { W x y X Y} {
  variable options

  foreach {w cx cy} [tablelist::convEventFields $W $x $y] {}

  set row [$w containing $cy]
  if {$row < 0} {
    set state disabled
  } else {
    set state normal
  }

  set m .popup
  if {[winfo exists $m]} {
    destroy $m
  }
  menu $m -tearoff 0
  $m add command -label [msgcat::mc {Add Row}] -image p_insert_table_row -compound left \
    -state normal -command [subst {Gpx::RowAdd $w $row}]
  $m add command -label [msgcat::mc {Delete Row}] -image p_delete_table_row -compound left \
    -state $state -command [subst {Gpx::RowDelete $w $row}]
  $m add separator
  $m add command -label [msgcat::mc {View}] -image p_fileopen -compound left \
    -state $state -command [subst {Gpx::View $w}]
  tk_popup $m $X $Y
}

# -----------------------------------------------------------------------------

proc Gpx::UnsetArray { old new op } {
  if {[regexp {(\.gpx\d*)} $old all array]} {
    unset ::Gpx::$array
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ShowTravelbugs { w id } {
  set tl .travelbugs
  if {[winfo exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl

  set f $tl.f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 1

  set f $tl.f.m
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid columnconfigure $f 1 -weight 0
    grid rowconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 0
    set lb $f.lb
    grid [tablelist::tablelist $lb -height 16 -width 55 -showseparators 1 -titlecolumns 1 \
      -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
    grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
    $lb configure -yscrollcommand "$f.sbv set"
    grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
    $lb configure -xscrollcommand "$f.sbh set"

  TableColumnsSet $lb [subst {
    ref {def {8 [msgcat::mc {Reference}] left}}
    id {def {9 [msgcat::mc {ID}] right} -hide 1}
    name {def {40 [msgcat::mc {Name}] left} -stretchable 1}
  }]

  upvar #0 ::Gpx::[winfo toplevel $w] gpx

  foreach tb $gpx($id,travelbugs) {
    $lb insert end [list $gpx($id,travelbug,$tb,ref) $tb $gpx($id,travelbug,$tb,groundspeak:name)]
  }

  wm title $tl "[msgcat::mc Travelbugs] $id ([$lb size])"
  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::ShowWaypoints { w id } {
  variable setup

  set tl .waypoints
  if {[winfo exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl

  set f $tl.f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 1

  set f $tl.f.m
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid columnconfigure $f 1 -weight 0
    grid rowconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 0
    set lb $f.lb
    grid [tablelist::tablelist $lb -height 16 -width 100 -showseparators 1 -titlecolumns 1 \
      -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
    grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
    $lb configure -yscrollcommand "$f.sbv set"
    grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
    $lb configure -xscrollcommand "$f.sbh set"

  TableColumnsSet $lb [subst {
    name {def {8 [msgcat::mc {Name}] left}}
    latitude {def {11 [msgcat::mc {Latitude}] right}}
    longitude {def {12 [msgcat::mc {Longitude}] right}}
    desc {def {16 [msgcat::mc {Description}] left} -stretchable 1}
    sym {def {8 [msgcat::mc {Symbol}] left}}
    cmt {def {20 [msgcat::mc {Comment}] left}}
  }]

  upvar #0 ::Gpx::[winfo toplevel $w] gpx

  foreach wp $gpx($id,waypoints) {
    $lb insert end [list \
      $wp \
      [Gpx::Wgs84 $gpx($id,waypoint,$wp,lat) $setup(cofmt) lat 1] [Gpx::Wgs84 $gpx($id,waypoint,$wp,lon) $setup(cofmt) lon 1] \
      $gpx($id,waypoint,$wp,desc) $gpx($id,waypoint,$wp,sym) $gpx($id,waypoint,$wp,cmt) \
    ]
  }

  wm title $tl "[msgcat::mc Waypoints] $id ([$lb size])"
  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::ShowLogs { w id } {
  variable setup

  set tl .logs
  if {[winfo exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl

  set f $tl.f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 1

  set f $tl.f.m
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid columnconfigure $f 1 -weight 0
    grid rowconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 0
    set lb $f.lb
    grid [tablelist::tablelist $lb -height 20 -width 100 -showseparators 1 -titlecolumns 1 \
      -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
    grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
    $lb configure -yscrollcommand "$f.sbv set"
    grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
    $lb configure -xscrollcommand "$f.sbh set"

  TableColumnsSet $lb [subst {
    date {def {20 [msgcat::mc {Date}] left} -sortmode command -sortcommand Gpx::CompareDateTime}
    id {def {9 [msgcat::mc {ID}] right} -hide 1}
    finderid {def {9 [msgcat::mc {Finder ID}] right} -hide 1}
    finder {def {9 [msgcat::mc {Finder}] left}}
    text {def {40 [msgcat::mc {Text}] left} -stretchable 1}
    encoded {def {3 [msgcat::mc {encoded}] left}}
    type {def {8 [msgcat::mc {Type}] left}}
  }]

  upvar #0 ::Gpx::[winfo toplevel $w] gpx

  foreach log $gpx($id,logs) {
    if {![info exists gpx($id,log,$log,groundspeak:type)]} {
      set type {}
    } else {
      set type $gpx($id,log,$log,groundspeak:type)
    }
    ScanDateTime $gpx($id,log,$log,groundspeak:date) -format "$setup(dfmt) %H:%M:%S" -variable dt
    $lb insert end [list \
      $dt $log \
      $gpx($id,log,$log,groundspeak:finder,id) $gpx($id,log,$log,groundspeak:finder) \
      $gpx($id,log,$log,groundspeak:text) $gpx($id,log,$log,groundspeak:text,encoded) \
      $type
    ]
  }

  wm title $tl "[msgcat::mc Logs] $id ([$lb size])"
  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::HtmlImageCmd { file args } {
  variable setup
  variable images

  switch -glob -- $file {
    stars_1   { return stars1.gif }
    stars_1.5 { return stars1_5.gif }
    stars_2   { return stars2.gif }
    stars_2.5 { return stars2_5.gif }
    stars_3   { return stars3.gif }
    stars_3.5 { return stars3_5.gif }
    stars_4   { return stars4.gif }
    stars_4.5 { return stars4_5.gif }
    stars_5   { return stars5.gif }

    Traditional*  { return Traditional.gif }
    Virtual*      { return Virtual.gif }
    Unknown*      { return GEO_4boxcolor32x32.gif }
    Multi*        { return Multi-Cache.gif }
    Event*        { return Event.gif }
    Letterbox*    { return Letterbox.gif }
    Earthcache*   { return Earthcache.gif }
    Webcam*       { return Webcam.gif }
    Locationless* { return Locationless.gif }

    TB { return 21.gif }

    poc_N   { return N.gif }
    poc_NNE -
    poc_NE -
    poc_ENE { return NE.gif }
    poc_E   { return E.gif }
    poc_ESE -
    poc_SE -
    poc_SSE { return SE.gif }
    poc_S   { return S.gif }
    poc_SSW -
    poc_SW -
    poc_WSW { return SW.gif }
    poc_W   { return W.gif }
    poc_WNW -
    poc_NW -
    poc_NNW { return NW.gif }

    *icon_smile.gif             { return icon_smile.gif }
    *icon_smile_8ball.gif       { return icon_smile_8ball.gif }
    *icon_smile_angry.gif       { return icon_smile_angry.gif }
    *icon_smile_approve.gif     { return icon_smile_approve.gif }
    *icon_smile_big.gif         { return icon_smile_big.gif }
    *icon_smile_blackeye.gif    { return icon_smile_blackeye.gif }
    *icon_smile_blush.gif       { return icon_smile_blush.gif }
    *icon_smile_clown.gif       { return icon_smile_clown.gif }
    *icon_smile_cool.gif        { return icon_smile_cool.gif }
    *icon_smile_dead.gif        { return icon_smile_dead.gif }
    *icon_smile_disapprove.gif  { return icon_smile_disapprove.gif }
    *icon_smile_evil.gif        { return icon_smile_evil.gif }
    *icon_smile_kisses.gif      { return icon_smile_kisses.gif }
    *icon_smile_question.gif    { return icon_smile_question.gif }
    *icon_smile_sad.gif         { return icon_smile_sad.gif }
    *icon_smile_shock.gif       { return icon_smile_shock.gif }
    *icon_smile_shy.gif         { return icon_smile_shy.gif }
    *icon_smile_sleepy.gif      { return icon_smile_sleepy.gif }
    *icon_smile_tongue.gif      { return icon_smile_tongue.gif }
    *icon_smile_wink.gif        { return icon_smile_wink.gif }

    wp_final       { return flag.jpg }
    wp_nocoords    { return icon_nocoords.jpg }
    wp_notviewable { return icon_notviewable.jpg }
    wp_viewable    { return icon_viewable.jpg }
    wp_parking     { return pkg.jpg }
    wp_question    { return puzzle.jpg }
    wp_stages      { return stage.jpg }
    wp_trailhead   { return trailhead.jpg }
    wp_refpoint    { return waypoint.jpg }

    *big_smile.gif       { return big_smile.gif }
    *coord_update.gif    { return coord_update.gif }
    *dropped_off.gif     { return dropped_off.gif }
    *icon_attended.gif   { return icon_attended.gif }
    *icon_camera.gif     { return icon_camera.gif }
    *icon_disabled.gif   { return icon_disabled.gif }
    *icon_discovered.gif { return icon_discovered.gif }
    *icon_enabled.gif    { return icon_enabled.gif }
    *icon_greenlight.gif { return icon_greenlight.gif }
    *icon_needsmaint.gif { return icon_needsmaint.gif }
    *icon_note.gif       { return icon_note.gif }
    *icon_remove.gif     { return icon_remove.gif }
    *icon_rsvp.gif       { return icon_rsvp.gif }
    *icon_rsvp.gif       { return icon_rsvp.gif }
    *picked_up.gif       { return picked_up.gif }
    *traffic_cone.gif    { return traffic_cone.gif }
    
    default {
      if {$setup(download,img) && [string match -nocase http://* $file]} {
	if {[info exists images($file)]} {
	  return $images($file) 
	}
	if {[catch {image create photo -file $file} image]} {
	  return p_pict
	}
	update
	set images($file) $image
	return $image
      }
      return p_pict
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::HtmlHyperlinkCmd { args } {
  global tcl_platform

  switch -- $tcl_platform(platform) {
    windows {
      catch {dde request FireFox WWW_OpenURL "[lindex $args 0],,"}
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::HtmlIsVisitedCmd { args } {
  return 0
}

# -----------------------------------------------------------------------------

proc Gpx::ShowDescription { w id } {
  variable setup
  variable web
  variable images

  set tl .logs
  if {[winfo exists $tl]} {
    destroy $tl
    foreach {name image} [array get images] {
      image delete $image
      unset images($name)
    }
  }
  toplevel $tl
  wm withdraw $tl

  set e $tl.e
  entry $e
  set bg [$e cget -bg]
  set fg [$e cget -fg]
  destroy $e

  set f $tl.f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid columnconfigure $f 1 -weight 0
  grid rowconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 0
  set html $f.html
  grid [html $html -relief flat -height 480 -width 600 -fg $fg -bg $bg -takefocus 1 \
    -imagecommand Gpx::HtmlImageCmd -hyperlinkcommand Gpx::HtmlHyperlinkCmd -isvisitedcommand Gpx::HtmlIsVisitedCmd \
    ] -row 0 -column 0 -sticky nswe
  grid [ttk::scrollbar $f.sbv -orient vertical -command [list $html yview] -takefocus 0] -row 0 -column 1 -sticky ns
  $html configure -yscrollcommand "$f.sbv set"
  grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $html xview] -takefocus 0] -row 1 -column 0 -sticky we
  $html configure -xscrollcommand "$f.sbh set"

  upvar #0 ::Gpx::[winfo toplevel $w] gpx

  wm title $tl "[msgcat::mc {Please wait!}] / $id"
  wm deiconify $tl

  Cursor watch

  array set dc [CourseAndDistance $setup(home,lat) $setup(home,lon) $gpx($id,lat) $gpx($id,lon)]
  switch -- $setup(pofc) {
    t {
      set angle [PointOfCompass $dc(c12)]
    }
    default {
      if {[string is double -strict $dc(c12)]} {
	set angle [format %.1f $dc(c12)]&deg\;
      } else {
	set angle {}
      }
    }
  }

  $html clear
  $html parse "<p>
    <table border=0>
      <tr>
	<td><img src=$gpx($id,groundspeak:type)></td>
	<td><b>$gpx($id,groundspeak:name)</b> <a href=$gpx($id,url)>$gpx($id,name)</a><br>
	    <small><i>by $gpx($id,groundspeak:placed_by)</i></small></td>
      </tr>
    </table></p>"

  if {$gpx($id,found) || !$gpx($id,available) || $gpx($id,archived)} {
    $html parse "<p>"
    if {$gpx($id,found)} {
      $html parse "<font color=$web(darkgreen)>[msgcat::mc {found}]</font>&nbsp;"
    }
    if {$gpx($id,archived)} {
      $html parse "<font color=$web(red)>[msgcat::mc {archived}]</font>&nbsp;"
    } elseif {!$gpx($id,available)} {
      $html parse "<font color=$web(orange)>[msgcat::mc {temporarily unavailable}]</font>&nbsp;"
    }
    $html parse "</p>"
  }

  $html parse "<p>$gpx($id,groundspeak:type), Container <b>$gpx($id,groundspeak:container)</b>,
    Difficulty <img src=stars_$gpx($id,groundspeak:difficulty)>, Terrain <img src=stars_$gpx($id,groundspeak:terrain)><br>
    [msgcat::mc Hidden] [ScanDateTime $gpx($id,time) -format $setup(dfmt)], [msgcat::mc in] $gpx($id,groundspeak:country),
    [msgcat::mc {Last Log}] [clock format $gpx($id,logs,last) -format $setup(dfmt)]<br>
    WGS-84 <b>[Gpx::Wgs84 $gpx($id,lat) $setup(cofmt) lat 1] [Gpx::Wgs84 $gpx($id,lon) $setup(cofmt) lon 1]</b>,
    UTM [Gpx::Utm $gpx($id,lat) $gpx($id,lon)]<br>
    <img src=poc_[PointOfCompass $dc(c12)]> $angle [format %.2f $dc(d)] $setup(sf,unit) [msgcat::mc {from your home coordinates}]</p>"

  if {$setup(showhint)} {
    if {[string trim $gpx($id,groundspeak:encoded_hints)] != ""} {
      $html parse "<p>[msgcat::mc Hint]<br>&nbsp;&nbsp;[string map {\r\n <br> \r <br> \n <br>} $gpx($id,groundspeak:encoded_hints)]</p>"
    }
  }

  if {[llength $gpx($id,travelbugs)] > 0} {
    $html parse "<p>[msgcat::mc Inventory]"
    foreach tb $gpx($id,travelbugs) {
      $html parse "<br><img src=TB> $gpx($id,travelbug,$tb,groundspeak:name)"
    }
    $html parse "</p>"
  }

  $html parse "<p></p><hr>"

  if {[info exists gpx($id,groundspeak:short_description)] && [string trim $gpx($id,groundspeak:short_description)] != ""} {
    if {$gpx($id,groundspeak:short_description,html)} {
      $html parse <p>$gpx($id,groundspeak:short_description)</p><p></p><hr>
    } else {
      $html parse <p>[string map {\r\n <br> \r <br> \n <br>} $gpx($id,groundspeak:short_description)]</p><p></p><hr>
    }
  }

  if {[info exists gpx($id,groundspeak:long_description)]} {
    if {$gpx($id,groundspeak:long_description,html)} {
      $html parse <p>$gpx($id,groundspeak:long_description)</p>
    } else {
      $html parse <p>[string map {\r\n <br> \r <br> \n <br>} $gpx($id,groundspeak:long_description)]</p>
    }
  }

  if {[llength $gpx($id,waypoints)] > 0} {
    $html parse "<p></p><hr><p></p><p>[msgcat::mc Waypoints]</p><table border=0>"
    foreach wp $gpx($id,waypoints) {
      switch -- $gpx($id,waypoint,$wp,sym) {
	{Final Location}         { set img wp_final }
	{Parking Area}           { set img wp_parking }
	{Question to Answer}     { set img wp_question }
	{Reference Point}        { set img wp_refpoint }
	{Stages of a Multicache} { set img wp_stages }
	{Trailhead}              { set img wp_trailhead }
      }
      $html parse "<tr>
	  <td><img src=wp_viewable></td>
	  <td><img src=$img></td>
	  <td>[string range $wp 0 1]</td>
	  <td>$gpx($id,waypoint,$wp,desc) ($gpx($id,waypoint,$wp,sym))</td>
	  <td>[Gpx::Wgs84 $gpx($id,waypoint,$wp,lat) $setup(cofmt) lat 1] [Gpx::Wgs84 $gpx($id,waypoint,$wp,lon) $setup(cofmt) lon 1]</td>
	</tr>"
      if {[string trim $gpx($id,waypoint,$wp,cmt)] != ""} {
	$html parse "<tr><td colspan=2>Note:</td><td colspan=3>$gpx($id,waypoint,$wp,cmt)</td></tr>"
      }
    }
    $html parse </table>
  }

  foreach log $gpx($id,logs) {
    if {![info exists gpx($id,log,$log,groundspeak:type)]} {
      set type {}
    } else {
      set type $gpx($id,log,$log,groundspeak:type)
    }
    switch -- $type {
      {Archive} -
      {Archive (show)} -
      {Unarchive}                   { set img traffic_cone.gif }
      {Will Attend}                 { set img icon_rsvp.gif }
      {Attended}                    { set img icon_attended.gif }
      {Post Reviewer Note}          { set img big_smile.gif }
      {Didn't find it}              { set img icon_smile_sad.gif }
      {Enable Listing}              { set img icon_enabled.gif }
      {Update Coordinates}          { set img coord_update.gif }
      {Found it}                    { set img icon_smile.gif }
      {Needs Archived}              { set img icon_remove.gif }
      {Temporarily Disable Listing} { set img icon_disabled.gif }
      {Webcam Photo Taken}          { set img icon_camera.gif }
      {Write note}                  { set img icon_note.gif }
      {Needs Maintenance} -
      {Owner Maintenance}           { set img icon_needsmaint.gif }
      {Publish Listing}             { set img icon_greenlight.gif }
    }
    ScanDateTime $gpx($id,log,$log,groundspeak:date) -format $setup(dfmt) -variable dt
    $html parse "<p></p><hr><p><img src=$img> <b>$type</b> $dt $gpx($id,log,$log,groundspeak:finder)<br><br>"
    # {[]} {<>}
    $html parse <p>[string map {
      \r\n <br> \r <br> \n <br>
      {[b]} {<b>} {[/b]} {</b>}
      {[i]} {<i>} {[/i]} {</i>}
      {[u]} {<u>} {[/u]} {</u>}
      {[img]} {<img src="} {[/img]} {">}
      {[:)]} {<img src=icon_smile.gif>}
      {[:D]} {<img src=icon_smile_big.gif>}
      {[8D]} {<img src=icon_smile_cool.gif>}
      {[:I]} {<img src=icon_smile_blush.gif>}
      {[:P]} {<img src=icon_smile_tongue.gif>}
      {[\}:)]} {<img src=icon_smile_evil.gif>}
      {[;)]} {<img src=icon_smile_wink.gif>}
      {[:o)]} {<img src=icon_smile_clown.gif>}
      {[B)]} {<img src=icon_smile_blackeye.gif>}
      {[8]} {<img src=icon_smile_8ball.gif>}
      {[:(]} {<img src=icon_smile_frown.gif>}
      {[8)]} {<img src=icon_smile_shy.gif>}
      {[:O]} {<img src=icon_smile_shocked.gif>}
      {[:(!]} {<img src=icon_smile_angry.gif>}
      {[xx(]} {<img src=icon_smile_dead.gif>}
      {[|)]} {<img src=icon_smile_sleepy.gif>}
      {[:X]} {<img src=icon_smile_kisses.gif>}
      {[^]} {<img src=icon_smile_approve.gif>}
      {[V]} {<img src=icon_smile_disapprove.gif>}
      {[?]} {<img src=icon_smile_question.gif>}
      } $gpx($id,log,$log,groundspeak:text)]</p>
  }

  wm title $tl "[msgcat::mc Description] $id $gpx($id,groundspeak:name)"

  Cursor
}

# -----------------------------------------------------------------------------

proc Gpx::ShowData { which W {x -1} {y -1} } {
  if {$which == "auto"} {
    #set w [tablelist::getTablelistPath $W]
    foreach {w cx cy} [tablelist::convEventFields $W $x $y] {}
    set which [$w columncget [$w containingcolumn $cx] -name]
  } else {
    set w $W
  }

  if {[set sel [lindex [$w curselection] 0]] == ""} {
    return
  }
  set list [$w get $sel]
  set id [lindex $list 0]

  switch -- $which {
    travelbugs {
      ShowTravelbugs $w $id
    }
    wpt {
      ShowWaypoints $w $id
    }
    lastlog {
      ShowLogs $w $id
    }
    gc-name {
      set ::Gpx::tv(wpt) $id
      set ::Gpx::tv(info) ?
    }
    name {
      ShowDescription $w $id
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::SetAsCenter { {w {}} {row {}} } {
  variable setup

  if {[string is integer -strict $row]} {
    upvar #0 ::Gpx::[winfo toplevel $w] gpx

    set id [$w getcells $row,name]
    set setup(center,name) $id
    set setup(center,lat) $gpx($id,lat)
    set setup(center,lon) $gpx($id,lon)
  } else {
    set setup(center,name) home
    set setup(center,lat) $setup(home,lat)
    set setup(center,lon) $setup(home,lon)
  }
  RebuildViews
}

# -----------------------------------------------------------------------------

proc Gpx::PopupCs { W x y X Y} {
  variable options

  foreach {w cx cy} [tablelist::convEventFields $W $x $y] {}

  set row [$w containing $cy]
  if {$row < 0} {
    set state disabled
    set label [msgcat::mc {Set As Center}]
  } else {
    set state normal
    set label "[$w getcells $row,name] [msgcat::mc {As Center}]"
  }

  set m .popup
  if {[winfo exists $m]} {
    destroy $m
  }
  menu $m -tearoff 0
  $m add command -label $label -state normal -state $state -command [subst {Gpx::SetAsCenter $w $row}]
  $m add command -label [format [msgcat::mc {Home As Center}] $row] -command [subst {Gpx::SetAsCenter $w {}}]
  $m add separator
  $m add command -label "[msgcat::mc Show] [msgcat::mc Description]" -command [subst {Gpx::ShowData name $w}]
  tk_popup $m $X $Y
}

# -----------------------------------------------------------------------------

proc Gpx::InsertView { w *gpx id } {
  variable setup

  upvar ${*gpx} gpx

  if {$gpx($id,found)} {
    set found F
  } else {
    set found {}
  }
  if {$gpx($id,available)} {
    set available {}
  } else {
    set available U
  }
  if {$gpx($id,archived)} {
    set archived A
  } else {
    set archived {}
  }
  set travelbugs [llength $gpx($id,travelbugs)]
  if {$travelbugs == 0} {
    set travelbugs {}
  }
  set waypoints [llength $gpx($id,waypoints)]
  if {$waypoints == 0} {
    set waypoints {}
  }
  array set dc [CourseAndDistance $setup(center,lat) $setup(center,lon) $gpx($id,lat) $gpx($id,lon)]
  switch -- $setup(pofc) {
    t {
      set angle [PointOfCompass $dc(c12)]
    }
    default {
      if {[string is double -strict $dc(c12)]} {
	set angle [format %.1f $dc(c12)]
      } else {
	set angle {}
      }
    }
  }
  set lastlog {}
  $w insert end [list \
    $gpx($id,name) $gpx($id,id) $gpx($id,groundspeak:name) $gpx($id,groundspeak:owner) \
    [Gpx::Wgs84 $gpx($id,lat) $setup(cofmt) lat 1] [Gpx::Wgs84 $gpx($id,lon) $setup(cofmt) lon 1] \
    $gpx($id,groundspeak:type) $gpx($id,groundspeak:container) $gpx($id,groundspeak:difficulty) $gpx($id,groundspeak:terrain) \
    [clock format $gpx($id,logs,last)  -format $setup(dfmt)] $travelbugs $found $available $archived $waypoints [format %.2f $dc(d)] $angle \
  ]
}

# -----------------------------------------------------------------------------

proc Gpx::RebuildViews {} {
  Cursor watch
  ttk::dialog .msgRv -icon info -message [msgcat::mc {Please wait!}]\n\n[msgcat::mc {Updating Data ...}]
  update
  foreach w [winfo children .] {
    if {[winfo class $w] == "Toplevel" && [regexp {(\.gpx\d*)} $w all name]} {
      if {[winfo exists $name.f.m.lb] && [winfo class $name.f.m.lb] == "Tablelist"} {
	upvar #0 ::Gpx::[winfo toplevel $w] gpx

        set w $name.f.m.lb
	set list [$w getcolumns name]
	$w delete 0 end
	foreach id $list {
	  InsertView $w gpx $id
	}
      }
    }
  }
  destroy .msgRv
  Cursor
}

# -----------------------------------------------------------------------------

proc Gpx::View { W } {
  variable options
  variable setup
  global tcl_platform

  if {[winfo class $W] == "Tablelist"} {
    set w $W
  } else {
    set w [tablelist::getTablelistPath $W]
  }

  if {[set sel [lindex [$w curselection] 0]] == ""} {
    return
  }

  set item [$w get $sel]
  foreach $options(lb,cols) $item {}
  set file ${addon}-${gpxid}.gpx

  cd $setup(path)

  if {![file exists $file]} {
    ttk::dialog .warning -icon warning -buttons ok -title [msgcat::mc Warning] -message "[msgcat::mc {GPX file not converted, please convert first.}]"
    return
  }

  set tl .gpx${gpxid}
  if {[winfo exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl
  Cursor watch

  ttk::dialog .msg -icon info -message [msgcat::mc {Please wait!}]\n\n[msgcat::mc {Loading Data ...}]
  update

  set f $tl.f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 0 -weight 1

  set f $tl.f.m
  grid [ttk::frame $f -padding 3 -relief groove] -row 0 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid columnconfigure $f 1 -weight 0
    grid rowconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 0
    set lb $f.lb
    grid [tablelist::tablelist $lb -height 26 -width 140 -showseparators 1 -titlecolumns 1 \
      -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
    grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
    $lb configure -yscrollcommand "$f.sbv set"
    grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
    $lb configure -xscrollcommand "$f.sbh set"

  switch -- $tcl_platform(os) {
    Windows {
      bind [$lb bodytag] <ButtonPress-2> "Gpx::PopupCs %W %x %y %X %Y"
    }
    default {
      bind [$lb bodytag] <ButtonPress-3> "Gpx::PopupCs %W %x %y %X %Y"
    }
  }

  TableColumnsSet $lb [subst {
    name {def {8 [msgcat::mc {Name}] left}}
    id {def {9 [msgcat::mc {ID}] right} -hide 1}
    gc-name {def {18 [msgcat::mc {GC Name}] left} -stretchable 1}
    gc-owner {def {8 [msgcat::mc {GC Owner}] left}}
    latitude {def {11 [msgcat::mc {Latitude}] right}}
    longitude {def {12 [msgcat::mc {Longitude}] right}}
    gc-type {def {5 [msgcat::mc {GC Type}] left}}
    gc-container {def {5 [msgcat::mc {GC Container}] left}}
    gc-difficulty {def {4 [msgcat::mc {GC Difficulty}] right} -sortmode real}
    gc-terrain {def {4 [msgcat::mc {GC Terrain}] right} -sortmode real}
    lastlog {def {10 [msgcat::mc {Last Log}] left} -sortmode command -sortcommand Gpx::CompareDate}
    travelbugs {def {4 [msgcat::mc {Travelbugs}] right}}
    found {def {2 [msgcat::mc {Found}] left}}
    available {def {2 [msgcat::mc {Available}] left}}
    archived {def {2 [msgcat::mc {Archived}] left}}
    wpt {def {4 [msgcat::mc {Waypoints}] right}}
    distance {def {6 [msgcat::mc {Distance}] right} -sortmode real}
    direction {def {5 [msgcat::mc {Direction}] left} -sortmode command -sortcommand Gpx::ComparePofC}
  }]
  bind [$lb bodytag] <Double-1> "Gpx::ShowData auto %W %x %y"

  set f $tl.f.b
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky we
    grid [ttk::button $f.t -text "[msgcat::mc Show] [msgcat::mc Travelbugs]" -command "Gpx::ShowData travelbugs $lb"] -row 0 -column 0
    grid [ttk::button $f.w -text "[msgcat::mc Show] [msgcat::mc Waypoints]" -command "Gpx::ShowData wpt $lb"] -row 0 -column 1
    grid [ttk::button $f.l -text "[msgcat::mc Show] [msgcat::mc Logs]" -command "Gpx::ShowData lastlog $lb"] -row 0 -column 2
    grid [ttk::button $f.g -text "[msgcat::mc {Graphic Display}]" -command "Gpx::GraphicWaypoints $lb"] -row 0 -column 3
    grid [ttk::button $f.s -text "[msgcat::mc Search] [msgcat::mc Waypoint]" -command "Gpx::ShowData gc-name $lb"] -row 0 -column 4
    grid [ttk::button $f.d -text "[msgcat::mc Show] [msgcat::mc Description]" -command "Gpx::ShowData name $lb"] -row 0 -column 5

  trace add command $lb delete Gpx::UnsetArray
  upvar #0 ::Gpx::[winfo toplevel $tl] gpx

  set gpx(id) $gpxid

  #
  # /gpx
  #   ./name TEXT
  #   ./desc TEXT
  #   ./author TEXT
  #   ./email TEXT
  #   ./time 0000-00-00T00:00:00.0000000-07:00
  #   ./keywords TEXT
  #   ./bounds minlat 0.00000 minlon 0.00000 maxlat 0.00000 maxlon 0.00000
  # /gpx/wpt = lat 0.00000, lon 0.00000
  #   ./time = 0000-00-00T00:00:00.0000000-07:00
  #   ./name = GCxxxx
  #   ./desc = TEXT
  #   ./url = http://www.geocaching.com/seek/cache_details.aspx?guid=00000000-0000-0000-0000-000000000000
  #   ./urlname = TEXT
  #   ./sym = Geocache
  #   ./type = Geocache|Multi-cache
  # /gpx/wpt/groundspeak:cache
  #   ./groundspeak:name TEXT
  #   ./groundspeak:placed_by TEXT
  #   ./groundspeak:owner id 0000 TEXT
  #   ./groundspeak:type TEXT
  #   ./groundspeak:container TEXT
  #   ./groundspeak:difficulty TEXT
  #   ./groundspeak:terrain TEXT
  #   ./groundspeak:country TEXT
  #   ./groundspeak:state TEXT
  #   ./groundspeak:short_description = html True TEXT
  #   ./groundspeak:long_description = html True TEXT
  #   ./groundspeak:encoded_hints TEXT
  # /gpx/wpt/groundspeak:cache/groundspeak:logs
  #   ./groundspeak:log = id 0000
  #     ./groundspeak:date = 0000-00-00T00:00:00
  #     ./groundspeak:type = Publish Listing
  #     ./groundspeak:finder = 2
  #     ./groundspeak:text = encoded False TEXT
  # /gpx/wpt/groundspeak:cache/groundspeak:travelbugs
  #   ./groundspeak:travelbug = id 0000, ref TBxxxx
  #     ./groundspeak:name = TEXT
  #

  set count 0

  set fd [open $file r]
  fconfigure $fd -encoding utf-8

  set doc [dom parse -channel $fd]
  set root [$doc documentElement]
  $root setAttributeNS "" xmlns:my [$root getAttribute xmlns]

  foreach node [$root selectNodes {/my:gpx/*[local-name()!='wpt']}] {
    switch -- [$node nodeName] {
      bounds {
	set gpx([$node nodeName]) [$node text]
	foreach item {minlat minlon maxlat maxlon} {
	  set gpx([$node nodeName],$item) [$node getAttribute $item]
	}
      }
      default {
	set gpx([$node nodeName]) [$node text]
      }
    }
  }

  foreach node [$root getElementsByTagName wpt] {
    set wpt(lat) [$node getAttribute lat]
    set wpt(lon) [$node getAttribute lon]
    foreach node [$node childNodes] {
      switch -- [$node nodeName] {
	sym {
	  set wpt([$node nodeName]) [$node text]
	  set wpt(found) [string match -nocase *found* [$node text]]
	}
	groundspeak:cache {
	  set wpt(id) [$node getAttribute id]
	  set wpt(available) [$node getAttribute available]
	  set wpt(archived) [$node getAttribute archived]
	  set wpt(logs) {}
	  set wpt(logs,last) 0
	  set wpt(travelbugs) {}
	  foreach node [$node childNodes] {
	    switch -- [$node nodeName] {
	      groundspeak:short_description -
	      groundspeak:long_description {
	        set wpt([$node nodeName]) [$node text]
	        set wpt([$node nodeName],html) [$node getAttribute html]
	      }
	      groundspeak:encoded_hints {
	        set wpt([$node nodeName]) [$node text]
	      }
	      groundspeak:logs {
		foreach node [$node childNodes] {
		  set id [$node getAttribute id]
		  lappend wpt(logs) $id
		  foreach node [$node childNodes] {
		    switch -- [$node nodeName] {
		      groundspeak:date {
		        set wpt(log,$id,[$node nodeName]) [$node text]
			set ct [clock scan [$node text]]
			if {$ct > $wpt(logs,last)} {
			  set wpt(logs,last) $ct
			}
		      }
		      groundspeak:text {
		        set wpt(log,$id,[$node nodeName]) [$node text]
		        set wpt(log,$id,[$node nodeName],encoded) [$node getAttribute encoded]
		      }
		      groundspeak:finder {
		        set wpt(log,$id,[$node nodeName]) [$node text]
		        set wpt(log,$id,[$node nodeName],id) [$node getAttribute id]
		      }
		      default {
		        set wpt(log,$id,[$node nodeName]) [$node text]
		      }
		    }
		  }
		}
	      }
	      groundspeak:travelbugs {
		foreach node [$node childNodes] {
		  set id [$node getAttribute id]
		  lappend wpt(travelbugs) $id
		  set wpt(travelbug,$id,ref) [$node getAttribute ref]
		  foreach node [$node childNodes] {
		    switch -- [$node nodeName] {
		      default {
		        set wpt(travelbug,$id,[$node nodeName]) [$node text]
		      }
		    }
		  }
		}
	      }
	      default {
	        set wpt([$node nodeName]) [$node text]
	      }
	    }
	  }
	}
	default {
	  set wpt([$node nodeName]) [$node text]
	}
      }
    }

    lappend gpx(names) $wpt(name)
    set gpx($wpt(name),waypoints) {}
    foreach {n v} [array get wpt] {
      set gpx($wpt(name),$n) $v
    }
    array unset wpt

    incr count
  }

  $doc delete
  close $fd

  set file ${addon}-${gpxid}-wpts.gpx
  if {[file exists $file]} {
    set fd [open $file r]
    fconfigure $fd -encoding utf-8

    set doc [dom parse -channel $fd]
    set root [$doc documentElement]
    $root setAttributeNS "" xmlns:my [$root getAttribute xmlns]

    foreach node [$root getElementsByTagName wpt] {
      set wpt(lat) [$node getAttribute lat]
      set wpt(lon) [$node getAttribute lon]
      foreach node [$node childNodes] {
	switch -- [$node nodeName] {
	  default {
	    set wpt([$node nodeName]) [$node text]
	  }
	}
      }
      set tag GC[string range $wpt(name) 2 end]
      lappend gpx($tag,waypoints) $wpt(name)
      foreach {n v} [array get wpt] {
	set gpx($tag,waypoint,$wpt(name),$n) $v
      }
      array unset wpt
    }

    $doc delete
    close $fd
  }

  foreach id $gpx(names) {
    InsertView $lb gpx $id
  }

  destroy .msg
  wm title $tl "[msgcat::mc GPX] $gpx(id) $gpx(name) ([$lb size])"
  Cursor
  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::Zoom { w factor {xcenter {}} {ycenter {}} {winxlength {}} {winylength {}} } {
  variable options
  variable Pi

  if {$xcenter == ""} {
    set winxlength [winfo width $w]
    set winylength [winfo height $w]
    set xcenter [$w canvasx [expr {$winxlength / 2.0}]]
    set ycenter [$w canvasy [expr {$winylength / 2.0}]]
  }

  set options(scale) [expr {$options(scale) * $factor}]

  set xcenter [expr {$xcenter * $factor}]
  set ycenter [expr {$ycenter * $factor}]

  foreach item [$w find all] {
    set tags [$w gettags $item]

    regexp {d=(-?\d*\.?\d*)\s*} $tags all distance
    regexp {c=(-?\d*\.?\d*)\s*} $tags all course

    if {$distance == 0.0} {
      set x 0
      set y 0
    } else {
      set x [expr {(sin($course * ($Pi / 180)) * $distance) * $options(sf) * $options(scale)}]
      set y [expr {(cos($course * ($Pi / 180)) * $distance) * $options(sf) * $options(scale)}]
    }

    switch -exact [$w type $item] {
      oval {
	$w coords $item [Oval $x $y]
      }
      text {
	$w coords $item [X $x] [Y $y]
      }
    }
  }

  set x0 1.0e30
  set x1 -1.0e30
  set y0 1.0e30
  set y1 -1.0e30
  foreach item [$w find all] {
    switch -exact [$w type $item] {
      arc -
      line -
      oval -
      polygon -
      rectangle {
	set coords [$w coords $item]
	foreach {x y} $coords {
	  if {$x < $x0} {set x0 $x}
	  if {$x > $x1} {set x1 $x}
	  if {$y < $y0} {set y0 $y}
	  if {$y > $y0} {set y1 $y}
	}
      }
    }
  }

  set xlength [expr {$x1 - $x0}]
  set ylength [expr {$y1 - $y0}]

  foreach {ax0 ay0 ax1 ay1} [$w bbox all] {}

  while { $ax0 < $x0 || $ay0 < $y0 || $ax1 > $x1 || $ay1 > $y1 } {
    set x0 [expr {$x0 - $xlength}]
    set x1 [expr {$x1 + $xlength}]
    set y0 [expr {$y0 - $ylength}]
    set y1 [expr {$y1 + $ylength}]
    set xlength [expr {$xlength * 3.0}]
    set ylength [expr {$ylength * 3.0}]
  }

  set newxleft [expr {($xcenter - $x0 - ($winxlength / 2.0)) / $xlength}]
  set newytop  [expr {($ycenter - $y0 - ($winylength / 2.0)) / $ylength}]
  $w configure -scrollregion [list $x0 $y0 $x1 $y1]
  $w xview moveto $newxleft
  $w yview moveto $newytop

  Scrollregion $w
}

# -----------------------------------------------------------------------------

proc Gpx::ZoomMark {w x y} {
  variable zoom

  set zoom(x0) [$w canvasx $x]
  set zoom(y0) [$w canvasy $y]
  $w create rectangle $x $y $x $y -outline yellow -tag zoom
}

# -----------------------------------------------------------------------------

proc Gpx::ZoomStroke {w x y} {
  variable zoom

  set zoom(x1) [$w canvasx $x]
  set zoom(y1) [$w canvasy $y]
  $w coords zoom $zoom(x0) $zoom(y0) $zoom(x1) $zoom(y1)
}

# -----------------------------------------------------------------------------

proc Gpx::ZoomArea {w x y} {
  variable zoom

  set zoom(x1) [$w canvasx $x]
  set zoom(y1) [$w canvasy $y]
  $w delete zoom

  if {$zoom(x0) == $zoom(x1) || $zoom(y0) == $zoom(y1)} {
    return
  }

  set areaxlength [expr {abs($zoom(x1) - $zoom(x0))}]
  set areaylength [expr {abs($zoom(y1) - $zoom(y0))}]
  set xcenter [expr {($zoom(x0) + $zoom(x1)) / 2.0}]
  set ycenter [expr {($zoom(y0) + $zoom(y1)) / 2.0}]

  set winxlength [winfo width $w]
  set winylength [winfo height $w]

  set xscale [expr {$winxlength / $areaxlength}]
  set yscale [expr {$winylength / $areaylength}]
  if { $xscale > $yscale } {
    set factor $yscale
  } else {
    set factor $xscale
  }

  Zoom $w $factor $xcenter $ycenter $winxlength $winylength
}

# -----------------------------------------------------------------------------

proc Gpx::WaypointInfo { w } {
  if {![winfo exists $w]} {
    return
  }

  set id [$w find withtag current]
  set tags [$w gettags $id]

  regexp {wpt=(\w*)\s*} $tags all wpt
  regexp {gpx=(\d*)\s*} $tags all gpxid

  upvar #0 ::Gpx::.gpx$gpxid gpx

  set ::Gpx::tv(wpt) $wpt
  if {[info exists gpx($wpt,groundspeak:name)]} {
    set ::Gpx::tv(info) $gpx($wpt,groundspeak:name)
  } else {
    set id GC[string range $wpt 2 end]
    if {[info exists gpx($id,waypoint,$wpt,desc)]} {
      set ::Gpx::tv(info) "$id: $gpx($id,waypoint,$wpt,desc)"
    }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Scrollregion { w } {
  update
  foreach {x1 y1 x2 y2} [$w bbox all] {}

  set x1 [expr {$x1 - 50.0}] 
  set y1 [expr {$y1 - 50.0}] 
  set x2 [expr {$x2 + 50.0}] 
  set y2 [expr {$y2 + 50.0}] 

  $w configure -scrollregion "$x1 $y1 $x2 $y2"
}

# -----------------------------------------------------------------------------

proc Gpx::Center { w {name {}} } {
  variable tv

  if {$name == "" && [info exists tv(wpt)]} {
    set name $tv(wpt)
  }

  update

  set x {}
  set y {}
  foreach id [$w find withtag wpt=$name] {
    if {[$w type $id] == "text"} {
      foreach {x y} [$w coords $id] {}
      break
    }
  }

  if {$x == "" || $y == ""} {
    return
  }

  foreach {x1 y1 x2 y2} [$w cget -scrollregion] {}

  set vx [winfo width $w]
  set sx [expr {$x2 + $x1 * -1.0}]
  set t2x [expr {$vx / $sx / 2.0}]
  set vy [winfo height $w]
  set sy [expr {$y2 + $y1 * -1.0}]
  set t2y [expr {$vy / $sy / 2.0}]

  $w xview moveto [expr {($x + $x1 * -1.0) / $sx - $t2x}]
  $w yview moveto [expr {($y + $y1 * -1.0) / $sy - $t2y}]
}

# -----------------------------------------------------------------------------

proc Gpx::X { x } {
  return $x
}

proc Gpx::Y { y } {
  return [expr {$y * -1.0}]
}

# -----------------------------------------------------------------------------

proc Gpx::Oval { x y } {
  set d 6.0
  set x1 [expr {$x - $d / 2}]
  set y1 [expr {$y - $d / 2}]
  set x2 [expr {$x + $d / 2}]
  set y2 [expr {$y + $d / 2}]
  return [list [X $x1] [Y $y1] [X $x2] [Y $y2]]
}

# -----------------------------------------------------------------------------

proc Gpx::Point { w gpx wpt text distance course type } {
  variable options
  variable Pi

  switch -- $type {
    home {
      set color aquamarine3
    }
    additional {
      set color aquamarine2
    }
    geoname {
      set color orange
    }
    default {
      set color aquamarine1
    }
  }

  if {$distance == 0.0} {
    set x 0
    set y 0
    set color aquamarine3
  } else {
    set x [expr {(sin($course * ($Pi / 180)) * $distance) * $options(sf)}]
    set y [expr {(cos($course * ($Pi / 180)) * $distance) * $options(sf)}]
  }

  set i [$w create oval [Oval $x $y]]
  $w itemconfig $i -fill $color -activefill orange -tag [list point wpt=$wpt gpx=$gpx d=$distance c=$course]
  set i [$w create text [X $x] [Y $y] -text "  $text" -anchor w]
  $w itemconfig $i -tag [list name wpt=$wpt gpx=$gpx d=$distance c=$course]
  
  if {$type == "home"} {
    $w addtag home withtag wpt=$wpt
  }
  if {$distance == 0.0} {
    $w addtag center withtag wpt=$wpt
  }
}

# -----------------------------------------------------------------------------

proc Gpx::GraphicWaypoints { w } {
  variable options
  variable setup
  variable tv

  set size 512
  set options(sf) [expr {1 / 0.0254 / $setup(sf)}]

  set tl .graphicwpt
  if {[winfo exists $tl]} {
    destroy $tl
  }
  toplevel $tl
  wm withdraw $tl
  Cursor watch

  set tv(wpt) home
  set tv(text) {}
  set options(scale) 1.0

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

    set c $f.c
    grid [canvas $c -width $size -height $size -borderwidth 0 -highlightthickness 0] \
      -row 0 -column 0 -sticky nswe
    grid [ttk::scrollbar $f.sbh -orient horizontal -command "$c xview"] -row 1 -column 0 -sticky we
    $c configure -xscrollcommand "$f.sbh set"
    grid [ttk::scrollbar $f.sbv -orient vertical -command "$c yview"] -row 0 -column 1 -sticky ns
    $c configure -yscrollcommand "$f.sbv set"

  set f $tl.f.b
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky wnes
  grid columnconfigure $f 3 -weight 1
  grid rowconfigure $f 0 -weight 1
    grid [ttk::label $f.lw -text [msgcat::mc {Waypoint} -anchor nw]] -row 0 -column 0 -sticky w
    grid [ttk::entry $f.ew -textvariable Gpx::tv(wpt) -width 10] -row 0 -column 1
    grid [ttk::label $f.lt -text [msgcat::mc {Info}]] -row 0 -column 2 -sticky w
    grid [ttk::entry $f.et -textvariable Gpx::tv(info) -width 32 -state readonly] -row 0 -column 3 -sticky we
    grid [ttk::button $f.m -text - -width 2 -command "Gpx::Zoom $c 0.8"] -row 0 -column 4
    grid [ttk::button $f.p -text + -width 2 -command "Gpx::Zoom $c 1.25"] -row 0 -column 5
    grid [ttk::button $f.b -text [msgcat::mc Search] -command "Gpx::Center $c"] -row 0 -column 6

  bind $tl <MouseWheel> {Gpx::MouseWheel %W %X %Y %D}
  bind $c <<Wheel>> {%W yview scroll [expr {int(pow($Gpx::delta / -120,3))}] units}

  upvar #0 ::Gpx::[winfo toplevel $w] gpx

  set maxD 0
  set cnt 0
  array set dc [CourseAndDistance $setup(center,lat) $setup(center,lon) $setup(home,lat) $setup(home,lon)]
  Point $c $gpx(id) home Home $dc(d) $dc(c12) home
  if {$dc(d) > $maxD} {
    set maxD $dc(d)
  }
  incr cnt
  foreach id $gpx(names) {
    array set dc [CourseAndDistance $setup(center,lat) $setup(center,lon) $gpx($id,lat) $gpx($id,lon)]
    Point $c $gpx(id) $id "$id $gpx($id,groundspeak:name)" $dc(d) $dc(c12) standard
    incr cnt
    if {$dc(d) > $maxD} {
      set maxD $dc(d)
    }
    foreach wp $gpx($id,waypoints) {
      array set dc [CourseAndDistance $setup(center,lat) $setup(center,lon) $gpx($id,waypoint,$wp,lat) $gpx($id,waypoint,$wp,lon)]
      Point $c $gpx(id) $wp "$id $gpx($id,waypoint,$wp,desc)" $dc(d) $dc(c12) additional
      incr cnt
    }
  }

  # findNearbyPostalCodes
  # findNearbyPlaceName
  if {$dc(d) > 50} {
    set dc(d) 50
  }
  puts stderr "http://ws.geonames.org/findNearbyPostalCodes/[http::formatQuery lat $setup(center,lat) lng $setup(center,lon) radius $dc(d) maxRows 500 style SHORT]"
  set http [http::geturl http://ws.geonames.org/findNearbyPostalCodes \
    -query [http::formatQuery lat $setup(center,lat) lng $setup(center,lon) radius $dc(d) maxRows 500 style SHORT] ]
  if {[::http::status $http] == "ok"} {
    set doc [dom parse [http::data $http]]
    set root [$doc documentElement]
    foreach geoname [$root getElementsByTagName code] {
      set name [[$geoname getElementsByTagName name] text]
      set postalcode [[$geoname getElementsByTagName postalcode] text]
      set lat [[$geoname getElementsByTagName lat] text]
      set lon [[$geoname getElementsByTagName lng] text]
      array set dc [CourseAndDistance $setup(center,lat) $setup(center,lon) $lat $lon]
      Point $c void void "$postalcode $name" $dc(d) $dc(c12) geoname
      puts "  $postalcode $name $lat $lon"
    }
  }
  http::cleanup $http


  $c lower name
  $c bind point <1> "Gpx::WaypointInfo $c"
  $c bind name <1> "Gpx::WaypointInfo $c"

  bind $c <Control-ButtonPress-1> {%W configure -cursor fleur;%W scan mark %x %y}
  bind $c <Control-Button1-Motion> {%W configure -cursor fleur;%W scan dragto %x %y 1}
  bind $c <Control-ButtonRelease-1> {%W configure -cursor {}}

  switch -- $::tcl_platform(os) {
    Windows {
      bind $c <Button-2> "Gpx::ZoomMark $c %x %y"
      bind $c <Button2-Motion> "Gpx::ZoomStroke $c %x %y"
      bind $c <ButtonRelease-2> "Gpx::ZoomArea $c %x %y"
    }
    default {
      bind $c <Button-3> "Gpx::ZoomMark $c %x %y"
      bind $c <Button3-Motion> "Gpx::ZoomStroke $c %x %y"
      bind $c <ButtonRelease-3> "Gpx::ZoomArea $c %x %y"
    }
  }

  Scrollregion $c

  wm title $tl "[msgcat::mc {Graphic Display}] [msgcat::mc Waypoints] $gpx(id) $gpx(name) ($cnt)"
  Cursor
  wm deiconify $tl
}

# -----------------------------------------------------------------------------

proc Gpx::SetLanguage { *var name op } {
  upvar #0 ${*var} var

  msgcat::mclocale $var($name)

  destroy .menubar .f
  MainMenubar
  MainWindow
}

# -----------------------------------------------------------------------------

proc Gpx::MainMenubar {} {
  variable options
  variable setup

  set mb .menubar
  set options(menu) $mb
  menu $mb
  . config -menu $mb

  set mn $mb.file
  $mb add cascade -label [msgcat::mc File] -menu $mn -underline 0
  menu $mn -tearoff 0

  $mn add cascade -label [msgcat::mc {Choose Directory}] -menu $mb.file.choosepath -underline 0 -state normal \
    -image p_folder -compound left -command Gpx::ChooseDirectory -accelerator [msgcat::mc Ctrl]+o
  bind . <Control-Key-o> Gpx::ChooseDirectory
  $mn add cascade -label [msgcat::mc {Save Settings}] -menu $mb.file.save -underline 0 -state normal \
    -image p_filesave -compound left -command Gpx::SaveIni -accelerator [msgcat::mc Ctrl]+s
  bind . <Control-Key-s> Gpx::SaveIni
  $mn add separator
  $mn add cascade -label [msgcat::mc Settings] -menu $mb.file.settings -underline 3 -state normal
  $mn add separator
  $mn add command -label [msgcat::mc Quit] -underline 1 -state normal \
    -image p_quit -compound left -command Gpx::Quit -accelerator [msgcat::mc Ctrl]+q
  bind . <Control-Key-q> Gpx::Quit

  set mn $mb.file.settings
  menu $mn -tearoff 0
  # $mn add check -label [msgcat::mc Checkbox] -variable Gpx::options(cb)
  $mn add command -label [msgcat::mc Settings] -underline 1 -state normal \
    -image p_configure -compound left -command Gpx::Settings
  $mn add separator
  foreach item [lsort $options(lang)] {
    foreach {name lang state} $item {}
    $mn add radio -label [msgcat::mc $name] -variable Gpx::setup(lang) -value $lang -state $state \
      -image p_lang_$lang -compound left
  }

  trace add variable Gpx::setup(lang) write Gpx::SetLanguage

  set mn $mb.edit
  $mb add cascade -label [msgcat::mc Edit] -menu $mn -underline 0
  menu $mn -tearoff 0

  $mn add command -label [msgcat::mc Cut] -command {} -underline 0 -state normal \
    -image p_editcut -compound left -command {event generate [focus] <<Cut>>} \
    -accelerator [string map [list Control-Key [msgcat::mc Ctrl] - + < "" > ""] [lindex [event info <<Cut>>] 0]]
  $mn add command -label [msgcat::mc Copy] -command {} -underline 0 -state normal \
    -image p_editcopy -compound left -command {event generate [focus] <<Copy>>} \
    -accelerator [string map [list Control-Key [msgcat::mc Ctrl] - + < "" > ""] [lindex [event info <<Copy>>] 0]]
  $mn add command -label [msgcat::mc Paste] -command {} -underline 0 -state normal \
    -image p_editpaste -compound left -command {event generate [focus] <<Paste>>} \
    -accelerator [string map [list Control-Key [msgcat::mc Ctrl] - + < "" > ""] [lindex [event info <<Paste>>] 0]]
  $mn add command -label [msgcat::mc Delete] -command {} -underline 0 -state disabled \
    -image p_editdelete -compound left -command {event generate [focus] <<SelectAll>>} \
    -accelerator [string map [list Control-Key [msgcat::mc Ctrl] - + < "" > ""] [lindex [event info <<SelectAll>>] 0]]
  $mn add separator
  $mn add command -label [msgcat::mc Undo] -command {} -underline 0 -state disabled \
    -image p_undo -compound left
  $mn add command -label [msgcat::mc Redo] -command {} -underline 0 -state disabled \
    -image p_redo -compound left

  set mn $mb.gpx
  $mb add cascade -label [msgcat::mc GPX] -menu $mn -underline 0
  menu $mn -tearoff 0

  $mn add command -label [msgcat::mc {Add Row}] -image p_insert_table_row -compound left \
    -state normal -command {Gpx::RowAdd {} -1}
  $mn add command -label [msgcat::mc {Delete Row}] -image p_delete_table_row -compound left \
    -state disabled -command {Gpx::RowDelete {} -1}
  $mn add separator
  $mn add command -label [msgcat::mc {View}] -image p_fileopen -compound left \
    -state normal -command {Gpx::View $::Gpx::options(lb)}

  set mn $mb.help
  $mb add cascade -label [msgcat::mc Help] -menu $mn -underline 0
  menu $mn -tearoff 0

  $mn add command -label [msgcat::mc Info] -image p_documentinfo -compound left \
    -underline 0 -state normal -command Gpx::Info
}

# -----------------------------------------------------------------------------

proc Gpx::MainWindow {} {
  variable options
  variable setup

  set f .f
  pack [ttk::frame $f -padding 3 -relief groove] -expand 1 -fill both
  grid columnconfigure $f 0 -weight 1
  grid rowconfigure $f 1 -weight 1

  set f .f.t
  grid [ttk::frame $f -padding 3 -relief groove] -row 0 -column 0 -sticky we
    grid columnconfigure $f 1 -weight 1
    grid [ttk::label $f.l -text [msgcat::mc Directory]] -row 0 -column 0
    grid [ttk::entry $f.e -textvariable Gpx::setup(path) -width 64 -state readonly] -row 0 -column 1 -sticky we
    grid [ttk::button $f.b -text [msgcat::mc {Choose Directory}] -image p_folder -compound image -style Toolbutton \
      -command Gpx::ChooseDirectory] -row 0 -column 2

  set f .f.m
  grid [ttk::frame $f -padding 3 -relief groove] -row 1 -column 0 -sticky wnes
    grid columnconfigure $f 0 -weight 1
    grid columnconfigure $f 1 -weight 0
    grid rowconfigure $f 0 -weight 1
    grid rowconfigure $f 1 -weight 0
    set lb $f.lb
    grid [tablelist::tablelist $lb -height 24 -width 80 -showseparators 1 -titlecolumns 2 \
      -labelcommand tablelist::sortByColumn] -row 0 -column 0 -sticky nswe
    grid [ttk::scrollbar $f.sbv -orient vertical -command [list $lb yview] -takefocus 0] -row 0 -column 1 -sticky ns
    $lb configure -yscrollcommand "$f.sbv set"
    grid [ttk::scrollbar $f.sbh -orient horizontal -command [list $lb xview] -takefocus 0] -row 1 -column 0 -sticky we
    $lb configure -xscrollcommand "$f.sbh set"

  TableColumnsSet $lb [subst {
    id {def {8 [msgcat::mc {GPX ID}] right} -sortmode integer}
    name {def {20 [msgcat::mc {Name Addition}] left} -editable yes}
    group {def {10 [msgcat::mc {ASCII Export Group}] left} -editable yes}
    gccnt {def {4 [msgcat::mc {GC}] right} -sortmode integer}
    wpcnt {def {4 [msgcat::mc {WP}] right} -sortmode integer}
    note {def {32 [msgcat::mc {Note}] left} -stretchable 1}
  }]
  bind [$lb bodytag] <Double-1> "Gpx::View %W"
  switch -- $::tcl_platform(os) {
    Windows {
      bind [$lb bodytag] <ButtonPress-2> "Gpx::Popup %W %x %y %X %Y"
    }
    default {
      bind [$lb bodytag] <ButtonPress-3> "Gpx::Popup %W %x %y %X %Y"
    }
  }
  set options(lb) $lb

  set f .f.b
  grid [ttk::frame $f -padding 3 -relief groove] -row 2 -column 0 -sticky we
    grid columnconfigure $f 2 -weight 1
    grid [ttk::checkbutton $f.cbu -text [msgcat::mc Unzip] -variable Gpx::setup(unzip)] -row 0 -column 0
    grid [ttk::checkbutton $f.cbe -text [msgcat::mc Export] -variable Gpx::setup(export)] -row 0 -column 1
    grid [ttk::frame $f.f] -row 1 -column 2 -sticky wnes
    grid [ttk::button $f.b -text [msgcat::mc Convert] -image p_run16 -compound left \
      -command Gpx::Convert] -row 0 -column 3

  foreach gpxid $setup(gpx,ids) {
    $lb insert end [list $gpxid $setup($gpxid,addon) $setup($gpxid,group) $setup($gpxid,gc) $setup($gpxid,wp) $setup($gpxid,note)]
  }
} 

# -----------------------------------------------------------------------------

proc Gpx::Languages {} {
  variable options
  variable setup

  uplevel #0 msgcat::mclocale $setup(lang)
  uplevel #0 msgcat::mcload languages

  lappend options(lang) {Deutsch de normal}

  msgcat::mcset de {Yes} {Ja}
  msgcat::mcset de {No} {Nein}
  msgcat::mcset de {Cancel} {Abbrechen}
  msgcat::mcset de {File} {Datei}
  msgcat::mcset de {Choose Directory} {Verzeichnis wählen}
  msgcat::mcset de {Settings} {Einstellungen}
  msgcat::mcset de {Language} {Sprache}
  msgcat::mcset de {Quit} {Beenden}
  msgcat::mcset de {Print} {Drucken}
  msgcat::mcset de {Edit} {Bearbeiten}
  msgcat::mcset de {Help} {Hilfe}
  msgcat::mcset de {Info} {Info}
  msgcat::mcset de {Cut} {Ausschneiden}
  msgcat::mcset de {Copy} {Kopieren}
  msgcat::mcset de {Paste} {Einfügen}
  msgcat::mcset de {Delete} {Löschen}
  msgcat::mcset de {Undo} {Rückgängig}
  msgcat::mcset de {Redo} {Wiederherstellen}
  msgcat::mcset de {Directory} {Verzeichnis}
  msgcat::mcset de {Convert} {Konvertieren}
  msgcat::mcset de {Quit Program?} {Programm beenden?}
  msgcat::mcset de {Do you wish to quit the Program?} {Möchten Sie das Programm beenden?}
  msgcat::mcset de {Error reading} {Fehler beim Lesen}
  msgcat::mcset de {Unzip} {Unzip}
  msgcat::mcset de {Export} {Export}
  msgcat::mcset de {to} {-}
  msgcat::mcset de {Reading} {Lese}
  msgcat::mcset de {Writing} {Schreibe}
  msgcat::mcset de {Add Row} {Zeile Einfügen}
  msgcat::mcset de {Delete Row} {Zeile Löschen}
  msgcat::mcset de {Analysis} {Analyse}
  msgcat::mcset de {Analysing} {Analysiere}
  msgcat::mcset de {Save Settings} {Einstellungen sichern}
  msgcat::mcset de {Ctrl} {Strg}
  msgcat::mcset de {Error} {Fehler}
  msgcat::mcset de {Show} {Zeige}
  msgcat::mcset de {Travelbugs} {Travelbugs}
  msgcat::mcset de {ID} {ID}
  msgcat::mcset de {Name} {Name}
  msgcat::mcset de {Reference} {Referenz}
  msgcat::mcset de {GC Name} {GC Name}
  msgcat::mcset de {GC Owner} {Eigentümer}
  msgcat::mcset de {Latitude} {Breite}
  msgcat::mcset de {Longitude} {Länge}
  msgcat::mcset de {GC Type} {Typ}
  msgcat::mcset de {GC Container} {Behälter}
  msgcat::mcset de {GC Difficulty} {Schwierigkeit}
  msgcat::mcset de {GC Terrain} {Gelände}
  msgcat::mcset de {Last Log} {Letzer Log}
  msgcat::mcset de {Found} {Gefunden}
  msgcat::mcset de {Available} {Verfügbar}
  msgcat::mcset de {Archived} {Archiviert}
  msgcat::mcset de {Waypoints} {Wegepunkte}
  msgcat::mcset de {Distance} {Entfernung}
  msgcat::mcset de {Direction} {Richtung}
  msgcat::mcset de {Please wait!} {Bitte warten!}
  msgcat::mcset de {Loading Data ...} {Die Daten werden geladen ...}
  msgcat::mcset de {Updating Data ...} {Die Daten werden aktualisiert ...}
  msgcat::mcset de {Description} {Beschreibung}
  msgcat::mcset de {Comment} {Kommentar}
  msgcat::mcset de {Finder} {Finder}
  msgcat::mcset de {Type} {Typ}
  msgcat::mcset de {Date} {Datum}
  msgcat::mcset de {Text} {Text}
  msgcat::mcset de {Coordinates} {Koordinaten}
  msgcat::mcset de {Calculate} {Berechnung}
  msgcat::mcset de {To Waypoint} {Zu Wegepunkt}
  msgcat::mcset de {Angle} {Winkel}
  msgcat::mcset de {Point Of Compass} {Himmelsrichtung}
  msgcat::mcset de {Miscellaneous} {Verschiedenes}
  msgcat::mcset de {Ask Before Quit} {Vor dem Beenden fragen}
  msgcat::mcset de {Auto Save} {Automatisch Speichern}
  msgcat::mcset de {As Center} {als Mittelpunkt}
  msgcat::mcset de {Set As Center} {Setze als Mittelpunkt}
  msgcat::mcset de {Home As Center} {Home als Mittelpunkt}
  msgcat::mcset de {YYYY-MM-DD} {JJJJ-MM-DD}
  msgcat::mcset de {DD.MM.YYYY} {TT.MM.JJJJ}
  msgcat::mcset de {MM/DD/YYYY} {MM/TT/JJJJ}
  msgcat::mcset de {Graphic Display} {Grafische Anzeige}
  msgcat::mcset de {Waypoint} {Wegepunkt}
  msgcat::mcset de {Search} {Suche}
  msgcat::mcset de {from your home coordinates} {von den Home Koordinaten}
  msgcat::mcset de {Hidden} {Versteckt}
  msgcat::mcset de {in} {in}
  msgcat::mcset de {Inventory} {Inventar}
  msgcat::mcset de {temporarily unavailable} {vorübergehend nicht verfügbar}
  msgcat::mcset de {archived} {archiviert}
  msgcat::mcset de {found} {gefunden}
  msgcat::mcset de {Hint} {Hinweis}
  msgcat::mcset de {Download missing images from the Internet} {Lade fehlende Bilder aus dem Internet}
  msgcat::mcset de {Warning} {Warnung}
  msgcat::mcset de {GPX file not converted, please convert first.} {Die GPX-Datei ist noch nicht konvertiert, bitte zuerst konvertieren.}
  msgcat::mcset de {View} {Anzeigen}
  msgcat::mcset de {Display} {Anzeige}
  msgcat::mcset de {place names} {Ortsnamen}

  msgcat::mcset de {GPX ID} {GPX ID}
  msgcat::mcset de {Name Addition} {Namenszusatz}
  msgcat::mcset de {ASCII Export Group} {ASCII Exportgruppe}
  msgcat::mcset de {Note} {Notiz}

  lappend options(lang) {Español es disabled}
  msgcat::mcset es {Yes} {Sí}
  msgcat::mcset es {No} {No}

  lappend options(lang) {Français fr disabled}
  msgcat::mcset fr {Yes} {Qui}
  msgcat::mcset fr {No} {No}

  lappend options(lang) {Italiano it disabled}
  msgcat::mcset it {Yes} {Si}
  msgcat::mcset it {No} {No}
}

# -----------------------------------------------------------------------------

proc Gpx::ImagesTclTk {} {
  image create photo p_tcltk -data {
    R0lGODlhRABkAPUAMf//////zP//AP/MzP/Mmf/MAP+Zmf+ZZv+ZAP9mZv9mM/8zM8zM/8zM
    zMyZmcyZZsyZAMxmZsxmM8xmAMwzM8wzAMwAAJnM/5nMzJmZzJmZmZlmZpkzZpkzM2b/AGaZ
    zGZmmWZmZmYzZmYzMzOZzDNmmTMzmTMzZgBmmQAzmd0AAO7u7t3d3bu7u6qqqoiIiHd3d1VV
    VQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAB4ALAAA
    AABEAGQAAAb/QI9wSCwaj8ikcslsClesgdRAdTgekWzE4OwOVyvpgEA1WLFZCWW9rrjf8Djl
    MfAmDex2fM/v+ytbK0xgUVN1Hg5/iouKFFtXWhFqeXEOQgaMmZqbe1wemJyhon4El6OnqBWH
    oKmtmocDrrKLFIIesa0WFrN+FEO4mrt7ExO8fb5CwIwWEwUTwhYIAgLPxnESQyycBQVvzNMI
    xdZwEV8Umd/Vu9zPwuNu5U/nywjdbtECFe7vbg9E84uiIXAnAMK+Pbp0tbI0BOCicLssQLDX
    Z9cWPAdDeRLiUBFENwUMVqQwAIBJFhQybtrooaMfifcm5FNpQQILACswZFhhQOUr/yIS0Imr
    wK2ihBUAMqBIwVSDy02HhETgtItaRgu1VpRgyhXFBlQsiEzd9E1fHKwDWCzlyrXDqVpiqdbj
    Y4EAg7VsU3RQ8bbI2E3ODlo4oDVvChQi+MJJeLYPtrjbqnmLAGCr4RN8JoSLGZhPvCEPyBLl
    M+CDYaZu90AQQJFoa3JFEgVrdjYCi9MpEiOswNrbXM+xyWreQ8C0YRPzFN7jPfAehOZ7GA5h
    tUyzO6xqT++t4OhvOncQzHYqQj0gsesHjOcVwd0BUgASIsrUxPKTcMluDFjOe8IEiv8fABBB
    RPX4tEdUpqBCQQN44ZZCCAA8IExnjCDoAQGo1JSBg2xtIP+gQq8pEhYRyoRiQQTqcUjZgBJB
    R4stv2QYwX4OnmAAAClVgIB4tBhRIlUzcsiUCFKY5WKPRfxIVgQN4sbBCg7ocp4mjxGB0ikn
    NnlaBCvkOFQmn5mjYIMo0JjCCWQYqEiYDSmYVwlNwjBAjqJI908qJnAlwgtNanCAmorY2SYq
    J7SlAZmynVIfR6NgJAJXFGzIFgxPbVKKEUGJIgUFMTB1AopsYdaKhUJkGkpJLFgmwgMp6gPo
    IqR68BcnAzAAJ1MjGEBCWwtkoSCMkJ2KAVsdtLDfCSBEoSASs1qq3gkLmtlCT30d0awmxXEF
    bXZssZDAq70gYSpZBjxbS4MlrBD/X7WYimKBARpwFYNNDYLQ5a9HVKrIYPEeNoJtDWoQgL6M
    wGUEwS9F4ABTKPx7F1saKMnJiEUgbNTCh3WQAAN5OSAxVPmOQoEDhaLQQQQX5CXBxz8Vca8o
    FBhQqF4HpKwtBQRYrAipL4tyQAioPWCzpzG3sqg2o1iQAAxBD53bAdSisijL+yrwQsZCs8VB
    zuD+MbWCV5uMMlsR0ImKoLekEgGutmkLj85/oE31vhIs9S+3GrPQ8yj+JJkKBVuJEMHDmFHg
    AtKosGkflhWcgBiXS51QaAlzL6J4ecFkYYEIKEC7ArqYh3J50hIAMAAIZ57LVgZRnzL6KaWt
    RUGqbDUw/2AqVU6HJaioDbAf5XD/gQwRiY7igGUjZMvUC3+2MvwQxZtIQQtLiVAuVw4EL7wR
    oWVoeOcGSArCt648L0T33r9QQgTDpvCVLAYPca27HcQQAYOizkKxVLzMLkJqs7DQ/KhEBTjM
    CVAUOAABbgcrvyCkaxUgAI7esEDBHGRF4BLgHhyBDgO87F0SioMkFoNBRlxqCOPSx406sgsW
    ejA5D2gdVgAgwxVxRw99qM+40EJDOp3IAFsAyLs+uIU4HMB03lkRzg4wwPxUrBMTvAcFBFQB
    AETpHi98A86uQUMSCigC6PNDfVwYRX04YIJl80YWKSjEKdbQdF0ClKDI6MMVKs8Hiz17FwNn
    +MYbxUJNczzLjbwYHw7icTEPCKE+3HidFVEmdHDomzzOckYgwoEKc1hXBVa2Au9IgDq1GMAD
    5hGzHpYED34I097e4KtrqAEOlNAiKrWYRi3mAYfAyUZFrsLLPdTyHlfxxppI9A7tMSJ3lePH
    JpCpzP4Rs5nweyY0nUcESE6TE/Gz5jU3QTFtbjMTUfHmNxuYoFCwYRK3NCY4oXeMNWjBAVSQ
    gt6SQAgxiKEMZTgDGiIhAXSyIZxZgKcB5AksOxj0oAhNqEKJEAQAOw==
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ImagesFlags {} {
  image create photo p_lang_de -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAALCAIAAAD5gJpuAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAGzSURBVHjaYvTxcWb4+53h3z8G
    ZpZff/79+v3n/7/fDAz/GHAAgABi+f37e3FxOZD1Dwz+/v3z9y+E/AMFv3//+Qumfv9et241
    QACxMDExAVWfOHkJJAEW/gUEP0EQDn78+AHE/gFOQJUAAcQiy8Ag8O+fLFj1n1+/QDp+/gQi
    oK7fP378+vkDqOH39x9A/RJ/gE5lAAhAYhzcAACCQBDkgRXRjP034R0IaDTZTFZn0DItot37
    S94KLOINerEcI7aKHAHE8v/3r/9//zIA1f36/R+o4tevf1ANYNVA9P07RD9IJQMDQACxADHD
    3z8Ig4GMHz+AqqHagKp//fwLVA0U//v7LwMDQACx/LZiYFD7/5/53/+///79BqK/EMZ/UPAC
    SYa/v/8DyX9A0oTxx2EGgABi+a/H8F/m339BoCoQ+g8kgRaCQvgPJJiBYmAuw39hxn+uDAAB
    xMLwi+E/0PusRkwMvxhBGoDkH4b/v/+D2EDyz///QB1/QLb8+sP0lQEggFh+vGXYM2/SP6A2
    Zoaf30Ex/J+PgekHwz9gQDAz/P0FYrAyMfz7wcDAzPDtFwNAgAEAd3SIyRitX1gAAAAASUVO
    RK5CYII=
  }
  image create photo p_lang_en -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAALCAIAAAD5gJpuAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAJNSURBVHjaADEAzv8Bdn+ySSLr
    ALyz/0lXtNYL/v//LScKIayCARET3EFo0tbz/wD+TSv5/Z6NCFtlhsERAgAxAM7/AHaAsuTp
    /v729f7Uz/75++Dm/vT2/v7Mxf7XzvP1/t7j/v72+P6wpv7y8MfU/j9LpQIAMQDO/wIA/wL/
    AADL3AoBGx8BtLIfCvf09wEB09AB2tzy9gEhCvMBc3EBOUOWugz39gAA9P4CiImBkQEEGBlO
    PBJkcHNl2LNdnPkLw927DD9eSTF/YqgtY2hrmXJY+MNHhl+/QAoBAojFk+mYNJOJDNt7BqBF
    T/4zaGsxdDczXL7DcOEaw5GzDDl5DPdu5oh8ZeD/9V9Yko2hByAAADEAzv8E/8fAAEhJANHW
    AP4AAN7hAOrlAOzrAPDzAPj2AOjnAL5ZAPv4APj6APz8AGZY7gAAAoiFUUmVQV6FgUuQAWiF
    pSWDgABDYjzDZ6CGHwycbAxzZjN8+8bw4zfDz18M2sqC+w8CBBDLZsEAxX/ij35KAP1io/yB
    obmJYcsOBnNDBn52hrV7GSIDGWZNz14u8eX7Dw9mtpkM7gABxAQMeJDrgarl3jA01jNU1r/7
    ycbw7QfD9z/H3gkzTJ3HEBUz1eXK/19f/v79w8DwCyCAmEBO/89go/bp//Hd/1s7Dz/hY2H9
    /z8h8H+0n4Tg/5UPxf+t3/zr+J6FGT8Y/v5mYPgNEEAsDP//3t867drS2UyJhe2x6xje/7UO
    FbPYvQdoZ9854/8vrk5n+JrCdeXNts1SIHdYAgQYAHK0/st9mgoPAAAAAElFTkSuQmCC
  }
  image create photo p_lang_es -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAALCAIAAAD5gJpuAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAFnSURBVHjaYvzPgAD/UNlYEUAA
    muTYAAAQhAEYqF/zFbe50RZ1cMmS9TLi0pJLRjZohAMTGFUN9HdnHgEE1sDw//+Tp0ClINW/
    f0NIKPoFJH/9//ULyGaUlQXaABBALAx/Gf4zAt31F4i+ffj3/cN/XrFfzOx//v///f//LzAC
    M/79ZmD8/e8TA0AAMYHdDVT958vXP38nMDB0s3x94/Tj5y+YahhiAKLfQKUAAcQEdtJfoDHM
    F2L+vPzDmFXLelf551tGFOOhev4A/QgQQExgHwAd8IdFT/Wz6j+GhlpmXSOW/2z///8Eq/sJ
    18Dw/zdQA0AAMQExxJjjdy9x2/76EfLz4MXdP/i+wsyGkkA3Aw3984cBIIAYfzIwMKel/bt3
    jwEaLNAwgZIQxp/fDH/+MqqovL14ESCAWICeZvr9h0FSEhSgwBgAygFDEMT+wwAhgQgc4kAE
    VAwQQIxfUSMSTxxDAECAAQAJWke8v4u1tAAAAABJRU5ErkJggg==
  }
  image create photo p_lang_fr -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAALCAIAAAD5gJpuAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAGzSURBVHjaYiyeepkBBv79+Zfn
    x/f379+fP38CyT9//jAyMiq5GP77wvDnJ8MfoAIGBoAAYgGqC7STApL///3/9++/pCTv////
    Qdz/QO4/IMna0vf/z+9/v379//37bUUTQACBNDD8Z/j87fffvyAVX79+/Q8GQDbQeKA9fM+e
    /Pv18/+vnwzCIkBLAAKQOAY5AIAwCEv4/4PddNUm3ji0QJyxW3rgzE0iLfqDGr2oYuu0l54A
    Yvnz5x9Q6d+/QPQfyAQqAin9B3EOyG1A1UDj//36zfjr1y8GBoAAFI9BDgAwCMIw+P8Ho3GD
    O6XQ0l4MN8b2kUwYaLszqgKM/KHcDXwBxAJUD3TJ779A8h9Q5D8SAHoARP36+Rfo41+/mcA2
    AAQQy49ff0Cu//MPpAeI/0FdA1QNYYNVA/3wmwEYVgwMAAHE8uPHH5BqoD1//gJJLADoJKDS
    378Z//wFhhJAALF8A3rizz8uTmYg788fJkj4QOKREQyYxSWBhjEC/fcXZANAALF8+/anbcHl
    Hz9+ffvx58uPX9KckkCn/gby/wLd8uvHjx96k+cD1UGiGQgAAgwA7q17ZpsMdUQAAAAASUVO
    RK5CYII=
  }
  image create photo p_lang_it -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAALCAIAAAD5gJpuAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAE2SURBVHjaYmSYyMDwgwEE/jEw
    /GF4mvT0HyqQUlX9B5aEIIAAYmH4wlDtWg1SDwT//0lKSv7/D+T9/w+nYmL+//79/88fIPll
    0yaAAGJhYAGJP/n69O+/v0CAUAcHt2////ULqJpRVhZoA0AAsQCtAZoMVP0HiP7+RlcNBEDV
    YA0Mv38DNQAEEMj8vwx//wCt/AdC/zEBkgagYoAAYgF6FGj277+///wlpAEoz8AAEEAgDX/B
    Zv/69wuoB48GRrCTAAKICajh9//fv/6CVP/++wu7BrDxQFf/YWAACCCwk0BKf0MQdg1/gBqA
    Pv0L9ANAALEAY+33vz+S3JIgb/z5C45CBkZGRgY4UFICKQUjoJMAAoiRoZSB4RMojkHx/YPh
    bNVZoM3AOISQQPUK9vaQOIYAgAADAC5Wd4RRwnKfAAAAAElFTkSuQmCC
  }
}

# -----------------------------------------------------------------------------

proc Gpx::Images {} {
  image create photo p_button_ok -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAInSURBVHjaYvz//z8DJQAggJgY
    KAQAAcTCOIFRluEPw2OGr0DeNyD+DsR/QDJAzAbErEhsFiifjUGZjZ0hmZmZQRcggFgY/jJs
    BGqoBgpvx2sVyKf/GKSAZBJQfdqvPww/GFkYQgECiInpL9NvVg7WxQy/GRxxavzFIAwkywVk
    BPZJykg2A135iOEngxNQ7CJAADH9//R/lbCWsDCHHMcihh8MlgyMSJp/M4gDXZjBIs1yVNpX
    uoNfll/91e1X24Gaw4DqnoCUAAQQy/8P/0+/+/HuF1MIkwzDO4alDA8YQoDi14C2JjDIMyQw
    WjGas5myMXw4+YHh6/avqxmYGdIYOBg+wOwACCBGhhQGMaDC7QwFDEYMr4Ai8xnuAp34lsGO
    wYzBAsgXBeINQLyFYQIDF0MDAy/DR3CAcjAwMHIxMAAEEAswYF4x3GE4xXAdaIA6UCKcQZmB
    CYglwV5gYJgKDLJTDP0M7AyVQNv/ogcRQAAxAf3DwPCF4TTDJaAkyAXsQMwNxA+BeBYQ7wNq
    /M9Qhk0zCAAEEAvDZyD5l+EYwwWgs/WA3gHF8xOws38w3GbIAxo2mwFPcgMIIBawuX8ZbgA1
    XWV4ATTgF9i/74ERVQz083wGAmkVIIBYwP78B8Q/gY49BkwLzxleAzXHAVPdDpQoxQEAAogZ
    GFwMYIX/gU5+zmDO8AkYLywMe8BiLEhJGZlmhrAZgWyAAGKkNDcCBBDFuREgwADDUYv001Uh
    NgAAAABJRU5ErkJggg==
  }
  image create photo p_button_cancel -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAANVSURBVHjaVMcxEYAwEADBjQlM
    0GAB5ahAAR0OknzypOVmtrjyHChk2JY90lUbrdI7I/xaf2a65/TG4BNATGDNPxmEWARYlrMq
    8e7794shkwEHAGrO5BVl3sfOybj8718GIZAYQAAxAW2VYhFmWykYF+oklpPFwKEtNOXvD4Zs
    kMHI4M9vhmwhWbYpmo6KDMpGIk6cHAwr//9jkAIIICaGPwxqjLxsLqzScgxM7BIMkmGBTLya
    /BP+/WDIQNKcISzLPkHJQo6J+R8zAy8HEwMLC6ML0AA1gAAAQQC+/wNxGwDh9ub66fng2EcK
    HhICAwgRAAwcEAL35uNM9en75vz8ADMSHQaZJTkeAgT/AgD77PEA18rs/O7u/mwCAwCtAoi5
    UALot/8MZ748+P7u59N7nkDNjMysPAz8kpwM31+89uXkZfZVtpRjgGj+wHDj9Jv/r17/y2dm
    YpgG8iZAAIEN+AcM6X8MDKe/PPn56vvTx968UlwMLMycDAJiHAzCEtwMLH8ZwZqvn3nD8PrV
    v2xmZqBmBkjsAQQQC8N/REAxsjBMf3v969+/30/PVHZQY2AGReHvfwx/P3xhuHH2LUhzOlDz
    LOTABQgglmdPgSSSIaC4/Q/UxPD1O1AzUOLLDyAGJoo/QIMYIK4Fqf8H1QMQQEwomn8zZIIC
    TMlCloEZqPrv+09A278yMP/4y6AuysoA9NFMoDBKOgEIIBaUeJZjm6xsIcfA+p8Z6Of3DDfO
    vAXbrAHUzPb9H4M2B9DmHwxTX/1nYAJ6fypIH0AAgQxgBKawDGE5UDzLMrKCQ/s9w/XT4ABL
    Azn779dfs0CaWT8zMOj+ZWC8zMAw4SXQLCYGhhkAAcQC1GwPTJ7TVC1lGJjANoOi6u0foOZc
    YIDNZgQGx4tvDMxAmycDNbOwAg3UZWBgAWaXae8ZGK4DBBDQEIZbPz7/3/PpySeG32+Bms+8
    /vfq9d8CoOYZMO8xMzLMePmPoQBo8z9g/mIAOoQBGLR7gN64BRBAjMcEGBh+AjMTOxvDajZW
    Rqf3H/5nARPJdJhmYK5j+AvEwBwISnCZgkCbgZr3fWVgCAX6/x1AAIENAGXZX7+B2fkvgzYT
    E8N+5FBGNgCEgUodmRkYrgJtfwWSBwgwAM2zU6m+NiFjAAAAAElFTkSuQmCC
  }
  image create photo p_configure -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAOxSURBVHjaYuzpWcQgLS3OwMDw
    X5Cdne2EpqaS9L9/f/+/ffvxxbNnryZ+/vx9/u/ff74yMjLAwY8fvxlMTbUYrKz0GAACiOnH
    j18MDx48Zfj589eXb99+zvv+/TuXpqYij42NgYqJidZkFhbmXqDhrIxAE0CGwDAMAAQAQQC+
    /wNERFAF9/f4/vHx8/4HBwXF3Nzl7/39/DwbGxUHExMQAAICADTW1t/W+Pj61Pn5/AD7+/sA
    8vL1ABcXEgAjIxwAAogJaMhyoJmmnz9/Zbh69Q7D06cvJp05c+3khw+fwTbIyUkx8PBwev74
    8YP5169fDN++/2RgY2NjkJISA8sDBBCTjIx4sKys+Mpfv37r/v37l4GDg73a2trQhpeXG+it
    32BFQG/Ivv/0k/vr1+8Mnz5+YeDl5WTQ0VECywEEEAsbG/M3V1dbRWCgrXjx4s1BZ2ezTHV1
    BYaPH78CNbIwvHv3geHxs/fPhf7c/OTMu4vhE9AFHF+1gVrDwQYABBDLzZsPH1hZ6Wt5e9tq
    vX79TktFRZbh/fuPQKl/wMBiZnj9+j3Dl2//xD059+oIM3248o/xDwMbw2d4IAIEELOYmOaf
    //8ZfDU05BlERAQYvn37xfDv339gYP1lePToBcOHLwwM8px3lCQYj/v+/fXvz/fv/08xcgkx
    yNoms4KCACAAAEEAvv8DKCg5zdjY3v3q6u86//8ABPb2+ADq6u0A9/f4AAYJEAAD7MYAGeWR
    ABn45Q8AEQdJ8/YG9PLp6cfx6Nj/AQQJ/wKIMTS0lOHLly9ADT+9OTk57Hl5uTS+fv35QkhM
    0jDMQcBE5tUkBqnf9xhEgNZ942RleMTI++fubebir19/TPr87S8DQACxgPzByMjEwMrKshUY
    yls/ffrG+Y+R6bvYv73m/y/e3Kak9VGIl50L7F/u/wwMykw/WP7+5+y7cpWNgY2dYRJAADFr
    a1uDE8e/f/8YgF5g+M/I9keC8wNDkOzpp18+//vGxsPmKqHGyszAz87AAKSY//5nEOX6xfSP
    idnj+WvGlwABxMKAA/xnYGT49ZdpypEzDGy//jP0mLqxMjK8+8HA8OALA+Of/wy6wr8ZHwty
    NwIEEE4DwFHEzMAATFt9+44w/Pn5/UefjepfZgYOYOJ9/5uBCZjIuJn+CgEEEF4DYIYAvTZp
    1xEGhp/P/vc5W7EwM7D/Z3j/koHhyiPmSwABRNAAcHoHWvqPEWjILU6Gey9/1AnxMLDfe89+
    /e0PphyAAAMA72F4qGAYYSYAAAAASUVORK5CYII=
  }
  image create photo p_contents -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAPpSURBVHjaYpSsf85lyvzymw7T
    O4ZfDEwMyOD/f4YgVlbmoH9//8Z8/8vEsPLyX4a3b34wMDL8h6sBCCCmf/8Y2v7+Y1AFsjmR
    Nf/8/ZdXR1280dZczu3Pn3/yjIwgE8GGMiKrAwggJn4uJg0pfqZtf/79nw3ks4AE//37z8DP
    y5Hu5ayoY6IrJsrPy+7188cfj/+MjKuZWBhj/oOMAJkIxAABxPKPkWkNKw/XbLYfrCp/GRjP
    //z9p/fP3/+a1ibSlaKCbAxA1zHoa0u0GTExMrOIfObdcOaz6NMbz1czsDL/AFkGEEBMHz/+
    3PWcgeeWoAgvg4IMf/Hf//8tpSR4C+zNZYRACpiBwSIixCmgLCPAa6HIySAlyGIP9IQa2GSg
    /wECiJnXpuDjt7+M+hIMX4w0ZLh5JUT5HLXVReyU5HiZQQb8/s3AwMrCwvDy7VcG5n9/GN7+
    ZGC4cvfz53+fv+1hAHoVIICY/v/4xfDy+det79j5v794/YVBU11EiZ+PnQ0SFsDA/PmPgZeL
    meHL118M4mK8DCpCjAwiwhyBwFAWZvj5iwEggJj+Ag34++v31kuv/j16+vY7w9/fvxjevPkK
    1vz793+GP3/+Mnz//odBSICL4fuvvwziXAwMehqiygy/fjszfP/BABBATP///WVg+Pv399OX
    v1Z95eRnuHP3PYOwEA/DnQefgYHMCDTkH9CQfww8QFe8+/CDQUVWgEFBhJGJRYDHm+H3XyaA
    AGL6/e0Hw98f3xm+fvm14sabf78fPv/MwMb6n+HFq89A+X9As/8A4/4/w+t3Pxg4OVgYJEU5
    GWR4GRlU1CWcGL7/lAcIIKZ/QH/8+/mb4cfHL7cevvu359N/VoZHTz4zCAlyMjx9+okB5MBf
    QFe8ff+dQUGKj4EDGDxqklwM8pLsMoxcnA4AAcTMrBTL8P8P2Bv/Pn35wykuwunH9fMrAxcX
    CwMLMyODAB8nw52H7xkkhLkYhIGGPgeGz/0Hbxk4hfkYrt39LAwQQMwsagngJAny758fv66+
    /PJPR06ETYPx+xcGXj52YGAyMnz59ouBi5ON4ciZxwynL79kePHlH8OX/5wMV2+/ZwAIIEYO
    /0OIhA1074/331R0tIX3xJuyyfOx/GaQlxdheAkMj+dvvjG8/cXKcOfFb4ZbD37cunbq7rb/
    7z8tAgggRibrTcj5j+HfL2DK+fM/xMtDfkWYGScz4+8fDO9+sTPsOfOR4eKZ55efvPq6AJiq
    NgL9dxeUTAECiJHBYCVKFgZnkr9/GVjYmftUNEVzRYW4GK6evH/m3ZP3bQyfvp5kEOJ9xQBM
    OwzAqAXlS4AAAwBL74x8VYLyagAAAABJRU5ErkJggg==
  }
  image create photo p_delete_table_row -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsRAAALEQF/ZF+R
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAAA4UlEQVR42mJMiq74z0AAzF3SzohLDiCAGP4TACALgIABFwYIIJY3b74wUAIA
    AoiFmZmJIgMAAohxDScDwTAI/vYfZxgABBBL8O5FDAy/fkN4v/5A2L9/w9lrM6rwGg4QQCwM
    W6cxMHz5DuF9+cbA8PU7hA9if/9J0AsAAcTC8OEzkgFA+us3CP0T6Ir/BH3HABBAQAO+IAwA
    a/7BwPDnD9GBCBBALFt2P2dg+PsXwvv7jyhbkQFAAFGckAACiLJEAAQAAUSxAQABxJIcU0mR
    AQABxPif1EBDAwABRLEXAAIMAL+WuGQburE3AAAAAElFTkSuQmCC
  }
  image create photo p_documentinfo -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwY
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAACFUlEQVR42mJkwAWMZxag8M+mT8CmDCCAGNE0yQPJhSwsTHYyYtyMCpK8YOEH
    zz8zPHn19f+fP/8OAbnxQMMewrQABBAjsmYOdpbrHpYynKXplgxf2HkZ3nxjYPj5G4j/MDCw
    //jMsGLFcYZ9p558//P7jybMEIAAYkTWHOGmxBmf4sBw6Q3CUbdeQOjvvxgYuFkZGC7tOMBw
    +MS97//+QAwBCCAmqLqFIJthmkGKQThPn4FhijsDgzAHA8M3IP/hewYGQXMHBlN9GU6QHpBG
    gAACGwDyM8jZl14DFf6EKEYHIDEQfv2FgUHa3JKBgZHJDiQOEEAsoNAGBdgXNl6Gb+8hir5D
    Dag/ysBw8wWqASC5q794GYQEuRnfAfUCBBALSBIU2q+/oSoCgW4nCO26EFUORH9hAsXQZwaA
    AGKBOfHHb1QFyN548wURkDC5v/8hYgABxAKLZ1BUIRuA7n9kA0D0v18/wWIAAcQESmGgRMIB
    jOdvSDYgG4Iu/v3TJ4b/X9//B+kFCCBwLIBS2MqVxxl4WHAY8BOBf/z6x/D/7mmgE8CpkgEg
    gGBhEA9MYdd/MB7gFDBzgDtTuAlhCIj/49dfhv9PrzEwvHrwHZykgQAggFCSMhMLy3VQIpEy
    s2S48pEXJUx+fvrI8P/eGYjmf3/hSRkggLBmJlAiAcXzZ2BU/fsPCTCwn/9hZiaAAGKkNDsD
    BBgA6WQkVaV9760AAAAASUVORK5CYII=
  }
  image create photo p_edit -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAANESURBVHjaYjxx5tZzXh4OHgYk
    8O3nb4YfP/8zfHhwjkH3aTHD97+WP8+sP7Pw7d173UxMDC+Q1QIEEAsfH6eAhoo0B7Lgr9//
    GH7+Z2L4+KafQcrgMQPTX2keaVa1onU9924wMjLMZmBEqAUIIJb///7/YUADzKxMDP+fvWQQ
    ZN/EwPQaKHDxBMOni0xfPrJwX//99y8D43+EWoAAYmHAAkAWML9ZxsDB8JiB4T0DGJ/nMOb+
    kJHX9vXzu3hGBsb7rGxsDKwsbAwAAYTVgL+fPjMwf1jEwPwbyPnOxPDsKSPDV4swRnYedttH
    9587cXJxzb1x5hrD7ZuXGQACCKsBDB/3MzD/ucjA8ION4f/LvwwvpK0YZF2CGb7dvs5w6fvn
    n2/fvGTYs2MNUOEfBoAAYsKm///DyQysvJwMDF84GN5/5GP4ouHHICQjxcDM8JeBhYWV4cql
    K0BV/4CYjQEggDBc8PfFCQZmjltAzcCY/cTA8I5HjUHQIZSB8/9PBj4eXoYjBw8x3LtzHqiS
    lQ0UYQABhOoCYAj/ezaPgYkDKPwTaPMHHob3Mp4MglLSDCzABMDIzMjw5vVLTpBuIBYBYm6A
    AGKBRelPUNS8uMjAwv6QgfGPIJDDzvDqOzuDaFAsw68vHxh+//rJ8OnjJwYmRiZgmmGEuZwX
    IIDgXvgH8tLfH0CGNAMDBy/Dl+dPGd4IWzNw/GNm+Pn6DVAjI8M/oAtBQQQLKlAoAgQQE3Lc
    //nNCExELgx//7gyXLwmyPBb1YHhDzCd/f33n4GFlQWohhGkC6wRiD8A8VuAAIK7gJWZgeHh
    y28Ml/YdYZDm5GZgtM1i4FbSZPjz4xvYMmYWJgYubm4GJiamr0Bn/oZgJgaAAGJCdtOP738Y
    nt96zsBq7M8grGsODOMfDGzAaBMU5GMQExMHuoKZwcbe8begkCwDIyM4EhgAAoiFCQjBSReI
    NY3NGVQMTBmYBYSANv9m+C3Ay8DKzgb0+38GNjZmBmZmFgYuLk6m8KhYhlu3bjLcv3OdASCA
    WO7eefhh1Yo1PCAnMjIDYwcUWH9+AZ3FDHTZX4b/IE8Dw4CJiZHhAzAWgGHx68vXLwwKCooM
    qqrqDAABBgD54A4xrMo1ZAAAAABJRU5ErkJggg==
  }
  image create photo p_editcopy -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAKbSURBVHjaYjx69NZ7QUEelv//
    /zH8+fOP4d+//wxMTMwMV6/e47l48UYSKyvLfAYg+P37D4ODgymIyXDgwGkGoDiYDRBALJyc
    LDwaGhIQHlghA8OXLz8Z2NiYQdx5V67cEmFiYur+//8/AzYAEEBM6ALMQH1sbGwMt28/ZXjz
    5j0DBwd7158/f2tZWJiBLmPEMAAggDAMYASq+fv3D4OengqDmZk2g6amElCMUdHS0oCBl5eH
    Ad0lAAHEgs0AZqAzPn36xvD9+y8GPj5uBktLPTtRUYFZwHDgBCrhBhqy/d+/f7OBXmMACCAM
    A75+ZWB4//4LMBy+MBgZKQFtZQMJKwPFlT9//goMo79ANT8CgWHDDzSwByCAUAz48YOB4du3
    H+BYALmCnR0hzc3NAIwhTobXrz8wfP78GRg2HN0/f34WAwggeBiAvPbr128GFhYmoCQLyN8M
    6AHPwsIIDGBWBklJUQYVFTkGLi6OQoAAghvw6xfI/8zgMMAFQLHAzs7KIC4uzCAtLQI0kJUB
    IIBYQAGBbAMTEwswFkCKMSII6L1fwLD5Bk5snJzsYFcCBBDL58/fgX76DcQ/wBLA0AUa8B/o
    oj9gBTDw+fM/YOD9BHqLERgenOAwYgEGEUAAsezceXwZEKNY9/37z++6uqpOFhZKyrDA/fPn
    N9D/LGCXgbwCcSEjA0AAgYI5HlkzyGYJCWEQngd0jDJI3d+//4A2MgEN+Q+mQRgUXiAAEEAo
    0QjKMKAQNjHRAIcDyCt//jCCbYIkc0awN5EBQADBDQDZoqwsy6CvrwZOrv///wZrZGVlAIfF
    v3+sQPofEP+BplgmsDhAALEgJ2FgCmM4evQ8NF38/3nz5gMQ8wuUD3YVhP0XrP7Ll+9/AAIM
    AI729YDbYvpsAAAAAElFTkSuQmCC
  }
  image create photo p_editcut -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAPqSURBVHjaYvz//z/Dqg1nGf79
    /sHw/9dXBgYmJgYY+PePgYGbmz1ASkps/YcPn3yYmBi3Pn78iuHXr18M4uIiDAEBNgwAAQRW
    /WF/HQPTi0MM/JKKDDwCAgw8PFxgzMvLySApKTJLVlaU4dOnbwwfPnxnuHHjHtAORhGgxXkg
    vQABBDbg8+XtDH9P9zAwbvNl4P39kIFXSJyBj48TiEGGcIh+/fqN4f37jwxHj54DKRcUERFa
    oKGhMBHEAQggsAHMnNwMf3/9YPj16h7Dnx2pDJ/u7mF4+YGR4c2bDwzv3n1m+Pz5B9Dm+/9f
    v34vqKoqt9bf38b7/fvPb0F6AQII6uH/DIxAvzOysjMw/P3BIHymmOHXg50Mz9/+BRryCej0
    jwxXr95WMzXVWh0R4e54/vydv/v3nwJ7ASCAWBjQASMLA9u/bwzcL7cz3ORWY+Bg+sXw9u17
    Bmtr4/bAQHuOLVuOfNuz50SapKToMpBygABCMeDvXwbBPwwMit8Yuf6p/j5+988Xps97H7oy
    qKgpMLi4WHAsWLjj45Gj5+IkJcQ2sbCygvUABBDYgH//GUFRFqiq8b9ZWo9ZmYWP+dfLM8wn
    9Z4fnXdb3O4/C7sQ48SpG548un0pRliY7yAX608Glv9fwAYABBDYACbG/0HWBr/XcvKzfPp6
    798ZURVGBWUnZtff/wScLR8dYzy2+TLDv4cvW2sNbhzk4fjL8OsvIwPTW0agzlwGgABiBCWk
    TfFsz7Vk//EdvMEVqiXPsI2ThSFSP4p5GYMMG8PXC78ZONj+AWPo343P35ieP7ryd8WPL/9n
    MQKD32byFwaAAAK7QIn7t8Tdh8wXz9xjOPfp638BG4P/UQzv/jNc2Pz3gpgKkz63GwsjJw+D
    BicrgwaHMKPj8Q1/Pvz6w7AKpBcggMAGPHrD+MNW8a/+x1/fNoqLMArKijKp7lvPuu/5C4Y4
    aYW/T74c+vf56SmGbDYOBhFpfYY+YSmGyDvXGTYBtf4ACCCwATvvcKb/+fujxVDmr9mLP8wv
    dhxnrv35h3GistT/z99f/mN4c5/h4JWHLMu+/fjPoc/wr4qN7b/s+2/MXCADAAIIbMDXX4yL
    Tzxl3/Lz/y+eR1/YvjKwML0V4fnP8PbjPwbOh3/fSgj8NT54ncWMC5jOLDV/iTx/xXTp4zfm
    3yC9AAEEjQWG/5ws/9+xMTO8Y2cDGvuXgeH5ewaGL9+ZGB6/ZJyWYPqrNtroy1pmJgYhgd//
    GTbc5N7y5///zyC9AAHEhJ4QgYYxfP8BzKHAnA3MzQyvvjJ2zDrGMZeH8b8Y45///+ac4Oh7
    8Ylh1tcfEPUAAQYAZbWKZF50obcAAAAASUVORK5CYII=
  }
  image create photo p_editdelete -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAMOSURBVHjaYjx66tobAT4e9n//
    /zPAwD8g88ePnwy/f/1mePPuI9Of3//ef/78xYOZheUKExMTw+1bNxkaqjPAagECiIWdjY1b
    U02GgwEJ/Aca9uPXP4a/fxgY7t5/zMDDzcl198Hz/U+evQxlZ2c/gKwWIICYGLAARkZGBqBF
    DIxM/xiYmJkYJCRFGBysdUXUVeV3srKwhPxDOJYBIIBYsBkAkmcEYjY2Fob7Dx4zvHr5koGL
    m4Phz++/bJ8+flj979+/IqB0P0gtQABhdQHYFUxAVwBNERURYvj1j5Hh5YuPDHfuPmZg42Bj
    +PLlQxtMHUAAYXUByHYmoDf+/P7DoGugwfDrBzAwP/9kePf8OQMrKxvDqRNH4WoBAgjhgn9/
    GRiun2JguHmW4R+Q+wfojz+srAxf7t1jYOoqYmDdu46BlYsb6C1WBmZmZrg2gABCuODJLQaG
    4zsYGL5+YPj/6B7DV5sQhv8PbzIITqpiYN+7g4FT7hjDb20zBiYxaWAgI1wLEEAIAySVGRjk
    1RkY9qxjYL45n4H7zFEGpjt3GFgPH2T4LaXE8D6nkYFZRJyBlfEf1JMQABBALDD2DyYg0y6E
    gfHbTwbWFTMY2K9uZmB49Z7ht5YBw7u6qQy/FTUYmL98YQAmJnA0wwBAALHAAuEPMAgYWZkZ
    WJg4GBifvmJgePaageHzDwaGp28ZGIGxwAjyO1AfMyMsoiEAIICY/sFC/vcvBrbFkxnYOysY
    GF5/ZPhrZMHwj1uYgfXsDQZhH1cG9m1AF3FyQW1HuAAggOCxwHJkBwPr1A4GhhdvGH5FJjN8
    XriT4WvvLIb/fPwMzK9eMQgU5zIwvXrB8B8YjcgAIIAQXjBzYmB08mP4L6PC8CulEJwffvv6
    MnxYvZGBq6yA4Wt6FsM/XqBhwLSB7AWAAGKBpWtuQT4GhvbpYClWIP77F5SpgLSLHcOv8+eA
    XvzPwP31FwMLCytKIAIEEMvHT5++tnVO/sPExALNB/+APmQCav4HNuA/NGf8B3P+AzUzMXz5
    BEyWUAAQYABehgBuH0rveAAAAABJRU5ErkJggg==
  }
  image create photo p_editpaste -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwY
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAADW0lEQVR42mI8UyvNwMnyl+HoO22G098dLGVkJBqkpUQEGYDg7dtPv+7cez5B
    heXUGg/R4wzffzIyfP/NwMDIyAAHAAHEwvD/v8DfX389/v7+LSEqKpyVmBigKiMtzAAx4APD
    jFmbNf4++qPz//ffu//+Mx8Ean/E9P8v3ACAAGL5x8mzXMbF2OPffmEGMw19Bi5ODoZHj16D
    bWFnZ2ewtzMRPrbvWr24Cw/DyzOXLn2/+9z3D6fwIwYGJrABAAHEwiItZiFotoxB5H4Jw527
    t4GaHwKF/wIN+M/AzPyT4e9fZgYhiZ8M4paSDF9f3dd79k1N/Bpr+KM/f/8zMDL8ZwAIIJav
    P/6BTXK2/Mpw/dYRhj+/gRqZ/jL8/M3H8OStNQPQiwwqijYM926xMHzg1WRgNzLo+X/l0dp/
    /35MYmZiYgAIIJZPH3+DDeBn/85gbQr1GOMvhi9fmRiUf7gwKCgIMvwCKvn6hYGBQ+A/g+iv
    L3Z/GBjsrl27J/7z559qgABi+fzlF1jPr89AFZ+BHgciJqAB3759ZfjL8RvoFUYGdjZgeAiB
    TQZiPobTp76Dwqfq+/dfXAABxPLl2x+wAX9/fAO5FuxkJoZfDH9+8QKthDjoP1DsLzDgvwPj
    8NOn7wwiIvwMbGysDH/+/E0ACCCWf78hUfLn02egC4Bu/Q408M8Phv+MrAwMAoj4ZmYGYWYG
    FhYmBkFBfgYODjaGZ89eMQAEEMv/v1AXPHrHwPDzH9iV//8BQ5/lOwMyALng58/fDF+//mT4
    9+8/0AtsYO8BBBDL/1+QMPjz4Qs8iTGCDGD9CfEyVPO3b6Bw+Q70zj8GTk52sDgTEzMDQACx
    /P/6CxoGP+C2Mf37xfDv7w8GmAm/fv0D4t9gLwDTFtAeiLdB9gEEEAvDb0g0Mv1EGMD49xdQ
    EmgblP/nzx8gnwmcuEA0C8s/SIADLQAIIJan7xjuny7kNgSmCbgGht9/GH7z/WEQg3vhP9i/
    jMi5CAoAAojl+Qfm0EUnufL42f//+gc14e8vxl88IiySMcxMiSA+OzsrMOD+gg1gAtoEcvS/
    f6AwYGAACCAWoNhdFkaGfGDsMPyFpQOgR1nVfDUPHrwccZKN4f/fv8Bw/w9JDxAanPzZPn78
    8hUgwAAU8lj2lXLEnwAAAABJRU5ErkJggg==
  }
  image create photo p_fileopen -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAPKSURBVHjaYvz//z8DJQAggFhU
    4q8xWJuIMHz9/p/hL9CsYydeMliLf2SQ4fvD8PMnA8P3X38FuLlYm7g4WH2ePP1kCVTy8tN3
    BgYWJgaGQ5d+MwAEEAsuk/8BXfb9+287VWWRDh8PFUs+PnaGutYjdW/efcvedZaRgQFoOAPz
    PwaAAGL59+/fvF+//7P//g1xwe9f/77+/fuviJmROdvNWaHKzVGOj5ODmeHXr38MjnYKsctW
    X1nJwsJ06M8/RrBFAAHEoqHKm5gYL8Lw9hMDw9c/DAzaGuwMP68/9EkKlJPU1hRk+PaNgeHL
    l79AJzEwONvK8B469aLz78U34UDrH4EMAAggJjZWhi+CAkwMnBwMDL8efmDQ/v+eoSlfR1JX
    W5Dh928Ghj9AQ5mA/v377y/Dqw9/GHS0RCwkxf4GMPz4Iwz0pzBAALH8ARkOdPqLC28Y9Hn/
    MFiFKMLDgZ2dgYEN6NJ/QC9Uzn7McOn+HwYpcW4GBX2VZlFVRr+bl57/BQggFqDBDMxAG36w
    czKsOvOG4dz7Twy/f/5m+PP3L8NfoAtAAXPl9keG+x+YGeR05Rme/WBlEDSR4Xtz/KXDn69P
    zwIEEMvf//8YWIC2HLj4jUH0PyuDqAArww9mdoYP3/8xfPz8j+H9l/8M3HI8DDqa3AyXgL7+
    DhR/f+f9v8f7rzIx/PuxFyAAAEEAvv8E/v3/APD0AADp4vQAi3shAPb5/AAFFwUA1OMBAO7a
    /wDq6f8A4+j/AOvn+ADv5O0A8dvmAPnl8uYABfoKAAf+zAKIhQVo/bGz3xk05XkY2IWZGE5c
    Z2B4++Afw4PnzAx37/5jePLoB8Ozp78YPjx8y/Dr3XuQE4AhxviPgen3I4b/f68BBBATA9D5
    Fy6+Y7Az5mS4AZR//J2N4SrQgPcf/zH8/vOP4cev/wyf3n5n+AVKOGycwCQIjC7GvwLAFHOV
    4S/DHYAAYnn46CeDpiobg6QYBwM/UI0WHwODkAwzw8d3vxmkmP8wCP78xiDB/JfhNzD5/gUm
    lJ9f/zA+efyP5/Wzv5cZGBn/AAQQy/07nxmcjfkZ3ICGgFzDADT7hyQDOAF9+/qP4acFC8M3
    YMB9+fofyOcBpkgOhpb537+/fvTvLAMzIwNAALH8/vKTwVWfG6IZBIA0BzcTAzMzC8P/f7+B
    GoDR+QdEA73x6wfD1x+/Ga7d+PwMKHkcmMQYAAKIkcVw/3tTMxEebnYmBmDUA1PNX7Dff//4
    BfQ3CP9k+AXU9AfI/vMLlEP/MLy6/2cRwx+GZJBlAAEGAJ2gomER8m2dAAAAAElFTkSuQmCC
  }
  image create photo p_quit -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAALGPC/xhBQAAAAZi
    S0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALiAAAC4gB5Y4pSQAAAAd0SU1FB9EMHwAdGDQ8O6gA
    AALvSURBVHicTZJfaNZlFMc/55zn+e337n3f2eZ6l272blQUG27lGoQW2YV0lXfhRXrXBhZE
    kFdhXQRFBUWQRcyorCDCbqIUjAIp+rNESxNdNGnpmHM2W25ue3/b73m6eMfocA6cm8+XL+d8
    BeBQWzoYs3y4ua+HsFQjApLn5ESIAfIIBEIOmghzYxP4xoah3ZPzB+VQWzrY1NU13PvcE1z5
    +hiSpogB6lCDiGBOCKpgivOehsp6zrzwIfncjSH5oNnH+z96jXOvvkRycwXxhjmPOKVyzzZU
    jaujJ1HzqDMwQ52nfEeVk/vexN3U283fP31P0tqKFQsUyi30791P2rOV/9f86eOc//w9VrJF
    xIx8YYF0Qysu1jLi8gqSeArlFrYdOApAbXSEiRPHaWhaR8eDOyn1bWeg+15Ovf40y1mNiBBr
    GRoB8YI5T//e/QBc+uwAP7/9PJXNAxQrHfzw8pNMHn0ffInbduxCnEdFIUZU8hzUUWq/lbRn
    K7XREca//QJxjnLnXTR39yPOcXHkGMxdYd2WHYgpUZUQApoTUYPixirAGixmIAIIYoaYMfPb
    CADltg7EeQgBJQYigqrVr6W2BoDU2wxxCaICgC81I+pWBfKIOWHx2hQA7QMPrQIOBBCpw6a0
    9NY/szhzGa9GHnIUAkGVhZlpmJ2k1LedSs8AYsbytWmuXziLmFK972FoXE/252nUp+SrDlzI
    WQ2H8eu7L3L3vre4fc+zVMfPMHV2BFFhy+5nsPZuIGP8uyPgHOa0LqCJooA4x+LsNKdeeZzN
    u57Cd/ayqbN3LUhh6g/GvjrMcp6RFJvIV5ZAwc2NXaLQcQvZP/N121mNXz55g6RUpqV6JzjP
    9YkLqE/BOXxjiWK1i4tffkq+BPZoS+Pk1R/PP7Jp5wNY4rBCAz5NwAm1xX+p3ZjFOyOYYE6x
    xhKXvznC7+8cJmkqDwnAxxvLgytLS8PphlZiLYMYCSHA6uQhX9sjgZApSVNx6LG/Zg/+B3yO
    Cy8hh4B0AAAAAElFTkSuQmCC
  }
  image create photo p_filesave -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAALYSURBVHjaYvz//z8DJQAggFhA
    xJKlR+5xcrEJff/6i+Hf3z8MX75+Z/jw4QvDly/fGL5+/cHw7dt3IP7B8P37T4Z3778wXLx4
    9eOH9/eT//+/vgcggBhALli3/vTXTz/+/f/379//acuO/g/Jnv3/xMVHYD46vnrv7f/i2hX/
    2dnMn4H0AgQQE8gFnz//+P3h82+G778ZGE6cv8Pw4Mkrhr9/sXvt9bvPDF9//2Hg4OTiBfEB
    Agjsha9fvzF8BjqVk/U/g6eDPoOdpTaDtqoUVgN+/PjB8OThY4bfv3/9BfEBAghiwDeQP38x
    PPv1i8HGRBVo+1+G1++/Mjx7/QfM/gd0ze9//xi+fP7KcPL0dYZPnz4xAL0DNhAgAIdzjAMg
    CARR9N//lhRUhsWYjbCjIa4Uk9+9zAbcB6GXUs/ExJjBPf8q4eByx46ONUMhntxa+wCfAIIY
    AAz1z0BD/gMhHw87AysrIwMzIwPDf6CLvv/7xcD48ycDO9N/BiFeLoZXQLFPnz/BDQAIILAB
    30DR9vk7g5IMH4OptjRYAuhyYOD+BUcfCH/58gMo+p9h4eo9DNcuXYEbABBA0ECExPHPH38Z
    Xr38CXQR0GZglHwHaQaKgwz4+vUnAwvQWV+BBv349o0BlgABAgjigu8/GD4CA+jjJ3aGP38Y
    QdEKTzgwF4DCgpkJaMDXrww/fwK9+x/iAoAAgrrgO8ObN+8YhLjYgd75BuZ//w7RBDPo+/fv
    DEyMTED+V2BAfgPqgsQCQABBY+EbMOkCE4gQP9AVwFD/9B2sGWQIDIMMAhsANOzPn59wAwAC
    CGLApy+sH16+YXjJzszwU0Wc4devT0AMjFpg6P8ARumvXz+AGKjpPyPDh9evGf59/wTUxcgK
    0gsQQIygwODn97jHxPhfiIHxNwM3Nwc4kfz9+w9sCyhJg9TAEs7Xrx+BqfAzkMX6DpiZlAAC
    iJHS7AwQYACwk94yqAwVDQAAAABJRU5ErkJggg==
  }
  image create photo p_folder -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwY
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAAB/ElEQVR42mL8//8/AyUAIICYGCgEAAHEqKZt7ReXkLERm+S0ye1uTx9e3Y3P
    AIAAYmzpXvy/uiQGQ+Ln7/8MPROXYtXU21mh++71kysgNkAAscAEHz99w8DMxMTAxMwEpBkZ
    mJmZGbAZDAKfv//fwsjIbv7//8+XAAEEN4CLnZGBlRmkEYSBhjAzMPz68Z3BvOgZFiOs5A0y
    r73glnXxBwggFmRh++p3GEpnFckxMDKzMrz7AoktJkZGho+ffvx78+H/hxkMMzcCBBDcAJe6
    DwwLqxQZvv38z/DnLwPDXyAGqmU4cY+J4frTfww//wD5/5kY2IA6btz8/Hf/iR+3DLQZLAAC
    CMUF/4Eajtz9z/DtBwPD95+MYPztx38GkN3MDIxACHTBPyAbaDjD7//gJAAQQCgG/PzFANEM
    xD9+MTL8+s0A1gwyGawdpBGEgXIM3yF6AAIIxYDffxnBBvz4ycTwA6gZ5BWY5v9/QAqAXLSE
    CxBAKAZoSjAyCFozA21lhFoN8jcDhA3CQOf/+/uP4eNHTpYEVw71nu0vGQACCMUAIR4QZiSQ
    eP8y/P/LxKipwszfs52BASCAUAz4AXTm+0/AsAAG+a/fQPzrDzA2/kLcDUR///2H8v8xvP3M
    BtYDEECMYhJKllKBu46Rk5EuzncwAgggRkqzM0CAAQDWqbzmpJES5AAAAABJRU5ErkJggg==
  }
  image create photo p_insert_table_row -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsSAAALEgHS3X78
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAABgUlEQVR42mJMiq74z0AAzF3SzohLDiCAGP4TACALgIABFwYIIJY3b74wUAIA
    AoiFmZmJIgMAAohROVeWYBjcmfQIZxgABBBLZ109w5+/fxh+//nH8BtI//kLov8C6b8Mf4Hs
    qTOm4jUcIIBYVjyfwfDt1y+Gbz9/MXz/+RPM/vHzN8OP37+Bhv5h4GYQwGsAQAAx/QAq+glU
    DMZA9u/ff8Aa/wJd8O8/ku+mMf4H4u/oBgAEEMsvqOZfQE2/gJpBNNgL//4xvHwHjCENLLEE
    MyjrPydAALHcXfIJ2TwGRiDmALKeaDwGKpBiYDj+DGI7iM3A8Ays2RDIspR6CRIHCCBGcCrB
    BaYBAz8rFsjQB2ItqOA1IL4IlFsMcgEjQAARTIn/pwKVHAMpOwPBIDZQ7MuXb1YguwECiGIX
    AAQQI67MNNeqAxEG5xkYoGHwgGHaMyloGDwHsuUBAogRnwPAgQcJbUYo+xeQzQ5k/wSy2UDi
    AAHEgjeVgDRiF2eHMQECDADq6h3H5k7ztAAAAABJRU5ErkJggg==
  }
  image create photo p_redo -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAJpSURBVHjaYvz//z8DJQAggJgY
    KAQAAcTIUAokeYCYC8xT4uBlSBfkFohSEZIV4WblYrj1/s6fFx/er/72+V8zw2+G+wy/gOqY
    gfgDEL9lYAAIIBa4UX8ZPISlhGe66LnIpemlM6gxqALV/mZ4/e81w5QLkxPXnVlr/+3N9xCg
    JecZmMCWgQFAADEzWANJFgYnDmGODSbqJiIC7AIMRx4dZtj/YT/DXf47DC9YnzOoSKowMAsw
    C966ccv0/4//64Amf2P4AtT3h4EBIIBAXlAGGnCUg59D/Pfv34v+fvx7iuEfEHIw6PHL8icq
    uCqwCysJA5WwMFzafYnhxa4XOUCNUxk+QbwCEECMDPkM64DO0Qc6KwUocJyBneEHAytQ8h1Q
    5BuDA6M84wKueC5ZJmkmhr+v/jJ8m/ltO8NjBn+git8gLwAEEDODIUMikI4EajwH1PiH4SMk
    cIDOlAIazMzwiuH/7z+/bX5pAkOEHajnIlDlY4ZrQLViQFUKAAHEwvCfwRdmGjQwGcAhzQB0
    ES9DHdCDf8Aueg3EvECszCAH1LMJaAkLw1OG6wABxMxgAvQvKFpYoaniByRwgKLXgWxjBgdg
    dFgA+d+AGBRwckAsBVR5HhgK7xjiAQIIZAADhgG/wdH0leEzw1GgY90ZhBlEwLb/BOL3QLwS
    6MZrDBnAtLMRIIDwp0RWhrtAW+KAGp4AWQzgkN8IxOcYOoFyi0FKAAIItwsYoPQ/oE8/MDwA
    hoE3w3MGNqDTdwPVZQFZ4LABCCDCBkC8cwMYOyoML4Ha/jPEAtW9YmCB6AEIIEZKcyNAAFGc
    GwECDADsA7VEijwkewAAAABJRU5ErkJggg==
  }
  image create photo p_reload -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAAuNJ
    REFUeJx9k99LUwEUx7/b3XWbzi3d3LRtTs1fzcpMypRZyDQJNegliyQlCot+0V8gvdlDUA8h
    hYRF9FJgqKQVZhr+iGrRL8s2U8fmNle67d67uevdvT2YErI6cF4O3/PhHL7niPCfaDNBlmlE
    vVyR0BoOspdPjePzRo0kXuPVAiSnacWNhuItZ80Nh3d5bB+FsfsDkXhaYmOhsxx1RZXabktz
    ScsOa1aGUq2EmMgAQZA11iRfsoXlbM9C4P4JaDAgRZujOGau0qoQDQG+eSiUMVGWuTDNaK6o
    Zhh32V4iMDDgRTguoNcFd2mE6Qu6Q/szzZp0go/BPjEJWWgaKpKFKa8sx+ueyRW+hR9NAgIB
    AJ0VKGjQo7DBBH1tMgLnP8FrESLPqSWmVm1M1jy99/2aY5p6q0uNlKlIHkmJqVtFy87xXhcc
    YgBI1yv6rBebh4sO1A6RSuQDQOs7TM/YFusHOr+8FAsYPN6/cmloxHN3mXLBKAtBnSJtWnch
    RR7bnG1WEGHawPIR+NbWOT0BRxtYKwAxAAQ9XPusl24s1AgynVJSDkRXAVGW98M7b5IJJAkJ
    9ADca5ArAI/VxPxPzDIR7hdkEn2iVKLFGtm/xL6IuZwwGFSSTTrZGQC4bUF+lwUdt0qhWYPp
    VFCyUT4lsLgMeplbWXehVoOgWsm3aIwaKNX6ku2UI8pzkFtONLVzLF1VsbLU+8QP+qAaoiAd
    mZn6EejzMysPeuYwJQKANkBcXE9211VnH0owbIPHx8Ex5QzuOXpSxZNSDN68MTr7YfLIhfeY
    j3uJw4BgSeJHggxTmSGn9ampCTAV5sskgVmQ3lfIKrdmMuFojYXwjPa5sBD3lPu9oPbJucdO
    D5XI0oHcBMolTWT9IoRD4H12hIUknc/tme6ZE8b/Bog2jgQA13cib3exaqzYrNbY7UuUfS7w
    cDEgdHjewPbHlfWI+42CAL93gfE53fSdRSrWde41vsbTAcBv28c0QadsTMsAAAAASUVORK5C
    YII=
  }
  image create photo p_undo -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAJGSURBVHjaYvz//z8DJQAggJgY
    KAQAAcTIkAskuYGYB4xVeHkYN/z8/f/kr48MyQxfgWLfgBhE/4ey3wMxCxBzAjEbAwNAACFc
    8J+Bg/E34zRHDV9teRHVMIbfDIHEuAAggBAG/GCoUtdRd7W1smXwdvXl4RbingQ0xIyQAQAB
    xMxgAST/MMTy6PJ0qfqoMt9nvs/whv01A7sUO9+b62/s/n/+v53hL9jhDEADQRYxMICsZQXr
    ZgAIIGYGIwYnBhWGRcwxzFwvOV4yPIbCj3wfGf6I/hH5d/mfNsNPhh3gkMBiAEAAgQzYysDB
    IPPn9p8/vz79Yvqt8Jvh9+ffDD+3/2T4d+XfL4YPDMpATd+Amg4y/MIwgBUggFiAzk9nuA90
    5HUGfoaXDDMYtBjkwAoeAvEJhn5gaK8E8l8x/APyP6CEmjIQTwUIIGYGQ4ZHQMZjoPMeAk13
    ZpBlUGXgBYpwAPEdBnmG5ww7gfZfYfgMjCdGsEYOIG0LNGg10NonAAEEMgDsF4bvQDveMwgA
    lXkxyEAN4AO66gNDBFCzFMMXoMv+MpgDxTIYuBj6gGIgD0UABBAjMLmANDMwfALHhgjQsJ0M
    PsCQ0YEmHpCz70MTEcjQz0B8Bcj7wOALTID7AAKImcEYKvgFnC6/Ac09DfSQK9BQQXBAAVMb
    gwQ0pT4D4utAL79hiAVatBskDxBAjAwp0OT5ESmA/jIoApNrLdCpoUBPsAD5IPk3wDBaBnTl
    TKDIPbBaYHIGCCBGSnMjQABRnBsBAgwAfPOnyr1HEEIAAAAASUVORK5CYII=
  }
  image create photo p_run16 -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABGdBTUEAAK/INwWK6QAAABl0
    RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAAPsSURBVHjaYuTz2swAA////Wf4
    z8lqAqT1GD/93Pv/399XDD9+a/5jYbr8n4nx979ffxgY/v5l+P//P1wPQAAxwRiMf/+pMf78
    o/6Hj3/e9yijuX/URZb+5Wdf+9NV/cA/Qc5ahp9Azf/+MaADgABiAZP//hv91JNczvj1F+dv
    dQmBf/piDH9Y2axZ3v5gYNAUZvjHwlzJuPM60GqGGwy//x4D6njMwAgxACCAWEDOZvjL7PFD
    SluNwYmTgeHTbwaBhfcYeJ6/Z2BkY2RgPvuK4a8UL8tnP5OGjwy/GZh3XlnF+P5rFNCzf0EG
    AAQQE8hZQL/KMp99z8DwlYGB+9hbBpHvn59xSLKsZxdmOcYixPCb+8cXBjl2NgYxRWmGf7wc
    yv++f2f/9+sXw7+fvxkAAojp/39Gh98aEnH/zYHB8ek/A9e3Xzc5hDldmNhZwhk5mNw5pHlt
    2VUE9jEJ/mNQ/PObgc9EQ++vmNAyoBecQd4ACCCm/4wsWr8UlLn+aUgwMPxmBPqGnf/PC+ZX
    /z6z/+Zk4/oCDJ+TTGzMoZyqgge4FFgZxK2UWRlSfP3/y0r2Mvz+Iw4QQEz///75w3j5NwPD
    HqCHnjMwfBbnl2DgZy3gF+RiEBAUYhCUFGXg1RV+xyHDU8Yiy/2Ti+M/A+PHPwz/3359+p+R
    4QtAAABBAL7/AwYgVP8C9e8o//H2Cv8OEgv+EgcA/QIDOxYG/vsICALSBQgH+RD9BQABCQLm
    CAMBR+voAR7q0uoAANPWwAD8+8ACAEEAvv8DBiJZ5wH796AC9eoU/BsbPAAHCgD4AAYC8QAE
    QwYEADcPCAETCQUBDAz6Aknk6QFG1uAAEfwMDgAABQczAOPj2wKI5e93YFz/+8vyV1rcjkFc
    iIHhF9ArwNj8/Imd4fIPdqc7X/878XAwfPn+k5HnK0gpKO4UQWpYQanyBUAAsYCT5d//fP+Z
    /7EzKACl73xkYOAABq+SIMP/CwwM394zMnxjYeBhAKpnACZGht9AUw5eZWA4dnktw5+/xwEC
    iJlZMgCYjhm+M374eofhyQsPpgs3VjKdun3pvzSrPgOXCCMDMHkwqAKxNRAzgwIaaOHWXU8Y
    Pn30Y2BhfgsQQCwMsIzxn2Ef87Wnev8YmV79//aTEZh02f+nq0cy/AbG0Oxt1xm0eQQZsuwk
    GPZdB3rzZz8DM/NrkDaAAGKB5wqgq/+zsb76D8ptzEDj3n57w3D/BgPDg3dfGB48jGV4wabD
    8PJrKcP1Z3MY/v6ewMAMyYcAAcTIZrIIJTv/BSbR/7+ByZyFSRHI12f4BUx+TIxbGf7+YWX4
    8UeSgZX5EZAP1wMQYACFGXIs7PEZIwAAAABJRU5ErkJggg==
  }
  image create photo p_run32 -data {
    iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAAAsTAAALEwEAmpwY
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAAIo0lEQVR42mJkwANid3wX+P//v8G/vwwG///9E/j3j2HDygCeCyC5gOUfBBj+
    MwhsiBJ4wEAmAJrNABBATAQUFHxhYdx/WYyl/4YIa/3fP382mebOloTIMdT/+/v/vvf81xs9
    570NINcRAAGE4YCEPT8M4A74xyB/5i0rw2VxVobz0mwMt8U5ZKWtgm/6L31/AGh5wgEjXoaL
    mrx+39gZ1zv0XM0ixwEAAcSCZnkC0NL5sdu/PQQGdwPQEoNfz4ASP4GYnYHhohw7wzUpNl7+
    r3/tOb//Y/jEwwzG7F//Mkg/YwoFqloJxG9JcQBAADEi+/wnI8P5nVJsDKx//jPovPrFwAKk
    9ylxMPxmhihj+vmPgePNTwa2L38Y/nEwM/xjZ2L4IsHBwPn9L4Pmpc8MPM8+XWdgYssCOtzh
    37///MfKpAsJpQGAAGJBCu6AC39ZGN6BLAPil7IcUAmgxV//MYgcfs0gcOsTAwvTPwZGhr8M
    TMxMDMxsrAxsXOwMn+S4GO6q8DDwCLFpyt38vP+NJNBR738xmNRelT7TrB2GzxEAAQQPgZjt
    39bvPsUe8FIUmCwsgQJs0Dj6/JtBdvUjBk7G3wzMLL8YGDFMYGRg42Bn4OLjYfgizsVwSYef
    4Q/QA8y//zFo733JwPDs0cRLvRYFuEIAIICQQwCUrRgYbgKxEhBLQsTF971k4OH8y8DOw4Bw
    FQb4x/Dt5ycG3rf/GBzP/2c4qc7H8BOUxX78ZXh9adNJoAIpIH6GTSdAAMFzATDOLhio/GRg
    ZQG6QgQo8JuBgfXDbwaetz8YeGVYGZi5GZ79/fli2e8vD2eBaCb2P7fYBNgZkPFv1t8Mf/58
    Y7B9+JWB8z8jww92ZgYuOQsToGkfcEUBQAAxRm35qgB07HxowmG4z8TCcEYCEv9cr74zqJ57
    AYzv633na72XAYV+QQ37C8TMsj5Z6lIeiXmcwlK+yIZyMLMx/OPkYjjMxsKgePQ5w48HFwPv
    LfLfgC0KAAKIMXLTF4eXzMz79/FzYLiO+80PBoUjt85e7dFPB3IfA/ErLJ5gM+3ZnsCvadjD
    xMzMCxPk/8vBcJODg+HRy+8MQnc/MrC9+8EA9GTjrUlqDcgOAAggJmB+N3j3iREc5Oj4HyMw
    pTOxgAz9jMNyEPh1usRz1renV71ZeJk/w6LjlwADgyowYX2Q5GW4ZyPDcMtDCZg4mepF7GrN
    kTUDBBAT0FUCv14DHfCHAQN/52Jj+C8koCZply1HqEA5luZ8+N+vj6Fs/GwMIMwsxMrAy8HI
    IPz3P9isf0xMDO8VBRg45V2qkVMzQAAxgeL9/2+gouMMEAzKBe8QofBOlIdB2iG9Gk8WgIMd
    joo7mdj/LWXlAxZmQMzICywl/0HNegrMsc/+AjMaOESlYHoAAggUAg/+MwNZ36D4CRDfQ4TC
    SyEeBkYRcQeTsj0FRJXtnJw1jEyMDCDMDCwjGP9BQpfn1UcG7lfPn73eU9gISsAw9QABBHaA
    DP9PBjVJYGIR/M3Ay/8HYvlHiMv//GNiuC4qxMCtqN9p13Exh5ADVkgyPgCmrovg1PmXkeEn
    NCS53nxg+Pniwuyfz0+B6gp4FQ4QQEzAlHiB9c/ndskvF2YZsN0/YCr0kYGFBxglXxGh8AWY
    Nc/xCTCwi8lOdpt8s5pwDcN4n/MLA8MLIH7zDWLGVwl+BmYp83qx0AN+0GwMBgABBCtZQUEi
    DmLY99yc9IhbOvg+DzcDernLCSzxjBm/M4gy/bjCysFeuyqQdwM2+1Ou/9/P9InBYd0joAN+
    IsRZPwMLtYtPGBhfPKp9u8G5BZQNAQIIVhT/hRWVzBwCysyM/5DciADfGZgYjjBwMyiwsOlo
    /P+7Pmbb148srMz7mVmZLjKzMDKwsDDJMzEzOjx7xaBw6DnQ8h+o+n9zcjD8EOZlYHvwSRbI
    FQNlbYAAQmkPOE5+YfCJhdngDgcvJOXiAA9+szI8+M7KwMPyn1+Y/X+AMAdDABMw0X35wwi2
    9M13NA3sUAyMDuYP3xn+fHp4C9YWAQggFAeAquT33MDcJgoND1jwgSoiXmj2RApSkIVffjAy
    PPyIJz2wQ/0KrHWY/35nYH7zheHnpwfPoHmOASCAUBwALBPsv/CyQCxTBOK7QCwNxHxQBSCH
    vQHi5wxYowgr4IQ4nunTLwbeLXcZ/jw70fj9yoRb0HqFASCA0B1QqHD/4/kvQiwMXwVZGRiA
    9ZjktY8M/4E+f63Ky/CXDegNbmjSfUyk/ZffMHBse8nwl5WJ4c/jE41fDqduhmZDcAgABBBG
    +8Ks4XraX26+mXeNBRkkL31kYH32+uzfP78//xUWdXhsI8rwVYoDEjJ30TS6IbGBqZ/hBjSb
    /fnLwH/sOsPf9w+Wfd7l2we1/C2sMgIIIIxW8akGzVl/Xj3JVT70moHx1pVZl7r00q/2Gaf/
    vHYiU3bD/Wci594hlWOw/AUtqGFYBYiFoemKhZnhNxcHw9+fH19AKzSURitAADHiCT2QEQrQ
    GAdnUcX4nT6/dbU3P7EEJoyrDPCcIvDiBQML8w9QFDK8j5Zj+A9ssDKAcgJQDfujFwysu+/e
    +n4yP/3v27PXkGtVUAgABBC+jgnIpWeRm1Is/Ipf/gMtYZABctwhmFXxBwPX43cMf3YvS2e+
    fH8zz+bn8MTH+vcdA9u+p5+hlr/E1mQHCCAWUtrw4JoT5ABYOf73L4PAoWcMv15f6ftwtPos
    t1HFNg4WIXt2cXY+losfGZhuvH32897yEqDloFbULWx5ByCAGElxgGLmDWAf8f/5H6LABocY
    MIN/+8vAeu7hgZerbEtgLSbBwNNBf//+bvvz9sqKb0fTNkOz211YqkdvkgEEEEkOgAIuAbMS
    fxYhHXVGdkHJjydalv96efoxWr4QgLarX+HrKYEcABBA5DgA7hBoMQMq616SUDShOAAggBhB
    xEACgAADAHLXUaLd0LtWAAAAAElFTkSuQmCC
  }
  image create photo p_pict -data {
    iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwY
    AAAABGdBTUEAALGOfPtRkwAAACBjSFJNAAB6JQAAgIMAAPn/AACA6QAAdTAAAOpgAAA6mAAA
    F2+SX8VGAAAB0UlEQVR42mL8//8/AyUAIIBYQATjrC+LgVQMw58/DBD8C4h/MzD8/o2gfwHF
    fiNhY6sl/9N4YgECiBHkAsaZn/+f9uFleP+DgeHtNwaGN98Q9JuvQPZ3BoZ3QPwBKP/5JxAD
    9X+/sYbh/9IQRoAAYgK7A2gLIyOQBvmGEYzggBHKh9FwD//6AaYAAgjsBZCTTJY+hjr5F4T+
    /QvhdJDYr59IbCgGAoAAJIzBCQAgAALtjPbr03xt1iRBVo/jENF/kLDGFlThpopjywGuiUPs
    2xXU+nzTI4DgLmBiYgUrAisA4uNRYgyf3zEwfPnIwPD1MwPDjy8MDD+/Q1xf9+cvA8O/v2A2
    QABBXfATaDoH0GSIzSCbPr8HRst5zGirA2JwwEENAAggiAE/f4I16y2WhCts/YA77hmBFjD8
    +wdmAwQQ1As/wf4EgbflEEVfevAYAIoPqAsAAgjhAqgBMPDjK8S5OAHUBQABBDXgBzjwkEHN
    NwRbuJOB4XUZMMExMmIYABBALLBEAYoBmGKszkbWjGQAQADBXQAK/Yc5X6HxDYx7YFwzMjGC
    4xxEYwBoJgQIIHg08keuZGD4+wdhOgj/h9H/EfR/JBoIAAKIkdLsDBBgANuWwGjOwtHgAAAA
    AElFTkSuQmCC
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ImagesGC {} {
  image create photo Webcam.gif -data {
    R0lGODlhIAAgAOZlAHJycj8/P+Dg4ICAgEBAQL+/v39/f7GxtV9fX+/v7zAwMCYmJlxcXI+P
    j2BgYCEhIeXl5fDw8FVVVcTExxAQECwsLRMTEw8PD6CgoAsLCy8vL/r6+kJCQyoqKjk5OZ+f
    n9DQ0FhYWm9vb8/PzxwcHAcHBx8fH7W1uW5ucXBwcK+vr3l5fEdHR3x8fI+Pk5CQkCAgIJGR
    kRUVFebm59/f37CwsIyMjGpqaqWlqVBQUExMTNLS0r+/woODg62trszMzGNjZfLy8qSkpzw8
    PdXV1djY2szMzgkJCaCgoaWlpT4+PpSUl3Z2dri4uPX19bGxs3l5eevr64WFhfDw8U9PTzc3
    OOHh44qKiioqK2dnaL6+wNbW11ZWVpqanuvr7NjY2Nzc3sDAwAAAAJmZmf///////wAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAGUALAAAAAAgACAAAAf/gGWCg4SFhoeIhxEvBGKO
    YgopAomUggMUQ0gQZJwJKgEOEZWGEQQPTxucqqoFASCjhAocW6u1nQSTsAMPRqo0DQbBI6sJ
    ObACYkKqBmIdHh4dJVQJqh8YoykcYJwiFi1XAOE3JBqrAaMwLk5kBUctYzHh4jIGqgi5iMgH
    nAhQX2NjbDCRJ8HEsmuJwlTYR0ZDAQgAAUqRJ4YaGQMDKClkWBFiRHjhZBTghFHjgwOpAhQI
    8hFgDwBiVInISEkMDitkRCAgs6OlDRblOGEJU0kBiglkaFwYCaFJEoADSqjgRESMKEoYFs4g
    8+FCA04/xgCwsJNTFgewYAA5MYWdCRMIrxiQ8KrKhxh8GjO4OLGVzAhgIzlt0PKAJiwMGVYc
    mOCl1oYiSx6ghTWoBgUOXQ6cmMCZB44QGQxTFiTAgZgKIVCsCFElAwGio0kpeNRBCd7YhRo5
    YsAAd6JGJVgA6O37UCMG4SQUNy5GgnMCywkJuHTBwwISYnLUiD5ADILAED404GJBwavYAhRo
    GLYqSkQdYl6MBkFBTID7+O8v2L/AgpjJowgQxoAEFmjggNH5FggAOw==
  }
  image create photo Earthcache.gif -data {
    R0lGODlhIAAgAOYAAP+2ACg1ODAwMP/NLgoNDqCgoH9yLkBAQGBgYL+rRdDQ0A8OBRAQEP/h
    VjxPVJbG0jBoKsDAwN/HUFBQUAYQAxQ3Coy5xD85F2SEjJ+OOXitn/Dw8BhCDP/SOf/fjwQL
    AgIFAQgWBBxNDuDg4P+4BSAgIGCWeG9jKChgHR8cCxIyCR4nKkZcYoCAgHBwcLCwsC8qEe/V
    Vq+cP1BqcP++ERAsCI+AM3ClkpCQkP+6D1CHXr+/vxQaHP/IPx5TD//YRf+/H39/f19VIm6R
    mv/KKJDExkB3RIC1rFiOaw8PD3ifqP/eUP/bSjJCRv/Vb//PM//xzw8LAP/sv//afwobBf/R
    X8+fF//BFxY9C5+fn8+5Sm9mT9+fAP/jn9/f3//JO1aJajhwN29vbz8tAIKstr+xjz8/Pwwh
    Bkh/Uf/135jM0xpIDVp3fv/or49qD//kXCBZEP///wAAAKDU4P///wAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAHQALAAAAAAgACAAAAf/gHSCg4SFhoeIiYqLiCM4
    BwxyknICCC+MhyMIcg4YFhYEcloyJyklBZiDLXI1YHOvDkJyb7RvMjACCpgIFGtwJq8PBBKz
    tbQGcqiKvD5wcDqvGDBvcgMdDcYZyYkuFM3OcK9NNkxyAAAkA9i02rqGESAi4M6vw0RyHj3n
    NEu1JwKHBKiYBwfCHDIL3nyREydOmxzo1sVIoWxQhA8E4WiYM+RCAycMG0KBeKVWBoCEEAw0
    cgSJkY1zZhj40SVkQw/nOtCKIWcEIQbyYL56xcLAEyk2GwIBQKPWBRyEMKJQM3RogAQD0iSN
    gxPAOgMTCIX4VdUq1iySkpgRE6QMFwA/+2jZOECICpKyZq0k2RHHy44gW8ZEkQPjgoETdAc5
    wGtVRgo3TqZU6bGUBJElCTIsoJBYUADGrwIsEPJkgGlrTGhJMLCgAofOdHiAniM6QQxjEhIY
    uCCniRI4FcIOkmMBNAYHASZJIhCAxZAHc4rAOdOC0AQ2s7MPvQEHhDtBL2Rrf1UEgjM0MCFU
    QEmoBIbxc0wQRBEGzoeKFglA1849Yw3YKa0wng4ZYcHAd4VsIIAD+4GGAkEcgHBJIgquUBxo
    5oGjwjaMuEDADA1WlSEHIZSA4CIRHEAAC0pUdYQIFYTAQAsbpEKIAi4IIAdzK8jBwAQF1Gjj
    kEQWeUggADs=
  }
  image create photo Event.gif -data {
    R0lGODlhIAAgAOZQABAQEPP2pYCAgP39/Ts9IfDw8KCgoMDAwODg4CAgIGBgYLCwsDAwMDk5
    ONDQ0M6WltXV0uXl4nd6Qw4ODkBAQOnp5vX4tB0eEPL2ncfHxHJycCoqKhwcHA4PCFBQULO3
    ZebKypWYVI6OjIaJS/Hx8FVVVP398KurqO7u7Pr72erq6PT081lbMtHVdiwtGbm5tv7+9/j4
    9/Pz8cLGbfv7+3BwcGNjYu7c3OruqJCQkPv84ezwne/zrmhqO/f5w/P2su7u7ujrueDkfu7z
    jOzs6urtroCAfpOVZaSnXEdHRt65ue/0h750dOTk4QAAAP///////wAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFAALAAAAAAgACAAAAf/gFCCg4SFhoeIiYqLjI2O
    j5CEBwsClZULDpGCBQYeThcEEqISPQQdAAoLjgUCAAQhLUuys7MzIxcJBosHCQQftBgBwgEY
    tEshFxQIiAZOI7MWKTBP1NVPKRazQiwAmYULHTOyASbW5tUmPztFOEcAy4MIAEiyPuf31StN
    +zYMhAosZFmgpoSJwYMIDz54QmJfEw66BAGItWTak4IJMzJZOCDCPhEeBrkYV+2GkpMoU6IE
    QU3FPghOBhEgia8mNRQOYwqauQQDPo0GFz5xuU8nlA6zdNwDuvEJDYcvAAxK8GsJjwHnVKYE
    MaCCQw0KBhlwMSRIkwo0bFrr6jDDhAOEeBiUcBiBBNaaA2R4fLmhRiF5cx02ISIjxt0nMVag
    2MvXnyEHDBpkEEy58r4THDwUQFSgxgQNky1X1gAgByMECpwkMfJC9GUAQB5xUpDAyYQGuJ3Y
    3rdBgKZ4B4JvdtJERILfiZxA4KAK+aHIFJwjotBNuvXr2BcFAgA7
  }
  image create photo Letterbox.gif -data {
    R0lGODlhIAAgAPdKAAoKCisrKw4OD21tbSAgIExMTKSkpJGSloCAgLKys9HT30FBQeLm93h4
    eMrM1VdXVysrL9fb7BAQELq+zMDAwBUVFbCwsNjb6FZXXtTX41BQUKCgoI6OjuDg4NDQ0I+S
    nTY2Nt/i8sDBxxwdH6yvvfDw8GBgYDk6P0VFRpCQkMPFy5mZmdve7cnM3HBxdbm6vbi6xEdJ
    TrW2uGRmbqenqJ6gqW9xeWJiYnBwcM3Q2nN1foqKjJeZoBkZGjAwMF5eYX5/hYaHi4mKkMfI
    0Ly9woODg0BAQK+vrwAAAObq/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAEoALAAAAAAgACAAAAj/AJUIHEiwoMGD
    CBMqXMiwocOHECNKTFiCAgITRlJ0mKikIgINBP4RWHCjAYh/Gxx2sIDAiIR/AQoM4HCkZs0d
    /4wk9LChpQQAAR7MtFmTyBAFLJLMqKDToIWfQRusIJqAiIMMSZNoTRJBwICmBY3ALGDgiAwR
    Di6E2Mp2K4YFRcASNNKgyAIANiK03au1xb8VcQ8aKVKzSAABOvi2jVGAyAG5AwcfSZDgiOER
    HxQnIQHAwIXHggkTYeCgcpEKmPmeeCAiCWiDkoloJV15QAUIJNh++Gdg7euwotkyUFHz5Inc
    LQQE6DHBNWSBsfeGCAKgyAMAI/4VOPJAwITfc4O3kZ3g1SaHskSODBCA4rmS6GzJDziigEEO
    EUQyJFFwpAEA9/BpFQEE2ymgmAIGFKBBaEfIJiAECxzhQFsXwAAEChXk5AGDDiaBAQFHqJAE
    AxnU4AIIAEigQQoUKCTZC1p9aAANPPwQgEgmbLBhQ3TVJAQE/ywQkg84WLBRRAgABcA/OSHQ
    IkdQRinllFRWaaWUAQEAOw==
  }
  image create photo Locationless.gif -data {
    R0lGODlhIAAgANUiAJDY0mrP/729vQtVOQ9RNIHDxPm/cfyjVv/gwdX//7Lt/6LS7AhOL+dB
    De+kYJHc2I7Z1LLe/xBSNZHS2OxgJXmOhAxYOgRPMEBqV4rV0J/q6BZbQrLy/wVRMQBMK7L0
    /////wAAAP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAACIALAAA
    AAAgACAAAAbqQJFwSCwaj8ikcslsOouh0NMZAoGi02VIcD1IpVnjtutFYMPDcehwaCAMhnNY
    3X7DHQ5wVt0I2fN6e1xRZnGBc4MUX2hQg4eMQmqQR5KTjVeWl4+TlZkiUaByjFEdpR0fqKJP
    UR4eHRuoCgkJqZtIUQStHgMfGQsKshylHqqUurofDw8LvwmmuraRucceqBMBARcdBMPQtwQE
    Edy6vBDYAdwfpQQW4I8hFRgMAwADA62oAOca2Oql6h8OhZDAYB4AVPY+FFjIkCHAAB8CBAph
    j9cHAAd5YdzIEaO6cwHThBpJsiRJTyhTMgoCADs=
  }
  image create photo GEO_4boxcolor32x32.gif -data {
    R0lGODlhIAAgAPcAAAAAAP///ygYGh0REx0XGCMcHywhJhsLEyIUHDInLr25vCEdIf/7/xwU
    HS8tMSMdLAgFHBAOIyoqPr+/wv39//7+/6CgoRMWGxYaHg8VGUJERTVZYihKUMHHyAClvQCe
    tAGhtwCpvQCktQChswCiswGitgGitQKitwKhswOhtQOiswSgtAWltwagsgiouQmltgqltwym
    ugymtwucrB9/ipzX3rLk6gCmtwCjsQChsACdqgKktAOhsASksgmmtQqltQqmtAqksAuntw2k
    shKnthWTnhlsdCNeY5ba4QCgqgKqtQKlsQKjrhOLkyNtcrXj5gqgpiZBQh0zM+Hy8vr+/jY3
    N/z///7//6vAvyqxXCqkWR63VSixWS2yXDePVpzesyG6VCO2UyW6WCm1WCq5WC21WjGzXDGz
    W8LozySzUCiyUyu3VSu1VyqxVSy0Vy62Vy+5WC60Vy6zWC+yVjCzWDG1WTq4YTm2X0CUWqLc
    tCyyUy20VS+0VTG1VjGyVjKyWDS0WDe3XTm5XzepWER4Uy62UzC1UzGzUzSyVsDnyjtUPOPz
    5P/9AP/9CTQ0LP//+//5AP/2AP/6AP31APv0Af/3Av34AvvzAv/6A//5BffuBf/2Bvv0DfXt
    EOzkH/77rv/xAP7yAP3xAP/1Af3yAf7wAv/1A/7zA/3yA/7yB/3zCf/1Cv/1Df/yDf3yEfns
    GebcGvbsI+LXJ8S7Ma6nO/75i/74lf77s/76uv/93NfLKW9rNZeRS/72iJmPL9LDQtDDTrWz
    n2NeOFtULVJLLoV0QUtCKWpfRXNpVUlCOPzlxPrkw/305/KgNvzkw/+lMv+iMvqhMv+lNfmi
    NfaeNf+kOPukN/mhOfukOvmiOvmhOvqmP/ejPvqmQPqlQfmlQfunQvmmRPqpSsqJPOieR/+e
    M/qdMvygNv6iOPmfN/qhOfqgOvuiO/ihPPGePfmjQOeXO/mlQ+6dQc2MRaV6SXRaPf3Zr0U9
    NP7hwvyfPvzYsv/+/VhJPCUaFyMUE////yH5BAEAAP8ALAAAAAAgACAAAAj/AF15erVJ1apY
    q1oddJUqFa4rAdDACXQnEMWKFjPawUSrmCtGs+75kgSLVydQk25RiKjmz58ycdywiUOnJp05
    dlZpOsYrEy8JwmId8wXplCiVEetsAWPoEJ83caJKtdNKFi9kvH4B+7VLmKtRlEJ9ChBggiJC
    XriIWeNHzpu3b/bYcTVrFi8NVXwNuwfLUipUoG5ZCaBgAAIDirx0WdMnDh8+dOIIasiLlq8F
    ER7QahQpFajAgzs4MtDgMCEybuLsaSxHUCtNslbJcnThgQNfnSBRSkklohYvigxU0bIGEJ0y
    ZebI5STKEqtiEYTRIhZdViVKgiOGERMGDx4ydPgU/4pTho4fqqgiwXLwYBYjT8ci8IMV6VaF
    AInczGEjho1TMIP8IRMbOaECCS0RJBNLJJJ4kgwEwnTyyWBoyHTIG2a8MYcX93ghBiJ82KHK
    KZYUA8EumYSymy68FCNLdmjo8cYfZ5zRXQINKNJFZK6BYsowEewCSSmmoGSJJaJ8slKMckTF
    RiFZKEKAAR7y4Vokr1C3CyOnlBIKKaeQIgouS47hxmN0HCKGFwYgQMgWiNiB0ivGnCjJJJ9N
    IkooYlmxTyJpBKqHGm2koYUDBygyRhh81FKLLcFYEIyjlDraSy4U7LPIF3lw6mkeFoTaaR4V
    XLEPBQzsQwUDAVBgRQVUWP/BgBW9kWVrAK8G8Mg+t9oKUa+3UvFIAFdYAdEVv1qhLLJXIFED
    EliE+qyz09YwxT5WMKPPtvrYY0+334JrDw463ODEAAY04UESOXzQrg42kIUPNeaYc8450URz
    bznRXJNOOzHg4EET/fRjRAgrgEACDiAk8cQjVyxDjTbUUCOPOxg304w88FATDhEjiDBDFf5w
    AMUSOJDABA85xBvAMumcQ049/NRsMz/0NAPOECAsocQR/ghAww0jlJCCCSM8QZYz6ajDzT0Z
    NEAAARkYUIw754QjxMIkFOEAB0WYUAITTCzsMtPaSFMPBgsscAE/86CDTjvhyMADwyQ0sYQH
    KOz/cIMJLSRxdtPoFINBAQ/UI88z07QzTTgwtIADDibccAIPN0DRhAse5KB0AExnUw09FzhC
    TzXs5JNPO9eE8wIOIvCAwt0q/GzAEUX0MLg60oyjwTzPWDONOtpoow7kKZNAAsssFJGAP/0k
    EEUHS6ezDjTjIGMONsV33zoMyS+MAwpB0BBFAQgMoED166ADzzHwYHNN99p8Hz4OPOwAghLl
    byDFBOxjRzvqMQ5r0K9+yFPewlDQA7KJQAk3YMLn0LaOatSDHtBoB/1a94IcKBAHIzCBCBQ2
    AhC0rHrXUIc16IFB1l3jhdeIBxF+QEMg0PCGPvDBC1igtCso4x3e8MY2DMhBjm5844hIFEdA
    AAA7
  }
  image create photo Mega-Event.gif -data {
    R0lGODlhIAAgAPcAAAAAAAkMDg4ODhAQEBMZHBwcHB0mKiAgICoqKiYyODAwMDszOTk5ODA/
    RjpMVEBAQEdHRlBQUFVVVENYYk5ZYk1lcGBgYGNjYnBwcHJycGB+jGWAjGqLmn2ktqkJC6gV
    GKwWGK4YGrMnKbk3OKU5QJx0c75GSMNVV8VhYsllZsZ2ds50dYF0gICAfoCAgIuBjo6OjJCQ
    kIGcqJ6aqqCgoKurqL2qtLCwsLm5toexxJyyxZG+0pvL4KHO4a3R4K3U5bTY57rb6cuFhc2R
    kNSEhdmTlNufn9Wtq96io9iwr+Syst7AvsDAwMfHxNDQ0NXV0sDe69Hd4M3l79Po8d/u9enB
    wuDW0+7Q0e/b2uDg4OTk4efn5Onp5uzs6u7u7O/v7uby9+z1+fTg4Pft7fnv7/Dw8PPz8fT0
    8/j49/j7/f39/f///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAP8ALAAAAAAgACAAAAj/AP8JHEiwoMGD
    CBMqXKjQBYYHEB9YcHGD4UEaEQAACJDAQYUKExIY0KjARRaLMQ4AaKAhB4+XMGFycBAAgIWT
    CJ0oAODAZUwgQH/E5LFDQ4ABNA46GWCgw8seUKikWUO1apgpQGDucGCz4NIGO15KmVq1bFkw
    WX342NBVYJkBCZ6GMUvXrBkteGEAqPgPQ4CwPebWHUz1Ll4IB/69pTBDh40qa65UIUNVTBXI
    VCVLFhM5yRErTQDQuAHghYfTHkScFrFGCeoRa4ig9kCkCmoQVhhEcEFghocPSE4XUV1lhAck
    JzzY9qAkOREyRYyA8HAkwwMXCXyTWHN6jQnlsz18/zcRm7Zr1NUfxAigPUz371VCeChSpYiY
    081pfzdihPqFB0wAwIIHJATxnnKyobbGCrMRkcJsQyBgwT8HLECggR545wFnVRBBBBJrWFbF
    g0WMIYQKR6hQAgBM/EMDADJEgZcXahC2QgjGhYAFF3hp8QQCDwz0gAA49LjFGTXS1ZwJKCzR
    I14SDODEQGUoQOSTWnRRBhpccnnGFzw++QQEohVUJQAZYKnmmjUUMABfBrkAQAEwPLHmmhkA
    8ABOCGVhgUYQtFCDmk00gVdoGFikWAwZaTQnA44CYCgDiSlKkBNMxODCpkwEWEMNZVq6EAA1
    ACkqQwCQ2eKpCmk0IasNRQoA66y01mrrPwEBADs=
  }
  image create photo Multi-Cache.gif -data {
    R0lGODlhIAAgAOZTAA8NAb+/v4CAgB8bA/Dw8A8PD39/f/7+/u/v7+Dg4DAwMKCgoF9TCRAQ
    EOnp6f39/cDAwPPz89DQ0F9fX/f39y8pBPz8/PX19WBgYB8fH/r6+j8/P1BQUOTk5CAgIC8v
    L0BAQPLy8vv7+09PT+Li4o+Pj9XV1dvb25CQkDk5ORsbG9nZ2fKgDN7e3pSUlLOzs3BwcM/P
    z/n5+SUlJbCwsOXl5bS0tOfn5zg4OMzMzO7u7t/f3+uBBs3Nzevr6/b29rm5uU9FB+zs7POn
    Dnp6eh4eHm9hClVVVerq6r6+vsfHx+6QCeHh4X19fT83BuViAO3IFf///wAAAP///wAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFMALAAAAAAgACAAAAf/gFOCg4SFhoeIiYqLjI2O
    j5BTElKUlZaXlgKIBAsKAFBQT6KjpKOgAAJShxgNUgOgsLGysJ9QqoUSAE67vL2+vbVSmoUQ
    r7PHshVQA7eGKJjQ0ANIzYZSSSIUITo5R7WzlVEE1YVSAVEHPkRSDKGjQ6dBTlJRFOSE5lEl
    BU5QPKSgBjgxAkDKjCgP7g2SYiCDsiUAoTgxpiKAlBRRdiiU5OETi4gMahUwkCCKlA0bFNAw
    lABDrVJQQp7iECNKB5MpEzhrhczYABw2TtwkYXIBgUNSfvGqoKuICwc1mISI4gDBxilSkMWU
    0uRGBARCHDyQEWECh0QwCkAbMUHKgQcaayhEuBAFSAYQRxGl+gYFgIEoFqOgg9tiRIMFi1IZ
    sPTCAmB6gvUVwJBXUaooJiD0OOHgx4HAgD8ogODosoYVSkyQIGDBIoIJDYY5WpABgWARcy+0
    llKAg85IIKJ98EA6kvHjyJMrX868+fFAADs=
  }
  image create photo Mystery.gif -data {
    R0lGODlhIAAgAPc4ABUVHKCgoDAwMAACDAAbkAAenJCQkODg4PDw8GBgYAASYAANSAAJMAAG
    JAAEGLCwsAALPCAgIAAitFZWcgAgqAAQVKys5UBAQAotwwAZhEtg0FZo02Bw1SA+xyAgKmBg
    gHBwcIGBq3aB2RU1xUtNcDU1RzVMwIuLuhUXKGBlmAoKDhUofVBQUJab4CsrOUBXzqGh1ouQ
    0oGDtxAQENDQ0ICAgAAlwQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAADgALAAAAAAgACAAAAj/AHEIHEiwoMGD
    CBMqXMiwoUMcB2oI+DeAwYB/AgwgeFiwxowFGWyItGEihYsZATgKTNCgwMiXNjiEUJGAY40G
    EmDCfAEDQA2HB/5RECmhggOKEAiM1HDi34GGASCMrCCABkSPIUW2KGGgYQ0FIwdYHWjAwUgO
    E34y/CqygwcQBWcMtbEhLVQGIjHI8CCA4AWlNkQASMkQwQzAHSwAeDBQqA0MJPo6DDAAcIzF
    AsuKXDFj7OQZECoAuCCQxmEbCjqrxIHAQA3GrAUokACh6uqDAhYUaMBi4+2ODQgMqPm7YNAC
    DIgXJ1hDd4TlBi9kUKAcusB/DRx0tT7wQY0avrkHLpjBAEAEz8uP053w3HpzkSMUo//9/rF8
    6zQGuNTwob31AP8wgIJq3BVo4IEDBQQAOw==
  }
  image create photo Traditional.gif -data {
    R0lGODlhIAAgAOZqAPDw8MDAwEBAQAgKBj8/P4CAgICvZGybT/7+/kBXMuDg4Pj4+L+/vxAQ
    EO/v7/Pz8/v7+xAVDPX19Vh4RDBBJTAwMP39/Q8PDxggElBtPtDQ0C8vL/T09CAgIO3t7d/f
    3+rq6s/Pz6CgoEhiOGBgYDppHWCDS1BQUF9fX97e3jhMK+fn52KRRTVkGE9PT3alWXGgVERz
    J3ikXdPT01iHO7a2tuvr69nZ2czMzOzs7GuUUnBwcNfX1zo6OsrKym6aUx4eHr29vcnJyW5u
    bjw8PNjY2Ly8vGiOUY+Pjyg2H83NzeHh4c7Ozq+vr4SEhPf390NDQ+Xl5fLy8vHx8Z+fn29v
    b9TU1A0NDXOfWFOCNvb29sjIyOTk5JCQkGFhYR8fH39/f8PDw059MXuqXru7uzFgEyArGXCZ
    VwAAAP///////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAGoALAAAAAAgACAAAAf/gGqCg4SFhoeIiYqLjI2O
    j5CRkoYAAZaXmCIKkRoCaGagoaEDaAKPCiQDCTIGra4GMhSqA5uLAAUNFDo0Zb29MTAJAxQT
    Zh0AiyINZkcsLb5lJQcTEWYTKmhoyIkBHREmByXQJSxnZgMZIwNQH2iJCgLoMDHQLTRY2AnU
    G2QzadqUSKBJ8EMMtDJZxqhLYsLMFScP0vBIQyAAoVu5zhh4caBjxxcmMESYkACNFyVFODxY
    QdHioApoBoiaiWHAiAwDevjgsiRFGgkgWhJCc6ao0aNFI2AAUsNBDhArbKThwOHfNkFEkWq9
    gATCAglSHHh4ksaBBRQVCp3IxrZtNhQEntIgsOBVwgMEaWpsqKChUIELGLaGSMMgbhq5dCE4
    GNKgwKECbrMRwXG48OHLCKhcOFHLEOQ0KYJsmfEBhAS8li+HINDBJaLPEoyEYXIjCgAIhA07
    AIOmwNXXaA5PCSHEygcbWhBYbvJFQGdFXYJfXgDAw4MFFhhscNFAxCMAFVxMr349TRU0O343
    AkBCgPv370/0nUS/vv37+PPr3x8IADs=
  }
  image create photo TrashOut.gif -data {
    R0lGODlhIAAgAPYAAKezxTqYM8TM2TKbJBU1cbnE0Q86V0KiOYiZsxQ5aytGdtjq1N3i6uDl
    663WqCycIyeWG3G1bUdhhgcqZDOiJBxmRBlKW5jIlv77/xxcSyyXI2J4nNLb5ez16pimtjVQ
    ezObLDWbKoSTrBU2bid7RSiDNbLaszKhKTeZLUJdhl2sV3qMqRdBZQ0xZS6dKxlTUCRvRl90
    mPf59z1XgjKdKTKKOerr7TqcLcDcuhM1Y02jRGatXCJEbxY5Zz9UeVVrjBU2aS9Ldc7nyDGb
    LvT18yl6PTOXLxw9bjCaJxg2atzw3C2TMTaPQoLGfi5rVxk6ZnSFnnKFo1Vskfv++Wt8nC2T
    KREsajmbIyM+bjOXJ1eyTom8hBQxb2qFpHWIpTefKDWfLBM1bhUxahM0ahc2YUxmjvH78CBR
    YDFQeixObMTews7Y3zGOMR+XHxkwcnnCgieWIimdHRNCWBJGTn6QraDOnhA7b0yeQDBvRuPq
    7YDBgJDPh5jQmBY3bP///////yH5BAEAAH8ALAAAAAAgACAAAAf/gH+Cg4QMACsbKQo8MzF0
    AAyEkpODAisSTxYZMCSdFS8WaSIClJQMCDNyFUsoXxRfVxSyX0sZaQgNpZUbPRlGNEggICgo
    AydLJTAweE5yMaSlBVIGJRQPLsUgAcMwLFxuYlYTciwpBZQC0zVgNCA0IVlZISAVOQl9Iy1B
    Elz2M9CDGsQwUIPCgHYgQoQ4CONbkj76GvihMiFMDym5BiFIUoTdwSELQZxgY8HNiAQEWgBo
    AMlHiwQ9EFSakeHEEGE4QSA5UYFLggRArGzggGUClQIegrQ4Am2FASMuEuZ0N+RFGJhjJsRI
    MSGBCCJTFPiM8oeBhAouorrLScOFBZ9i/2YAEJCmBZkuHNYcsQLkiCEWS1wMQTEEobAhAywA
    SeAmho2JE0ZMeBIEgIIWdg5ZuDkgiwtgD7L8QvJCTD5zCLq06NGixYQCIibYWRHjxbsHKm7E
    QXLgQJYBWpyIGUOgAILJY/QVhWLjspQUMCig0NDEARsdHZTc2OGACYsJZab8mEDeCwcfZET4
    kdLigwISFAI80LGgTgQcJiIsiBAHhpUyRNgAAAIb5NGABwCsUcAHYvDAA3y87XGBEgtssYUM
    F8CBxA0ZWOHDCh4gIIEEBfiABRZkcAEEIySAUYwOfDjQgR9+4ACHBr8YYYE45LWGRQI5EEdA
    AmjEUMENV8RxR/8EWeBAYwdN3FAFCDcUIcZibvSRwBgj9OHlSWXQYUEIVwyRBQQHdCCDAwv4
    ocYBGoBQhRxiEJDEQ17m2YcdXgBggREgDOECBHr44QAEATgpxAFxoPACFyPcqaeXi0ESRAkn
    DIAECkL4EQEEGgSghKFVoJCBST3gmedJfv1BxwtfhKCBDmZMoUKcVezAxwUBVOXGPap6OUYC
    ZQgiAA8FwaECBrZOCQIcbVQxQA1yjDFpD9iuCBACcwwAwQ5T4HDHQWkhNkARQXY5aR89kBVQ
    bTRo8YYRSDwQ6BBGUFADCwT022UP+HSJRkaVpFCTBg8gEQIxIUj3KAH4QAxwlzwAREhoATO8
    wAYYAaBwAhhf1HAGEF4CLLGXPJxTigA/qFJCFUsUUYEcKgIh6QhhkEGkxZQ0gMATT7DwBBlj
    cGEnnj/ZwUIUBOtibBRHjADEGGE8kcSQdiRwhBQ8Oz2IIStI8YGDaJTRZyS6BAIAOw==
  }
  image create photo Virtual.gif -data {
    R0lGODlhIAAgAOZGAKioqD8/P+/v7xAQEICAgC8vL9/f3w8PD9DQ0PDw8GBgYF9fX9PT01RU
    VL+/vzAwMCoqKh8fH6CgoJCQkHBwcLCwsDQ0NH9/f29vbxUVFeDg4I+PjyAgIFBQUM/Pz0lJ
    Sd7e3ri4uPn5+Z+fnwoKCkBAQL29vU9PT78AAOnp6X5+fsDAwHR0dMjIyOPj4x8AAA8AAM8A
    ANjY2O7u7mlpaf8AAIODg5OTk6+vr7S0tJmZmS8AAE8AAGRkZN8AAIiIiK2trV5eXo8AAM7O
    zgAAAP///////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAEYALAAAAAAgACAAAAf/gEaCg4SFhoeIiYqLjI2O
    gwgEBAoEEwiPiAglGR8NngEZHBKYhAgDF0WpMzImACoRDwmkRg8bqbepKSEAAbGYEhFFAga4
    wroAFgqYHSOpBQ63HgUCMwA3RBqPRMRFG0SoI0QYqbsWE44JRLhEAUUB6qkMAA0ERgn1hiXn
    Rivtt+zu4BWRR89IhwGHKAy4hCDYv3bvbrUA8IEABSIlDlUgMqCCEYFFAEYkByDDgwMnlB3i
    gOGAAiICinArZkAEABpEChgoMOoQgQACFhCBFiHATA9CdSBbMIyIrEMJBuAosmCcAyI5AxyA
    QeSEPJhUVSKqcMCAA4cneKBAEUNIBAFfpos4GJBNEYVpBzwIO4CiRgwY0L56ONBzkYIDBWwV
    GfHCxw5URUAAgECkMKMJA8alCvDCXxGbKhZiquB5Z8xb8j6IdtSvmOuJQYjgu7zAtWsZQH5A
    4NCog2LbuERIJsIIwYHTwHEJKDAbUYIHkIuwYOEC+E6xYwcEMAFiyIIBDyDYMMEARAoXOXqQ
    oMCIAolOFoh00HCPQwYI+EkMUFB3lv//AAZoRCAAOw==
  }
  image create photo 21.gif -data {
    R0lGODlhEAAQAJEDAICAgAAAAODg4P///yH5BAEAAAMALAAAAAAQABAAAAI7nA2pxygPQXNB
    hFplA9ZioTEAdmHhwVVQNnHe9ZwORMuDW6q2S8ftWwK1SLDATgWz7Dor4a9nQykSjQIAOw==
  }
  image create photo stars1.gif -data {
    R0lGODlhPQANANQfAIo1GKGhoc1jOtO3rdvb275qRu9qL6lUO+7h2/6uhP6DOPj3979cOpxG
    Jr29vf10KMqWiurGttLS0uaWfr+Nfd5nM/7Ms5JONrCwsObl5cnJyeC0ouTPyP7fz7h+bv//
    /yH5BAEAAB8ALAAAAAA9AA0AAAX/4CeO3DaeYpahqMqe7puu8pg09YflOr/nv5yigahhFkCk
    USk75jqPg6Wm4VVzVyprwelYLAkFo/DtRIojSfZDQKlP7dN7FE+vMxyG4cFXCCp8BgwUMDQL
    Gg5MHzGHiYUijYqMGhIfCx4MFQZ7DwINAAMSEkwLBBgBBAuipKaoqqMjpaepq7GtdR8WB5pR
    ABdoH47BAQE0IsIOxMbBSsnFJ8jKLApiDwwAiiJVGQHZ2ovdL9vhLOPeCw8VAJkHAy2uDrgp
    8PLgqfEs3Pf1Fg8AFBAYMPBwYkEACQQQrrF0MCGBhQYRKkQR0eHCDxMaUECy4UADJQswaECS
    QYMTSCJJJZoklXLRylgtS56EJMDdCAQHOIgggGuBA2M8C/6k03PozqI0QgAAOw==
  }
  image create photo stars1_5.gif -data {
    R0lGODlhPQANAMQSAIo1GKGhodvb2+9qL6lUO/6DOPj3979cOpxGJr29vf10KNLS0t5nM/7M
    s7CwsObl5cnJyeTPyAAAAP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAABIALAAAAAA9AA0AAAX/oCSOUTKeYvSg6LOyowvH7yw2iN04tsj3P1vQpkDUUAqH
    AbgUNmfK3kNBaMCmkJ4k2+PavCODqkEuHA5k8YPcKCzAAtT7FD/NR/XRXfSIHAYKgWYMgX6A
    gjISBhAJTxKJi40nkIyOlAsiZwwDhwc5EpqcigIOAQIGCwtPBqSmqKphraepq7InDQSbVACf
    EribEgkBAUeSwcPFTcLEJ8bLRyIFZgoHAI7SB48BjiNZD9sw3uAs4tyKCgwABwwEJiIG6ACn
    CXkxrvQs3/P1fPf8vgoADDuT7UZAUwsEgFEUIGHChQYaCniIIqJDhSwKIACHq5dGcA8gRHnn
    AMKSkCMVIpU8KXLVykctTxg44I4PgXc0ZSaoIaCegZ14fAIV0VPnixAAOw==
  }
  image create photo stars2.gif -data {
    R0lGODlhPQANANQfAIo1GKGhoc1jOtS5rdvb27p3YalUO+9qL/bg1f6uhP6DOL5QHPj39ry8
    vJxGJtLS0sqWiurGtuaWfr+Le8jIyN5nM/7NtP10KJJONrCwsObl5eC0os6uouTPyL5dPv//
    /yH5BAEAAB8ALAAAAAA9AA0AAAX/4CeO3TaepImOmrae7cu68pg49XfnWf71PN9H4aC9iMZV
    hsFj1pY5xMVgkUmpNYpPm+OeGB2ExZJQeDxjMUdMNqMtiEjy4f0QUPTT/ZRndTwHF4IKAhWC
    BwuAgheEhheIEDA0DBQNTiIxH5SWkiKUD5oFHhUHgRcCDgADDKKkpqiqDw+XDAQZAQQMsrS2
    uLqzI7W3eyIWBqRTABgII8bIBsrMIpwfDQEBSdTW2Cfa10kiCmYXHgCX4ePl5yJaGgHr7B/u
    8B/t7ysMFxUAowYDX/r4VfCHwl2uBsRY+EK4wiADhigsXAAwAcKZAickUrToAWOwAA8IhKyj
    CaRIAiQZPZgcuUKCgwlMNhhwcMklzA8yaXrKQIGJBgpQdvaUB5QWT59Fvwj4NwKBgQ6elp5w
    CtVOwodGCFxtkHUrjRAAOw==
  }
  image create photo stars2_5.gif -data {
    R0lGODlhPQANAMQYAIo1GKGhoc1jOtS5rdvb27p3YalUO+9qL/bg1f6uhP6DOPj39ry8vJxG
    JtLS0r+Le8jIyN5nM/7NtP10KLCwsObl5eTPyL5dPgAAAP///wAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAABgALAAAAAA9AA0AAAX/ICaO1jCepImS1XpWrSvC8pk0NXbXCZVjvlwwp2jEXMUj
    SkFZCJ21Zg4xMUhkVKuLCvl1c9/TwoKQSBKKy8VcHpTP6bVk/EY7whgC6n7Sn/gjFRYXBxOG
    CgIRhgdqhYeJhoOOEwo0GAsQDFAzMZiaL50QDpcFFxEHjgINAAMLpaepOK6mqJcEFAEECw4O
    mwu3ubu9I7+4fiISBqdVAA0II8nLBs3QygcYDAEBSp/Y2txQ2dsrCmkTFwCbIuUX5+kn7BgV
    AeojXfP1Ivf0KwsTEQBMGVAhwh9AgQQNAtDF4FigYA1XzGPoENkEAA8eqClwQsLFjBuhfSTg
    gACeSwFKQJY8uSAlSZMrbjxwMsBAg00yadockVMeBCkFKUBwUuGnL6FEjYoRQBADAgMWCjI9
    8VRq0wUMjhBwiFUr16wiQgAAOw==
  }
  image create photo stars3.gif -data {
    R0lGODlhPQANANQfAIo1GKGhoc1jOtS5rtvb27p3YalUO+9qL/bg1f6uhP6DOL1PHfn39ry8
    vJxGJtLS0sqWiurGtuaWfr+KesjIyN5nM/7NtP10KJJONrCwsObl5eC0os6uouTQyL5cPv//
    /yH5BAEAAB8ALAAAAAA9AA0AAAX/4CeO3TaepIme5TpqmvvG8pg49Xfne53lnx9Q4aC5iMYV
    0sfIZZo1xMVgkUmp1mlVRgF2UYwOwmJJKDwe8pgzLp/TlnXbjFZHko+viIDKnzQdHgcXhAoC
    FYQHC4KEF4aIF4qMhYeJCxB/NAwUDVAiMCKbDx8MBR4VB4MXAg4AA6WnqYSsrrCoqrQDDw+e
    DAQZAQQMu72/wScWBqhTABgII8nLBs3PItGD084jnSINAQFJ3B/e4EpnqwCeIgrnAuko7B7o
    6iJdGgH09R/3+QwXFQBOGRhwwh9AgQRHGAxYYSCKe8Ia8HkYjIHEFRYuAJgAAU0BZBo5egS5
    saOHjwoDTDwgsFJPKJUsCbgUIcHBhCYbDDjwVPPmh5w7R/TEqRMKgwwUmmig8EQhUqVM1TEQ
    kFAEAgMdQlE9cTUrqa0juu6ZGKqBEQJkSZkVEQIAOw==
  }
  image create photo stars3_5.gif -data {
    R0lGODlhPQANAMQZAIo1GKGhoc1jOtS5rtvb27p3YalUO+9qL/bg1f6uhP6DOPn39ry8vJxG
    JtLS0r+KesjIyN5nM/7NtP10KLCwsObl5eC0ouTQyL5cPgAAAP///wAAAAAAAAAAAAAAAAAA
    ACH5BAEAABkALAAAAAA9AA0AAAX/YCaOlzWepIme5UpW7ljB8Zg0dXbne5xQuQwwqGjQXMXj
    KhlTUBa5Zw4xMUhiVCu2el1RIUEwanFBSCQJBQZzNg/MaDVb4oan1+33Oe0QiwgofScVFxgH
    E4gKAhGIBwaGiBOKjBOOkImLjZeSMyILEAxQMjSfDhkLBRgRB4cTAg0AA6iqrIivsbOrrbe5
    rKcEFAEECw4Oor/BgCMSBqtVAA0Iy82HBtDSIszO1tHZ1BkMAQFKoSLh40tqrgDHIgrqAuwo
    7xjr7fQZFQHtI2D6/KcmRACgysCAEwsEEoxgEKHCggc9KRzGQNkgYQsqrpAwAcCDB2sKnODo
    ESQGkcs6TX4MmdIjAQcE/HgKABOmTBE3HkCxYKDBsZw7e/5soDMDT584iUKpAEGKJwoQljZt
    t0BARBEIDFzwZPVE1q2nuo74GvbqKQZHCFg8SyMEADs=
  }
  image create photo stars4.gif -data {
    R0lGODlhPQANANQfAIo1GKGhoc1jOtS5rtvb27p3YalUO+9qL/bg1f6uhP6DOLxPHfn39L29
    vZxGJtLS0sqWiurGtuaWfr6KesjIyN5nM/7NtP10KJJONrCwsObk5OC0os6uouTQyL5cPv//
    /yH5BAEAAB8ALAAAAAA9AA0AAAX/4CeO3TaepIme5cqqrqa5aOLQoo1/Ot7TmZ1I4ZjRiEYX
    ErcEMnaIi8FCi06rUqrLqnVRVowOwmJJKDwe8pgzLp/TlnXbjFZb2GQ6HBFJfh5fIxodHgcX
    hwoCFYcHC4WHF4mLF42PiIqMjoaXk40QJzIiDBQPHwwFHhUHmwIOAAOnqauHra+xqqyusKi4
    tLoPD0+iBBkBBCcWBqpSABgII8nLBs3PItGG087QytjUIw3CDQEBfkNnFwIAwiMK5+nr5h7o
    6ijt8u8rXxoB8KIXFQBSGRhwgsG/gBUGFjwokOAIgwAbothHgEGDYygsXAAwAQKaAsg2dvwY
    kqNHDyChS4k8mVJUgAcEYAY6IcHBhCcbDDhYV/Pmh5w7R/TEqZOnTaJBTWWg8EQDhQzwGAhw
    KAKBgQ6ipp6witWU1hFcs1L9EPYDAYyiGhgJAQA7
  }
  image create photo stars4_5.gif -data {
    R0lGODlhPQANAMQWAIo1GM1jOtS5rtvb27p3YalUO+9qL/bg1f6uhP6DOPn39JxGJuaWfr6K
    esjIyN5nM/7NtP10KObk5OC0ouTQyL5cPgAAAP///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAABYALAAAAAA9AA0AAAX/oCWO1DSepIme5cqqLiW5KLLQoo1bOt67CMFOlFjMaMWj
    K4ljLgWK3SFSgNCm1SvV6sJyUVPHSkE5QCCIRKVyNgvMaDUb4oan123I+3yfH/ZxA2IjEhQV
    BhGJCQEPiQYFh4kRi40Rj5GKjI6QiJmVBpiTEkcKDgMWCgQVDwadAQsAUKqsrrCyq62Jr7Gp
    uLUAvbSogg2nIxAFrFQACwfHyYgFzM4iyMrSzc/X09XQFg4NDUojCWoRAcAo5RXn6Sfr7VHv
    5ujyRGoWA1BjEQ8AqwWEjFDQ79+DgCcI+gMoUIRCgwgdFhxQytgJCBEAhFtD4GLGjRU6HvvY
    gKNHjSVDQY7UKEjQCgYLGkSZUGCBPZgyLdC0OQLnzJo3Y/7kacGnBQkO9g0M0NDCgQIUHDI9
    8TQqqqkjqkptqlUBVocOjoQAADs=
  }
  image create photo stars5.gif -data {
    R0lGODlhPQANANQfAIo1GNO3q81jOr5qRu7h26lUO+9qL/6uhP5/KrxPHZlDJvTu6vpoLuTQ
    yMqWir5cPuaWfv7CormDc9ZqMuKqjpJONv7fz6BKJt6+tv12J/779/7RusKCcuBmM/6ITP//
    /yH5BAEAAB8ALAAAAAA9AA0AAAX/4CeODTWepIme5cqq7tfG4nHRn43rNB/7MY+CQBMSg8Ni
    Enl0WTKFSOwZnUKlzqu1OtI0LJvNAfEYhMEBsJhs3qDV4/J5kw7H2287+2xpPAwZgQgCHYEG
    CX+BGYOFGYeJgoSGiICRjY+Vi5KOCR8LEg8dDAYMGRMKAAGfoaOlp6mroqSmqKqgsq61sa20
    qSMRBR2ABQAVTcDCUMXHwcPLv83KxtDJxNMnHmQZAgALKNkP293f2tze2OXj6OHmKBqmAKEF
    ASfvE/Ed8/Xw8vRd/Pn8ibCHT9+JCBkAcHDw4IGEgwkXNnz4KyJDhxAVXqQoAqHGiSggXJCg
    4QOGAgpKMooQSdIkSpUfWJY8mXKETJc1V46c+bLLBIEfCBRoMPDnCaFEP2gwOgJpUaBOlTIV
    4TQEADs=
  }
  image create photo N.gif -data {
    R0lGODlhEAAQAPMAALUJCcM/P////759fbceHrtZWcPIyLpNTb1ra8Cfn7+NjcTQ0LUEBAAf
    k8Xf3////yH5BAEAAA8ALAAAAAAQABAAAARM8MlJq5Uh3Jt3bQzTeE/THCFiXo2zACHhOOPn
    KGGY0LYT5IzDrCZpGWA5wII3aQ2AoQGz6CgwCAJBbEqdeYeW1tdLpJi+K48pTSJFAAA7
  }
  image create photo NE.gif -data {
    R0lGODlhEAAQALMJAM9dXcBGRjoXZv77+7wuLrYLC7UEBAAfk8Xf3////wAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAkALAAAAAAQABAAAARFMMlJq70458OPHABQcUiJCEJhENRhEkYsj0ig
    yrE4uXVxx6eWKTAY5BCenamkKpSSEt6wAHjSlgHrdakdSZHQSyeswUQAADs=
  }
  image create photo E.gif -data {
    R0lGODlhEAAQAPMAAAsdi7YLC7cZGcI/P76Cgv///7tSUsTLy7UHB8Cfn2wPPrgoKAAfk7UE
    BMXf3////yH5BAEAAA8ALAAAAAAQABAAAARD8MlJq704Z8aZ5k7ocBcjGsQRepUZNg0yEEAr
    OgIMBwrlhgWdcCD5OYJCGPFhTAp6PhEMYUiwoqFFalUykjYdjfgRAQA7
  }
  image create photo SE.gif -data {
    R0lGODlhEAAQALMJAM9dXcBGRjoXZv77+7wuLrYLC7UEBAAfk8Xf3////wAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAkALAAAAAAQABAAAARFMMlJq70458OP5kiIcNchnqNlnkHoUasYFIAL
    o4VR2FMcDAODofaSrGY5oQEhqJhmSiXAKSJEhaoYU5AjlDqSAWCqsUQAADs=
  }
  image create photo S.gif -data {
    R0lGODlhEAAQAPMAALUJCf///7ceHr59fcY/P7tZWbk/P7pNTb1ra8Cfn7+NjcTNzbUEBAAf
    k8Xf3////yH5BAEAAA8ALAAAAAAQABAAAARG8MlJq70458ab5k7ocFcjnqNloqFHmQLDBEFc
    pJM5yDwz4BLTAtADLICP1aFnaFVMiZ4CGQzFGEZqsIGQHUgbmUvzIBAuEQA7
  }
  image create photo SW.gif -data {
    R0lGODlhEAAQALMJAM9dXcBGRjoXZv77+7wuLrYLC7UEBAAfk8Xf3////wAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAkALAAAAAAQABAAAARFMMlJq70468PPPkiIcBconl5lBueohkDBopRZ
    GEWbSiBgGIPBzDURIH6/giy0S/iQSVYzAf0RRE0CTmDETgGAAa+jKUcAADs=
  }
  image create photo W.gif -data {
    R0lGODlhEAAQAPMAAAsdi7YLC7cZGcI/P76Cgv///7tSUsTLy7UHB8Cfn2wPPrgoKAAfk7UE
    BMXf3////yH5BAEAAA8ALAAAAAAQABAAAARD8MlJq704Z8aZ5k7ocBcTHoQhehVADEjTrJUS
    yLIgjtKA/4Udy/eTBWkThaAorDAShtgsxKKYHKgF0nmlVksdjfgRAQA7
  }
  image create photo NW.gif -data {
    R0lGODlhEAAQALMJAM9dXcBGRjoXZv77+7wuLrYLC7UEBAAfk8Xf3////wAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAkALAAAAAAQABAAAARFMMlJq704VwCGPOBxEUYhCEiKgJbhvoQqUsD7
    FsFaoXaBp7MPomYYDHJAyiFVKKmSk+Xw9wwKEUiZRfqEVkBV6zakKUcAADs=
  }
  image create photo icon_smile.gif -data {
    R0lGODlhEAAQALMLAAAAAP+tGf/sf38AAP/////eGeUxAMfHx0dHR//Csv//AP///wAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAQABAAAARecEkJap2YgsD5zRtXjB2AhUKqCt60CQhwEPHB
    mssrAApB8IRUwARIFQ4ABAGJKKSIgpFv6hvtclEpdeSECgbgcPhJ+RrOaMPgSuEMDIm4uuVy
    i+mnUAmX0Vj6gIELEQA7
  }
  image create photo icon_smile_8ball.gif -data {
    R0lGODlhDwAPAMQAAP////T09O3t7efn59PT07q6urOzs6ioqJmZmY2NjYSEhHt7e3V1dWZm
    ZllZWVNTU0pKSkJCQjo6OjMzMykpKSIiIhgYGBAQEAgICAAAAP///wAAAAAAAAAAAAAAAAAA
    ACH5BAUUABoALAAAAAAPAA8AAAWKoCZqBOJE0FKMLMIwzgNJFMRqSLI0zjxRlsoop1ARCIZH
    5YKZkBgLhgAQGAAAE0wmg0A0GonrQnJVbDMPh7oRABgU18j5EpFFDtcr4byNRCQNV15lcwsT
    E2EAWwUABWcPBRQVDFcJgQAIZwgaEBYXeHkFF1tOIhVaFX4VczcTfGelNwgPoxcPmyMhADs=
  }
  image create photo icon_smile_angry.gif -data {
    R0lGODlhDwAPALMAAP////yBhvrVqPplZvpVSvpGOPo8KfovG/ojFPoYDQAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARXcEmpap30lEL6LsaBJMqicNUwENVYUgkizwmJ
    nZs32iZHDIqAghVymWIHS+Uget2UmOfnQHL2fB6REfcTAkGiqg+oIoYryA/IULQpYgqmXGGc
    oGu1iyQCADs=
  }
  image create photo icon_smile_approve.gif -data {
    R0lGODlhDwAPALMAAP///wD5ewDnUgDgMwDaHQDUEgDPCQDIBADAAgC2AAAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARZcEmpap30lEL6LsaBJMqicNZgjSWVIHCckNi5
    eSNtFgoxCIIBAXRQ0IyIg2EZOohaNQsUY7IVDqSprdNTiFg7zi84DLFOBKn6/NJ8mKJj20mH
    6SjG2ewiiQAAOw==
  }
  image create photo icon_smile_big.gif -data {
    R0lGODlhDwAPALMAAP//////o///hv//af/89P/yU//mP//SKP++G/+nCAAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARTcEmpap30KAA290qiLEqhCMJZauIUIlaVtG5h
    D0OBzCNpxsDQSNjhFBUwDCWmdJVsB1GPYqvgfrvhTRAICHKGQ7YEMoKkoo/503pVzEjacka/
    SCIAOw==
  }
  image create photo icon_smile_blackeye.gif -data {
    R0lGODlhDwAPALMAAP/////jyP/atf/ToP/MkP/Hgf++a/+1Xf+qQufn53Z2dQAAAMDAwAAA
    AAAAAAAAACH5BAEAAAwALAAAAAAPAA8AQARXkEm5ap3UEDL6JoVxIAuzcEIqKOxCTu4hz8gL
    f95on10VrAvFYlSKGQrIkEFUwpgsTefzYyBFTZtOr0LEcnxAIZE3UK1aY8RB80mKXkYDVGaj
    uGq1iyQCADs=
  }
  image create photo icon_smile_blush.gif -data {
    R0lGODlhDwAPAMQAAP/////59P/s7v/l5v/g4f/c2v/Y0v/Syf/Lwf/CuP+Kiv9eXv1xcfsA
    APg1NQAAAMDAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAABAALAAAAAAPAA8AQAVnICSKT1mO5NE0yzAU8LImD/QUQ+nmD0KPjwRiSEz8gDDc
    y3e8KQUCV8Fw8NWCiMPDwD14ETWUzRQWj5MHWtkG28GqVjauFOVV1QWGw+Eu7BlqCQ4MCgYw
    XAp6P1heJnBHJEFGRiciIQA7
  }
  image create photo icon_smile_clown.gif -data {
    R0lGODlhDwAPALMAAP////v7+/T09O7u7ufn5+Hh4dzc3NOToMcNMcRGX8QCKMJzgnZ1dQAA
    AMDAwAAAACH5BAEAAA4ALAAAAAAPAA8AQARf0EnZap2UVHCSSsdQGY3TCMAGpI1BTq2hVYRb
    woLQrLpNoakOAtQg1EqtBmPAHBAYxRvGcsHATrmj1JRL8YpHLvWwOFBJ2NUCoVAsmLYWNEcO
    iRiv1ozBp72uLoFVDhEAOw==
  }
  image create photo icon_smile_cool.gif -data {
    R0lGODlhDwAPALMAAP//////AP/4AP/xAP/lAP/YAP/KAEBAUiAgIA8PAAYGBwAAAMDAwAAA
    AAAAAAAAACH5BAEAAAwALAAAAAAPAA8AQARTkEmpap2UjMDDHgRRGAqjbMihBMqBjCVlwJVh
    x9SwdTaZsxZgzae4AY5HBcGHMVlwzYoORIKeOh3RzVlxWVLPFSchBmqHhHDlbJqlkUUYZti7
    SCIAOw==
  }
  image create photo icon_smile_dead.gif -data {
    R0lGODlhDwAPAMQAAP////8AAPoVAPkOAOkAAOAAANoqANH5e9EAAMLnUsDAwLzgM7baHbLU
    Eq3PCajIBKHAApi2AHwPAAAAAMDAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAAAoALAAAAAAPAA8AQAVkoCKKU1mO5NM0TLs2zgNFkzI107Lk9zPRox9kGEAMIsDg
    yzVL3lqlxIJRkgGFJYQEUYXUUDbTlwIuvR60L2nVcslmtRsrOu2m5zoebAIXql4OgVZxNA8P
    BgQCMj9qYUgIBVxqIQA7
  }
  image create photo icon_smile_disapprove.gif -data {
    R0lGODlhDwAPALMAAP////yBhvrVqPplZvpVSvpGOPo8KfovG/ojFPoYDQAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARbcEmpap30lEL6LsaBJMqicEM6EIVITkqCVEid
    vPDnjfjZDYGAgqAIjUoxxMHADB1EJYzJEpVOPwdS1bTpeERHLiq4ArkqHMLAQqRpZZqNxfhK
    PqkVHCV2u10kEQA7
  }
  image create photo icon_smile_evil.gif -data {
    R0lGODlhDwAPALMAAP///+fn5+K4tN6kndyWhNqJcteAYtRwStFiPcxSJYYSAAAAAMDAwAAA
    AAAAAAAAACH5BAEAAAwALAAAAAAPAA8AQARfkC1Gl7VUylMK+V1hHEgyLd6wDAOxkGa2JEht
    J3GFdmBpnh6CQiFoDWEn2sFwOTgRk4zuIq1IdoXDTyMpWD6ErCW2EypWi2PJEmSxwqP1DHFg
    ixZxstLJr+V0OIEYFBEAOw==
  }
  image create photo icon_smile_kisses.gif -data {
    R0lGODlhDwAPALMAAP/////39//l8//b7f/U6f/P5P/K3v/C1/+50f+uyv+AwP8AgIAAgAAA
    AMDAwAAAACH5BAEAAA4ALAAAAAAPAA8AQARb0EnZap30lEL6LsaBJI3TcM2gEkUzllSCWFVC
    YufmjbfJERXBANgQ3RqkxkLRUCwqMwyFJp1UPockTtdhiV6+VnBoeZ06qhXIWENorpUQGDlj
    LO4MV29q618kEQA7
  }
  image create photo icon_smile_question.gif -data {
    R0lGODlhDwAPALMAAP///wDa3wDO0gDGxQC+ugC3rgCtngCilQCVhQAAAMDAwAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAoALAAAAAAPAA8AQARVUEmZap3UEDK6LcaBJEpCWGg1TglyHJa4shs3
    EDJZclUQCLeKiNQ6GApIkCGkw6QwTtPGMGruTqnE8Nr7BbVVm2AsQA2LGmwCGVqhDZbQltVC
    2KuTCAA7
  }
  image create photo icon_smile_sad.gif -data {
    R0lGODlhDwAPALMAAP///wDv+wDO+QC89gCw9ACo8QCg7gCU6gCG5wB34wAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARYcEmpap30lEL6LsaBJMqicNUwENVYUgkizwmJ
    nZs32iY6CIIVSOEyxSqGZEX0ulmaGBOucCBBcR2PqIizBFktK4egUhFAImtM81SEuMeDXE7k
    URS1/EUSAQA7
  }
  image create photo icon_smile_shock.gif -data {
    R0lGODlhDwAPALMAAP/////v+//O+f+89v+w9P+o8f+g7v+U6v+G5/934wAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARYcEmpap30lEKILcaBJMqicNUweMpYUgmCWCOJ
    nVtX1O/ZDQLBCiSyKWIHS+Uget2UmOemcCA5TbkOgSoz5ipCVrHCIahU25DriNAoFWoj8t29
    VhJ4/EUSAQA7
  }
  image create photo icon_smile_shy.gif -data {
    R0lGODlhDwAPAMQAAP/////+/v/89P/y5v/v4f/t2v/r0v/nyf/kwv/eufrt2Pbu1vbftvHw
    2/HlvQAAAMDAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAABAALAAAAAAPAA8AQAVjICSKT1mO5FEUZVMs5YEkD/SwAPAMT/7Qox8CYZoBg6sC
    gVAw1my4kk7X+9WEB4PWcOgSUSTTE2y7rQ60MZT1WLIdwFsvN6U/GCWlmFdMJxAqSVsycX8H
    D12HQ0dhCY6OJyIhADs=
  }
  image create photo icon_smile_sleepy.gif -data {
    R0lGODlhDwAPALMAAP///+fn58DAwLCwsKenp6CgoJqampOTk42NjYiIiAAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAIALAAAAAAPAA8AQARTUEipap30EEP63geCKILiDSgokpTovizFcZvL
    moQyLLppKKuS6GAoGg7IEYZiiS0rn9BloqB5CKFgtVNZDHKKbIWGShXFQIRmZhQL1c20kipf
    xSIAOw==
  }
  image create photo icon_smile_tongue.gif -data {
    R0lGODlhDwAPAMQAAP////8AAPoVAOkAAOfn5+AAANoqANEAAHwPAHALAADnUgDgMwDaHQDU
    EgDPCQDIBADAAgC2AAAAAMDAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAEAABMALAAAAAAPAA8AQAVm4CSKUlmO5NM0EsOsjSNBkTRJDbOUC1PSNlIkcAhAjpEa
    CrdyNWjKW06nUPRiD+AtAnlIDohEKRtcmsqoG7PxqKGZLmdWC99JemMgzrXoXx1ZblxeZyZ0
    gwIDBj9aIxIBBQFJbiMhADs=
  }
  image create photo icon_smile_wink.gif -data {
    R0lGODlhDwAPALMAAP////332vXKYvK6QfKkGvKcEfKPCfKBBvJyA/CvKAAAAMDAwAAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAPAA8AQARZcEmpap3UEJL6JoVyIMqicEM6JNVYUsghzwiJ
    nZs32iaXDALBCmRwmWIGRWGZLL5ulifGhCMYSFJcx1M04qKrShHrUw1DLgXyA1qKbOqDIWmR
    8SjqWu0iiQAAOw==
  }
  image create photo flag.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAHAAAQEA
    AAAAAAAAAAAAAAAAAAcIAQEBAQAAAAAAAAAAAAAAAAAGAQMQAAEEAgIBAwUAAAAAAAAAAAIB
    AwQFBgcAERIhMRNRgSIkFREAAgEDAgYCAwAAAAAAAAAAAQIEMQMFABEhQVFxEwZhEvCRQ//a
    AAwDAQACEQMRAD8AdbvdewNcWGdRUi1N2eJ3bcwa61rmH0fp7BlqSw2J+KOD4gSihIXv9eFP
    dcvKxF6FdsbeG6pVgR/RTX7VG40r9HxMXMXZEaSWDqd1YGganChAOq219t6q2jWXTUbUH8uG
    tJKnQs0iMxTr0/XJxv8AMkbcE19OvBC6X365t69nb2Ut/a5Ge18nip7EhSe+2sc3gbeNJUSE
    dwdinEOO9V/bA6NMq1xjOQ7xwSTcsjOw7OcNjwcvlMOD8EabUNibQTHULpn5BNQRC6UlRRTi
    Ofj4mShrakqG8dwOoO/Qg05dR86OwpsnHzPNHYqWQqSOX5y1Zkl/BqLCbLHsbn00GBEp5ESs
    rIb7AiIowQg22AF9kRE5VAUACg1Xd7jl3JLE7kmpPU6//9k=
  }
  image create photo icon_nocoords.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAG8AAQEB
    AAAAAAAAAAAAAAAAAAcFCAEAAgMAAAAAAAAAAAAAAAAAAwYEBQcQAAICAgICAgMAAAAAAAAA
    AAMEAgUBBhEHAAgSEyEkFREAAgEDAwQCAwAAAAAAAAAAAQIRIQMEMRIFAEEyBhMjYSJi/9oA
    DAMBAAIRAxEAPwDYHtg1FHPrfqSOwL9e1+91F87d7SGRk5DaqUK9hcpyItV5z4lEphYFliMJ
    ELGcozkOEfK/kixUD5DbEmWETABMCaCTHRrO+Tstm49AqiZJJA0FTr0DetlJu3aN7vYFewbf
    HXnWeV9hsGTMtHZtmYSeglgBbAzTQU24p5yZfLBIYzHOMfmWc+LnE3MvLv7ReZrVt5kwGIgQ
    rbYBmp00NZMHrUOUwOP9b47bftI+fkW4KeSY4aQWEz9o0/llMU1r+ykex+8VEI1Wr/wtQ6hW
    LQqsuwMZ6zO0IOCODrw/snUg0kvjlcZJ/DP2xiSOJR8JylnLz7jKLZ+O21B4l6wSrGniWjQd
    iwmQLjMvj/WsNMi1cS7nXlkEQ64wIkSBP2TH5QzSkFt9TqtfXqHuVwyC2p1Dur0WuUVacbld
    N2wrgWrtmyqnbRE79JGbb4jySPMpYlxzx5I9U46/iWrrX1YM9wmGILbQAiyV/XRe1AKdIvMZ
    gyroYGaVNasSWY1rqe/X/9k=
  }
  image create photo icon_notviewable.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAHMAAQEB
    AAAAAAAAAAAAAAAAAAcCCAEAAgMAAAAAAAAAAAAAAAAAAgUDBAcQAAAHAAICAwEAAAAAAAAA
    AAECAwQFBgcREhMIAEEjJBEAAQQBAwIEBwAAAAAAAAAAARECAwQxACFBEgXwYYEG0SIyEyNj
    FP/aAAwDAQACEQMRAD8A0PqPuI3rzDccyoGSDaL1R4eJj6jp8QyjHcLHyUrCJKKKTbt+cG6T
    hk98n5n7+QQ6GT/NQRhtW46zS+RyDxgDc+mrFWpNakEcTS5x4Gi6G9l5vYL5hdSsFeb45XZF
    qspISkX/ADNpixQsSLtZgSSWE3mSWd+NIjYFVFDlUKJzHMIcoq1ifuVjrjeWwscMZeQhQ+Sc
    Hg4XGjdy7bS9rUPs2I2zX54yocjm12OUKP3A4I+kg+XVT3E9dv5tLo2e2ZjWcoVuC9oGu6HI
    Fhkns3ICJ5RZk5ZRjh0o3FcTHRBUpigUS9TD9jbrz9znMbmlkcZKEg/OcA8bZxwc76Dt9yn7
    YpNsQvZPcnaEQg/zNIXIX8uNinQRuNKee+rV+j6jCVTUNhpMbnWdzhbtAZvRXBpJ1L2Bhwux
    XkZmURQW6kVTKIot25AU5EBNzwPx3Spis0hVLip2TgDYDAAaPjrP7lx9p4c7gJkkncuJJOSS
    4knX/9k=
  }
  image create photo icon_viewable.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAGwAAQEB
    AAAAAAAAAAAAAAAAAAQFCAEBAQEAAAAAAAAAAAAAAAAAAAMCEAABBAICAwADAQAAAAAAAAAD
    AgQFBgEHERIAEwgjFBYXEQEAAAUEAgMBAAAAAAAAAAABESECAwQAUWESMdFBkRMU/9oADAMB
    AAIRAxEAPwDQ20vsZtXI7eetaFqb+lu9EhYplVdpRTOLdQcbIycIMxSzjyQWlsE7F57ORr7+
    zr1UP8ZM5lkZNFintWwIw+VV8AE14NVs2a71XWgi+doB5VZByy1R1/8AaNaun+P6/ndKJoNi
    v8TKNp28yA2WK8V/HwazjHXpIbp1+2Ry8UNI0rLkqU5x27rWlXjHyaL52oYzg+RHZGY8Jpes
    12autZzuJuJJOTRdhfL1+kqVaKRrXbNDm9X7Ukz2u1azvp1xrqJmpXHukCx8xFBObKSEIvgT
    gCvXjjCVZzjnyeZif0BBRpq7Cbkfsgsdbxr7ZapCVCJx7kQ0bWnyjd4Ok1yk7G25R4bWOspg
    dxrGsaG4VIuJWdjcJMwNITMoEB+oyCTyEAEeznOMq54z4xMVsdorU1Vdl5gEuIB70yL/AOrT
    KBSQDiK/cV9a/9k=
  }
  image create photo pkg.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAHAAAAMB
    AAAAAAAAAAAAAAAAAAMFBwkBAAIDAAAAAAAAAAAAAAAAAAEEAgMFEAACAwACAgEFAAAAAAAA
    AAACAwEEBRIGEQcAMYEiEzQRAAIBAwMEAgMAAAAAAAAAAAECESESAwAxBEFRYQWxBpEiE//a
    AAwDAQACEQMRAD8A1p0+8+v+q5PWqZdby+w7NjMrt0oGKagrz+kP6rDo8CxheeIT+ReCL6RM
    /Mrk+0CZ04+Ff6Zmn9QQLQu5dmICjYC6JYgCpGmhgC4zlytYgipBMk9FAqe9NhXT2hvev+29
    S3rWdkY+ZtVqV0Ty+NMrKmqQRiayV55R4mCgh+/iYmIt4nsFzlsbCzKjFWRiLlZfkEVVhRgZ
    GovhICuhuRgCrCYIPwehBqDqSZPqxLrtu4z2tlUuv7lFTIx7ApfZqssqOblWVtcNfgTWyfMl
    S4ZGAg4DkJZ/L+scDkszugvZri1Lp8GKeO2mMXsc+OADQCI6aOXrCtV09XX1/aef2PKzqb7G
    FmpYKLTbI1oFKWgt5I4LYMmErWLCk5AyJcCHwcX6xweOyuFl1a68mXnsW3I8aL+yzvImFIiB
    t+Nf/9k=
  }
  image create photo puzzle.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAHMAAAMB
    AAAAAAAAAAAAAAAAAAQFBgkBAAIDAQAAAAAAAAAAAAAAAAIDAQQFBhAAAQQCAQMEAwAAAAAA
    AAAAAgEDBAURBiEAQRIxQhMUJBUWEQABAgUDBQEAAAAAAAAAAAABAgMAESEEBTFBUWGh4RIG
    E//aAAwDAQACEQMRAD8A0s3+dYKGmVGjhXQbCrroU/a1crYrpv8A2GANhCN5o1ITwfljv3yn
    XDfXZa6t1Nt2qilYPsqgIUnYTPXiK1w6UmQMXOp2f9Fr28s7Dp1TW2tXCkSK+fBr0Zj+BsGQ
    ADhouXWlTK4L0VF460vm8vc36Fm4QEkGhGhHFTOY7wxh0r1hO+5a1p0dlGm0ey1cuLFYkVIy
    o8a0j/hgIgbsh4GxbadDn3YJeOOWZTEuPufqmRnIS8wLzKiqYrB0a1/YMWYSp0fXpFbW2KT4
    Ed6LIjTHHWjbYZYkmSOOZEvJfjbTlMZThFDG4txl/wBlUCeNCT306CIaaUFV2j//2Q==
  }
  image create photo stage.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAHIAAQEA
    AAAAAAAAAAAAAAAAAAcIAQEBAQAAAAAAAAAAAAAAAAAFBAYQAAEEAQIFBAMAAAAAAAAAAAQB
    AgMFBiEHADESIhMRQTIIURQXEQACAAUDBAIDAAAAAAAAAAABEQBBAgMEIVEFMXESMmEUkVJD
    /9oADAMBAAIRAxEAPwCkCMu3gxz7c4XjZYI5G199DRsSqlroZhZq84SNspSPWJX+Vk6u6ndX
    NNdF4zGTyWRZ5Gm0fSpLSRn+Y12Px+Jd4uq7/UPVzB0C6JRQGSfZXaVm8979fgMGVL0cM0V2
    UsDHYPHZRhPK8LERvk7Wp8+XVy/PCg5W0cn6+vlvJpqCjwt8Yf22PHaaae3WG6+xvCc+xLD6
    +1zefGi6gUR0pFNcJXFSMSFrZQyXxSNe6Fy/ONV9FVE9eXFWRi03wGSFMFHs9jMRBj5Ndgk0
    gF/sPIdw5iRgI/muHl757jbl2GPCgmB1ZUmN5YmQBlxWhRAP6nZXMhbJFIjFXVZXa+3dobb4
    2k51WQaagZFjxOi9UxpuYTr5KsYNOOKwQeoRY1fs0dfiP//Z
  }
  image create photo trailhead.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAPAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQABgQEBAUEBgUFBgkGBQYJCwgGBggLDAoKCwoKDBAMDAwMDAwQDA4PEA8ODBMTFBQT
    ExwbGxscHx8fHx8fHx8fHwEHBwcNDA0YEBAYGhURFRofHx8fHx8fHx8fHx8fHx8fHx8fHx8f
    Hx8fHx8fHx8fHx8fHx8fHx8fHx8fHx8fHx8f/8AAEQgAEAAQAwERAAIRAQMRAf/EAGwAAQEB
    AAAAAAAAAAAAAAAAAAQFBwEBAQEAAAAAAAAAAAAAAAAAAgQFEAACAQMDBAMAAAAAAAAAAAAC
    AwERBAUAIRIiQiMUEyQGEQACAQMEAQUAAAAAAAAAAAABAgQAESExURIDsWFxweEi/9oADAMB
    AAIRAxEAPwDXchnZLFKXY4lyz6FzkfWWa/GyFtLnMHHaUVKNZcqcQpChr72xg5q/pigkFitt
    r50xRsXlbhqVA92MuZcBAS0BPsD9djOc1gRiYJdC4xTfRiTHdlDFTfa99Cfil3xlUEgMPGtJ
    zWGxicSu+tLkpuaINlqppFFSMPl8IzO+5SW2nNiLwLKDy9L75xQjSDyCk/n63qzhQxaPyoQb
    VBeTZREhJDDOcppxpWta7U1ZGW3WoOvEeKm7jdz7mv/Z
  }
  image create photo waypoint.jpg -data {
    /9j/4AAQSkZJRgABAgAAZABkAAD/7AARRHVja3kAAQAEAAAAUAAA/+4ADkFkb2JlAGTAAAAA
    Af/bAIQAAgICAgICAgICAgMCAgIDBAMCAgMEBQQEBAQEBQYFBQUFBQUGBgcHCAcHBgkJCgoJ
    CQwMDAwMDAwMDAwMDAwMDAEDAwMFBAUJBgYJDQsJCw0PDg4ODg8PDAwMDAwPDwwMDAwMDA8M
    DAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwM/8AAEQgAEAAQAwERAAIRAQMRAf/EAGQAAAMB
    AAAAAAAAAAAAAAAAAAMEBQkBAQEBAQAAAAAAAAAAAAAAAAABBAUQAAIDAQEAAwAAAAAAAAAA
    AAECAwQGBREUFQcRAAICAgMBAQEAAAAAAAAAAAECIQMRMQBxBEFRgf/aAAwDAQACEQMRAD8A
    017XdxfGxcu63tuDJ4LOx1ebF8ClE3Q6l8RRrNI8qxPKQZWKIiEEkMzN54BtooZ2CIMuZnQH
    OX6/WlSNbaxWtTiNsf5O4AHZ4TidrHdXHV9rirVbV/n+rrXaVaxdpRLf5l9YpFjZZGiSUeSo
    EZJCSCVZW89BX0MjFHGHExojk8nrS1FtqYtWxxOwdfZ3BB7HJ+hz2K0mLbA79fusX2hV6lO5
    y7MTXObcaKNpoZoVkEo8lBdWQEgllZfACVF5Rg6HDiJ0Ry+vyJajVWqWrY5janfe5BHXHeFx
    cXxsXUwWFijzuCycF2+ZelaiF/qdBopGiSOFpGlIMrB3dwCSFVV89IX3s7F3bLGI0OTyeRKk
    WqpStanM7Y7+zuSTj8HP/9k=
  }
  image create photo traffic_cone.gif -data {
    R0lGODlhEAAQALMJAP7u5/6yeP5zTv5PLuNQL8EkGP6ZTP///wAAAP///wAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAkALAAAAAAQABAAAARKMMkpEb0XHYsxKlxHfaGYaMVmVsYBrqcxvKYm
    p2XWHm4+IQjZYAj0AA1IAgFYQgAARyVQ+gsAkExlgVYJWJGCLfcXBAuKIiZ6EgEAOw==
  }
  image create photo big_smile.gif -data {
    R0lGODlhEwATAPfNAEuLSf///zMzZ9opEtLS3QEBAerIn7SneP7+/ubo5+m/l/7dA/3+/Yqe
    j9jBlau2rdK+kSJ2Lxx4LjSSNkeOVvb698DYxuXGnOHEmlGqQSGCMY65l9Xl2UmHIt3q4KXG
    qh56M6LFqnuwfIWzj4+6lJzCpYKyjXbATjaaOCt+PGasTbqZbtvBlkqjPih8O4HBatfAlX7A
    VlynTtfm2kWZRG+laRhwKxR4K36yfXOscuvy7afYdDOEQszf0HSqdNfuuldXgfv9/GenT7zV
    wV6oWPr8+l+mUGi6SZDOYRV3LHu3YZvCoHm9XOm9lRJwJ3uUgTaCPIO+dRt5LxpxJoCsiTKN
    N2C2RzOKNo7NX1+xRlqrRPH28jOMQMbnn02sQdfAlHOofyN2NWu4W/7kNXGnfM/rrJbQYBlv
    K3qthhx0LBxxLDWEQonKVjaYOC6AOJi+oN/Dmd3CmGi6T1CuQWaqVvn7+ufHnXK/TITBb3qU
    gCN8N0OhPSV4N9O+kmWuSC1+P3a7atXttrbfiOjw6XevdRl0K4LHUt/r4TCCQYC4cFayRJ/E
    pFmZZmGebeXv5+rHnk+kVh10LavLsiV6OOXEm02GR16aazaFQ0mIRv/++Nzp3rnTvrvVwS2L
    NB1yLDmVOv7dAWerVFGZSDSWN3a/TkWNU+nInobHWnXBTjSCRW2pb0uQWiGAMHm2a1WWY5/A
    kVyrTWW3SDCUPh+AMjuVOG6qcDQ0Z5/Dp1ijST2ENCV9NX/FWkuhRieCMlCGVXm0cD+IQSl+
    M4jJVxx7LqDEplynW8LYx2q0SnG1UOPFm162RySMOVanT////wAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAM0ALAAAAAATABMAAAj/AJsJHNgsQIGD
    mQgqHBjA4BhQCwo0XDhw0AgyHwpEfAUGzYwACx3x+MSrSq4CHTTIWgbCAsiBdUox+yUiCo0p
    nQARqgUJ0RaCnK60CkCACh5aYnwQIJCDy62XARrBwkFAgIBEcl5YJUCsGAUEAoOsYUKCQAAB
    NXYpEUC0BxE9RQpqChZjEQFbAnwJE7LVg4xhxgLouNSClKoAQASIQuXHaoAlx/boOrRhwhxl
    WXDRUXEKCRZkoYxoOaJoFKNUKLxYuWPIjKAyPwJ12cHmRKwMbQql6AWgt+/fwN04ccUKQLI4
    Dr70gQDDAQs4F+yYAjDLBQcpwAAYMPCoyYDvAxRsR+99QxKCNxKgAMCgADx4SgAwJTHBoBmC
    IX/SVOp9oP+K3hFMUkJ9AgVQwSaW8GGDGpF4ckYYq4RA0UAJPNDAE3k08EACCgUEADs=
  }
  image create photo coord_update.gif -data {
    R0lGODlhEAAQAPcAACm9KWNj53t754SE54TWGIyMpbXnEL2998bvCM7vCN7eMe/vGPf/AP8A
    AP8A//8IAP8hAP8hIf85AP+1tf+9AP/e3v//AP//////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    /////////////////////////////////yH5BAEAAA4ALAAAAAAQABAAAAiZAB04ODAggMGD
    AQQMOCBwYAEFFiJKtLBAQQGGBwpMtEBBAgQIEigwuDgAokQKDyZUqDDhAQUFAgQsmChBZYQI
    LCUsEBBgI4QKERo0wAnBgsGNNYHinCDBaM+JKFWydOl0I0ePIClEPIoAgFcAVrf2ZEDga9iq
    EbuCDRugpEQDazcqWKhRYgKrIzE+DFvxYsMDPBEajMnQQUAAADs=
  }
  image create photo dropped_off.gif -data {
    R0lGODlhEAAQAKIFAP///wAAAICAgAAAoODg4P///wAAAAAAACH5BAEAAAUALAAAAAAQABAA
    AANMWCXc+lCQSQV8MoANArGXoHVjAEZaoKYnRqzrdy0Ex8nD4JIqgOsKiW3z28F8lhymY9Mk
    gYvUqvPMDTKkzk+J7XmquxtIGWkwHtBCAgA7
  }
  image create photo icon_attended.gif -data {
    R0lGODlhEAAQALMMAP+ysu7/GQAA/7ImAKWyAP9/f/97GQBIf7IAAH9RAP/jsgAAAP///wAA
    AAAAAAAAACH5BAEAAAwALAAAAAAQABAAAARgkLFFpU3YTlol/hInMl+5KQu6kIniZqnrrq2s
    JIyB7gapCL9bzqbo1WQ43UCxNDKfuGHT41oKJbqe55iRHL7bBGIMOhAIgYMFUSgAEN7zOc1g
    t93wrx487o81gIGCgxYRADs=
  }
  image create photo icon_camera.gif -data {
    R0lGODlhEAAQALMJANXlAEdHR////6WyADc3N+XEAFdXV7e3twAAAP///wAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAkALAAAAAAQABAAAARWMMlUZJ2YWnrzLGDnYUE5ZoGgBhOCjGUse8Zg
    37dBAgPg+yobSxIACHyEJHAwTBSPhIPgQDACmk8AQSWoHrHW6NR7nWS1yh/YyOX6mgnDb+7T
    oWSzSQQAOw==
  }
  image create photo icon_disabled.gif -data {
    R0lGODlhEAAQAPcAAO/v7/d7e/e1tf8AAP8ICP8YGP8pKf9SUv//////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    /////////////////////////////////yH5BAEAAAAALAAAAAAQABAAQAhyAAEIHEiwoMAC
    AwQgWMhwoYABBgQKIDCgosWLBAQMDHCx44AABQNQ7EgA5EADCQWoXLkSosADHj0eKCjggIEC
    Bgwc0GgQQM2bOHca5BizokkAMItanAkAocKGDB8WEIiSpdWHEX2OLJqRoEiPJXuKJRgQADs=
  }
  image create photo icon_discovered.gif -data {
    R0lGODlhEAAQALMLAJo3N7wLC612dqBgYKssLK8TE/UAAMzMzJmZmf///wAAAP///wAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAQABAAAARZcC1Eq7wYnZPSRtilKYlCKgcYjuWZhtNpnupl
    sF352jc3k7uFwSDQ5DqG3ZAwMP4SQ6EhAJhwjtDhsDppPbXcmPcEMBQypFZHgQhLRtcDqpbZ
    2IOwigW2iAAAOw==
  }
  image create photo icon_enabled.gif -data {
    R0lGODlhEAAQAPcAACF7ISl7KTmMOUKMQkqMSmula5S9lKXGpb3Wvc7ezu/v7///////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    /////////////////////////////////yH5BAEAAAoALAAAAAAQABAAQAiHABUIHEiwoMAB
    ARYkGAAgwIEFCw4ECCBAIIKJARpqnGggAIKBHTlKnNjQQMGQGDGaHEhg48aHCwIMEFggQAGI
    OA80zFigIIICBAYILfDRoIKfQocWJYjSJceBNTECmJrxoc2DAUJqhNlxpgKGOAsYgKhzKgGL
    EwckgNhx41IFWqVmNUqXYEAAADs=
  }
  image create photo icon_greenlight.gif -data {
    R0lGODlhEAAQALMKAAbMn/f39wAAAACZmQgICABmZpn/zACyjOfn5wCysv///wAAAAAAAAAA
    AAAAAAAAACH5BAEAAAoALAAAAAAQABAAAARSUElJqp1YisC7yIpAIIYBnEBAfNNmomehTkTw
    wvHs4vCgCjtezAcMCFED4uY4DBSZh4HseZQ6P0ueNOFsBQqAaEFKxBTHyfEVtOlwWKAQcJ6J
    AAA7
  }
  image create photo icon_needsmaint.gif -data {
    R0lGODlhEAAQAJEAAAAAACA/tP///wAAACH5BAAAAAAALAAAAAAQABAAAAIuhI9pwu2+XJjh
    xUYrBC9bLlGfkJXiYpoCmnpsO60gJm5d3VyMZ4dx/wjKFERDAQA7
  }
  image create photo icon_note.gif -data {
    R0lGODlhEAAQALMLAAAAAP///+fn5/8AAMfHx9fX1wgICJeXl7IAAPf39/+ysv///wAAAAAA
    AAAAAAAAACH5BAEAAAsALAAAAAAQABAAAARacMk5x6B4HhtuPkAIDIjiSSIgrAKJnAtACEFt
    mXA823USABgZrSZIAH5BQqEoKiApwlBhWgNCZ9Sa4TkRag0Gp7U7A0+dXBQWHaCN1awVQZQk
    zEWHTAyvp0QAADs=
  }
  image create photo icon_remove.gif -data {
    R0lGODlhDwAPAMQAAP////8AAMz//5mZzP8zM8xmmZnMzGaZzMzM/8yZmf9mmczMzJnM///M
    zP+ZmcxmZmZmmf9mZgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    ACH5BAAAAAAALAAAAAAPAA8AAAVlIMCMRlkOA6CqyOoih/G6K3ykLK0KKA4sgEYgohIqED4g
    wBFwCAgEAW8lU0WGgYUASV09A4ktV1UVLAKBAmLtS0mhhSx75b4uENC5aiBIgNdnD2MAfFJr
    ayODA4eMiW09kD0QKiEAOw==
  }
  image create photo icon_rsvp.gif -data {
    R0lGODlhEAAQALMIAP7lTMfHx6enp/f397ImAOfn5////wAAAP///wAAAAAAAAAAAAAAAAAA
    AAAAAAAAACH5BAEAAAgALAAAAAAQABAAAARVEMlJEbk1WwLw+WDIAd0XGEWqXtxlomrBssFX
    CLB8pbs94DpCytA7FAY/lLBgEBRTSCBToCvYjoIArql93pA/AZhqNYrB6Gg5G2i739qQfP7R
    2O/2CAA7
  }
  image create photo picked_up.gif -data {
    R0lGODlhEAAQAKIFAAAAAICAgP///+Dg4M4AAP///wAAAAAAACH5BAEAAAUALAAAAAAQABAA
    AANQWBXcWuSpMIYQNSgS5wACAAKDxnXUOJbQuahiaJ5Eel3k/FChCLKcneV2AXYWFt9P5yEW
    mYueUqCRvKSjqiQA6uG0QlIxZ/VUzmBrw1EuJAAAOw==
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ClipboardSelectAll {} {
  set w [focus]
  if {$w == ""} return
  switch -- [winfo class $w] {
    Entry { $w selection range 0 end }
    Text  { $w tag add sel 0.0 end }
  }
}

# -----------------------------------------------------------------------------

proc Gpx::ClipboardDelete {} {
  set w [focus]
  if {$w == ""} return
  catch { $w delete sel.first sel.last }
}

# -----------------------------------------------------------------------------

proc Gpx::Init {} {
  variable options
  variable setup
  global tcl_platform

  set options(pwd) [file normalize [pwd]]

  # option add *TFrame.relief groove
  option add *Tablelist.stripeBackground #e0e8f0

  set tl .init
  toplevel $tl
  wm withdraw $tl

  ImagesTclTk

  pack [ttk::frame $tl.f -padding 3 -relief groove]
  pack [ttk::frame $tl.f.t -padding 10]
  pack [ttk::label $tl.f.t.l -width 60 -anchor nw -textvariable Gpx::options(info) -image p_tcltk -compound left]
  pack [ttk::frame $tl.f.b -padding 10] -fill x
  set pb $tl.f.b.pb
  pack [ttk::progressbar $tl.f.b.pb -length 200 -mode indeterminate -maximum 9]

  wm deiconify $tl
  update
  $pb start 250

  update
  package require Tablelist_tile

  update
  package require vfs::zip

  update
  package require vfs::urltype
  vfs::urltype::Mount http

  update
  package require tdom
  dom setResultEncoding iso8859-1

  update
  package require http
  http::config -useragent GPXconvert

  update
  package require Tkhtml

  if {$::tcl_platform(platform) == "windows"} {
    update
    package require dde
  }

  bind HtmlClip <Motion> {
    set parent [winfo parent %W]
    set url [$parent href %x %y]
    if {[string length $url] > 0} {
      $parent configure -cursor hand2
    } else {
      $parent configure -cursor {}
    }
  } 

  bind HtmlClip <1> {
    set parent [winfo parent %W]
    set url [$parent href %x %y]
    if {[string length $url]} {
      eval [$parent cget -hyperlinkcommand] $url
    }
  }   

  update
  catch {source [file join $options(pwd) gpxconvert.ini]}

  if {[info exists setup(gpx)]} {
    # convert old ini file
    set setup(gpx,ids) {}
    foreach item $setup(gpx) {
      foreach $options(lb,cols) $item {}
      lappend setup(gpx,ids) $gpxid
      set setup($gpxid,addon) $addon
      set setup($gpxid,group) $group
      set setup($gpxid,note) $note
    }
    unset setup(gpx)
  }
  foreach gpxid $setup(gpx,ids) {
    foreach tag {gc wp} {
      if {![info exists setup($gpxid,$tag)]} {
        set setup($gpxid,$tag) {}
      }
    }
  }

  foreach w {Entry TEntry Text} {
    switch -- $tcl_platform(platform) {
      unix {
        bind $w <Alt-a> "event generate %W <<SelectAll>>"
        bind $w <Alt-x> "event generate %W <<Cut>>"
        bind $w <Alt-c> "event generate %W <<Copy>>"
        bind $w <Alt-v> "event generate %W <<Paste>>"
        if {$w == "Text"} {
          bind $w <Control-b> ""
          bind $w <Control-v> ""
        }
      }
      macintosh {
        bind $w <Command-a> "event generate %W <<SelectAll>>"
      }
      windows {
        bind $w <Control-a> "event generate %W <<SelectAll>>"
      }
    }
    bind $w <<SelectAll>> Gpx::ClipboardSelectAll
    bind $w <<Delete>> Gpx::ClipboardDelete
  }

  Languages
  Images
  ImagesFlags
  ImagesGC
}

# -----------------------------------------------------------------------------

proc Gpx::Main {} {
  variable options
  variable setup

  set tl .
  wm withdraw $tl
  Cursor watch

  Init

  wm title $tl {GPXconvert}
  wm protocol $tl WM_DELETE_WINDOW Gpx::Quit

  MainMenubar
  MainWindow

  after 250

  Cursor
  wm deiconify $tl
  destroy .init
}

# -----------------------------------------------------------------------------

if {[file rootname [file tail $argv0]]  == "$::Gpx::options(script,main)"} {
  set argc [llength $argv]
  for {set i 0} {$i < $argc} {incr i} {
    switch -- [lindex $argv $i] {
      -d -
      --debug {
        set ::Gpx::options(debug) 1
      }
       --flag {
        set ::Gpx::options(odbc,db) [lindex $argv [incr i]]
      }
    }
  }

  if {![TkPresent]} {
    puts stderr "no Tk present - abort"
  } else {
    if {$::Gpx::options(debug)} {
      puts stderr "argv0='$argv0' argc='$argc' argv='$argv'"
    }
    Gpx::Main
  }
}

