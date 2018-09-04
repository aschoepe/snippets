#
# Ereignisse jahr bundesland
#  returns with a data tripple (name date type)
#

proc Ereignisse {y {bl {}}} {
  # Julianisch -> Gregorianisch
  proc J2G { jda } {
    set jd [expr {floor($jda + 0.5)}]
    set z [expr {$jda - $jd + 0.500001}]
    set tz [expr {$jd - 1721117}]
    if {$jd > 2299160} {
      set tz [expr {$tz + (floor(($tz + 2) / 36524.25) - floor(($tz + 2) / 146097) - 2)}]
    }
    set y [expr {floor(($tz - 0.2) / 365.25)}]
    set r [expr {$tz - floor($y * 365.25)}]
    set m [expr {floor(($r - 0.5) / 30.6)}]
    set d [expr {$r - floor($m * 30.6 + 0.5)}]
    set m [expr {$m + 3}]
    if {$m > 12} {
      set m [expr {$m - 12}]
      set y [expr {$y + 1}]
    }

    set h1 [expr {$z * 24}]
    set ho [expr {floor($h1)}]
    set mi [expr {floor(($h1 - $ho) * 60)}]

    return [format {%04d-%02d-%02d %02d:%02d} [expr {int($y)}] [expr {int($m)}] [expr {int($d)}] [expr {int($ho)}] [expr {int($mi)}]]
  }

  # BW Baden-Württemberg
  # BY Bayern
  # BE Berlin
  # BB Brandenburg
  # HB Bremen
  # HH Hamburg
  # HE Hessen
  # MV Mecklenburg-Vorpommern
  # NI Niedersachsen
  # NW Nordrhein-Westfalen
  # RP Rheinland-Pfalz
  # SL Saarland
  # SN Sachsen
  # ST Sachsen-Anhalt
  # SH Schleswig-Holstein
  # TH Thüringen

  # g   gesetzlicher Feiertag
  # f|F Festtag
  # e|E Ereigniss

  # a alle Bundesländer

  set defs {
    Neujahr                           01-01 g a
    "Heilige drei K\u00f6nige"        01-06 g {BW BY ST}
    Karfreitag                        O-2   g a
    Ostersonntag                      O+0   g BB
    Ostermontag                       O+1   g a
    {Tag der Arbeit}                  05-01 g a
    {Christi Himmelfahrt}             O+39  g a
    Pfingstsonntag                    O+49  g BB
    Pfingstmontag                     O+50  g a
    Fronleichnam                      O+60  g {BW BY HE NW RP SL}
    "Mari\u00e4 Himmelfahrt"          08-15 g {BY SL}
    {Tag der deutschen Einheit}       10-03 g a
    Reformationstag                   10-31 g {BB HB HH MV NI SN ST SH TH}
    Allerheiligen                     11-01 g {BW BY NW RP SL}
    "Bu\u00df- und Bettag"            A-32  g SN
    {1. Weihnachtstag}                12-25 g a
    {2. Weihnachtstag}                12-26 g a

    Weiberfastnacht                   O-52  f a
    Fastnachtssamstag                 O-50  f a
    Fastnachtssonntag                 O-49  f a
    {Rosenmontag, Fastnacht}          O-48  f a
    Aschermittwoch                    O-46  f a
    Valentinstag                      02-14 f a
    Frauentag                         03-08 f a
    Palmsonntag                       O-7   f a
    Gr\u00fcndonnerstag               O-3   f a
    Karsamstag                        O-1   f a
    Walpurgisnacht                    04-30 f a
    Muttertag                         MT    f a
    {17. Juni 1953}                   06-17 f a
    Johannistag                       06-24 f a
    Siebenschl\u00e4fertag            06-27 f a
    {Peter und Paul}                  06-29 f a
    {Augsburger Friedensfest}         08-08 f BY
    Michaelistag                      09-29 f a
    Erntedankfest                     ED    f a
    Halloween                         10-31 f a
    Allerseelen                       11-02 f a
    Martinstag                        11-11 f a
    Volkstrauertag                    A-35  f a
    Totensonntag                      A-28  f a
    Barbara                           12-04 f a
    Nikolaus                          12-06 f a
    {1. Advent}                       A-21  f a
    {2. Advent}                       A-14  f a
    {3. Advent}                       A-7   f a
    {4. Advent}                       A-0   f a
    Heiligabend                       12-24 F a
    Silvester                         12-31 F a

    "meteo. Fr\u00fchlingsanfang"     03-01 e a
    Fr\u00fchlingsanfang              FaB   E a
    Sommerzeitbeginn                  SZB   e a
    {meteo. Sommeranfang}             06-01 e a
    {Sommeranfang, Sommersonnenwende} SaB   E a
    {meteo. Herbstanfang}             09-01 e a
    Herbstanfang                      HaB   E a
    Sommerzeitende                    SZE   e a
    {meteo. Winteranfang}             12-01 e a
    {Winteranfang, Wintersonnenwende} WaB   E a
  }
  
  # astronomische Jahreszeiten
  set Y [expr {($y - 2000) / 1000.0}]
  # März-Äquinoktium (Beginn des astronomischen Frühlings):
  set FaB [J2G [expr {2451623.80984 + 365242.37404 * $Y + 0.05169 * pow($Y,2) - 0.00411 * pow($Y,3) - 0.00057 * pow($Y,4)}]]
  # Juni-Solstitium (Beginn des astronomischen Sommers):
  set SaB [J2G [expr {2451716.56767 + 365241.62603 * $Y + 0.00325 * pow($Y,2) + 0.00888 * pow($Y,3) - 0.00030 * pow($Y,4)}]]
  # September-Äquinoktium (Beginn des astronomischen Herbstes)
  set HaB [J2G [expr {2451810.21715 + 365242.01767 * $Y - 0.11575 * pow($Y,2) + 0.00337 * pow($Y,3) + 0.00078 * pow($Y,4)}]]
  # Dezember-Solstitium (Beginn des astronomischen Winters):
  set WaB [J2G [expr {2451900.05952 + 365242.74049 * $Y - 0.06223 * pow($Y,2) - 0.00823 * pow($Y,3) + 0.00032 * pow($Y,4)}]]

  # Advent = 4. Advent am Sonntag vor Heilig Abend oder wenn Heilig Abend Sonntag an Heilig Abend
  set diff [expr {[clock format [set a4 [clock scan ${y}-12-24]] -format %u] * -1}]
  if {$diff != -7} {
    set a4 [clock add $a4 $diff days]
  }

  # Lichtenberg-Formel (korrigierte Gauss-Formel) zur Berechnung des Ostersonntags (Offset zum 1. März):
  set a  [expr {$y % 19}]                                    ;# Mondparameter
  set k  [expr {$y / 100}]                                   ;# Säkularzahl
  set m  [expr {15 + (3 * $k + 3) / 4 - (8 * $k + 13) / 25}] ;# säkulare Mondschaltung
  set d  [expr {(19 * $a + $m) % 30}]                        ;# Keim für den ersten Vollmond im Frühling
  set s  [expr {2 - (3 * $k + 3) / 4}]                       ;# säkulare Sonnenschaltung
  set r  [expr {$d / 29 + ($d / 28 - $d / 29) * $a / 11}]    ;# kalendarische Korrekturgröße (beseitigt die Gaußschen Ausnahmeregeln)
  set og [expr {21 + $d - $r}]                               ;# Ostergrenze (Märzdatum des Ostervollmonds)
  set sz [expr {7 - ($y + $y / 4 + $s) % 7}]                 ;# erster Sonntag im März
  set oe [expr {7 - ($og - $sz) % 7}]                        ;# Entfernung des Ostersonntag von der Ostergrenze in Tagen
  set o  [expr {$og + $oe - 1}]                              ;# Datum des Ostersonntags als Märzdatum (32. März = 1. April usw.)

  set os [clock add [clock scan $y-03-01] $o days]

  set ereignisse {}
  foreach {name tag typ land} $defs {
    if {$bl ne {} && $bl ni $land && $land ne {a}} {
      set typ f
    }
    switch -glob -- $tag {
      FaB {
        # astronomischer Frühlingsbeginn nordhemisphäre; südhemisphäre: Herbst
        lappend ereignisse $name [string range $FaB 0 9] $typ
      }
      SaB {
        # astronomischer Sommerbeginn nordhemisphäre; südhemisphäre: Winter
        lappend ereignisse $name [string range $SaB 0 9] $typ
      }
      HaB {
        # astronomischer Herbstbeginn nordhemisphäre; südhemisphäre: Frühling
        lappend ereignisse $name [string range $HaB 0 9] $typ
      }
      WaB {
        # astronomischer Winterbeginn nordhemisphäre; südhemisphäre: Sommer
        lappend ereignisse $name [string range $WaB 0 9] $typ
      }
      ED {
        # Erntedankfest = erster Sonntag im Oktober
        set diff [expr {7 - [clock format [set ct [clock scan ${y}-10-01]] -format %u]}]
        lappend ereignisse $name [clock format [clock add $ct $diff days] -format %Y-%m-%d] $typ
      }
      MT {
        # Muttertag = zweiter Sonntag im Mai, wenn Pfingsten und Muttertag gleich, dann Muttertag eine Woche früher
        set diff [expr {7 - [clock format [set ct [clock scan ${y}-05-01]] -format %u] + 7}]
        set muttertag [clock add $ct $diff days]
        if {$muttertag == [clock add $os +49 days]} {
          set muttertag [clock add $muttertag -7 days]
        }
        lappend ereignisse $name [clock format $muttertag -format %Y-%m-%d] $typ
      }
      SZB {
        # Sommmerzeitbeginn = letzter Sonntag im März
        set diff [expr {[clock format [set ct [clock scan ${y}-03-31]] -format %u] * -1}]
        if {$diff == -7} {
          set diff 0
        }
        lappend ereignisse $name [clock format [clock add $ct $diff days] -format %Y-%m-%d] $typ
      }
      SZE {
        # Sommmerzeitende = letzter Sonntag im Oktober
        set diff [expr {[clock format [set ct [clock scan ${y}-10-31]] -format %u] * -1}]
        if {$diff == -7} {
          set diff 0
        }
        lappend ereignisse $name [clock format [clock add $ct $diff days] -format %Y-%m-%d] $typ
      }
      O* {
        # abhängig von Ostern
        lappend ereignisse $name [clock format [clock add $os [string range $tag 1 end] days] -format %Y-%m-%d] $typ
      }
      A* {
        # abhängig vom Advent
        lappend ereignisse $name [clock format [clock add $a4 [string range $tag 1 end] days] -format %Y-%m-%d] $typ
      }
      default {
        lappend ereignisse $name $y-$tag $typ
      }
    }
  }
  return $ereignisse
}
