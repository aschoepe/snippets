#
# found on http://wiki.tcl.tk/48065
# if and expr modified, 2017 Alexander Schoepe
#

#//#
# <h2>Package Noon</h2>
# This is a Tcl implementation of sunrise/sunset calculation according to the <b>receipe</b><br>
# <a id="RECEIPE"></a>
# <blockquote>
# {@link http://williams.best.vwh.net/sunrise_sunset_algorithm.htm http://williams.best.vwh.net/sunrise_sunset_algorithm.htm}
# </blockquote>
# <p>
# which is derived from:<br>
# <blockquote>
# <em>
# Almanac for computers, 1990<br>
# published by Nautical Almanac Office<br>
# US Naval Observatory<br>
# Washington, DC 20392<br>
# </em>
# </blockquote>
# </p>
#
# <h2>Sunrise and Sunset Times</h2>
# There are 4 different categories of sunrise and sunset times and appropriate twilight intervals.
# They differ in the threshold value that denotes how far the solar disk is below the horizon.
# The threshold value can be expressed by the zenith angle.
# From the position of an observer the zenith can be seen as a pointer from the ground up in the sky
# and an arrow can be drawn from the observer's location to the horizon forming this angle.
# It is quite clear that this definition depends solely on the coordinates of the location of the observer
# and does not consider true horizon.
# 
# <dl>
# <dt>official Sunrise/Sunset</dt>
#   <dd>The official Sunrise/Sunset denotes the time when the edge of the solar disk seems to touch the horizon.
#       The zenith value for this kind of sunrise/sunset is available with <var>$Noon::OFFICIAL</var> (i.e. 90&deg;50').
#       This is also the default value for the procedures in this package</dd>
#   <dt>civilian Sunrise/Sunset</dt>
#   <dd>The civilian Sunrise/Sunset depicts the situation where the horizon is just visible (i.e. the sun is clearly below the horizon
#       but lights up the atmosphere).
#       The defined zenith value is 96&deg; available with <var>$Noon::CIVILIAN</var></dd>
#   <dt>nautical Sunrise/Sunset</dt>
#   <dd>Because on sea the horizon is visible earlier and longer another zenith angle is defined for this situation.
#       102&deg; / <var>$Noon::NAUTICAL</var></dd>
#   <dt>astronomical Sunrise/Sunset</dt>
#   <dd>When the sun is 108&deg; below the horizon the stars become visible. This zenith angle is defined by 
#       <var>$Noon::ASTRONOMICAL</var></dd>
# </dl>
# @author T. Heinlein
# @version 1.0
#//#

package provide noon 1.0

namespace eval Noon  {
  namespace export sunriseUTC
  namespace export sunriseDecUTC
  namespace export sunsetUTC
  namespace export sunsetDecUTC
  namespace export formatDecTime

  # zenith controls kind of sunrise/sunset times
  variable OFFICIAL      90.8333      ;# solar disk touches horizon
  variable CIVILIAN      96.0         ;# horizon is still/already visible
  variable NAUTICAL     102.0         ;# horizon is visible on sea
  variable ASTRONOMICAL 108.0         ;# the stars become visible / will disappear

  variable D2R          0.01745329252 ;# factor degree to rad i.e. PI / 180

  # formats a time given as decimal float value (0 ... 24) in HH:MM presentation
  # Regardless of the timezone offset given the resulting time is garanteed in 00 ... 24 interval.
  # @param dec  time of day as decimal number   e.g. 23.5
  # @param offset  time offset from GMT in hours (decimal) default is 0.
  # @return time in Hours/Minutes format HH:MM  e.g. 23:30
  # {@link #RECEIPE  receipe}
  proc formatDecTime { dec {offset 0} } {
    set decTime [expr {$dec + $offset}]

    if { $decTime >= 24.0 } {
      set $decTime [expr {$decTime - 24}]
    }
    if { $decTime < 0.0 } {
      set $decTime [expr {$decTime + 24}]
    }
    set hours [expr {floor($decTime)}]
    set frac  [expr {$decTime - $hours}]
    set mins  [expr {round($frac * 60.0)}]
    return [format "%02d:%02d" [expr {int($hours)}] [expr {int($mins)}]]
  }

  # sinus with argument in degrees
  # @param deg  angle in degrees 0 ... 360
  # @return sinus value -1 ... +1
  proc _sin { deg } {
    variable D2R
    return [expr {sin($deg * $D2R)}]
  }

  # cosinus with argument in degrees
  # @param deg  angle in degrees 0 ... 360
  # @return cosinus value -1 ... +1
  proc _cos { deg } {
    variable D2R
    return [expr {cos($deg * $D2R)}]
  }

  # tangens with argument in degrees
  # @param deg  angle in degrees 0 ... 360
  # @return tangens value 
  proc _tan { deg } {
    variable D2R
    return [expr {tan($deg * $D2R)}]
  }

  # invers tangens returning degrees
  # @param val  value for looking up tangens 
  # @return tangens value in degrees 0 ... 360
  proc _atan { val } {
    variable D2R
    return [expr {atan($val) / $D2R}]
  }

  # invers sinus returning degrees
  # @param val  value for looking up sinus 
  # @return sinus value in degrees 0 ... 360
  proc _asin { val } {
    variable D2R
    return [expr {asin($val) / $D2R}]
  }

  # invers cosinus returning degrees
  # @param val  value for looking up cosine  -1 ... +1
  # @return cosinus value in degrees 0 ... 360
  proc _acos { val } {
    variable D2R
    return [expr {acos($val) / $D2R}]
  }

  # given a date calculates day of the year 1 ... 365 or 366 
  # step 1 from (**)
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @return day of the year (count from 1st January)
  proc _dayOfYear { y m d } {
    set n1 [expr {floor(275 * $m / 9.0)}]
    set n2 [expr {floor(($m + 9) / 12.0)}]
    set n3 [expr {(1 + floor(($y -4 * floor($y / 4.0) + 2) / 3.0))}]
    set N  [expr {int($n1 - ($n2*$n3) + $d - 30)}]
    return $N
  }

  # calculate a time base for sunrise
  # step 2 from (**)
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @param longitude  longitude
  # @return base time of day for calculating sunrise (decimal)
  proc _sunriseBaseTime { y m d longitude } {
    set N [_dayOfYear $y $m $d]
    set longH  [expr {$longitude / 15.0}] ;# from step 2: Longitude in hours (24h -> 360 deg)
    set bt [expr {$N + ((6 - $longH) / 24.0)}]
    return $bt
  }

  # calculate a time base for sunset
  # step 2 from (**)
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @param longitude  longitude
  # @return base time of day for calculating sunset (decimal)
  proc _sunsetBaseTime { y m d longitude } {
    set N [_dayOfYear $y $m $d]
    set longH  [expr {$longitude / 15.0}] ;# from step 2: Longitude in hours (24h -> 360 deg)
    set bt [expr {$N + ((18 - $longH) / 24.0)}]
    return $bt
  }

  # calculate sun's true longitude
  # step 5a from (**)
  # @param timeBase a time base for sunrise or sunset
  #        (@see #_sunriseBaseTime _sunriseBaseTime
  #         @see #_sunsetBaseTime _sunsetBaseTime)
  # @return the true longitude of the sun
  proc _trueLongitude { timeBase } {
    set M [expr {(0.9856 * $timeBase) - 3.289}] ;# anomaly of the sun
    set trueL [expr {$M + (1.916 * [_sin $M]) + (0.020 * [_sin [expr {2 * $M}]]) + 282.634}]
    # align
    if { $trueL > 360.0 } {
      set trueL [expr {$trueL - 360.0}]
    } elseif { $trueL < 0.0 } {
      set trueL [expr {$trueL + 360.0}]
    }
    return $trueL
  }

  # compute sun's right ascension
  # steps 5b and 5c from (**)
  # @param trueLong the true longitude of the sun
  # @return the right ascension of the sun in hours
  proc _rightAscension { trueLong } {
    set ra [expr {[_atan [expr 0.91764 * [_tan $trueLong]]]}]
    # align
    if { $ra > 360.0 } {
      set ra [expr {$ra - 360.0}]
    } elseif { $ra < 0.0 } {
      set ra [expr {$ra + 360.0}]
    }
    set Lq  [expr {(floor($trueLong/90.0) * 90.0)}]
    set RAq [expr {(floor($ra/90.0) * 90.0)}]
    set RA  [expr {$ra + ($Lq - $RAq)}]
    return [expr {$RA / 15.0}]
  }

  # compute hour angle for sunrise
  # step 7a from (**)
  # @param lat the latitude
  # @param trueLong_sr the true longitude of the sun at sunrise
  # @param zenith the zenith in degrees
  # @return the hour angle of the sun
  proc _hourAngleSunrise { lat trueLong_sr zenith } {
    # step 6: 
    set sinDec [expr {0.39782 * [_sin $trueLong_sr]}]
    set cosDec [expr {[_cos [_asin $sinDec]]}]
  
    # step 7:
    set cosH [expr {([_cos $zenith] - ($sinDec * [_sin $lat])) / ($cosDec * [_cos $lat])}]
    if { $cosH > 1 } {
      # sun does not rise this day at this location
      return "none"
    }
    set Hrise [expr {(360 - [_acos $cosH]) / 15.0}]
    return $Hrise
  }

  # compute hour angle for sunset
  # step 7a from (**)
  # @param lat the latitude
  # @param trueLong_ss the true longitude of the sun at sunset
  # @param zenith the zenith in degrees
  # @return the hour angle of the sun
  proc _hourAngleSunset { lat trueLong_ss zenith } {
    # step 6: 
    set sinDec [expr {0.39782 * [_sin $trueLong_ss]}]
    set cosDec [expr {[_cos [_asin $sinDec]]}]

    # step 7:
    set cosH [expr {([_cos $zenith] - ($sinDec * [_sin $lat])) / ($cosDec * [_cos $lat])}]
    if { $cosH < -1 } {
      # sun does not set this day at this location
      return "none"
    }
    set Hset [expr {[_acos $cosH] / 15.0}]
    return $Hset
  }

  # calculates the time of sunrise. Date and Time are UTC.
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @param lat  latitude
  # @param lon  longitude
  # @param zenith  the zenith in degrees (default is Noon::OFFICIAL)
  # @return the UTC time of sunrise as a decimal value 0 ... 24
  #    or the string 'none' if there is no sunrise at this location
  proc sunriseDecUTC { y m d lat lon {zenith {}} } {
    if {$zenith eq {}} {
      set zenith $Noon::OFFICIAL
    }
    set longH    [expr {$lon / 15.0}] ;# from step 2: Longitude in hours (24h -> 360 deg)
    set baseTime [_sunriseBaseTime  $y $m $d $lon]
    set trueLong [_trueLongitude    $baseTime]
    set ra       [_rightAscension   $trueLong]
    set ha       [_hourAngleSunrise $lat $trueLong $zenith]

    if { $ha eq {none"} } {
      return $ha
    }

    set t   [expr {$ha + $ra - (0.06571 * $baseTime) - 6.622}]
    set utc [expr {$t - $longH}]

    if { $utc > 24 } {
      set utc [expr {$utc - 24}]
    }  elseif { $utc < 0 } {
      set utc [expr {$utc + 24}]
    }
    return $utc
  }

  # calculates the time of sunrise. Date and Time are UTC.
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @param lat  latitude
  # @param lon  longitude
  # @param zenith  the zenith in degrees (default is Noon::OFFICIAL)
  # @return the UTC time of sunrise in hours and minutes / HH:MM
  #    or the string 'none' if there is no sunrise at this location
  proc sunriseUTC { y m d lat lon {zenith {}} } {
    set utc [sunriseDecUTC $y $m $d $lat $lon $zenith]
    if { $utc eq {none} } {
      return $utc
    }
    return [formatDecTime $utc]
  }

  # calculates the time of sunset
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @param lat  latitude
  # @param lon  longitude
  # @param zenith  the zenith in degrees (default is Noon::OFFICIAL)
  # @return the UTC time of sunrise as a decimal value 0 ... 24
  #    or the string 'none' if there is no sunset at this location
  proc sunsetDecUTC { y m d lat lon {zenith {}} } {
    if {$zenith eq {}} {
      set zenith $Noon::OFFICIAL
    }
    set longH    [expr {$lon / 15.0}] ;# from step 2: Longitude in hours (24h -> 360 deg)
    set baseTime [_sunsetBaseTime  $y $m $d $lon]
    set trueLong [_trueLongitude    $baseTime]
    set ra       [_rightAscension   $trueLong]
    set ha       [_hourAngleSunset  $lat $trueLong $zenith]

    if { $ha eq {none} } {
      return $ha
    }

    set t   [expr {$ha + $ra - (0.06571 * $baseTime) - 6.622}]
    set utc [expr {$t - $longH}]

    if { $utc > 24 } {
      set utc [expr {$utc - 24}]
    }  elseif { $utc < 0 } {
      set utc [expr {$utc + 24}]
    }
    return $utc
  }

  # calculates the time of sunset. Date and Time are UTC.
  # @param y  year (4 digits)
  # @param m  month where January is 1 .... December is 12
  # @param d  day of month  (1 ... 31)
  # @param lat  latitude
  # @param lon  longitude
  # @param zenith  the zenith in degrees (default is Noon::OFFICIAL)
  # @return the UTC time of sunset in hours and minutes / HH:MM
  #    or the string 'none' if there is no sunset at this location
  proc sunsetUTC { y m d lat lon {zenith {}} } {
    if {$zenith eq {}} {
      set zenith $Noon::OFFICIAL
    }
    set utc [sunsetDecUTC $y $m $d $lat $lon $zenith]
    if { $utc eq {none} } {
      return $utc
    }
    return [formatDecTime $utc]
  }
}
# --- end namespace

if {0} {
  # Bochum
  # lon 7.2149783 lat 51.4815833
  # Datum: 27.10.2017
  # Sonnenaufgang: 08:15
  # Sonnenuntergang: 18:14

  # set y 2017
  # set m 10
  # set d 27

  set lon 7.2149783
  set lat 51.4815833
  set ct [clock seconds]
  set y  [clock format $ct -format %Y]
  set m  [string trimleft [clock format $ct -format %m] 0]
  set d  [string trimleft [clock format $ct -format %d] 0]

  set sunrise [clock scan [clock format $ct -format "%Y-%m-%d [Noon::sunriseUTC $y $m $d $lat $lon]:00"] -gmt 1]
  set sunset  [clock scan [clock format $ct -format "%Y-%m-%d [Noon::sunsetUTC  $y $m $d $lat $lon]:00"] -gmt 1]
  puts "sunrise [clock format $sunrise -format {%Y-%m-%d %H:%M %Z}]"
  puts "sunset  [clock format $sunset  -format {%Y-%m-%d %H:%M %Z}]"
}
