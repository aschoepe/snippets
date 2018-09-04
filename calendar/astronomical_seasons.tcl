
# with a deviation less than 15 minutes

proc AstronomicalSeasons {year} {
  set Y [expr {($year - 2000) / 1000.0}]

  # Julian to Gregorian
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

  # March-equinox (Beginning of the astronomical spring):
  set jda [expr {2451623.80984 + 365242.37404 * $Y + 0.05169 * pow($Y,2) - 0.00411 * pow($Y,3) - 0.00057 * pow($Y,4)}]
  puts "Spring [J2G $jda]"

  # June-solstice (Beginning of the astronomical summer):
  set jda [expr {2451716.56767 + 365241.62603 * $Y + 0.00325 * pow($Y,2) + 0.00888 * pow($Y,3) - 0.00030 * pow($Y,4)}]
  puts "Summer [J2G $jda]"

  # September-equinox (Beginning of the astronomical autumn)
  set jda [expr {2451810.21715 + 365242.01767 * $Y - 0.11575 * pow($Y,2) + 0.00337 * pow($Y,3) + 0.00078 * pow($Y,4)}]
  puts "Autumn [J2G $jda]"

  # December-solstice (Beginning of the astronomical winter):
  set jda [expr {2451900.05952 + 365242.74049 * $Y - 0.06223 * pow($Y,2) - 0.00823 * pow($Y,3) + 0.00032 * pow($Y,4)}]
  puts "Winter [J2G $jda]"
}

AstronomicalSeasons 2018
