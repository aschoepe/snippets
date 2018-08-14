proc UnicodeCharsToHex { txt } {
  set new {}
  foreach {a b c d} [split [binary encode hex [encoding convertto unicode $txt]] {}] {
    if {[expr 0x${c}${d}${a}${b}] <= 0x7f} {
      append new [binary decode hex ${c}${d}${a}${b}]
    } else {
      append new \\u $c $d $a $b
    }
  }
  return $new
}

puts [UnicodeCharsToHex \u65B0\u7D30\u660E\u9AD4]
puts [UnicodeCharsToHex Arbeitsblätter]
