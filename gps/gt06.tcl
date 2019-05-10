#
# GT06 GPS Tracker Protocol
# 2019-05-07 Alexander Schoepe, Bochum, DE
#

array set cData {}

set tabITU {
  0x0000 0x1189 0x2312 0x329B 0x4624 0x57AD 0x6536 0x74BF 0x8C48 0x9DC1 0xAF5A 0xBED3 0xCA6C 0xDBE5 0xE97E 0xF8F7
  0x1081 0x0108 0x3393 0x221A 0x56A5 0x472C 0x75B7 0x643E 0x9CC9 0x8D40 0xBFDB 0xAE52 0xDAED 0xCB64 0xF9FF 0xE876
  0x2102 0x308B 0x0210 0x1399 0x6726 0x76AF 0x4434 0x55BD 0xAD4A 0xBCC3 0x8E58 0x9FD1 0xEB6E 0xFAE7 0xC87C 0xD9F5
  0x3183 0x200A 0x1291 0x0318 0x77A7 0x662E 0x54B5 0x453C 0xBDCB 0xAC42 0x9ED9 0x8F50 0xFBEF 0xEA66 0xD8FD 0xC974
  0x4204 0x538D 0x6116 0x709F 0x0420 0x15A9 0x2732 0x36BB 0xCE4C 0xDFC5 0xED5E 0xFCD7 0x8868 0x99E1 0xAB7A 0xBAF3
  0x5285 0x430C 0x7197 0x601E 0x14A1 0x0528 0x37B3 0x263A 0xDECD 0xCF44 0xFDDF 0xEC56 0x98E9 0x8960 0xBBFB 0xAA72
  0x6306 0x728F 0x4014 0x519D 0x2522 0x34AB 0x0630 0x17B9 0xEF4E 0xFEC7 0xCC5C 0xDDD5 0xA96A 0xB8E3 0x8A78 0x9BF1
  0x7387 0x620E 0x5095 0x411C 0x35A3 0x242A 0x16B1 0x0738 0xFFCF 0xEE46 0xDCDD 0xCD54 0xB9EB 0xA862 0x9AF9 0x8B70
  0x8408 0x9581 0xA71A 0xB693 0xC22C 0xD3A5 0xE13E 0xF0B7 0x0840 0x19C9 0x2B52 0x3ADB 0x4E64 0x5FED 0x6D76 0x7CFF
  0x9489 0x8500 0xB79B 0xA612 0xD2AD 0xC324 0xF1BF 0xE036 0x18C1 0x0948 0x3BD3 0x2A5A 0x5EE5 0x4F6C 0x7DF7 0x6C7E
  0xA50A 0xB483 0x8618 0x9791 0xE32E 0xF2A7 0xC03C 0xD1B5 0x2942 0x38CB 0x0A50 0x1BD9 0x6F66 0x7EEF 0x4C74 0x5DFD
  0xB58B 0xA402 0x9699 0x8710 0xF3AF 0xE226 0xD0BD 0xC134 0x39C3 0x284A 0x1AD1 0x0B58 0x7FE7 0x6E6E 0x5CF5 0x4D7C
  0xC60C 0xD785 0xE51E 0xF497 0x8028 0x91A1 0xA33A 0xB2B3 0x4A44 0x5BCD 0x6956 0x78DF 0x0C60 0x1DE9 0x2F72 0x3EFB
  0xD68D 0xC704 0xF59F 0xE416 0x90A9 0x8120 0xB3BB 0xA232 0x5AC5 0x4B4C 0x79D7 0x685E 0x1CE1 0x0D68 0x3FF3 0x2E7A
  0xE70E 0xF687 0xC41C 0xD595 0xA12A 0xB0A3 0x8238 0x93B1 0x6B46 0x7ACF 0x4854 0x59DD 0x2D62 0x3CEB 0x0E70 0x1FF9
  0xF78F 0xE606 0xD49D 0xC514 0xB1AB 0xA022 0x92B9 0x8330 0x7BC7 0x6A4E 0x58D5 0x495C 0x3DE3 0x2C6A 0x1EF1 0x0F78
}


proc CrcITU { data } {
  global tabITU
  
  set fcs 0xFFFF
  if {[binary scan $data cu* bytes]} {
    foreach byte $bytes {
      set fcs [expr {($fcs >> 8) ^ [lindex $tabITU [expr {($fcs ^ $byte) & 0xFF}]]}]
    }
  }
  return [expr {$fcs ^ 0xFFFF}]
}


proc Answer { fd protocol msgId } {
  set length 5
  set data [binary format ccS $length $protocol $msgId]
  set raw [binary format H4ccSSH4 7878 $length $protocol $msgId [set crc [CrcITU $data]] 0d0a]
  puts -nonewline $fd $raw
  puts "  > answer length $length proto $protocol [format 0x%x $protocol] msgId $msgId [format 0x%x $msgId] crc $crc [format 0x%x $crc]"
  puts "$fd answer  [binary encode hex $raw]"
}


proc SplitLocation {} {
  global cs gpsRtDp gpsPos ewLon snLat course
  global gps gpsLen gpsSat

  set gpsRtDp [expr {$cs >> 13 & 0x1}]
  if {$gpsRtDp} {
    set gpsRtDp {real-time}
  } else {
    set gpsRtDp {differential}
  }
  set gpsPos [expr {$cs >> 12 & 0x1}]
  set ewLon [expr {$cs >> 11 & 0x1}]
  if {!$ewLon} {
    set ewLon {E}
  } else {
    set ewLon {W}
  }
  set snLat [expr {$cs >> 10 & 0x1}]
  if {!$snLat} {
    set snLat {S}
  } else {
    set snLat {N}
  }
  set course [expr {$cs & 0x3FF}]

  set gpsLen [expr {$gps >> 4 & 0x0F}]
  set gpsSat [expr {$gps & 0x0F}]
}


proc SplitStatus {} {
  global ti tiCurcit tiGPS tiAlarm tiCharge tiACC tiActive voltage signal alarm lang

  set tiCurcit [expr {$ti >> 7 && 0x1}]
  switch -- $tiCurcit {
    0 { set tiCurcit On }
    1 { set tiCurcit Off }
  }
  set tiGPS [expr {$ti >> 6 && 0x1}]
  switch -- $tiGPS {
    1 { set tiGPS on }
    0 { set tiGPS off }
  }
  set tiAlarm [expr {$ti >> 3 && 0x7}]
  set tiCharge [expr {$ti >> 2 && 0x1}]
  set tiACC [expr {$ti >> 1 && 0x1}]
  switch -- $tiACC {
    1 { set tiACC high }
    0 { set tiACC low }
  }
  set tiActive [expr {$ti && 0x1}]

  switch -- $voltage {
    0 { set voltage {NoPower} }
    1 { set voltage {ExtremelyLow} }
    2 { set voltage {VeryLow} }
    3 { set voltage {LowBattery} }
    4 { set voltage {Medium} }
    5 { set voltage {High} }
    6 { set voltage {VeryHigh} }
  }
  switch -- [expr {$signal & 0xF}] {
    0 { set signal {No} }
    1 { set signal {ExtremelyWeak} }
    2 { set signal {VeryWeak} }
    3 { set signal {Good} }
    4 { set signal {Strong} }
  }
  switch -- $alarm {
    0 { set alarm {Normal} }
    1 { set alarm {SOS} }
    2 { set alarm {PowerCut} }
    3 { set alarm {Shock} }
    4 { set alarm {FenceIn} }
    5 { set alarm {FenceOut} }
  }
  switch -- $lang {
    1 { set lang {Chinese} }
    2 { set lang {English} }
  }
}


proc EncodeData { fd } {
  global cData
  global cs gpsRtDp gpsPos ewLon snLat course
  global gps gpsLen gpsSat
  global ti tiCurcit tiGPS tiAlarm tiCharge tiACC tiActive voltage signal alarm lang

  set raw $cData($fd,raw)

  set startBytes {}
  set dataLength 999
  set protocol -1
  while {[set rawLength [string length $raw]] > 3} {
    puts "$fd encode  [string length $raw] [binary encode hex $raw]"
    set dataLength 999
    set protocol -1
    if {[binary scan $raw H4cucu startBytes dataLength protocol]} {
      if {$startBytes eq {7878}} {
        break
      } else {
        # startBytes != 7878
        set raw [string range $raw 1 end]
        set cData($fd,raw) $raw
        continue
      }
    } else {
      # error binary scan
      return
    }
  }
  if {$rawLength < $dataLength + 3} {
    # encode data to short
    return
  }
  set toCrc [string range $raw 2 ${dataLength}]
  set raw [string range $raw 4 ${dataLength}+2]
  binary scan [string range $raw end-3 end] SuSu msgId crc
  set cData($fd,raw) [string range $cData($fd,raw) ${dataLength}+5 end]
  if {$crc == [CrcITU $toCrc]} {
    set crcT true
  } else {
    set crcT false
  }
  puts [format {  > start %s length %d proto 0x%02x msg %d crc %04x %s %s / %s} $startBytes $dataLength $protocol $msgId $crc $crcT [binary encode hex $raw] [binary encode hex $cData($fd,raw)]]

  switch -- [format %02x $protocol] {
    01 {
      # 0x01 Login Message
      puts "  > 0x01 Login Message"
      binary scan $raw H16 IMEI
      puts "IMEI $IMEI"
      set cData($fd,IMEI) $IMEI
    }
    12 {
      # 0x12 Location Data
      puts "  > 0x12 Location Data"
      binary scan $raw cucucucucucucuIuIucuSuSucuSuH6 year month day hour minute second gps lat lon speed cs MCC MNC LAC cellId
      SplitLocation
      puts "  > date [format {%04d-%02d-%02d %02d:%02d:%02d} [expr {$year + 2000}] $month $day $hour $minute $second]"
      puts "  > gpsLen $gpsLen gpsSat $gpsSat lat [format %.6f [expr {$lat / 1800000.0}]] lon [format %.6f [expr {$lon / 1800000.0}]] speed $speed gpsRtDp $gpsRtDp gpsPos $gpsPos ewLon $ewLon snLat $snLat course $course"
      puts "  > Mobile Country Code MCC $MCC Mobile Network Code MNC $MNC Location Area Code LAC $LAC cellId [scan $cellId %x]"
    }
    13 {
      # 0x13 Status information
      puts "  > 0x13 Status information"
      binary scan $raw cucucucucu ti voltage signal alarm lang
      SplitStatus
      puts "  > tiCurcit $tiCurcit tiGPS $tiGPS tiAlarm $tiAlarm tiCharge $tiCharge tiACC $tiACC tiActive $tiActive voltage $voltage signal $signal alarm $alarm lang $lang"
    }
    15 {
      # 0x15 String information
      puts "  > 0x15 String information"
    }
    16 {
      # 0x16 Alarm data
      puts "  > 0x16 Alarm data"
      binary scan $raw cucucucucucucuIuIucuSuSucuSuH6cucucucucu year month day hour minute second gps lat lon speed cs MCC MNC LAC cellId ti voltage signal alarm lang
      SplitLocation
      SplitStatus
      puts "  > date [format {%04d-%02d-%02d %02d:%02d:%02d} [expr {$year + 2000}] $month $day $hour $minute $second]"
      puts "  > gpsLen $gpsLen gpsSat $gpsSat lat [format %.6f [expr {$lat / 1800000.0}]] lon [format %.6f [expr {$lon / 1800000.0}]] speed $speed gpsRtDp $gpsRtDp gpsPos $gpsPos ewLon $ewLon snLat $snLat course $course"
      puts "  > Mobile Country Code MCC $MCC Mobile Network Code MNC $MNC Location Area Code LAC $LAC cellId [scan $cellId %x]"
      puts "  > tiCurcit $tiCurcit tiGPS $tiGPS tiAlarm $tiAlarm tiCharge $tiCharge tiACC $tiACC tiActive $tiActive voltage $voltage signal $signal alarm $alarm lang $lang"
    }
    1a {
      # 0x1a GPS, query address information by phone number
      puts "  > 0x1a GPS, query address information by phone number"
    }
    80 {
      # 0x80 Command information sent by the server to the terminal
      puts "  > 0x80 Command information sent by the server to the terminal"
    }
    default {
      puts "  > unknown protocol"
    }
  }
  Answer $fd $protocol $msgId
}


proc ClientClose { fd } {
  global cData

  array unset cData $fd,*
  catch {close $fd}
  puts "$fd closed"
}


proc ClientRead { fd } {
  global cData

  if {[eof $fd]} {
    puts "$fd EOF"
    ClientClose $fd
  } else {
    set raw [read $fd]
    if {[string length $raw] >= 0} {
      append cData($fd,raw) $raw
      puts "$fd read    [string length $raw] [binary encode hex $raw]"
      EncodeData $fd
    }
  }
}


proc ClientConnect { fd addr port } {
  global cData

  puts "$fd connect $addr $port"
  set cData($fd,raw) {}
  set cData($fd,IMEI) {}
  fconfigure $fd -blocking 0 -buffering none -encoding binary -translation binary
  fileevent $fd readable [list ClientRead $fd]
}


socket -server ClientConnect 7700
vwait ::forever

