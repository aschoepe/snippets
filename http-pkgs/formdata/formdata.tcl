#
# found at http://wiki.tcl.tk/13675
#
# 2005-02-27 Colin McCormack
# 2005-06-02 Benny Riefenstahl
# 2005-08-09 Erland Lewin
#
# $Id: formdata.tcl,v 1.1 2005/08/09 13:09:25 alex Exp alex $
#

namespace eval form-data {}

proc form-data::compose {partv {type multipart/form-data}} {
  upvar 1 $partv parts

  set mime [mime::initialize -canonical $type -parts $parts]
  set packaged [mime::buildmessage $mime]
  foreach part $parts {
    mime::finalize $part
  }
  mime::finalize $mime

  return $packaged
}

proc form-data::add_binary {partv name filename value type} {
  upvar 1 $partv parts
  set disposition "form-data; name=\"${name}\"; filename=\"$filename\""
  lappend parts [mime::initialize -canonical $type -string $value -encoding binary -header [list Content-Disposition $disposition]]
}

proc form-data::add_field {partv name value} {
  upvar 1 $partv parts
  set disposition "form-data; name=\"${name}\""
  lappend parts [mime::initialize -canonical text/plain -string $value -header [list Content-Disposition $disposition]]
}

proc form-data::format {name filename value type args} {
  set parts {}
  foreach {n v} $args {
    add_field parts $n $v
  }
  add_binary parts $name $filename $value $type
  return [compose parts]
}

proc form-data::post {url file content} {
  set field fileName
  set type {}
  set params {}
  set headers {}

  set message [eval [list form-data::format $field [file tail $file] $content $type] $params]

  set headerEnd [string first "\r\n\r\n" $message]
  incr headerEnd 1
  set bodystart [expr $headerEnd + 3]
  set headers_raw [string range $message 0 $headerEnd]
  set body [string range $message $bodystart end]
  set headers_raw [string map {"\r\n " " " "\r\n" "\n"} $headers_raw]
  regsub {  +} $headers_raw " " headers_raw

  foreach line [split $headers_raw "\n"] {
    regexp {^([^:]+): (.*)$} $line all label value
    lappend headers $label $value
  }

  array set ha $headers
  set content_type $ha(Content-Type)
  unset ha(Content-Type)
  set headers [array get ha]

  set token [http::geturl $url -type $content_type -binary true -headers $headers -query $body]
  http::wait $token

  return $token
}


package provide form-data 1.0
