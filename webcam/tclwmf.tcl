package require tclwmf
package require Img

set save 0
set count 0

proc cb { args } {
  global cam img count save
  
  incr count
  wmf image $cam $img
  if {$save} {
     $img write webcam.png -format png
     set save 0
  }
}

set img [image create photo]
set dev0 [lindex [wmf devices] 0]
set cam [wmf open $dev0 cb]

label .img -image $img
pack .img
entry .cnt -textvariable count -state readonly
pack .cnt
button .start -text Start -command [subst {set count 0; wmf start $cam}]
button .stop -text Stop -command [subst {wmf stop $cam}]
button .save -text Save -command {set ::save 1}
pack .start -side left
pack .stop -side left
pack .save -side left

