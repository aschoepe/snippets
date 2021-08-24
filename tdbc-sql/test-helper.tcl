#!/bin/sh
#\
exec /usr/local/bin/tclsh8.6 "$0" "$@"

package require tdbc::mysql

set src [tdbc::mysql::connection new -user root -passwd {} -host localhost -db test]

source helper-gen-sql-statement.tcl

set table test
set ignore {cdate cinfo}

puts [sqlStm $src select $table]
puts [sqlStm $src select $table -tolower]
puts [sqlStm $src insert $table]
puts [sqlStm $src replace $table]
puts [sqlStm $src replace $table -ignore $ignore]
puts [sqlStm $src update $table]
puts [sqlStm $src update $table -ignore $ignore]
puts [sqlStm $src update $table -ignore $ignore -altnkey]
puts [sqlStm $src delete $table]
puts [sqlStm $src delete $table -altnkey]

# create table test (
#   info varchar(64) not null,
#   data int(11),
#   cdate datetime,
#   cinfo varchar(16),
#   udate datetime,
#   uinfo varchar(16),
#   primary key (info)
# )

# select info, data, cdate, cinfo, udate, uinfo from test where info = :info
# insert into test (info, data, cdate, cinfo, udate, uinfo) values (:info, :data, :cdate, :cinfo, :udate, :uinfo)
# replace into test (info, data, cdate, cinfo, udate, uinfo) values (:info, :data, :cdate, :cinfo, :udate, :uinfo)
# replace into test (info, data, udate, uinfo) values (:info, :data, :udate, :uinfo)
# update test set info = :info, data = :data, cdate = :cdate, cinfo = :cinfo, udate = :udate, uinfo = :uinfo where info = :info
# update test set info = :info, data = :data, udate = :udate, uinfo = :uinfo where info = :info
# update test set info = :info, data = :data, udate = :udate, uinfo = :uinfo where info = :KeYclAuSe_info
# delete from test where info = :info
# delete from test where info = :KeYclAuSe_info
