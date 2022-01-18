package require tdbc::mysql

set db [tdbc::mysql::connection new -user USER -passwd PASS -host localhost -db DB]


set count 0
set updateInterval 3
set avgCount 20
set avgRange [expr {$avgCount - 1}]

set statusVars {
  {Connections} @calc_connections
  {Traffic MB/s} @calc_traffic
  {Selects per Second} @calc_qps
  {InnoDB Reads per Second} @calc_innodb_reads_per_second
  {InnoDB Writes per Second} @calc_innodb_writes_per_second
  {InnoDB Buffer Usage %} @calc_ib_usage
  {Query Cache Usage %} @calc_qcache_usage
  {Key Efficiency %} @calc_key_efficiency
  {Slow Queries} Slow_queries
  {Aborted Clients} Aborted_clients
  {Max Connections} max_connections
  {Aborted Connects} Aborted_connects
  {Connection errors (accept)} Connection_errors_accept
  {Connection errors (internal)} Connection_errors_internal
  {Connection errors (max connections reached)} Connection_errors_max_connections
  {Connection errors (peer address)} Connection_errors_peer_address
  {Connection errors (select)} Connection_errors_select
  {Connection errors (tcpwrap)} Connection_errors_tcpwrap
  {Threads Created} Threads_created
  {Threads Running} Threads_running
  {Threads Connected} Threads_connected
  {Threads Cached} Threads_cached
  {MySQL Version} version
}

# {Query Cache Hit Rate} @calc_hitrate

set fd [open mysql.stat a+]

$db foreach row {
  show global variables where Variable_name in (
    'max_connections', 'version'
  )
} {
  set m([dict get $row Variable_name]) [dict get $row Value]
}

while {true} {
  $db foreach row {
    show global status where Variable_name in (
      'Key_reads', 'Key_read_requests',
      'Innodb_data_reads', 'Innodb_data_writes',
      'Bytes_sent', 'Bytes_received',
      'Threads_connected',
      'Com_select',
      'Qcache_hits', 'Qcache_inserts', 'Qcache_not_cached',
      'Qcache_free_blocks', 'Qcache_total_blocks',
      'Queries',
      'Innodb_buffer_pool_pages_free', 'Innodb_buffer_pool_pages_total',
      'Aborted_clients', 'Aborted_connects', 
      'Threads_cached', 'Threads_connected', 'Threads_created', 'Threads_running',
      'Connections', 
      'Connection_errors_accept', 'Connection_errors_internal', 'Connection_errors_max_connections', 'Connection_errors_peer_address', 'Connection_errors_select',  'Connection_errors_tcpwrap', 
      'Delayed_errors',
      'Uptime', 'Uptime_since_flush_status', 
      'Slow_queries'
    )
  } {
    set m([dict get $row Variable_name]) [dict get $row Value]
  }

  if {$count} {
    set m(@calc_connections) [expr {($m(Connections) - $last(Connections)) / $updateInterval}]
    set m(@calc_traffic) [format {%.2f} [expr {($m(Bytes_sent) - $last(Bytes_sent)) / $updateInterval / 1024.0 / 1024.0}]]
    set m(@calc_key_efficiency) [format {%.1f} [expr {100 - ($m(Key_reads) / $m(Key_read_requests) * 100.0 / $updateInterval)}]]
    set m(@calc_qps) [expr {($m(Com_select) - $last(Com_select)) / $updateInterval}]
    set m(@calc_qcache_usage) [format {%.1f} [expr {100 * (1.0 * ($m(Qcache_total_blocks) - $m(Qcache_free_blocks)) / $m(Qcache_total_blocks))}]]
    set m(@calc_ib_usage) [format {%.1f} [expr {100 * (1.0 * ($m(Innodb_buffer_pool_pages_total) - $m(Innodb_buffer_pool_pages_free)) / $m(Innodb_buffer_pool_pages_total))}]]
    set m(@calc_innodb_reads_per_second) [expr {($m(Innodb_data_reads) - $last(Innodb_data_reads)) / $updateInterval}]
    set m(@calc_innodb_writes_per_second) [expr {($m(Innodb_data_writes) - $last(Innodb_data_writes)) / $updateInterval}]
    #set m(@calc_hitrate) [expr {100 * ($m(Qcache_hits) / ($m(Qcache_hits) + $m(Qcache_inserts) + $m(Qcache_not_cached)))}]

    foreach calc [array names m @*] {
      lappend a($calc) $m($calc)
      if {[llength $a($calc)] >= $avgCount} {
        set a($calc) [lrange $a($calc) end-$avgRange end]
      }
    }

    puts {}
    puts [format {%-43s %-8s %-8s} {} "$updateInterval sec" "[expr {$updateInterval * $avgCount}] sec"]
    set list [clock seconds]
    foreach {description varName} $statusVars {
      if {![info exists a($varName)] || $count <= $avgCount} {
        puts [format {%-43s %-8s} $description $m($varName)]
      } else {
        set sum 0
        foreach val $a($varName) {
          set sum [expr {$sum + $val}]
        }
        if {$varName ni {@calc_connections}} {
          set avg [expr {$sum / [llength $a($varName)]}]
        } else {
          set avg $sum
        }
        switch -- $varName {
          {@calc_key_efficiency} - {@calc_qcache_usage} - {@calc_ib_usage} {
            set avg [format {%.1f} $avg]
          }
          {@calc_traffic} {
            set avg [format {%.2f} $avg]
          }
        }
        puts [format {%-43s %-8s %-8s} $description $m($varName) $avg]
      }
      lappend list $m($varName)
    }
    puts $fd [join $list \t]
    flush $fd
  }

  foreach name {Connections Bytes_sent Com_select Innodb_data_reads Innodb_data_writes} {
    set last($name) $m($name)
  }
  after [expr {$updateInterval * 1000}]
  incr count
}

close $fd

$db destroy
