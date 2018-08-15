package require tcom

set path {C:\Dokumente und Einstellungen\alex\Desktop\outlook}

# http://msdn2.microsoft.com/en-us/library/aa219371(office.11).aspx

proc BuildArray { v l } {
  upvar #0 $v my
  foreach {a b} $l {
    set my($a) $b
    set my($b) $a
  }
}

BuildArray olSaveAsType {
  olDoc 4
  olHTML 5
  olICal 8
  olMSG 3
  olMSGUnicode 9
  olRTF 1
  olTemplate 2
  olTXT 0
  olVCal 7
  olVCard 6
}

BuildArray olItemType {
  olAppointmentItem 1
  olContactItem 2
  olDistributionListItem 7
  olJournalItem 4
  olMailItem 0
  olNoteItem 5
  olPostItem 6
  olTaskItem 3
}

BuildArray olDefaultFolders {
  olFolderCalendar 9
  olFolderContacts 10
  olFolderDeletedItems 3
  olFolderDrafts 16
  olFolderInbox 6
  olFolderJournal 11
  olFolderJunk 23
  olFolderNotes 12
  olFolderOutbox 4
  olFolderSentMail 5
  olFolderTasks 13
  olPublicFoldersAllPublicFolders 18
  olFolderConflicts 19
  olFolderLocalFailures 21
  olFolderServerFailures 22
  olFolderSyncIssues 20
}

BuildArray olObjectClass {
  olAction 32
  olActions 33
  olAddressEntries 21
  olAddressEntry 8
  olAddressList 7
  olAddressLists 20
  olApplication 0
  olAppointment 26
  olAttachment 5
  olAttachments 18
  olConflict 117
  olConflicts 118
  olContact 40
  olDistributionList 69
  olDocument 41
  olException 30
  olExceptions 29
  olExplorer 34
  olExplorers 60
  olFolder 2
  olFolders 15
  olFormDescription 37
  olInspector 35
  olInspectors 61
  olItemProperties 98
  olItemProperty 99
  olItems 16
  olJournal 42
  olLink 75
  olLinks 76
  olMail 43
  olMeetingCancellation 54
  olMeetingRequest 53
  olMeetingResponseNegative 55
  olMeetingResponsePositive 56
  olMeetingResponseTentative 57
  olNamespace 1
  olNote 44
  olOutlookBarGroup 66
  olOutlookBarGroups 65
  olOutlookBarPane 63
  olOutlookBarShortcut 68
  olOutlookBarShortcuts 67
  olOutlookBarStorage 64
  olPages 36
  olPanes 62
  olPost 45
  olPropertyPages 71
  olPropertyPageSite 70
  olRecipient 4
  olRecipients 17
  olRecurrencePattern 28
  olReminder 101
  olReminders 100
  olRemote 47
  olReport 46
  olResults 78
  olSearch 77
  olSelection 74
  olSyncObject 72
  olSyncObjects 73
  olTask 48
  olTaskRequest 49
  olTaskRequestAccept 51
  olTaskRequestDecline 52
  olTaskRequestUpdate 50
  olUserProperties 38
  olUserProperty 39
  olView 80
  olViews 79
}

proc ProcessItems { obj } {

  set iobj [$obj Items]
  set icount [$iobj Count]

  puts icount=$icount

  for {set i 1} {$i <= $icount} {incr i} {
    set item [$iobj Item $i]
    set fn [$item FullName]
    if {[string trim $fn] == ""} {
      set fn [$item CompanyName]
      if {[string trim $fn] == ""} {
	set fn $i
      }
    }
    set fn [string map {/ _} $fn]
    $item SaveAs [file join $::path ${fn}.vcf] $::olSaveAsType(olVCard)
  }
}

file mkdir $path

set com(ol) [::tcom::ref createobject "Outlook.Application"]
set com(ol,session) [$com(ol) Session]
set com(ol,contactsFolder) [$com(ol,session) GetDefaultFolder $olDefaultFolders(olFolderContacts)]
ProcessItems $com(ol,contactsFolder)

if 0 {
package require tcom
set com(ol) [::tcom::ref createobject "Outlook.Application"]
set com(ol,session) [$com(ol) Session]
set com(ol,contactsFolder) [$com(ol,session) GetDefaultFolder 10]
set items [$com(ol,contactsFolder) Items]
set item [$items Item 1]
set itemprop [$item ItemProperties]
set count [$itemprop Count]
for {set i 0} {$i <= $count} {incr i} {
  set prop [$itemprop Item $i]
  puts ---$i---
  puts n=[$prop Name]
  puts v=[$prop Value]
}
}
