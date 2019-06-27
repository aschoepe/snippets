set v(http,data) {<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE fmresultset PUBLIC "-//FMI//DTD fmresultset//EN" "https://156.67.166.120:14343/fmi/xml/fmresultset.dtd">
<fmresultset xmlns="http://www.filemaker.com/xml/fmresultset" version="1.0">
  <error code="0"/>
  <product build="4/7/2016" name="FileMaker Web Publishing Engine" version="15.0.1.137"/>
  <datasource database="sd" date-format="MM/dd/yyyy" layout="test" table="test" time-format="HH:mm:ss" timestamp-format="MM/dd/yyyy HH:mm:ss" total-count="689"/>
  <metadata>
    <field-definition auto-enter="yes" four-digit-year="no" global="no" max-repeat="1" name="recid" not-empty="yes" numeric-only="yes" result="number" time-of-day="no" type="normal"/>
    <field-definition auto-enter="yes" four-digit-year="no" global="no" max-repeat="1" name="rgnr" not-empty="no" numeric-only="no" result="text" time-of-day="no" type="normal"/>
    <field-definition auto-enter="yes" four-digit-year="no" global="no" max-repeat="10" name="preis" not-empty="no" numeric-only="no" result="number" time-of-day="no" type="normal"/>
  </metadata>
  <resultset count="1" fetch-size="1">
    <record mod-id="0" record-id="756">
      <field name="recid">
        <data>81</data>
      </field>
      <field name="rgnr">
        <data>B10042</data>
      </field>
      <field name="preis">
        <data>14,95</data>
        <data>29,95</data>
        <data/>
        <data/>
        <data/>
      </field>
    </record>
  </resultset>
</fmresultset>}

# fmresultset -> resultset -> record -> field[name="rgnr"]

package require tdom
dom parse $v(http,data) doc
set root [$doc documentElement]
set node [lindex [$root selectNodes -namespaces [list X [$root namespaceURI]] {//X:field[@name='rgnr']/X:data/text()}] 0]
puts rgnr=[$node nodeValue]
