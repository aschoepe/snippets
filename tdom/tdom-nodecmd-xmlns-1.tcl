package require tdom

dom createDocumentNS http://aaa a:foo doc
dom createNodeCmd textNode t
dom createNodeCmd -namespace http://bbb elementNode b:foo
dom createNodeCmd -namespace http://ccc elementNode c:foo

[$doc documentElement] appendFromScript {
   b:foo { t {Text 1} }
   b:foo {
     c:foo { t {Text 2} }
   }
}

puts [$doc asXML -indent 2]
