(set-env!
 :resource-paths #{"src"})

(task-options!
  pom {:project 'stern-brocot-tree
       :version "0.0.1"}
  jar {:manifest {"URL" "http://github.com/timrichardt/stern-brocot-tree"}})
