(declare-project
  :name "janorth"
  :description "A basic FORTH interpreter in Janet")

(declare-source
  :source ["/src/janorth.janet"])

(declare-executable
 :name "janorth"
 :entry "/src/janorth.janet"
 :install true)
