module Falka.FsYacc
open Yard.Generators.FsYaccPrinter

let print filename gr = 
  let s = Yard.Generators.FsYaccPrinter.Generator.generate gr filename
  System.IO.File.WriteAllLines(filename, [s])

