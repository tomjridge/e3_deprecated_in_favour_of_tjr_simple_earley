#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

def f(x:String) = {
  val mods = x.split(" ").map(
    x => s"""#use "$x";;"""
  ).mkString("\n")
  mods
}
val s : String = io.Source.stdin.getLines().next()
println(f(s))
