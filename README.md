renderable
==========

[![Build Status](https://travis-ci.org/schell/renderable.svg?branch=master)](https://travis-ci.org/schell/renderable)
[![Hackage](https://img.shields.io/hackage/v/renderable.svg)](http://hackage.haskell.org/package/renderable)

The `renderable` package provides a method for managing resources of a
rendering system. Resources are allocated according to a strategy and released
automatically when your renderable data changes. These changes are detected 
during each draw call based on the hash of your renderable datatype.

This package is meant to be pulled in as a portion of your rendering system. It
aims to ease the task of managing allocation of resources over time as the value 
of your renderable datatype changes.
