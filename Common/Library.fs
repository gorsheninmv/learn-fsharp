namespace Common

[<AutoOpen>]
module Exceptions =

  exception InvalidCaseException

  exception InvalidCaseExceptionWithMessage of string
