module Abstr.Tests
open Fuchu

open FsharpMyExtension
#if INTERACTIVE
#load @"..\..\src\Shared\Shared.fs"
#load @"..\..\src\Server\Abstr.fs"
#endif
open Shared
open Abstr
open FsharpMyExtension.ListZipperCircle2

let equal act exp msg = Assert.Equal(msg, exp, act)
