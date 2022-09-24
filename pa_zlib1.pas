// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit pa_zlib1;

interface

uses entities, sysutils;

type
  gzFile= Thandle; 

function gzopen(fn: pchar; mode: pchar): gzFile;
cdecl;
external 'zlib1.dll';

function gzread(fh: gzFile; buffer: pchar; maxlen: longint): longint;
cdecl;
external 'zlib1.dll';

function gzclose(fh: gzFile): longint;
cdecl;
external 'zlib1.dll';


implementation

end.
