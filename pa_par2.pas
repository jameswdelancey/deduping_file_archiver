// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit pa_par2;

interface

uses entities;

function paCheckPar(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;


implementation

uses dateutils, sysutils;

function par2cmdline(argc: longint; argv: ppchar): longint;
cdecl;
external 'libpar2.dll';

function paCheckPar(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;

const
  argc: longint = 2;
var 
  rc: longint = 0;
  argv: array [0..1] of pchar = ('v', nil);
  updated_at: longint = 0;
begin
  if context.is_dos then
    begin
      if blobs_row.path[2] = '/' then
        blobs_row.path[2] := '\';
      if blobs_row.path[5] = '/' then
        blobs_row.path[5] := '\';
    end;
  argv[1] := pchar(context.filestorage_root + blobs_row.path);
  rc := par2cmdline(argc, argv);
  if context.debug then
    writeln(stderr, 'par2 rc is: ', rc);
  if rc <> 0 then
    begin
      writeln(stderr, 'cannot open file ', context.filestorage_root + blobs_row.path);
      // halt(1); // non-zero rc will signal this failure in the db
    end;
  updated_at := DateTimeToUnix(Now());
  blobs_row.updated_at := updated_at;
  blobs_row.par2_last_checked := updated_at;
  blobs_row.record_status := rc; // par2 rc
  Result := paEXIT_SUCCESS;
end;


end.
