// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit command_parser;

interface

uses entities;

function parseArgs(var context: context_t): paExitStatus_t;

implementation

uses sysutils;

procedure getArgs(var argc: longint;  var argv: array of ansistring);

var 
  i: longint;

begin
  argc := paramCount();
  for i:=0 to argc do
    begin
      argv[i] := paramStr(i)
    end;
end;

procedure printArgs(var argc: longint;  var argv: array of ansistring);

var 
  i: longint;
begin
  for i:=0 to (argc-1) do
    begin
      write(stderr, argv[i], ',');
    end;
  writeln(stderr);
end;

function parseArgs(var context: context_t): paExitStatus_t;

var 
  optind: longint = 0;
begin
  context.mode := paUNSET;

  getArgs(context.argc, context.argv);
  // printArgs(argc, argv);
  if context.argc = 0 then
    begin
      writeln(stderr, 'Usage: ', context.argv[0], ' {create-db,check-sha,check-par}');
      writeln(stderr, 'options:');
      writeln(stderr, '        --debug            verbose logging');
      writeln(stderr, '        --is_dos           is dos filesystem');
      writeln(stderr, '        --db_filename      [short filename] filename for sqlite3 db');
      writeln(stderr, '        --filestorage_root [C:\example\] the filestorage location');
      Result := paARG_PARSING_ERROR_NO_ARGS;
    end
  else
    for optind:=1 to context.argc do
      begin
        if CompareStr(context.argv[optind], 'create-db') = 0 then
          begin
            context.mode := paCREATE_DB_MODE
          end
        else if CompareStr(context.argv[optind], 'check-sha') = 0 then
               begin
                 context.mode := paCHECK_SHA_MODE
               end
        else if CompareStr(context.argv[optind], 'check-par') = 0 then
               begin
                 context.mode := paCHECK_PAR_MODE
               end
        else if CompareStr(context.argv[optind], '--debug') = 0 then
               begin
                 context.debug := true;
               end
        else if CompareStr(context.argv[optind], '--is_dos') = 0 then
               begin
                 context.is_dos := true;
               end
        else if CompareStr(context.argv[optind], '--db_filename') = 0 then
               begin
                 context.db_filename := context.argv[optind+1];
               end
        else if CompareStr(context.argv[optind], '--filestorage_root') = 0 then
               begin
                 context.filestorage_root := context.argv[optind+1];
               end;
        Result := paEXIT_SUCCESS;
      end;
end;

end.
