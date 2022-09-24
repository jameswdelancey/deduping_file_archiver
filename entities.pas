// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit entities;

interface

type 
  paMode_t = (paUNSET, paCREATE_DB_MODE, paCHECK_SHA_MODE, paCHECK_PAR_MODE);
  paExitStatus_t = (paEXIT_SUCCESS=0, paARG_PARSING_ERROR_NO_ARGS, paCOMMAND_ROUTER_NO_MATCH, paDB_OPEN_ERROR,
  paSTOP_ITERATION);
  psqlite3= pointer;
  pStmt= Pointer;

  context_t = record
    argc: longint;
    argv: array [0..23] of ansistring;
    is_dos: boolean;
    debug: boolean;
    mode: paMode_t;
    db_filename: ansistring;
    db_handle: psqlite3;
    db_stmt_handle: pStmt;
    filestorage_root: ansistring;
    db_update_inc: longint;
    db_update_stmt_handle: pStmt;
  end;
  
  paBlob_t = record
    id: longint;
    created_at: longint;
    updated_at: longint;
    path: array [0..127] of char;
    size: int64;
    sha256: array [0..31] of char;
    sha2_last_checked: longint;
    record_status: longint;
    is_compressed: longint;
    compression_type: array [0..15] of char;
    par2_exists: longint;
    par2_error: longint;
    par2_last_checked: longint;
    is_deleted: longint;
  end;

procedure ent_print_entity(var context: context_t);

implementation

procedure ent_print_entity(var context: context_t);

var 
  i: longint;
begin
  writeln(stderr, 'argc: ', context.argc);
  for i:=0 to (argc-1) do
    begin
      writeln(stderr, 'argv[', i ,']: ', context.argv[i])
    end;
  writeln(stderr, 'debug: ', context.debug);
  writeln(stderr, 'mode: ', context.mode);
  writeln(stderr, 'db_filename: ', context.db_filename);
end;

end.
