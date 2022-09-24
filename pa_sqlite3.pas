// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit pa_sqlite3;

interface

uses entities;

type
  ppsqlite3= ^psqlite3;
  ppStmt= ^pStmt;

function sqlite3_open(db_filename: Pchar; db: ppsqlite3): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_exec(db: psqlite3; sql: Pchar; cb: pointer; cb2: pointer; zErrMsg: ppchar): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_close(db: psqlite3): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_prepare_v2(
  db: psqlite3;            //* Database handle */
  zSql: pchar;  //* SQL statement, UTF-8 encoded */
  nByte: longint; // nByte: longint,              //* Maximum length of zSql in bytes. */
  sqlite3_stmt: ppStmt;  //* OUT: Statement handle */
  pzTail: Pointer  //const char **pzTail     //* OUT: Pointer to unused portion of zSql */
): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_step(sqlite3_stmt: pStmt): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_finalize(sqlite3_stmt: pStmt): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_column_int(sqlite3_stmt: pStmt; ilCol: longint): longint;
cdecl;
external 'sqlite3.dll';

function sqlite3_column_int64(sqlite3_stmt: pStmt; ilCol: longint): int64;
cdecl;
external 'sqlite3.dll';

function sqlite3_column_text(sqlite3_stmt: pStmt; ilCol: longint): pchar;
cdecl;
external 'sqlite3.dll';

function sqlite3_column_blob(sqlite3_stmt: pStmt; ilCol: longint): pchar;
cdecl;
external 'sqlite3.dll';

function sqlite3_reset(sqlite3_stmt: pStmt): longint;
cdecl;
external 'sqlite3.dll';

procedure sqlite3_free(zErrMsg: pchar);
cdecl;
external 'sqlite3.dll';

function sqlite3_errmsg(db: psqlite3): pchar;
cdecl;
external 'sqlite3.dll';

function sqlite3_bind_int(sqlite3_stmt: pStmt; i1: longint; i2: longint): longint;
cdecl;
external 'sqlite3.dll';

implementation

end.
