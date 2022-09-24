// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit create_db;

interface

uses entities, pa_sqlite3;

function paCreateDatabase(var context: context_t): paExitStatus_t;

implementation

type 
  p = array of pchar;

const 
  s: p = (
          'CREATE TABLE directories ( '+
          'id                    INTEGER PRIMARY KEY     NOT NULL, '+ // pri key
          // unixepoch
          'created_at            INTEGER                 NOT NULL, '+
          // unixepoch
          'updated_at            INTEGER                 NOT NULL, '+
          // fk to self
          'parent_directory_id   INTEGER                 NOT NULL, '+
          'legacy_path           TEXT                    NOT NULL, '+ // utf-8
          // unixepoch
          'accessed              INTEGER                 NOT NULL, '+
          // unixepoch
          'modified              INTEGER                 NOT NULL, '+
          // unixepoch
          'created               INTEGER                 NOT NULL, '+
          // int status code
          'record_status         INTEGER                 NOT NULL, '+
          // boolean
          'is_deleted            INTEGER                 NOT NULL );',

          'CREATE TABLE files ( '+
          'id                    INTEGER PRIMARY KEY     NOT NULL, '+ // pri key
          // unixepoch
          'created_at            INTEGER                 NOT NULL, '+
          // unixepoch
          'updated_at            INTEGER                 NOT NULL, '+
          // foreign key to blob pri key
          'blob_id               INTEGER                 NOT NULL, '+
          // foreign key to directory pri key
          'directory_id          INTEGER                 NOT NULL, '+
          // utf-8 256 zs char
          'short_file_name       TEXT                    NOT NULL, '+
          // utf-8 1024 zs char
          'file_name_extension   TEXT                    NOT NULL, '+
          // unixtime
          'accessed              INTEGER                 NOT NULL, '+
          // unixtime
          'modified              INTEGER                 NOT NULL, '+
          // unixtime
          'created               INTEGER                 NOT NULL, '+
          'record_status         INTEGER                 NOT NULL, '+ // int
          // boolean
          'is_deleted            INTEGER                 NOT NULL );',

          'CREATE TABLE blobs ( '+
          'id                    INTEGER PRIMARY KEY     NOT NULL, '+ // pri key
          // unixtime
          'created_at            INTEGER                 NOT NULL, '+
          // unixtime
          'updated_at            INTEGER                 NOT NULL, '+
          // utf-8 1024 zs char
          'path                  TEXT                    NOT NULL, '+
          // long long
          'size                  INTEGER                 NOT NULL, '+
          // 32 uint8
          'sha256                BLOB                    NOT NULL, '+
          // unixtime
          'sha2_last_checked     INTEGER                 NOT NULL, '+
          'record_status         INTEGER                 NOT NULL, '+ // int
          'is_compressed         INTEGER                 NOT NULL, '+ // boolean
          // 255 zs char
          'compression_type      TEXT                    NOT NULL, '+
          'par2_exists           INTEGER                 NOT NULL, '+ // BOOL
          'par2_error            INTEGER                 NOT NULL, '+ // BOOL
          'par2_redundancy_pct   INTEGER                 NOT NULL, '+ // int
          // unixtime
          'par2_last_checked     INTEGER                 NOT NULL, '+
          'is_deleted            INTEGER                 NOT NULL );', // bool

          'CREATE UNIQUE INDEX blobmodel_sha256 ON blobs (sha256);',

          'CREATE UNIQUE INDEX blobmodel_path on blobs (path);',

          'CREATE INDEX directorymodel_parent_directory_id ON directories '+
          '(parent_directory_id);',

          'CREATE UNIQUE INDEX directorymodel_legacy_path ON '+
          'directories (legacy_path);',

          'CREATE INDEX filemodel_blob_id ON files (blob_id);',

          'CREATE INDEX blobmodel_sha2_last_checked ON blobs (sha2_last_checked);',
          'CREATE INDEX blobmodel_par2_last_checked ON blobs (par2_last_checked);',

          'CREATE UNIQUE INDEX filemodel_directory_id_short_file_name '+
          'ON files (directory_id, short_file_name);'
         );

function paCreateDatabase(var context: context_t): paExitStatus_t;

var 
  sql: pchar;
  rc: longint;
  zErrMsg: pchar;
  db: psqlite3;
begin
  if context.debug then
    begin
      writeln(stderr, 'could not map the proper command in the '+
              'command router');
    end;

  rc := sqlite3_open(pchar(context.db_filename), @db);
  if db = nil then
    writeln(stderr, 'db is null, error opening db.');
  if rc <> 0 then
      WriteLn(stderr, 'Failed to open DB. Existing.')
  else
      WriteLn(stderr, 'Opened database successfully');

  for sql in s do
    begin
      rc := sqlite3_exec(db,pchar(sql),nil,nil,@zErrMsg);
      if rc<>0 then
        begin
          writeln(stderr,'SQL error: creating table with error: ', zErrMsg, '. exiting.');
          sqlite3_free(zErrMsg);
          zErrMsg := nil;
        end
      else
        begin
          writeln(stderr,'Table created successfully');
        end
    end;
  Result := paCOMMAND_ROUTER_NO_MATCH;
  Result := paEXIT_SUCCESS;
end;

end.
