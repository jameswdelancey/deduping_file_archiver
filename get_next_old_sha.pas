// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit get_next_old_sha;

interface

uses entities, pa_sqlite3, sysutils, dateutils, strings;

function paOpenDatabase(var context: context_t): paExitStatus_t;
function paCloseDatabase(var context: context_t): paExitStatus_t;
function paGetNextBlob(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;
function paGetNextBlobStop(var context: context_t): paExitStatus_t;

function paPrepareExpiredSha(var context: context_t): paExitStatus_t;
function paUpdateBlobShaStart(var context: context_t): paExitStatus_t;
function paUpdateBlobShaStop(var context: context_t): paExitStatus_t;
function paUpdateNextBlobSha(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;

function paPrepareExpiredPar(var context: context_t): paExitStatus_t;
function paUpdateBlobParStart(var context: context_t): paExitStatus_t;
function paUpdateBlobParStop(var context: context_t): paExitStatus_t;
function paUpdateNextBlobPar(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;

implementation

function paOpenDatabase(var context: context_t): paExitStatus_t;
var
  rc: longint;
begin
  rc := sqlite3_open(pchar(context.db_filename), @context.db_handle);
  result := paDB_OPEN_ERROR;
  if context.db_handle = nil then
    writeln(stderr, 'db is null, error opening db.')
  else if rc <> 0 then
      WriteLn(stderr, 'Failed to open DB. Existing.')
  else
  begin
      if context.debug then
        WriteLn(stderr, 'Opened database successfully');
      result := paEXIT_SUCCESS;
  end;
end;

function paCloseDatabase(var context: context_t): paExitStatus_t;
var
  rc: longint;
begin
  rc := sqlite3_close(context.db_handle);
  result := paDB_OPEN_ERROR;
  if rc <> 0 then
      WriteLn(stderr, 'Failed to close DB. Existing.')
  else
  begin
      if context.debug then
        WriteLn(stderr, 'Closed database successfully');
      result := paEXIT_SUCCESS;
  end;
end;

function paPrepareExpiredSha(var context: context_t): paExitStatus_t;

const
  sha2_safe_age_seconds: int64 = 60*60*24*30*6; // 6 months
var
  sql: ansistring;
  rc: longint = 0;
begin
  sql := 'select * from blobs where sha2_last_checked < ' + inttostr(DateTimeToUnix(Now()) - sha2_safe_age_seconds) + '';
  if context.debug then
    writeln(stderr, 'sql statement is: ', sql);
  rc := sqlite3_prepare_v2(context.db_handle, pchar(sql), -1, @context.db_stmt_handle, nil);
  result := paDB_OPEN_ERROR;
  if context.db_stmt_handle = nil then
    writeln(stderr, 'db is null, error creating prepared statement.')
  else if rc <> 0 then
    begin
      WriteLn(stderr, 'Failed to open DB. Existing.');
      halt(1);
    end
  else
  begin
      if context.debug then
        WriteLn(stderr, 'Opened prepared statement successfully');
      result := paEXIT_SUCCESS;
  end;
end;

function paPrepareExpiredPar(var context: context_t): paExitStatus_t;

const
  par2_safe_age_seconds: int64 = 60*60*24*30*6; // 6 months
var
  sql: ansistring;
  rc: longint = 0;
begin
  sql := 'select * from blobs where par2_last_checked < ' + inttostr(DateTimeToUnix(Now()) - par2_safe_age_seconds);
  if context.debug then
    writeln(stderr, 'sql statement is: ', sql);
  rc := sqlite3_prepare_v2(context.db_handle, pchar(sql), -1, @context.db_stmt_handle, nil);
  result := paDB_OPEN_ERROR;
  if context.db_stmt_handle = nil then
    writeln(stderr, 'db is null, error creating prepared statement.')
  else if rc <> 0 then
    begin
      WriteLn(stderr, 'Failed to open DB. Existing.');
      halt(1);
    end
  else
  begin
      if context.debug then
        WriteLn(stderr, 'Opened prepared statement successfully');
      result := paEXIT_SUCCESS;
  end;
end;

function paGetNextBlob(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;
const
  SQLITE_ROW: longint = 100;
  SQLITE_DONE: longint = 101;
var 
  rc: longint = 0;
  sztemp: pchar = nil;
  sztemp2: pchar = nil;
  btemp: pointer = nil;
begin
  rc := sqlite3_step(context.db_stmt_handle);
  if rc = SQLITE_ROW then
    begin
      blobs_row.id := sqlite3_column_int(context.db_stmt_handle, 0);
      blobs_row.created_at := sqlite3_column_int(context.db_stmt_handle, 1);
      blobs_row.updated_at := sqlite3_column_int(context.db_stmt_handle, 2);
      
      sztemp := sqlite3_column_text(context.db_stmt_handle, 3);
      strcopy(blobs_row.path, sztemp);
      // sztemp := nil;
      
      blobs_row.size := sqlite3_column_int64(context.db_stmt_handle, 4);
      
      btemp := sqlite3_column_blob(context.db_stmt_handle, 5);
      move(btemp^, blobs_row.sha256, 32);
      // btemp := nil;
      
      blobs_row.sha2_last_checked := sqlite3_column_int(context.db_stmt_handle, 6);
      blobs_row.record_status := sqlite3_column_int(context.db_stmt_handle, 7);
      blobs_row.is_compressed := sqlite3_column_int(context.db_stmt_handle, 8);
      
      sztemp2 := sqlite3_column_text(context.db_stmt_handle, 9);
      strcopy(blobs_row.compression_type, sztemp2);
      // sztemp := nil;
      
      blobs_row.par2_exists := sqlite3_column_int(context.db_stmt_handle, 10);
      blobs_row.par2_error := sqlite3_column_int(context.db_stmt_handle, 11);
      blobs_row.par2_last_checked := sqlite3_column_int(context.db_stmt_handle, 12);
      blobs_row.is_deleted := sqlite3_column_int(context.db_stmt_handle, 13);
      Result := paEXIT_SUCCESS;
    end
  else if rc = SQLITE_DONE then
    begin
      Result := paSTOP_ITERATION;
    end
  else
    begin
      writeln(stderr, 'error in pa get next blob, error rc from sqlite3 is: ', rc, 'exiting.');
      halt(1);
    end;
end;

function paGetNextBlobStop(var context: context_t): paExitStatus_t;
var 
  rc: longint = 0;

begin
  rc := sqlite3_finalize(context.db_stmt_handle);
  if rc <> 0 then
    begin
      writeln(stderr, 'error finalizing update statement: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  if context.debug then
    writeln(stderr, 'finalized db stmt handle successfully');
  result := paEXIT_SUCCESS;
end;


function paUpdateBlobShaStart(var context: context_t): paExitStatus_t;
const
  sql_start_tran: pchar = 'begin transaction;';
  sql_update_blob_sha: pchar = 'update blobs set updated_at=?, record_status=?, sha2_last_checked=? where id = ?;';
var 
  rc: longint = 0;

  zErrMsg: array [0..255] of char;
begin
  if context.debug then
    writeln(stderr, 'starting transaction for db update');
  rc := sqlite3_exec(context.db_handle, sql_start_tran, nil, nil, @zErrMsg);
  if rc <> 0 then
    begin
      writeln(stderr, 'error starting transction for update, with error code: ', rc, '. error is: ', zErrMsg);
      halt(1);
    end;
  rc := sqlite3_prepare_v2(context.db_handle, sql_update_blob_sha, -1, @context.db_update_stmt_handle, nil);
  if rc <> 0 then
    begin
      writeln(stderr, 'error updating blob, with error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  result := paEXIT_SUCCESS;
end;

function paUpdateBlobShaStop(var context: context_t): paExitStatus_t;
const
  sql_stop_tran: pchar = 'commit;';
var 
  rc: longint = 0;

  zErrMsg: array [0..255] of char;
begin
  rc := sqlite3_exec(context.db_handle, sql_stop_tran, nil, nil, @zErrMsg);
  if rc <> 0 then
    begin
      writeln(stderr, 'error ending transction for update, with error code: ', rc, '. error is: ', zErrMsg);
      halt(1);
    end;
  if context.debug then
    writeln(stderr, 'committed db update stmt handle successfully');
  rc := sqlite3_finalize(context.db_update_stmt_handle);
  if rc <> 0 then
    begin
      writeln(stderr, 'error finalizing update statement: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  if context.debug then
    writeln(stderr, 'finalized db update stmt handle successfully');
  result := paEXIT_SUCCESS;
end;

function paUpdateNextBlobSha(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;
const
  sql_start_tran: pchar = 'begin transaction;';
  sql_stop_tran: pchar = 'commit;';

var 
  rc: longint = 0;

  zErrMsg: array [0..255] of char;
begin
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 1, blobs_row.updated_at);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 1 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 2, blobs_row.record_status);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 2 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 3, blobs_row.sha2_last_checked);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 3 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 4, blobs_row.id);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 4 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;

  rc := sqlite3_step(context.db_update_stmt_handle);
  if rc <> 101 then
    begin
      writeln(stderr, 'step errorno: ', rc, ': ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_reset(context.db_update_stmt_handle);
  if rc <> 0 then
    begin
      writeln(stderr, 'sqlite3 reset error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;

  if context.db_update_inc mod 100 = 0 then
    begin
      if context.debug then
        writeln(stderr, 'committing transaction for db update, at 100 updates');
      rc := sqlite3_exec(context.db_handle, sql_stop_tran, nil, nil, @zErrMsg);
      if rc <> 0 then
        begin
          writeln(stderr, 'error ending transction for update, with error code: ', rc, '. error is: ', zErrMsg);
          halt(1);
        end;
      rc := sqlite3_exec(context.db_handle, sql_start_tran, nil, nil, @zErrMsg);
      if rc <> 0 then
        begin
          writeln(stderr, 'error starting transction for update, with error code: ', rc, '. error is: ', zErrMsg);
          halt(1);
        end;
    end;

  inc(context.db_update_inc);
  Result := paEXIT_SUCCESS;
    
end;

//
function paUpdateBlobParStart(var context: context_t): paExitStatus_t;
const
  sql_start_tran: pchar = 'begin transaction;';
  sql_update_blob_sha: pchar = 'update blobs set updated_at=?, par2_error=?, par2_last_checked=? where id = ?;';
var 
  rc: longint = 0;

  zErrMsg: array [0..255] of char;
begin
  if context.debug then
    writeln(stderr, 'starting transaction for db update');
  rc := sqlite3_exec(context.db_handle, sql_start_tran, nil, nil, @zErrMsg);
  if rc <> 0 then
    begin
      writeln(stderr, 'error starting transction for update, with error code: ', rc, '. error is: ', zErrMsg);
      halt(1);
    end;
  rc := sqlite3_prepare_v2(context.db_handle, sql_update_blob_sha, -1, @context.db_update_stmt_handle, nil);
  if rc <> 0 then
    begin
      writeln(stderr, 'error updating blob, with error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  result := paEXIT_SUCCESS;
end;

function paUpdateBlobParStop(var context: context_t): paExitStatus_t;
begin
  result := paUpdateBlobShaStop(context);
end;

function paUpdateNextBlobPar(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;
const
  sql_start_tran: pchar = 'begin transaction;';
  sql_stop_tran: pchar = 'commit;';

var 
  rc: longint = 0;

  zErrMsg: array [0..255] of char;
begin
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 1, blobs_row.updated_at);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 1 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 2, blobs_row.par2_error);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 2 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 3, blobs_row.par2_last_checked);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 3 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_bind_int(context.db_update_stmt_handle, 4, blobs_row.id);
  if rc <> 0 then
    begin
      writeln(stderr, 'stmt, 4 error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;

  rc := sqlite3_step(context.db_update_stmt_handle);
  if rc <> 101 then
    begin
      writeln(stderr, 'step errorno: ', rc, ': ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;
  rc := sqlite3_reset(context.db_update_stmt_handle);
  if rc <> 0 then
    begin
      writeln(stderr, 'sqlite3 reset error code: ', rc, '. error is: ', sqlite3_errmsg(context.db_handle));
      halt(1);
    end;

  if context.db_update_inc mod 100 = 0 then
    begin
      if context.debug then
        writeln(stderr, 'committing transaction for db update, at 100 updates');
      rc := sqlite3_exec(context.db_handle, sql_stop_tran, nil, nil, @zErrMsg);
      if rc <> 0 then
        begin
          writeln(stderr, 'error ending transction for update, with error code: ', rc, '. error is: ', zErrMsg);
          halt(1);
        end;
      rc := sqlite3_exec(context.db_handle, sql_start_tran, nil, nil, @zErrMsg);
      if rc <> 0 then
        begin
          writeln(stderr, 'error starting transction for update, with error code: ', rc, '. error is: ', zErrMsg);
          halt(1);
        end;
    end;

  inc(context.db_update_inc);
  Result := paEXIT_SUCCESS;
    
end;


end.
