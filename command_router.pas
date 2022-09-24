// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit command_router;

interface

uses entities, create_db, check_sha, pa_par2, get_next_old_sha;

function paRunCommand(var context: context_t): paExitStatus_t;

implementation

function paRunCommand(var context: context_t): paExitStatus_t;
var
  blobs_row: paBlob_t;
begin
  if context.mode = paCREATE_DB_MODE then
    begin
      if context.debug then
        begin
          writeln(stderr, 'starting module: create database')
        end;
      paCreateDatabase(context);
    end
  else if context.mode = paCHECK_SHA_MODE then
         begin
           if context.debug then
             begin
               writeln(stderr, 'starting module: check sha')
             end;
         if paOpenDatabase(context) <> paEXIT_SUCCESS then
           begin
             writeln(stderr, 'error opening database');
             halt(1);
           end
         else if paPrepareExpiredSha(context) <> paEXIT_SUCCESS then
           begin
             writeln(stderr, 'error preparing expired sha');
             halt(1);
           end
         else if paUpdateBlobShaStart(context) <> paEXIT_SUCCESS then
           begin
             writeln(stderr, 'error preparing paUpdateBlobShaStart');
             halt(1);
           end
         else
           begin
             while paGetNextBlob(context, blobs_row) = paEXIT_SUCCESS do
             begin
               if context.debug then
                 writeln(stderr, 'printing blobs row');
               if paCheckSha(context, blobs_row) <> paEXIT_SUCCESS then
                 begin
                   writeln(stderr, 'check sha failed');
                   halt(1);
                 end
               else if paUpdateNextBlobSha(context, blobs_row) <> paEXIT_SUCCESS then
                 begin
                   writeln(stderr, 'check sha db updated failed');
                   halt(1);
                 end;  
             end;
             if context.debug then
               writeln(stderr, 'finished loop');
             if paGetNextBlobStop(context) <> paEXIT_SUCCESS then
               begin
                 writeln(stderr, 'error stopping next blob');
                 halt(1);
               end
             else if paUpdateBlobShaStop(context) <> paEXIT_SUCCESS then
               begin
                 writeln(stderr, 'error stopping blob sha');
                 halt(1);
               end
             else if paCloseDatabase(context) <> paEXIT_SUCCESS then
               begin
                 writeln(stderr, 'error closing database');
                 halt(1);
               end;
           end
         end
  else if context.mode = paCHECK_PAR_MODE then
         begin
           if context.debug then
             begin
               writeln(stderr, 'starting module: check par')
             end;
         if paOpenDatabase(context) <> paEXIT_SUCCESS then
           begin
             writeln(stderr, 'error opening database');
             halt(1);
           end
         else if paPrepareExpiredPar(context) <> paEXIT_SUCCESS then
           begin
             writeln(stderr, 'error preparing expired par');
             halt(1);
           end
         else if paUpdateBlobParStart(context) <> paEXIT_SUCCESS then
           begin
             writeln(stderr, 'error preparing paUpdateBlobParStart');
             halt(1);
           end
         else
           begin
             while paGetNextBlob(context, blobs_row) = paEXIT_SUCCESS do
             begin
               if context.debug then
                 writeln(stderr, 'printing blobs row');
               if paCheckPar(context, blobs_row) <> paEXIT_SUCCESS then
                 begin
                   writeln(stderr, 'check par failed');
                   halt(1);
                 end
               else if paUpdateNextBlobPar(context, blobs_row) <> paEXIT_SUCCESS then
                 begin
                   writeln(stderr, 'check par db updated failed');
                   halt(1);
                 end;  
             end;
             if context.debug then
               writeln(stderr, 'finished loop');
             if paGetNextBlobStop(context) <> paEXIT_SUCCESS then
               begin
                 writeln(stderr, 'error stopping next blob');
                 halt(1);
               end
             else if paUpdateBlobParStop(context) <> paEXIT_SUCCESS then
               begin
                 writeln(stderr, 'error stopping blob par');
                 halt(1);
               end
             else if paCloseDatabase(context) <> paEXIT_SUCCESS then
               begin
                 writeln(stderr, 'error closing database');
                 halt(1);
               end;
           end
         end

  else
    begin
      if context.debug then
        writeln(stderr, 'could not map the proper command in the command router'
        );
      Result := paCOMMAND_ROUTER_NO_MATCH;
    end;
  Result := paEXIT_SUCCESS;
end;

end.
