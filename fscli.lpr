// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

uses 
command_parser,
entities,
command_router,
windows;

var 
  context: context_t;
  ret: paExitStatus_t;

begin
  ZeroMemory(@context,SizeOf(context));
  ret := parseArgs(context);

  // Parse the arguments
  if ret <> paEXIT_SUCCESS then
    begin
      writeln(stderr, 'error parsing arguments with error: ', ret, ', exiting.')
      ;
      Halt(1);
    end;
  assert(context.mode <> paUNSET, 'The context.mode is UNSET.'+
         ' There is likely a problem with the parser, '+
         'but this should be covered bt the paEXIT_SUCCESS.'
  );
  assert(context.argc < 250,
         'The argc list is beyond the max size for the static array.');
  assert(length(context.db_filename) <> 0,
         'You must specify a --db_filename argument.');
  assert(length(context.filestorage_root) <> 0,
         'You must specify a --filestorage_root argument.');

  if context.debug then
    ent_print_entity(context);


  // Map the module entrypoint to the args in the context
  ret := paRunCommand(context);
  if ret <> paEXIT_SUCCESS then
    begin
      writeln(stderr, 'error in paruncommand with error: ', ret, ', exiting.')
      ;
      Halt(1);
    end;

end.
