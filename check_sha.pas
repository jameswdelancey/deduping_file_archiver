// Copyright 2022 James Delancey

// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

{$mode objfpc}{$H+}{$J-}{$apptype CONSOLE}

unit check_sha;

interface

uses entities;

function paCheckSha(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;

implementation

uses sysutils, StrUtils, dateutils, pa_Zlib1;

const 
  SHA256_DIGEST_LENGTH = 32;
  SHA_LBLOCK = 16;

type 
  SHA_LONG = uint32;
  //  SHA256_CTX = packed record
  //    h: array [0..Pred(8)] of SHA_LONG;
  //    Nl: SHA_LONG;
  //    Nh: SHA_LONG;
  //    data: array [0..Pred(SHA_LBLOCK)] of SHA_LONG;
  //    num: uint32;
  //    md_len: uint32;
  //  end;
  SHA256_CTX = array [0..256] of uint8;
  pSHA256_CTX = ^SHA256_CTX;
  hash_t = array [0..31] of uint8;
  phash = ^hash_t;
  data_t = array [0..(16384*4)-1] of uint8;
  pdata = ^data_t;

function SHA256_Init (c: pSHA256_CTX): longint;
cdecl;
external 'libcrypto.dll';

function SHA256_Update (c: pSHA256_CTX; data: pdata; len: longint): longint;
cdecl;
external 'libcrypto.dll';

function SHA256_Final (hash:phash; c: pSHA256_CTX): longint;
cdecl;
external 'libcrypto.dll';

function ByteArrayToHexString(BA: hash_t; Sep: string): string;

var 
  i: integer;
begin
  result := '';
  for i:=0 to 31 do
    result := result + IntToHex(BA[i], 2);
end;


function paCheckSha(var context: context_t; var blobs_row: paBlob_t): paExitStatus_t;

var 
  sha256: SHA256_CTX;
  nb: longint = 0;
  buffer: array [0..(16384*4)-1] of char;

  bhash: hash_t;
  shash: ansistring = '';

  // fh: Thandle;
  fh: gzFile;
  fn: ansistring;

  updated_at: longint = 0;
begin
  SHA256_Init(@sha256);
  if context.is_dos then
    begin
      if blobs_row.path[2] = '/' then
        blobs_row.path[2] := '\';
      if blobs_row.path[5] = '/' then
        blobs_row.path[5] := '\';
    end;

  // fh := FileOpen(context.filestorage_root + blobs_row.path, fmOpenRead);
  fn := context.filestorage_root + blobs_row.path;
  fh := gzopen(pchar(fn), pchar('rb'));
  if context.debug then
    writeln(stderr, 'handle is: ', fh);
  if fh < 0 then
    begin
      writeln(stderr, 'cannot open file ', context.filestorage_root + blobs_row.path);
      halt(1);
    end;

  // nb := FileRead(fh, buffer, 16384*4);
  nb := gzread(fh, buffer, 16384*4);
  if context.debug then
    writeln(stderr, 'nb is: ', nb);
  if nb < 0 then
    begin
      writeln(stderr, 'cannot read file ', context.filestorage_root + blobs_row.path);
      // halt(1); // will fail below and update db as not a match
    end
  else
    begin
      while nb > 0 do
        begin
          SHA256_Update(@sha256,@buffer,nb);
          // nb := FileRead(fh, buffer, 16384*4);
          nb := gzread(fh, buffer, 16384*4);
          if context.debug then
            writeln(stderr, 'nb is: ', nb);
        end;
    end;
  // FileClose(fh);
  gzclose(fh);

  SHA256_Final(@bhash,@sha256);

  if context.debug then
  begin
    // e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    shash := ByteArrayToHexString(bhash, shash);
    writeln(stderr, 'the hash is: ', shash);
  end;
  updated_at := DateTimeToUnix(Now());
  if comparemem(@bhash, @blobs_row.sha256, 32) then
    begin
      if context.debug then
        writeln(stderr, 'the hash is a match');
      blobs_row.updated_at := updated_at;
      blobs_row.sha2_last_checked := updated_at;
      blobs_row.record_status := 0; // sha match
    end
  else
    begin
      blobs_row.updated_at := updated_at;
      blobs_row.sha2_last_checked := updated_at;
      blobs_row.record_status := 1; // sha has no match
    end;
  Result := paEXIT_SUCCESS;
end;

end.
