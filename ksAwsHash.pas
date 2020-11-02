{*******************************************************************************
*                                                                              *
*  ksAwsHash - Amazon Web Service Hashing Functions                            *
*                                                                              *
*  https://github.com/gmurt/ksAws                                              *
*                                                                              *
*  Copyright 2020 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksAwsHash;

interface

{$I include.inc}

uses Classes {$IFDEF USE_INDY} ,IdGlobal {$ENDIF};

  function URLEncode(AUrl: string): string;
  function ParamEncode(AParam: string): string;
  function GetHashSHA256Hex(AValue: string): string; overload;
  function GetHashSHA256Hex(AValue: TStream): string; overload;

  function GenerateSignature(ARequestTime: TDateTime; AStringToSign, APrivateKey, ARegionStr, AServiceName: string): string;


  {$IFDEF USE_INDY}
  function CalculateHMACSHA256Hex(const AValue: string; const AKey: TIdBytes): string;
  function CalculateHMACSHA256(const AValue: string; const AKey: TIdBytes): TIdBytes;
  {$ELSE}
  function CalculateHMACSHA256(const AValue: string; const AKey: TArray<Byte>): TArray<Byte>;
  function CalculateHMACSHA256Hex(const AValue: string; const AKey: TArray<Byte>): string;
  {$ENDIF}

implementation

uses ksAwsConst, SysUtils,

  {$IFDEF USE_INDY}
  IdHashSHA, IdHMAC, IdHMACSHA1, IdSSLOpenSSL, IdURI
  {$ELSE}
  System.Hash, System.NetEncoding
  {$ENDIF}
  ;

const
  C_UNSAFE_CHARS: array[1..27] of Byte = (Ord(' '), Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='), Ord('>'),
      Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#'),
      Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~'));

{$IFDEF USE_INDY}

function ParamEncode(AParam: string): string;
var
  AChar: Cardinal;
begin
  Result := TIdURI.ParamsEncode(AParam);
  for AChar in C_UNSAFE_CHARS do
  begin
    Result := StringReplace(Result, Char(AChar), '%'+Copy(ToHex([AChar]), 1, 2), [rfReplaceAll]);
  end;
end;

function URLEncode(AUrl: string): string;
begin
  Result := TIdURI.ParamsEncode(AUrl);
end;

function GenerateSignature(ARequestTime: TDateTime; AStringToSign, APrivateKey, ARegionStr, AServiceName: string): string;
var
  ADateKeyIndy, ARegionKeyIndy, AServiceKeyIndy, ASigningKeyIndy: TIdBytes;
begin
  ADateKeyIndy := CalculateHMACSHA256(FormatDateTime(C_SHORT_DATE_FORMAT, ARequestTime), IndyTextEncoding_UTF8.GetBytes('AWS4' + APrivateKey));
  ARegionKeyIndy := CalculateHMACSHA256(ARegionStr, ADateKeyIndy);
  AServiceKeyIndy := CalculateHMACSHA256(AServiceName, ARegionKeyIndy);
  ASigningKeyIndy := CalculateHMACSHA256('aws4_request', AServiceKeyIndy);
  Result := LowerCase(CalculateHMACSHA256Hex(AStringToSign, ASigningKeyIndy));
end;

function GetHashSHA256Hex(AValue: string): string;
var
  ASha256: TIdHashSHA256;
begin
  if TIdHashSHA256.IsAvailable then
  begin
   ASha256:= TIdHashSHA256.Create;
   try
     Result := LowerCase(ASha256.HashStringAsHex(AValue));
   finally
    ASha256.Free;
   end;
  end;
end;

function GetHashSHA256Hex(AValue: TStream): string;
var
  ASha256: TIdHashSHA256;
begin
  if TIdHashSHA256.IsAvailable then
  begin
    ASha256 := TIdHashSHA256.Create;
    try
      Result := LowerCase(ASha256.HashStreamAsHex(AValue));
    finally
      ASha256.Free;
    end;
  end;
end;

function CalculateHMACSHA256(const AValue: string; const AKey: TIdBytes): TIdBytes;
var
  hmac: TIdHMACSHA256;
begin
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  hmac := TIdHMACSHA256.Create;
  try

    hmac.Key := AKey;
    Result := hmac.HashValue(IndyTextEncoding_UTF8.GetBytes( AValue));



  finally
    hmac.Free;
  end;
end;

function CalculateHMACSHA256Hex(const AValue: string; const AKey: TIdBytes): string;
begin
  Result := ToHex(CalculateHMACSHA256(AValue, AKey));
end;

{$ELSE}

function ParamEncode(AParam: string): string;
begin
  Result := TNetEncoding.URL.EncodeQuery(AParam,  [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='), Ord('>'),
      Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#'),
      Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~')]);
end;

function UrlEncode(AUrl: string): string;
begin
  Result := AUrl;
end;

function GenerateSignature(ARequestTime: TDateTime; AStringToSign, APrivateKey, ARegionStr, AServiceName: string): string;var
  ADateKey, ARegionKey, AServiceKey, ASigningKey: TArray<Byte>;
begin
  ADateKey := CalculateHMACSHA256(FormatDateTime(C_SHORT_DATE_FORMAT, ARequestTime), TEncoding.UTF8.GetBytes('AWS4' + APrivateKey));
  ARegionKey := CalculateHMACSHA256(ARegionStr, ADateKey);
  AServiceKey := CalculateHMACSHA256(AServiceName, ARegionKey);
  ASigningKey := CalculateHMACSHA256('aws4_request', AServiceKey);
  Result := CalculateHMACSHA256Hex(AStringToSign, ASigningKey);
end;

function GetHashSHA256Hex(AValue: string): string;
begin
  Result := THash.DigestAsString(THashSHA2.GetHashBytes(AValue));
end;

function GetHashSHA256Hex(AValue: TStream): string;
begin
  AValue.Position := 0;
  Result := THash.DigestAsString(THashSHA2.GetHashBytes(AValue));
end;

function CalculateHMACSHA256(const AValue: string; const AKey: TArray<Byte>): TArray<Byte>;
begin
  Result := THashSHA2.GetHMACAsBytes(AValue, AKey);
end;

function CalculateHMACSHA256Hex(const AValue: string; const AKey: TArray<Byte>): string;
begin
  Result := THash.DigestAsString(CalculateHMACSHA256(AValue, AKey));
end;

{$ENDIF}

initialization

{$IFDEF USE_INDY}
  LoadOpenSSLLibrary;
{$ENDIF}

end.
