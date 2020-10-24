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

  function GetHashSHA256Hex( HashString: string): string;
  function CalculateHMACSHA256(const AValue: string; const AKey: TArray<Byte>): TArray<Byte>;
  function CalculateHMACSHA256Hex(const AValue: string; const AKey: TArray<Byte>): string;


implementation

uses System.Hash;

function GetHashSHA256Hex( HashString: string): string;
begin
  Result := THash.DigestAsString(THashSHA2.GetHashBytes(HashString));
end;

function CalculateHMACSHA256(const AValue: string; const AKey: TArray<Byte>): TArray<Byte>;
begin
  Result := THashSHA2.GetHMACAsBytes(AValue, AKey);
end;

function CalculateHMACSHA256Hex(const AValue: string; const AKey: TArray<Byte>): string;
begin
  Result := THash.DigestAsString(CalculateHMACSHA256(AValue, AKey));
end;

end.
