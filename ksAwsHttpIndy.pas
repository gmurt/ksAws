{*******************************************************************************
*                                                                              *
*  ksAwsHttpIndy - ksAws TIdHttp Interface                                     *
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

unit ksAwsHttpIndy;

interface

uses ksAwsHttpIntf;

  function CreateksAwsHttpIndy: IksAwsHttp;

implementation

uses Classes, IdHttp, IdSSL, IdSSLOpenSSL, SysUtils;

var
  ASsl: TIdSSLIOHandlerSocketOpenSSL;

type
  TksAwsIndyHttp = class(TInterfacedObject, IksAwsHttp)
  private
    function CreateHttp(AHeaders: TStrings): TIdHttp;
  protected
    function Get(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
    function Put(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
    function Post(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
    function Delete(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
  end;

function CreateksAwsHttpIndy: IksAwsHttp;
begin
  Result := TksAwsIndyHttp.Create;
end;

{ TksAwsIndyHttp }

function ResponseToKsResponse(AIdResponse: TIdHTTPResponse): TksAwsHttpResponse;
begin
  Result.StatusCode := AIdResponse.ResponseCode;
  Result.ETag := AIdResponse.ETag;
  Result.LastModified := DateToStr(AIdResponse.LastModified);
end;

function TksAwsIndyHttp.CreateHttp(AHeaders: TStrings): TIdHttp;
var
  ICount: integer;
begin
  Result := TIdHTTP.Create;
  Result.IOHandler := ASsl;
  for ICount := 0 to AHeaders.Count-1 do
    Result.Request.CustomHeaders.Values[AHeaders.Names[ICount]] := AHeaders.ValueFromIndex[ICount];
end;

function TksAwsIndyHttp.Delete(AUrl: string; AHeaders: TStrings;
  const AResponseStream: TStream): TksAwsHttpResponse;
var
  AHttp: TIdHttp;
begin
  AHttp := CreateHttp(AHeaders);
  AHttp.Delete(AUrl, AResponseStream);
  Result.ContentText := '';
  Result := ResponseToKsResponse(AHttp.Response);
end;

function TksAwsIndyHttp.Get(AUrl: string; AHeaders: TStrings;
  const AResponseStream: TStream = nil): TksAwsHttpResponse;
var
  AHttp: TIdHttp;
begin
  AHttp := CreateHttp(AHeaders);
  try
    if AResponseStream <> nil then
      AHttp.Get(AUrl, AResponseStream)
    else
      Result.ContentText := AHttp.Get(AUrl);
    Result  := ResponseToKsResponse(AHttp.Response);
  finally
    AHttp.Free;
  end;
end;

function TksAwsIndyHttp.Post(AUrl, APayload: string; AHeaders: TStrings;
  const AResponseStream: TStream = nil): TksAwsHttpResponse;
var
  AHttp: TIdHttp;
  AContentStream: TStringStream;
  AResponse: TStringStream;
begin
  AHttp := CreateHttp(AHeaders);
  AContentStream := TStringStream.Create(APayload);
  AResponse := TStringStream.Create;
  try
    AHttp.Post(AUrl, AContentStream, AResponse);
    Result.ContentText := AResponse.DataString;
    if AResponseStream <> nil then
    begin
      AResponse.Position := 0;
      AResponseStream.CopyFrom(AResponse, AResponse.Size);
    end;
    Result  := ResponseToKsResponse(AHttp.Response);
  finally
    AHttp.Free;
    AContentStream.Free;
    AResponse.Free;
  end;
end;

function TksAwsIndyHttp.Put(AUrl, APayload: string; AHeaders: TStrings;
  const AResponseStream: TStream): TksAwsHttpResponse;
var
  AHttp: TIdHttp;
  AContentStream: TStringStream;
begin
  AHttp := CreateHttp(AHeaders);
  AContentStream := TStringStream.Create(APayload);
  try
    AHttp.Put(AUrl, AContentStream, AResponseStream);
    Result := ResponseToKsResponse(AHttp.Response);
  finally
    AHttp.Free;
    AContentStream.Free;
  end;
end;

initialization

  ASsl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

finalization

  ASsl.Free;

end.

