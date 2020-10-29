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
  TksIdHttp = class(TIdHTTP);

  TksAwsIndyHttp = class(TInterfacedObject, IksAwsHttp)
  private
    function CreateHttp(AHeaders: TStrings): TksIdHttp;
  protected
    function Head(AUrl: string; AHeaders: TStrings): IksAwsHttpResponse;
    function Get(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Put(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Post(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Delete(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
  end;

function CreateksAwsHttpIndy: IksAwsHttp;
begin
  Result := TksAwsIndyHttp.Create;
end;

{ TksAwsIndyHttp }

function ResponseToKsResponse(AResponse: TIdHTTPResponse): IksAwsHttpResponse;
begin
  Result := CreateAwsHttpResponse;
  Result.ContentStream := AResponse.ContentStream;
  Result.StatusCode := AResponse.ResponseCode;
  Result.ETag := AResponse.ETag;
  Result.LastModified := DateToStr(AResponse.LastModified);
  Result.HeaderValue['x-amz-bucket-region'] := AResponse.RawHeaders.Values['x-amz-bucket-region'];
end;

function TksAwsIndyHttp.CreateHttp(AHeaders: TStrings): TksIdHttp;
var
  ICount: integer;
begin
  Result := TksIdHttp.Create;
  Result.HandleRedirects := False;
  Result.IOHandler := ASsl;
  Result.HTTPOptions := Result.HTTPOptions + [hoForceEncodeParams];
  for ICount := 0 to AHeaders.Count-1 do
    Result.Request.CustomHeaders.Values[AHeaders.Names[ICount]] := AHeaders.ValueFromIndex[ICount];
end;

function TksAwsIndyHttp.Delete(AUrl: string; AHeaders: TStrings;
  const AResponseStream: TStream): IksAwsHttpResponse;
var
  AHttp: TIdHttp;
begin
  AHttp := CreateHttp(AHeaders);
  AHttp.Delete(AUrl);
  Result := ResponseToKsResponse(AHttp.Response);
end;

function TksAwsIndyHttp.Get(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
var
  AHttp: TIdHttp;
  AStream: TStream;
begin
  AStream := TMemoryStream.Create;
  AHttp := CreateHttp(AHeaders);
  try
    AHttp.Get(AUrl, AStream);
    AStream.Position := 0;
    if AResponseStream <> nil then
    begin
      AResponseStream.CopyFrom(AStream, AStream.Size);
      AResponseStream.Position := 0;
      AStream.Position := 0;
    end;
    Result  := ResponseToKsResponse(AHttp.Response);
    Result.ContentStream := AStream;
  finally
    AHttp.Free;
    AStream.Free
  end;
end;

function TksAwsIndyHttp.Head(AUrl: string; AHeaders: TStrings): IksAwsHttpResponse;
var
  AHttp: TksIdHttp;
begin
  AHttp := CreateHttp(AHeaders);
  try
    //AHttp.Head(AUrl);
    AHttp.DoRequest(Id_HTTPMethodHead, AURL, nil, nil, [301]);
    Result  := ResponseToKsResponse(AHttp.Response);
  finally
    AHttp.Free;
  end;
end;

function TksAwsIndyHttp.Post(AUrl, APayload: string; AHeaders: TStrings;
  const AResponseStream: TStream = nil): IksAwsHttpResponse;
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
  const AResponseStream: TStream): IksAwsHttpResponse;
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

